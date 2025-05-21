#include <Rcpp.h>
#include <random>
using namespace Rcpp;
// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
NumericMatrix cpp_calc_neib_prop(IntegerVector ego_ID,
                                 IntegerMatrix neib_list,
                                 IntegerVector neib_attr) {
  IntegerVector attr_type = unique(na_omit(neib_attr));
  int n_attr_type = attr_type.size();
  int n_agents = ego_ID.size();

  if (n_attr_type == 0) return NumericMatrix(n_agents, 0);

  NumericMatrix neib_eth_prop(n_agents, n_attr_type);

  for (int k = 0; k < n_attr_type; ++k) {
    for (int i = 0; i < n_agents; ++i) {
      int ego_i = ego_ID[i] - 1;
      if (ego_i < 0 || ego_i >= neib_list.nrow()) continue;

      IntegerVector ego_i_neib = neib_list(ego_i, _) - 1;
      std::vector<int> ego_i_neib_attr;

      for (int j = 0; j < ego_i_neib.size(); ++j) {
        int alt_id = ego_i_neib[j];
        if (alt_id >= 0 && alt_id < neib_attr.size()) {
          int attr_val = neib_attr[alt_id];
          if (attr_val > 0) {
            ego_i_neib_attr.push_back(attr_val);
          }
        }
      }

      if (!ego_i_neib_attr.empty()) {
        IntegerVector attr_vec = wrap(ego_i_neib_attr);
        LogicalVector neib_eth = attr_vec == attr_type[k];
        neib_eth_prop(i, k) = sum(neib_eth) / static_cast<double>(neib_eth.size());
      }
    }
  }

  return neib_eth_prop;
}


// [[Rcpp::export]]
List cpp_move_same_ethnicity(
    IntegerMatrix agent_profile,
    IntegerMatrix place_profile,
    IntegerMatrix neib_place_id) {

  // すべてのhouse ID
  IntegerVector place_ID = place_profile(_, 0);
  //Restrictionを先に取得しておく
  IntegerVector place_restriction = place_profile(_, 3);

  //エージェントごとに順にループを回す
  for(int i = 0; i < agent_profile.nrow(); i++){

    //エージェントの情報を取得する
    int agent_ID = agent_profile(i, 0);
    int agent_ethnicity = agent_profile(i, 1);
    int agent_SES = agent_profile(i, 2);
    int agent_preference_ethnicity = agent_profile(i, 3);
    int agent_current_house = agent_profile(i, 5);

    //候補となる家のリストを取得する
    IntegerVector current_resident_ID = place_profile(_, 4);

    IntegerVector candid_house_ID;
    for(int k = 0; k < place_profile.nrow(); k++){
      if(current_resident_ID[k] == 0 && (place_restriction[k]==0 || place_restriction[k]==agent_ethnicity)){
        candid_house_ID.push_back(place_ID[k]);
      }
    }

    //候補の場所が見つからない場合には次へ
    if(candid_house_ID.size()==0){
      continue;
    }

    //候補の家に現状の家も足す
    candid_house_ID.push_back(agent_current_house);

    //現状のすべての土地のethnicityのプロファイルを取得する
    IntegerVector neib_attr_ethnicity = place_profile(_, 5);
    //候補地の周囲のethnicityのプロファイルを取得する
    NumericMatrix neib_ethnicity_prop = cpp_calc_neib_prop(candid_house_ID,
                                                           neib_place_id,
                                                           neib_attr_ethnicity);

    //# 対象地のスコアを計算する
    NumericVector place_score = neib_ethnicity_prop(_, agent_ethnicity - 1) * agent_preference_ethnicity;
    NumericVector prop = exp(place_score)/sum(exp(place_score));

    //重み付きのランダム選択によって、移動先を選択する
    std::random_device rd;
    std::mt19937 gen(rd());
    std::discrete_distribution<> dist(prop.begin(), prop.end());
    int selected_index = dist(gen);
    int selected_place = candid_house_ID[selected_index];

    //移動を反映させる（house ID → 行番号）
    int old_row;
    int new_row;
    for(int j = 0; j < place_ID.size(); j++){
      if(agent_current_house==place_ID[j]){
        old_row = j;
      }
      if(selected_place==place_ID[j]){
        new_row = j;
      }
    }

    //古い家の記録を消す
    place_profile(old_row, 4) = 0;
    place_profile(old_row, 5) = 0;
    place_profile(old_row, 6) = 0;
    //新しい家に登記
    place_profile(new_row, 4) = agent_ID;
    place_profile(new_row, 5) = agent_ethnicity;
    place_profile(new_row, 6) = agent_SES;
    //エージェントの住んでいる場所を更新
    agent_profile(i, 5) = selected_place;
  }
  //エージェント単位のループここまで//

  //リターン
  return List::create(
    Named("agent_profile") = agent_profile,
    Named("place_profile") = place_profile
  );

}


/*** R
cpp_move_same_ethnicity(agent_profile = as.matrix(E$agent_profile),
                        place_profile = as.matrix(E$place_profile),
                        neib_place_id = as.matrix(G$neib_place_id))
*/
