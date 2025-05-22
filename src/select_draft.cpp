#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

/**
 * @title Remove a column from a Numeric Matrix
 * @description
 * Removes a single column (by zero-based index) from a numeric matrix.
 * This function is used internally in \code{rABM} and is not intended for direct use.
 *
 * @param mat A numerix matrix.
 * @param col_to_remove An integer index (zero-based) of the column to remove.
 * @return A new numeric matrix with the specified column removed.
 * @keyword internal
 */
NumericMatrix remove_column(NumericMatrix mat, int col_to_remove) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  NumericMatrix result(nrow, ncol - 1);
  for (int i = 0; i < nrow; i++) {
    int new_col = 0;
    for (int j = 0; j < ncol; j++) {
      if (j == col_to_remove) continue;
      result(i, new_col++) = mat(i, j);
    }
  }
  return result;
}

/**
 * @title Weighted Draft Selection with preference-Based Filtering
 * @description
 * Performs a draft selection process where agents take turns selecting options
 * based on their numeric preferences. Once an option is chosen,
 * it becomes unavailable to the remaining agents.
 *
 * @param x A numeric matrix where rows represent agents and columns represent
 * options. Each element is a preference score. The matrix must not contain, NA,
 * NaN, or Inf. Optionally, row and column names can be provided. If missing,
 * numeric labels are assigned automatically.
 *
 * @param size A positive integer. The number of options each agent selects.
 * Default is 1.
 * @return A character vector of selected option names. The names attribute
 * indicates which agent made each selection.
 * If fewer than \code{size} options are available for an agent, NA is returned
 * for remaining slots.
 *
 * @details
 * The function processes agents sequentially. Each agent selects up to \code{size} options
 * based on preference-weighted random sampling among remaining available options.
 * Previously selected options are removed from the matrix before the next agent's turn.
 *
 * @examples
 * mat <- matrix(c(1,1,1,1,
 *                 1,2,3,1,
 *                 0,0,0,0,
 *                 0,0,0,0), 4, 4, byrow = TRUE)
 * dimnames(mat) <- list(1:4, paste0("O", 1:4))
 * select_draft(mat, size = 1)
 *
 * @export
 *
 */
// [[Rcpp::export]]
CharacterVector select_draft(NumericMatrix x, int size = 1) {
  //入力値の確認
  if (Rf_isNull(x)) {
    stop("x is NULL.");
  }
  // NA / NaN チェック
  if (is_true(any(is_na(x)))) {
    stop("x contains NA or NaN.");
  }
  // Inf / -Inf チェック
  if (is_true(any(is_infinite(x)))) {
    stop("x contains Inf or -Inf.");
  }
  // サイズの妥当性チェック
  if (size <= 0) {
    stop("size must be a positive integer.");
  }

  //行列の数を取得する
  int n_agent = x.nrow();
  int n_col = x.ncol();
  //行列の名前を取得する
  CharacterVector agent_names = rownames(x);
  CharacterVector option_names = colnames(x);
  //もしも行や列の名前が設定されていなければ設定する
  if(agent_names.size()==0){
    agent_names = CharacterVector(n_agent);
    for(int i = 0; i < n_agent; ++i){
      agent_names[i] = std::to_string(i + 1);
    }
    rownames(x) = agent_names;
  }
  if(option_names.size()==0){
    option_names = CharacterVector(n_col);
    for(int i = 0; i < n_col; i++){
      option_names[i] = std::to_string(i + 1);
    }
    colnames(x) = option_names;
  }

  //col_namesをコピーして、optionsベクトルを作成する
  CharacterVector options = option_names;

  //結果をまとめる空のベクトルを作成
  CharacterVector out(0);
  CharacterVector out_label(0);

  //エージェントをいったん仮に0番目とする
  for(int agent_idx = 0; agent_idx < n_agent; agent_idx++){
    //当該エージェントの選好ベクトル
    NumericVector agent_preference = x(agent_idx, _);
    //選好ベクトル値が0以上の数の場合にはsizeを調整して選ばせる
    LogicalVector eval_non_zero = agent_preference > 0;
    int n_possible_options = sum(eval_non_zero);
    //選ばれたオプションを入れる空のベクトルを用意
    CharacterVector selected_option(0);
    //選べる数があるかを先に確認
    if(n_possible_options < size){
      //選べるオプションがsizeよりも小さい場合
      //選べるオプションはそのまま選択させる
      for(int option_idx = 0; option_idx < eval_non_zero.size(); option_idx++){
        if(eval_non_zero[option_idx]){
          selected_option.push_back(options[option_idx]);
        }
      }
      //選択不能だった分はそのままNAを代入する
      for(int p = 0; p < (size - n_possible_options); p++){
        selected_option.push_back(NA_STRING);
      }
    }else{
      //選択対象がちゃんとsize以上はある場合
      //当該エージェントに通常通り選ばせる
      selected_option = sample(options, size, false, agent_preference);
    }
    //選択したエージェントのラベルを作成し、付加する
    String label = agent_names[agent_idx];
    for(int k = 0; k < size; k++){
      out.push_back(selected_option[k]);
      out_label.push_back(label);
    }
    //選ばれた選択肢を候補から外す
    for(int j = 0; j < selected_option.size(); j++){
      String value = selected_option[j];
      if(value != NA_STRING){
        IntegerVector matched_idx = match(CharacterVector::create(value), options);
        options.erase(matched_idx[0]-1);
        x = remove_column(x, matched_idx[0]-1);
      }
    }
  }
  //ラベルを付与する
  out.names() = out_label;
  return out;
}
