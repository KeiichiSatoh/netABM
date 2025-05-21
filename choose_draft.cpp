#include <Rcpp.h>
#include <random> // C++標準のランダム関連パッケージ

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector choose_draft_cpp(IntegerVector x,
                               NumericMatrix prob,
                               int size = 1,
                               bool replace = false) {
  // probの行数チェック（xに対応しているか）
  if(prob.ncol() != x.size()){
    stop("Number of columns in 'prob' must match length of 'x'");
  }

  // 1行目の重みを手動でコピー
  NumericVector prob_i(x.size());
  for (int i = 0; i < x.size(); ++i) {
    prob_i[i] = prob(0, i);
  }

  // 重みの合計チェック
  double total = std::accumulate(prob_i.begin(), prob_i.end(), 0.0);
  if(total <= 0.0){
    stop("Sum of probabilities must be greater than 0");
  }

  // 乱数エンジンと重み付き分布のセットアップ
  std::random_device rd;          //乱数シードを生成
  std::mt19937 gen(rd());         //乱数生成器を初期化
  std::discrete_distribution<> dist(prob_i.begin(), prob_i.end()); //重み付き確率分布を構築

  // インデックスを選択
  int selected_index = dist(gen);

  // 結果をIntegerVectorで返す
  return IntegerVector::create(x[selected_index]);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
x <- c(1,2,3)
prob <- matrix(c(0.3,0.3,0.4), nrow = 1, ncol = 3)
choose_draft_cpp(x = x, prob = prob)
*/
