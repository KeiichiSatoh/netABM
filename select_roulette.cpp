#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP select_roulette(SEXP x, NumericVector score, int size = -1, bool replace = true, double min_score = 0.1) {
  int n = Rf_length(x); // xの長さを取得

  // sizeが-1ならxの長さを使用
  if (size == -1) {
    size = n;
  }

  // 負のスコアの補正
  double min_val = min(score);
  if (min_val < 0) {
    score = score + std::abs(min_val) + min_score;
  }

  // 確率を計算
  NumericVector prob = score / sum(score);

  // xの型を判定
  if (is<CharacterVector>(x)) {
    // 文字列または因子の場合
    CharacterVector x_char = as<CharacterVector>(x);
    CharacterVector result = sample(x_char, size, replace, prob);
    return result;
  } else {
    // 数値の場合
    NumericVector x_num = as<NumericVector>(x);
    NumericVector result = sample(x_num, size, replace, prob);
    return result;
  }
}
