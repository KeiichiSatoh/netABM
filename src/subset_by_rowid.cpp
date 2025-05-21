#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame subset_by_rowid(DataFrame df, IntegerVector row_ids) {
  int n_rows = row_ids.size();
  int n_cols = df.size();

  // 結果格納用リスト
  List out(n_cols);
  CharacterVector col_names = df.names();

  for(int j = 0; j < n_cols; ++j){
    SEXP column = df[j];
    switch(TYPEOF(column)){
    case INTSXP: {
      IntegerVector col = column;
      IntegerVector out_col(n_rows);
      for (int i = 0; i < n_rows; ++i){
        out_col[i] = col[row_ids[i]];
      }
      out[j] = out_col;
      break;
    }
    case REALSXP: {
      NumericVector col = column;
      NumericVector out_col(n_rows);
      for(int i = 0; i < n_rows; ++i){
        out_col[i] = col[row_ids[i]];
      }
      out[j] = out_col;
      break;
    }
    case  STRSXP: {
      CharacterVector col = column;
      CharacterVector out_col(n_rows);
      for(int i = 0; i < n_rows; ++i){
        out_col[i] = col[row_ids[i]];
      }
      out[j] = out_col;
      break;
    }
    case LGLSXP: {
      LogicalVector col = column;
      LogicalVector out_col(n_rows);
      for(int i = 0; i < n_rows; ++i){
        out_col[i] = col[row_ids[i]];
      }
      out[j] = out_col;
      break;
    }
    default:
      stop("Unsupported column type.");
    }
  }

  out.attr("names") = col_names;
  out.attr("class") = "data.frame";
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -n_rows);
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
df <- data.frame(a = 1:10, b = letters[1:10], c = rnorm(10))
subset_by_rowid(df, c(1,2,3))
*/
