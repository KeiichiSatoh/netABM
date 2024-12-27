#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix neib8_rcpp(NumericMatrix mat, IntegerMatrix posit_ind, bool include_ego) {
  int mat_rows = mat.nrow();
  int mat_cols = mat.ncol();

  int n = posit_ind.nrow();
  int num_neighbors = include_ego ? 9 : 8;
  NumericMatrix neib_mat(n, num_neighbors);

  // Compute neighbors (column-major order)
  for (int p = 0; p < n; ++p) {
    int row = posit_ind(p, 0) - 1;
    int col = posit_ind(p, 1) - 1;

    // First calculate in column-major order
    int indices[9][2] = {
      {row - 1, col - 1}, {row, col - 1}, {row + 1, col - 1},
      {row - 1, col},     {row, col},     {row + 1, col},
      {row - 1, col + 1}, {row, col + 1}, {row + 1, col + 1}
    };

    int idx = 0;
    for (int i = 0; i < 9; ++i) {
      if (!include_ego && i == 4) continue; // Skip ego if not included

      int new_row = indices[i][0];
      int new_col = indices[i][1];

      if (new_row >= 0 && new_row < mat_rows &&
          new_col >= 0 && new_col < mat_cols) {
        neib_mat(p, idx) = mat(new_row, new_col);
      } else {
        neib_mat(p, idx) = NA_REAL;
      }
      idx++;
    }
  }

  return neib_mat;
}
