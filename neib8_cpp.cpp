#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix neib8_cpp(const NumericMatrix& mat, bool include_ego = false) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();

  // Define the output matrix dimensions
  int num_neighbors = include_ego ? 9 : 8;
  NumericMatrix config(nrow * ncol, num_neighbors);

  // Function to check bounds and fetch value or return NA
  auto get_value = [&](int i, int j) -> double {
    if (i >= 0 && i < nrow && j >= 0 && j < ncol) {
      return mat(i, j);
    } else {
      return NA_REAL;
    }
  };

  // Fill the config matrix
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      int index = i * ncol + j;

      config(index, 0) = get_value(i - 1, j);     // U
      config(index, 1) = get_value(i - 1, j + 1); // UR
      config(index, 2) = get_value(i, j + 1);     // R
      config(index, 3) = get_value(i + 1, j + 1); // RD
      config(index, 4) = get_value(i + 1, j);     // D
      config(index, 5) = get_value(i + 1, j - 1); // LD
      config(index, 6) = get_value(i, j - 1);     // L
      config(index, 7) = get_value(i - 1, j - 1); // LU

      if (include_ego) {
        config(index, 8) = mat(i, j);             // ego
      }
    }
  }

  return config;
}
