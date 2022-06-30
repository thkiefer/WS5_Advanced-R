#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol(); // geht in einer Zeile; use .nrow/.ncol to get dim
  NumericVector out(nrow);
  
  for(int i = 0; i < nrow; i++) {
    double row_tot = 0;
    for(int j = 0; j < ncol; j++) {
      row_tot += x(i, j); // subset Matrix with (, )
    }
    out[i] = row_tot;
  }
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
library(microbenchmark)
x <- matrix(sample(100), nrow = 10)
microbenchmark(rowSumsC(x), rowSums(x))
*/
