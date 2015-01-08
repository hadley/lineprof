#include <Rcpp.h>
using namespace Rcpp;

// Determine if haystack contains needle in order
// 
// @param haystack character vector to search in
// @param needle vector to search for
// @return 0 if no match, otherwise location of first element of needle.
// [[Rcpp::export]]
int contains(CharacterVector haystack, CharacterVector needle) {
  for (int i = 0; i < haystack.length() - needle.length() + 1; ++i) {
    // Keep track of how many characters we successfully matched
    int j;
    for (j = 0; j < needle.length() && haystack[i + j] == needle[j]; ++j);
    // Matched every element
    if (j == needle.length()) return i + 1;
  }
  
  return 0;
}

// [[Rcpp::export]]
int firstTRUE(LogicalVector x) {
  for (int i = 0; i < x.length(); ++i) {
    if (x[i] && !LogicalVector::is_na(x[i])) return i + 1;
  }
  
  return 0;
}
