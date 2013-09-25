#include <Rcpp.h>
using namespace Rcpp;

std::string slurpSpace(std::string::iterator& input) {
  if (*input != ' ') stop("Space needed");

  input++;
  return " ";
}

std::string slurpRef(std::string::iterator& input) {  
  
  std::string output;
  for(; *input != ' '; input++) {
    output.push_back(*input);
  }

  return output;
}

std::string slurpName(std::string::iterator& input) {  
  if (*input != '"') stop("Double quote needed");

  std::string output;

  input++; // skip leading quote
  for(; *input != '"'; input++) {
    output.push_back(*input);
  }
  input++; // skip trailing quote

  return output;
}

// [[Rcpp::export]]
List parseLineProfileRefs(std::string input) {
  std::string::iterator iref = input.begin(), input_end = input.end();
  std::vector<std::string> refs, names;

  while(iref < input_end) {
    if (*iref == ' ') {
      slurpSpace(iref); 
    } else if (*iref == '"') {
      refs.push_back("");
      names.push_back(slurpName(iref));
    } else {
      refs.push_back(slurpRef(iref));
      slurpSpace(iref); 
      names.push_back(slurpName(iref));
    }
  }

  return List::create(names, refs);
}
