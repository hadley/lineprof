#include <Rcpp.h>
#include <unistd.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;

//' Pause execution
//'
//' This is similar to \code{\link{Sys.sleep}} but is captured during
//' profiling, making it useful when generating simple examples.
//'
//' @export
//' @param sec Number of seconds to pause (millsecond resolution).
// [[Rcpp::export]]
void pause(double sec) {
  nanotime_t ns = sec * 1e9;
  nanotime_t start = get_nanotime();

  while(get_nanotime() - start < ns) {
    usleep(1e3); // check every 10 ms
    checkUserInterrupt();
  }
}
