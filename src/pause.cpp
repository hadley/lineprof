#include <Rcpp.h>
#include <unistd.h>
using namespace Rcpp;


//' Pause execution
//'
//' This is similar to \code{\link{Sys.sleep}} but is captured during
//' profiling, making it useful when generating simple examples
//'
//' @export
//' @param sec Number of seconds to pause (millsecond resolution). Currently
//'   this is an underestimate of how long this function will take.
// [[Rcpp::export]]
void pause(double sec) {
  int mu_sec = sec * 1e6;

  // Check for interrupts every ms
  int times = mu_sec / 1e3;
  for (int i = 0; i < times; ++i) {
    usleep(mu_sec / 1e3);
    checkUserInterrupt();
  }
  usleep(mu_sec - (times * 1e3));
}
