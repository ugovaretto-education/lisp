#include <iostream>
#include <chrono>

using namespace std;

 // (defun harmonic-tr (n accum)
 //  "tail-recursive harmonic function - cpb"
 //  (if (<= n 1) (+ accum 1)
 //      (harmonic-tr (- n 1) (+ accum (/ 1.0 n)))))
///  (time (loop for i from 1 to 10 collect (harmonic-tr 1000000 0)))
double harmonic_tr(int n, double accum) {
  if (n <= 1) {
    return (accum + 1);
  }
  return harmonic_tr(n -1, accum  + 1.0/n);
}

int main(int, char**) {
  using chrono::high_resolution_clock;
  using chrono::duration_cast;
  using chrono::duration;
  using chrono::milliseconds;

  auto t1 = high_resolution_clock::now();
  auto v = 0.;
  for(int i = 0; i < 10; ++i) {
    v += harmonic_tr(1000000 + i, 0.);
  }
  auto t2 = high_resolution_clock::now();

  /* Getting number of milliseconds as an integer. */
  auto ms_int = duration_cast<milliseconds>(t2 - t1);

  /* Getting number of milliseconds as a double. */
  duration<double, std::milli> ms_double = t2 - t1;

  cout << v << endl;
  cout << ms_int.count() << "ms\n";
  cout << ms_double.count() << "ms\n";
  return 0;
}
