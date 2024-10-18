#include <iostream>
#include <chrono>
#include <vector>

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

void harmonic_benchmark() {
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
}

void matrix_benchmark() {
  const size_t SIZE = 100;
  vector<double> m1 = vector<double>(100 * 100, 4.55);
  vector<double> m2 = m1;

  using chrono::high_resolution_clock;
  using chrono::duration_cast;
  using chrono::duration;
  using chrono::milliseconds;

  auto t1 = high_resolution_clock::now();
  vector<double> c = vector<double>(m1.size(), 0.);
  const auto rows1 = SIZE;
  const auto cols1 = SIZE;
  const auto cols2 = SIZE;
  for(auto i = 0; i != rows1; ++i) {
    for(auto j = 0; j != cols2; ++j) {
      for(auto k = 0; k != cols1; ++k) {
        c[i*cols2 + j] += m1[i*cols1 + k] * m2[k*cols2 + j];
      }
    }
  }
  auto t2 = high_resolution_clock::now();

  /* Getting number of milliseconds as an integer. */
  auto ms_int = duration_cast<milliseconds>(t2 - t1);

  /* Getting number of milliseconds as a double. */
  duration<double, std::milli> ms_double = t2 - t1;

  cout << c[1] << endl;
  cout << ms_int.count() << "ms\n";
  cout << ms_double.count() << "ms\n";
}

int main(int, char**) {
  cout << "Harmonic" << endl;
  harmonic_benchmark();
  cout << "Matrix" << endl;
  matrix_benchmark();
  return 0;
}
