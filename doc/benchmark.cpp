#include <chrono>
#include <iostream>
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
  return harmonic_tr(n - 1, accum + 1.0 / n);
}

void harmonic_benchmark() {
  using chrono::duration;
  using chrono::duration_cast;
  using chrono::high_resolution_clock;
  using chrono::milliseconds;

  auto t1 = high_resolution_clock::now();
  auto v = 0.;
  for (int i = 0; i < 10; ++i) {
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

constexpr bool isnan(double n) { return n != n; }

void matrix_benchmark() {
  using Real = double;
  const size_t SIZE = 3000;
  vector<Real> m1 = vector<Real>(100 * 100, 0.00032);
  vector<Real> m2 = m1;
  using chrono::duration;
  using chrono::duration_cast;
  using chrono::high_resolution_clock;
  using chrono::milliseconds;

  auto t1 = high_resolution_clock::now();
  vector<Real> c = vector<Real>(m1.size(), 0.);
  const auto rows1 = SIZE;
  const auto cols1 = SIZE;
  const auto cols2 = SIZE;
  const auto rows2 = SIZE;
  for (auto k = 0; k != cols1; ++k) {
    for (auto i = 0; i != rows2; ++i) {
      for (auto j = 0; j != cols2; ++j) {
        c[i * cols2 + j] += m1[i * cols1 + k] * m2[k * cols2 + j];
        // if (isnan(m1[i * cols1 + k])) {
        //   cerr << "Error A " << i << " " << j << " " << k << endl;
        //   return;
        // }
        // if (isnan(m2[k * cols2 + j])) {
        //   cerr << "Error B " << i << " " << j << " " << k << endl;
        //   return;
        // }
        // if (isnan(c[i * cols2 + j])) {
        //   cerr << "Error C " << i << " " << j << " " << k
        //        << c[i * cols2 + j] << " " << m1[i * cols1 + k] << " "
        //        << m2[k * cols2 +j] << " " << i*cols1 + k <<  endl;
        //   return;
        // }
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

int main(int, char **) {
  cout << "Harmonic" << endl;
  harmonic_benchmark();
  cout << "Matrix" << endl;
  matrix_benchmark();
  return 0;
}
