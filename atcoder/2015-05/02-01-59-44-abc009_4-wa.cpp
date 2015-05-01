#include <vector>
#include <cassert>
#include <iostream>
using namespace std;
#define repi(_I, _Init, _N) for (unsigned int _I = (_Init); (_I) < (_N); ++(_I))
#define rep(_I, _N) repi(_I, 0, _N)
using uint = unsigned int;

template<typename T>
struct AddMulRing {
	static T zero() { return 0; }
	static T one() { return 1; }
	static decltype(std::declval<T>() + std::declval<T>()) add(T const& lhs, T const& rhs) { return lhs + rhs; }
	static decltype(std::declval<T>() * std::declval<T>()) mul(T const& lhs, T const& rhs) { return lhs * rhs; }
};
template<typename T>
struct BitXorAndRing {
	static T zero() { return 0; }
	static T one() { return ~0; } //0b1111...
	static decltype(std::declval<T>() ^ std::declval<T>()) add(T const& lhs, T const& rhs) { return lhs ^ rhs; }
	static decltype(std::declval<T>() & std::declval<T>()) mul(T const& lhs, T const& rhs) { return lhs & rhs; }
};

template<typename T, typename Ring = AddMulRing<T>>
class Matrix {
	uint rows_, cols_;
	vector<T> v_;
public:
	Matrix(uint rows, uint cols) : rows_(rows), cols_(cols), v_() { v_.resize(rows * cols, Ring::zero()); }
	Matrix(uint rows, uint cols, std::vector<T> init) : rows_ { rows }, cols_ { cols }, v_( std::move(init) ) {
		assert(v_.size() == rows_* cols_);
	}
	Matrix(Matrix const& src) : Matrix(src.rows(), src.cols(), src.v_) {}
	Matrix(Matrix&& src) : Matrix(src.rows(), src.cols(), std::move( src.v_)) {}

	Matrix& swap(Matrix& src) {
		using std::swap; swap(rows_, src.rows_); swap(cols_, src.cols_); swap(v_, src.v_);
		return *this;
	}
	Matrix& operator=(Matrix const& src) { Matrix tmp = src; return swap(tmp); }
	Matrix& operator=(Matrix&& src) { Matrix tmp = std::move(src); return swap(tmp); }

	uint rows() const { return rows_; }
	uint cols() const { return cols_; }
	T const& at(uint i, uint j) const {
		assert(i < rows() && j < cols());
		return v_[i * cols() + j];
	}
	T& at(uint i, uint j) { return const_cast<T&>(const_cast<Matrix const*>(this)->at(i, j)); }

	template<typename Fun>
	static Matrix generate(uint rows, uint cols, Fun&& f) {
		Matrix self { rows, cols };
		rep(i, rows) rep(j, cols) { self.at(i, j) = f(i, j); }
		return std::move(self);
	}
	static Matrix iden(uint n) {
		return generate(n, n, [](uint i, uint j) { return (i == j ? Ring::one() : Ring::zero()); });
	}
	/*
	Matrix& operator+=(Matrix const& rhs) {
		assert(rows() == rhs.rows() && cols() == rhs.cols());
		rep(i, rows()) rep(j, cols()) {
			at(i, j) = Ring::add(at(i, j), rhs.at(i, j));
		}
		return *this;
	}
	Matrix operator+(Matrix const& rhs) const { Matrix tmp = *this; tmp += rhs; return std::move(tmp); }
	//*/
	Matrix operator*(Matrix const& rhs) const {
		Matrix const& lhs = *this;
		assert(lhs.cols() == rhs.rows());
		return generate(lhs.rows(), rhs.cols(), [&lhs, &rhs](uint i, uint j) {
			uint x = Ring::zero();
			rep(t, lhs.cols()) { x = Ring::add(x, Ring::mul(lhs.at(i, t), rhs.at(t, j))); }
			return x;
		});
	}
	Matrix& operator*=(Matrix const& rhs) { return (*this) = (*this) * rhs; }
	Matrix pow(uint n) {
		assert(rows() == cols());
		if ( n == 0 ) return iden(rows());
		if ( n == 1 ) return *this;
		Matrix z = ((n & 1) == 0 ? iden(rows()) : Matrix { *this });
		Matrix w = (*this) * (*this);
		n >>= 1;
		do {
			if ( (n & 1) != 0 ) {
				z *= w; n ^= 1;
			} else {
				w *= w; n >>= 1;
			}
		} while ( n != 0 );
		return std::move(z);
	}
};
#ifdef _LOCAL
template<typename T, typename Ring>
std::ostream& operator<<(std::ostream& os, Matrix<T, Ring> mat) {
	os << '[';
	rep(i, mat.rows()) {
		if ( i != 0 ) { os << ','; } os << "[";
		rep(j, mat.cols()) {
			if ( j != 0 ) { os << ','; } os << mat.at(i, j);
		}
		os << "]";
	}
	os << ']';
	return os;
}
#endif

uint k;//<=100
uint m;//<=10^9
uint as[100+1];
uint cs[100+1];

uint calc()
{
	using mat_t = Matrix<uint, BitXorAndRing<uint>>;
	auto ak = mat_t::generate(k, 1, [](uint i, uint j) {
		return as[k - 1 - i];
	});

	//漸化式を1つ進める行列
	auto mat = mat_t::generate(k, k, [](uint i, uint j) {
		return (i == 0
			? cs[j]
			: (i - 1 == j ? ~0 : 0));
	});

	auto mat2 = mat.pow(m - k + 1);
	auto ak2 = mat2 * ak;

	return ak2.at(0, 0);
}

//http://abc009.contest.atcoder.jp/tasks/abc009_4
//http://www.slideshare.net/chokudai/abc009
void mymain()
{
	cin >> k >> m;
	--m;
	rep(i, k) { cin >> as[i]; }
	rep(i, k) { cin >> cs[i]; }
	cout << calc() << endl;
}

int main() { std::ios::sync_with_stdio(false); std::cin.tie(0); mymain(); return 0; }
