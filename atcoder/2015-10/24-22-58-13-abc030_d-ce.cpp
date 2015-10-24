#include <iostream>
#include <cstdio>
#include <string>
#include <cstring>
#include <algorithm>
#include <vector>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <functional>
#include <cmath>
#ifndef ONLINE_JUDGE
# include <complex>
# include <random>
# include <array>
# include <unordered_map>
# define mkt make_tuple
#endif
#ifdef _LOCAL
# include "local/local.hpp"
#else
# define assert(...) ((void)0)
# define ifdebug if (false)
# define echo(...) ((void)0)
#endif
using namespace std;
typedef long long ll; typedef unsigned long long ull;
#define repi(_I, _B, _E) for(int _I = (_B); (_I) < (_E); ++ (_I))
#define rep(_I, _N) for(int _I = 0; (_I) < (_N); ++ (_I))
#define all(_X) (_X).begin(), (_X).end()

#include <vector>
#include <iostream>
#include <string>
#include <cstdint>
#include <climits>
#include <cmath>

// cf. <http://qiita.com/marionette-of-u/items/f27ba0c5b03bbb8b87d8>
namespace multi_precision {
	template<class UInt = std::uint16_t, class DoubleUInt = std::uint32_t, class DoubleInt = std::int32_t, DoubleUInt BitNum = sizeof(UInt) * CHAR_BIT>
	class integer
	{
	public:
		static const DoubleUInt base_mask = (static_cast<DoubleUInt>(1) << BitNum) - 1;
		static const UInt half = static_cast<UInt>(static_cast<DoubleUInt>(1) << (BitNum - 1));

		using data_type = std::vector<UInt>;
		data_type data;
		DoubleInt sign = +1;

		integer() = default;
		integer(const integer&) = default;
		integer(integer &&other) : data(std::move(other.data)), sign(other.sign) {}
		integer(int x)
		{
			if ( x == 0 ) { return; }

			data.resize(1);
			data[0] = std::abs(x);

			sign = x >= 0 ? +1 : -1;
		}

		integer(UInt x)
		{
			if ( x == 0 ) { return; }

			data.resize(1);
			data[0] = x;

			sign = +1;
		}

		integer(const char *str)
		{
			build_from_str(str);
		}

		integer(const std::string &str)
		{
			build_from_str(str.begin());
		}

		template<class StrIter>
		void build_from_str(StrIter iter)
		{
			if ( *iter == '-' ) {
				sign = -1;
				++iter;
			}

			char buff[2] = { 0 };
			while ( *iter ) {
				buff[0] = *iter;
				int a = std::atoi(buff);
				*this *= 10;
				unsigned_single_add(a);
				++iter;
			}
		}

		~integer() = default;

		integer &operator =(const integer &other)
		{
			data = other.data;
			sign = other.sign;
			return *this;
		}

		integer &operator =(integer &&other)
		{
			data = std::move(other.data);
			sign = std::move(other.sign);
			return *this;
		}

		bool operator <(const integer &other) const
		{
			if ( sign < other.sign ) { return true; }
			if ( sign > other.sign ) { return false; }
			if ( data.size() < other.data.size() ) { return sign > 0; }
			if ( data.size() > other.data.size() ) { return sign < 0; }
			std::size_t i = data.size() - 1;
			do {
				if ( data[i] < other.data[i] ) { return true; }
			} while ( i-- > 0 );
			return false;
		}

		bool operator >(const integer &other) const
		{
			return other < *this;
		}

		bool operator <=(const integer &other) const
		{
			if ( sign < other.sign ) { return true; }
			if ( sign > other.sign ) { return false; }
			if ( data.size() < other.data.size() ) { return sign > 0; }
			if ( data.size() > other.data.size() ) { return sign < 0; }
			std::size_t i = data.size() - 1;
			do {
				if ( data[i] > other.data[i] ) { return false; }
			} while ( i-- > 0 );
			return true;
		}

		bool operator >=(const integer &other) const
		{
			return other <= *this;
		}

		bool operator ==(const integer &other) const
		{
			return sign == other.sign && data == other.data;
		}

		bool operator !=(const integer &other) const
		{
			return !(*this == other);
		}

		static bool unsigned_less(const data_type &lhs, const data_type &rhs)
		{
			if ( lhs.size() < rhs.size() ) { return true; }
			if ( lhs.size() > rhs.size() ) { return false; }
			for ( std::size_t i = lhs.size() - 1; i + 1 > 0; --i ) {
				if ( lhs[i] < rhs[i] ) { return true; }
				if ( lhs[i] > rhs[i] ) { return false; }
			}
			return false;
		}

		static bool unsigned_less_eq(const data_type &lhs, const data_type &rhs)
		{
			if ( lhs.size() < rhs.size() ) { return true; }
			if ( lhs.size() > rhs.size() ) { return false; }
			for ( std::size_t i = lhs.size() - 1; i + 1 > 0; --i ) {
				if ( lhs[i] < rhs[i] ) { return true; }
				if ( lhs[i] > rhs[i] ) { return false; }
			}
			return true;
		}

		void add(const integer &other)
		{
			if ( sign == other.sign ) {
				unsigned_add(other);
			} else {
				if ( unsigned_less(data, other.data) ) {
					integer t(other);
					t.unsigned_sub(*this);
					data.swap(t.data);
					sign = other.sign;
				} else { unsigned_sub(other); }
			}
		}

		void sub(const integer &other)
		{
			if ( sign != other.sign ) {
				unsigned_add(other);
			} else {
				if ( unsigned_less(data, other.data) ) {
					integer t(other);
					t.unsigned_sub(*this);
					data.swap(t.data);
					sign = other.sign;
				} else { unsigned_sub(other); }
			}
		}

		void unsigned_add(const integer &other)
		{
			if ( data.size() < other.data.size() ) {
				data.resize(other.data.size());
			}

			DoubleUInt c = 0;
			std::size_t i;
			for ( i = 0; i < other.data.size(); ++i ) {
				DoubleUInt v = static_cast<DoubleUInt>(data[i]) + static_cast<DoubleUInt>(other.data[i]) + c;
				data[i] = static_cast<UInt>(v & base_mask);
				c = v >> BitNum;
			}

			for ( ; c > 0; ++i ) {
				if ( i >= data.size() ) { data.resize(data.size() + 1); }
				DoubleUInt v = static_cast<DoubleUInt>(data[i]) + c;
				data[i] = static_cast<UInt>(v & base_mask);
				c = v >> BitNum;
			}
		}

		void unsigned_single_add(UInt c, std::size_t i = 0)
		{
			if ( i >= data.size() ) { data.resize(i + 1); }
			for ( ; c > 0; ++i ) {
				DoubleUInt v = static_cast<DoubleUInt>(data[i]) + c;
				data[i] = static_cast<UInt>(v & base_mask);
				c = v >> BitNum;
			}
		}

		void unsigned_sub(const integer &other, std::size_t i = 0)
		{
			UInt c = 0;
			for ( ; i < other.data.size(); ++i ) {
				UInt t = data[i] - (other.data[i] + c);
				if ( data[i] < other.data[i] + c ) {
					c = 1;
				} else {
					c = 0;
				}
				data[i] = t;
			}
			for ( ; c > 0; ++i ) {
				UInt t = data[i] - c;
				if ( data[i] < c ) {
					c = 1;
				} else {
					c = 0;
				}
				data[i] = t;
			}
			normalize_data_size();
		}

		static void unsigned_mul(integer &r, const integer &lhs, const integer &rhs)
		{
			std::size_t s = lhs.data.size() + rhs.data.size();
			r.data.resize(s + 1);
			for ( std::size_t i = 0; i < lhs.data.size(); ++i ) {
				for ( std::size_t j = 0; j < rhs.data.size(); ++j ) {
					DoubleUInt c = static_cast<DoubleUInt>(lhs.data[i]) * static_cast<DoubleUInt>(rhs.data[j]);
					for ( std::size_t k = 0; i + j + k < s + 1; ++k ) {
						std::size_t u = i + j + k;
						DoubleUInt v = static_cast<DoubleUInt>(r.data[u]) + c;
						UInt a = static_cast<UInt>(v & base_mask);
						r.data[u] = a;
						c = v >> BitNum;
					}
				}
			}
			r.normalize_data_size();
		}

		static integer mul(const integer &lhs, const integer &rhs)
		{
			integer r;
			unsigned_mul(r, lhs, rhs);
			r.sign = lhs.sign * rhs.sign;
			return r;
		}

		static void unsigned_single_mul(integer &r, const integer &lhs, UInt rhs)
		{
			std::size_t s = lhs.data.size() + 1;
			r.data.resize(s + 1);
			for ( std::size_t i = 0; i < lhs.data.size(); ++i ) {
				DoubleUInt c = static_cast<DoubleUInt>(lhs.data[i]) * static_cast<DoubleUInt>(rhs);
				for ( std::size_t k = 0; i + k < s + 1; ++k ) {
					std::size_t u = i + k;
					DoubleUInt v = static_cast<DoubleUInt>(r.data[u]) + c;
					UInt a = static_cast<UInt>(v & base_mask);
					r.data[u] = a;
					c = v >> BitNum;
				}
			}
			r.normalize_data_size();
		}

		struct quo_rem
		{
			integer quo, rem;

			quo_rem() = default;
			quo_rem(const quo_rem&) = default;
			quo_rem(quo_rem &&other) : quo(std::move(other.quo)), rem(std::move(other.rem)) {}
			~quo_rem() = default;
		};

		static quo_rem unsigned_div(const integer &lhs, const integer &rhs)
		{
			quo_rem qr;
			qr.quo.data.reserve(lhs.data.size());
			qr.rem.data.reserve(rhs.data.size());
			qr.rem.data.push_back(0);
			for ( std::size_t i = lhs.data.size() * static_cast<std::size_t>(BitNum)-1; i + 1 > 0; --i ) {
				qr.rem <<= 1;
				qr.rem.data[0] |= (lhs.data[i / static_cast<std::size_t>(BitNum)] >> (i % static_cast<std::size_t>(BitNum))) & 1;
				if ( unsigned_less_eq(rhs.data, qr.rem.data) ) {
					qr.rem.unsigned_sub(rhs);
					std::size_t t = i / static_cast<std::size_t>(BitNum);
					if ( qr.quo.data.size() < t + 1 ) { qr.quo.data.resize(t + 1); }
					qr.quo.data[t] |= 1 << (i % BitNum);
				}
			}
			return qr;
		}

		static quo_rem div(const integer &lhs, const integer &rhs)
		{
			quo_rem qr = unsigned_div(lhs, rhs);
			qr.quo.sign = qr.rem.sign = lhs.sign == rhs.sign;
			return qr;
		}

		void shift_container(std::size_t n)
		{
			data.insert(data.begin(), n, 0);
		}

		std::size_t bit_num() const
		{
			return bit_num(data.size() - 1);
		}

		std::size_t bit_num(std::size_t offset = data.size() - 1) const
		{
			std::size_t n = 0, count = static_cast<std::size_t>(BitNum / 2);
			UInt mask = (1 << count) - 1, x = data[offset];
			while ( x ) {
				n += x & mask > 0 ? count : 0;
				count /= 2;
				x >>= count;
				mask = (1 << count) - 1;
			}
			return n;
		}

		std::size_t finite_bit_shift_lsr(std::size_t n)
		{
			std::size_t
				digit = n / static_cast<std::size_t>(BitNum),
				shift = n % static_cast<std::size_t>(BitNum),
				rev_shift = static_cast<std::size_t>(BitNum)-shift;
			UInt c = 0;
			for ( std::size_t i = 0; i < data.size(); ++i ) {
				UInt x = data[i];
				data[i] = (x << shift) | c;
				if ( rev_shift < BitNum ) {
					c = x >> rev_shift;
				} else {
					c = 0;
				}
			}
			if ( c > 0 ) { data.push_back(c); }
			return digit;
		}

		void bit_shift_lsr(std::size_t n)
		{
			shift_container(finite_bit_shift_lsr(n));
		}

		void bit_shift_rsl(std::size_t n)
		{
			std::size_t
				digit = n / static_cast<std::size_t>(BitNum),
				shift = n % static_cast<std::size_t>(BitNum),
				rev_shift = BitNum - shift,
				size = data.size();

			if ( digit > 0 ) {
				for ( std::size_t i = 0, length = size - digit; i < length; ++i ) {
					data[i] = data[i + digit];
				}
				for ( std::size_t i = 0; i < digit; ++i ) {
					data[size - i - 1] = 0;
				}
			}

			UInt c = 0;
			for ( std::size_t i = 0; i < data.size(); ++i ) {
				std::size_t j = size - i - 1;
				UInt x = data[j];
				data[j] = (x >> shift) | c;
				if ( rev_shift < BitNum ) {
					x = x << rev_shift;
				} else {
					c = 0;
				}
			}
			normalize_data_size();
		}

		void normalize_data_size()
		{
			if ( data.size() == 0 && data.front() == 0 ) {
				data.clear();
			} else {
				std::size_t n = data.size() - 1;
				while ( n > 0 && data[n] == 0 ) { --n; }
				data.resize(n + 1);
			}
		}

#define MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(assign_op, op, type) \
    integer &operator assign_op(const type &rhs){ *this = *this op integer(rhs); return *this; }

		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(+= , +, int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(-= , -, int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(*= , *, int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(/= , / , int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(%= , %, int);

		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(+= , +, unsigned int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(-= , -, unsigned int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(*= , *, unsigned int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(/= , / , unsigned int);
		MULTI_PRECISION_ASSIGN_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(%= , %, unsigned int);
	};

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator +(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		integer<UInt, DoubleUInt, DoubleInt, BitNum> t(lhs);
		t.add(rhs);
		return t;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator -(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		integer<UInt, DoubleUInt, DoubleInt, BitNum> t(lhs);
		t.sub(rhs);
		return t;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator *(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		return integer<UInt, DoubleUInt, DoubleInt, BitNum>::mul(lhs, rhs);
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator /(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		return integer<UInt, DoubleUInt, DoubleInt, BitNum>::div(lhs, rhs).quo;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator %(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		return integer<UInt, DoubleUInt, DoubleInt, BitNum>::div(lhs, rhs).rem;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator <<(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, UInt n)
	{
		integer<UInt, DoubleUInt, DoubleInt, BitNum> x(lhs);
		x.bit_shift_lsr(n);
		return x;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator >>(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, UInt n)
	{
		integer<UInt, DoubleUInt, DoubleInt, BitNum> x(lhs);
		x.bit_shift_rsl(n);
		return x;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum>& operator --(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		sub(rhs);
		return *this;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> &operator /=(integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		lhs = integer<UInt, DoubleUInt, DoubleInt, BitNum>::div(lhs, rhs).quo;
		return lhs;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator %=(integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs)
	{
		lhs = integer<UInt, DoubleUInt, DoubleInt, BitNum>::div(lhs, rhs).rem;
		return lhs;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator <<=(integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, std::size_t n)
	{
		integer<UInt, DoubleUInt, DoubleInt, BitNum> x(lhs);
		x.bit_shift_lsr(n);
		lhs = std::move(x);
		return lhs;
	}

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	integer<UInt, DoubleUInt, DoubleInt, BitNum> operator >>=(integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, std::size_t n)
	{
		integer<UInt, DoubleUInt, DoubleInt, BitNum> x(lhs);
		x.bit_shift_rsl(n);
		lhs = std::move(x);
		return lhs;
	}

#define MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(op, type) \
    template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum> \
    integer<UInt, DoubleUInt, DoubleInt, BitNum> operator op(const integer<UInt, DoubleUInt, DoubleInt, BitNum> &lhs, const type &rhs){ return lhs op integer<UInt, DoubleUInt, DoubleInt, BitNum>(rhs); } \
    template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum> \
    integer<UInt, DoubleUInt, DoubleInt, BitNum> operator op(const type &lhs, const integer<UInt, DoubleUInt, DoubleInt, BitNum> &rhs){ return integer<UInt, DoubleUInt, DoubleInt, BitNum>(lhs) op rhs; }

	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(+, int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(-, int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(*, int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(/ , int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(%, int);

	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(+, unsigned int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(-, unsigned int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(*, unsigned int);
	MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(/ , unsigned int);
	//MULTI_PRECISION_OPERATOR_OVERLOAD_FOR_EXPLICIT_TYPE(%, unsigned int);

	template<class UInt, class DoubleUInt, class DoubleInt, DoubleUInt BitNum>
	std::ostream &operator <<(
		std::ostream &os,
		integer<UInt, DoubleUInt, DoubleInt, BitNum> rhs
		)
	{
		using integer = integer<UInt, DoubleUInt, DoubleInt, BitNum>;
		std::vector<std::string> rseq;
		integer lo(10);
		for ( ; rhs.data.size() > 0; rhs /= lo ) {
			rseq.push_back(std::to_string((rhs % lo).data[0]));
		}
		os << (rhs.sign > 0 ? "" : "-");
		for ( auto iter = rseq.rbegin(); iter != rseq.rend(); ++iter ) {
			os << *iter;
		}
		return os;
	}
}


int main()
{
	int n, a;
	string s;
	cin >> n >> a >> s;
	--a;
	vector<int> bs(n);
	rep(i, n)
	{
		cin >> bs[i];
		--bs[i];
	}

	auto k = multi_precision::integer<>(s.c_str());

	vector<int> cis(n, -n - 1);
	vector<vector<int>> cycles;
	set<int> visited;
	rep(i, n)
	{
		for ( int j = i; j < n; ) {
			if ( visited.count(j) == 0 ) {
				visited.insert(j);
				cis[j] = -i - 1;
				j = bs[j];

			} else {
				if ( cis[j] != (-i - 1) ) break;

				cycles.push_back({});
				auto& c = cycles.back();
				int ci = cycles.size() - 1;
				int j0 = j;

				//j0以降は巡回
				do {
					c.push_back(j);
					cis[j] = ci;
					j = bs[j];
				} while ( j != j0 );
				break;
			}
		}
	}

	{
		int i = a;
		//巡回に入るまで動かす
		while ( k >= 1 && cis[i] < 0 ) {
			i = bs[i];
			k -= 1;
		}
		if ( k >= 1 ) {
			assert(cis[i] >= 0);
			auto& cyc = cycles[cis[i]];
			k = k % cyc.size();
			while ( k >= 1 ) {
				i = bs[i]; k -= 1;
			}
		}
		cout << (i + 1) << endl;
	}
	return 0;
}
