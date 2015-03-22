#include <iostream>
#include <string>

bool couldBeKaibun(std::string s)
{
	size_t const len = s.size();
	for ( size_t i = 0; i < (len / 2) + (len & 2); ++i ) {
		auto const c1 = s[i];
		auto const c2 = s[len - i - 1];
		if ( c1 != '*' && c2 != '*' && c1 != c2 ) return false;
	}
	return true;
}

int main() {
	std::string input;
	std::cin >> input;
	std::cout << (couldBeKaibun(input) ? "YES" : "NO") << std::endl;
	return 0;
}
