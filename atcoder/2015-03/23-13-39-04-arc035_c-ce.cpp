#include <iostream>
#include <string>
#include <vector>
#include <map>

using namespace std;
using N = unsigned int;

void SolveARC025C()
{
	using routes_t = vector<std::tuple<N, N, N>>;
	auto ReadRoutes = [](N count)
	{
		routes_t routes; // 都市 (i, j) 間の距離
		routes.reserve(count);
		for ( N i = 0; i < count; ++i ) {
			N city1, city2;
			N dist;
			std::cin >> city1 >> city2 >> dist;
			routes.push_back(std::make_tuple(city1, city2, dist));
		}
		return move(routes);
	};

	N count_cities;
	N count_old_routes;
	routes_t old_routes;
	N count_new_routes;
	routes_t new_routes;
	cin >> count_cities;
	cin >> count_old_routes;
	old_routes = ReadRoutes(count_old_routes);

	cin >> count_new_routes;
	new_routes = ReadRoutes(count_new_routes);

	struct RouteInfo { N dest; N dist; };
	using wgraph_t = multimap<N, RouteInfo>;
	wgraph_t cur_routes;
	auto AddRoute = [&](wgraph_t& r, std::tuple<N, N, N> t)
	{
		N const city1 = get<0>(t) - 1;
		N const city2 = get<1>(t) - 1;
		N const dist = get<2>(t);
		r.insert({ city1, { city2, dist } });
		r.insert({ city2, { city1, dist } });
	};

	for ( auto it : old_routes ) {
		AddRoute(cur_routes, it);
	}
	for ( N k = 0; k < count_new_routes; ++k ) {
		AddRoute(cur_routes, new_routes[k]);

		N sum_min_dists = 0;
		for ( N i = 0; i < count_cities; ++i ) {
			for ( N j = i + 1; j < count_cities; ++j ) {
				// Dijkstra法
				N d_ij;

				struct PathInfo { N dist; bool is_done; };
				map<N, PathInfo> min_dists = { { i, { 0, true } } };

				for ( ;; ) {
					for ( auto src : min_dists ) {
						if ( !src.second.is_done ) continue;
						N const p = src.first;

						auto const ran = cur_routes.equal_range(p);
						for ( auto route = ran.first; route != ran.second; ++route ) {
							N const dist = src.second.dist + route->second.dist;

							auto dest = min_dists.find(route->second.dest);
							if ( dest == min_dists.end() ) {
								dest = min_dists.insert({ route->second.dest, { dist, false } }).first;
							} else {
								if ( dist < dest->second.dist ) {
									dest->second.dist = dist;
								}
							}
						}
					}

					N next_city;
					N next_min_dist = numeric_limits<N>::max();
					for ( auto pi : min_dists ) {
						if ( !pi.second.is_done && pi.second.dist < next_min_dist ) {
							next_city = pi.first;
							next_min_dist = pi.second.dist;
						}
					}
					min_dists[next_city].is_done = true;
					if ( next_city == j ) { d_ij = min_dists[j].dist; break; } // complete
				}

				sum_min_dists += d_ij;
			}
		}
		cout << sum_min_dists << endl;
	}
}
