#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <set>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <cstdarg>
#include <chrono>
#include <cstring>
#include <regex>
#include <random>

#include "common.h"
//#include "geom2d.h"

void __never(int a){printf("\nOPS %d", a);}
#define ass(s) {if (!(s)) {__never(__LINE__);cout.flush();cerr.flush();abort();}}

template<typename T> T Sqr(const T &x) { return x * x; }
typedef long long LL;
typedef unsigned long long u64;

using namespace std;

//random_device random_dev;
mt19937 RGEN(1235321);

constexpr int inf = 1000000000;

int problem_id = -1;

typedef string Problem;
typedef string Solution;
typedef vector<int> Permutation;

struct ArgParser {
    int argc;
    char **argv;
    char * get_arg(const char * name) {
        for (int i = 1; i + 1 < argc; i++) {
            if (strncmp(argv[i], "--", 2) == 0) continue;
            if (strcmp(argv[i], name) == 0)
                return argv[i + 1];
            i++;
        }
        return nullptr;
    }
    bool has_option(const char * name) {
        for (int i = 1; i < argc; i++)
            if (strcmp(argv[i], name) == 0)
                return true;
        return false;
    }
};

bool STDOUT = false;

void writeSolution(const Problem &p, const Solution &sol, string fname) {
    if (!fname.empty() && !STDOUT) {
        auto f = fopen(fname.c_str(), "wt");
        if (!f) exit(13);
        fprintf(f, "%s", sol.c_str());
        fclose(f);
    } else {
        printf("%s", sol.c_str());
    }
}

struct IPoint {
    int x = 0, y = 0;

    IPoint operator - (const IPoint &other) const {
        return {x - other.x, y - other.y};
    }

    IPoint operator + (const IPoint &other) const {
        return {x + other.x, y + other.y};
    }

    LL sqd(const IPoint &other) const {
        return Sqr((LL)x - other.x) + Sqr((LL)y - other.y);
    }

    bool operator < (const IPoint &other) const {
        return make_pair(x, y) < make_pair(other.x, other.y);
    }
};

// const int max_delta = 10000;
// const int max_vel = 50;
// const int max_steps = 50;

const int max_delta = 1000;
const int max_vel = 25;
const int max_steps = 100;

const int D = 2 * max_delta + 1;

vector<IPoint> pts;

pair<int, int> dp[2 * max_vel + 1][max_steps][2 * max_delta + 1];
pair<int, int> from[2 * max_vel + 1][max_steps][2 * max_delta + 1];

inline bool update_min(int &t, int v) {
    if (v < t) {
        t = v;
        return true;
    }
    return false;
}

inline bool update_max(int &t, int v) {
    if (v > t) {
        t = v;
        return true;
    }
    return false;
}

inline void update(int i, int step, int d2, int v, int cmd) {
    if (d2 < 0 || d2 >= D) return;
    if (v < -max_vel || v > max_vel) return;
    if (update_min(dp[i][step][d2].first, v)) from[i][step][d2].first = cmd;
    if (update_max(dp[i][step][d2].second, v)) from[i][step][d2].second = cmd;
}

void init_dp() {
    for (int i = 0; i < 2 * max_vel + 1; i++) {
        for (int k = 0; k < max_steps; k++)
            for (int j = 0; j < D; j++)
                dp[i][k][j] = {inf, -inf};
    }
    for (int i = 0; i < 2 * max_vel + 1; i++) {
        int iv = i - max_vel; // initial velocity
        dp[i][0][max_delta] = {iv, iv};
        for (int step = 0; step + 1 < max_steps; step++) {
            for (int delta = 0; delta < D; delta++)
                if (dp[i][step][delta].first != inf) {
                    auto [v0min, v0max] = dp[i][step][delta];
                    { // v0min + increase
                        int v = v0min + 1;
                        update(i, step+1, delta + v, v, 1);
                    }
                    { // v0min + decrease
                        int v = v0min - 1;
                        update(i, step+1, delta + v, v, -1);
                    }
                    { // v0min + keep
                        int v = v0min;
                        update(i, step+1, delta + v, v, 0);
                    }

                    { // v0max + increase
                        int v = v0max + 1;
                        update(i, step+1, delta + v, v, 1);
                    }
                    { // v0max + decrease
                        int v = v0max - 1;
                        update(i, step+1, delta + v, v, -1);
                    }
                    { // v0max + keep
                        int v = v0max;
                        update(i, step+1, delta + v, v, 0);
                    }
                }
        }
    }
}

const char* cmd_map[3] = {"147", "258", "369"};

namespace precise {

    const int max_v = 25;
    const int max_delta = 1000;
    const int max_steps = 60;

    const int D = 2 * max_delta + 1;
    const int V = 2 * max_v + 1;

    u64 dpx[V][V][D];
    //int from[V][D][max_steps];

    bool dp_init = false;

    inline void mark(int v0, int v, int d, u64 mask) {
        if (v >= 0 && v < V && d >= 0 && d <= D) {
            dpx[v0][v][d] = dpx[v0][v][d] | mask;
        }
    }

    // inline bool get_dp(int v0, int v, int d, int step) {
    //     v += max_v;
    //     d += max_delta;
    //     if (v < 0 || v >= V || d < 0 || d >= D) return false;
    //     return dp[v][d][step] != 0;
    // }

    void init_dp_precise() {
        if (dp_init) return;
        dp_init = true;
        memset(dp, 0, sizeof(dp));
        for (int v0 = 0; v0 < V; v0++)
            dpx[v0][v0][max_delta] = 1;

        for (int i = 0; i + 1 < max_steps; i++) {
            u64 mask1 = 1ull << i;
            u64 mask2 = 1ull << (i + 1);
            for (int v0 = 0; v0 < V; v0++)
                for (int v = 0; v < V; v++)
                    for (int d = 0; d < D; d++)
                        if (dpx[v0][v][d] & mask1) {
                            int vr = v - max_v;
                            mark(v0, v-1, d+vr-1, mask2);
                            mark(v0, v, d+vr, mask2);
                            mark(v0, v+1, d+vr+1, mask2);
                        }
        }
    }

    vector<int> get_commands_1d(int v0, int v1, int delta, int steps) {
        v0 += max_v;
        v1 += max_v;
        delta += max_delta;
        vector<int> res;
        for (int i = steps; i > 0; i--) {
            // acc = 0?
            int pmask = 1ull << (i - 1);
            int real_v = v1 - max_v;
            int pdelta = delta - real_v;
            ass(pdelta >= 0 && pdelta <= D);
            int acc;
            if (dpx[v0][v1][pdelta] & pmask)
                acc = 0;
            else if (v1 > 0 && dpx[v0][v1-1][pdelta] & pmask)
                acc = 1;
            else if (v1 + 1 < V && dpx[v0][v1+1][pdelta] & pmask)
                acc = -1;
            else
                ass(false);
            res.push_back(acc);
            delta -= (v1 - max_v);
            v1 -= acc;
        }
        ass(delta == max_delta);
        ass(v1 == v0);
        reverse(res.begin(), res.end());
        return res;
    }

    int get_moves(IPoint delta_adj, IPoint v1, IPoint v2, Solution *sol = nullptr) {
        //int dx = delta.x + max_delta;
        //ass(dx >= 0 && dx <= D);
        //int dy = delta.y + max_delta;
        //ass(dy > 0 && dy <= D);
        int dx = delta_adj.x;
        int dy = delta_adj.y;

        auto mask1 = dpx[v1.x + max_v][v2.x + max_v][dx];
        if (mask1 == 0) return inf;
        auto mask = dpx[v1.y + max_v][v2.y + max_v][dy] & mask1;
        if (mask == 0) return inf;
        int res = std::__countr_zero(mask);
        if (sol) {
            auto cx = get_commands_1d(v1.x, v2.x, delta_adj.x - max_delta, res);
            auto cy = get_commands_1d(v1.y, v2.y, delta_adj.y - max_delta, res);
            for (int j = 0; j < res; j++)
                sol->push_back(cmd_map[cx[j]+1][cy[j]+1]);
        }
        return res;
    }

    const int wnd = 15;
    const int W = wnd * 2 + 1;
    int thr = 3;
    const int MAX_PTS = 66000;

    int dpp[MAX_PTS][W][W];
    IPoint from[MAX_PTS][W][W];

    inline bool is_valid_delta(IPoint p) {
        return p.x >= 0 && p.x < D && p.y >= 0 && p.y < D;
    }

    int score_permutation(const Permutation &p, Solution *sol = nullptr) {
        init_dp_precise();
        int n = (int)p.size();
        for (int i = 0; i < n; i++)
            for (int x = 0; x < W; x++)
                for (int y = 0; y < W; y++)
                    dpp[i][x][y] = inf;

        IPoint offset = {wnd, wnd};
        IPoint delta_offset = {max_delta, max_delta};
        IPoint delta0 = pts[p[0]] + delta_offset;
        if (!is_valid_delta(delta0)) return inf;
        for (int x = 0; x < W; x++)
            for (int y = 0; y < W; y++)
            {
                dpp[0][x][y] = get_moves(delta0, {}, {x - wnd, y - wnd});
                from[0][x][y] = offset;
            }

        for (int i = 0; i + 1 < n; i++) {
            int best = inf;
            for (int x = 0; x < W; x++)
                for (int y = 0; y < W; y++) {
                    best = min(best, dpp[i][x][y]);
                }
            auto delta_adj = pts[p[i+1]] - pts[p[i]] + delta_offset;
            if (!is_valid_delta(delta_adj)) return inf;
            //if (best < inf) return inf;
            ass(best < inf);
            for (int x = 0; x < W; x++)
                for (int y = 0; y < W; y++) {
                    int base = dpp[i][x][y];
                    if (base <= best + thr) {
                        for (int x1 = 0; x1 < W; x1++)
                            for (int y1 = 0; y1 < W; y1++) {
                                int &tgt = dpp[i+1][x1][y1];
                                int t = base + get_moves(delta_adj, {x - wnd, y - wnd}, {x1 - wnd, y1 - wnd});
                                if (t < tgt) {
                                    tgt = t;
                                    from[i+1][x1][y1] = {x, y};
                                }
                            }
                    }
                }
        }
        int best = inf;
        IPoint best_v;
        for (int x = 0; x < W; x++)
            for (int y = 0; y < W; y++) {
                int t = dpp[n-1][x][y];
                if (t < best) {
                    best = t;
                    best_v = {x, y};
                }
            }
        ass(best < inf);
        if (sol) {
            vector<string> parts;
            for (int i = n - 1; i >= 0; i--) {
                auto delta = i > 0 ? pts[p[i]] - pts[p[i-1]] : pts[p[i]];
                string part;
                auto v0 = from[i][best_v.x][best_v.y];
                int t = get_moves(delta + delta_offset, v0 - offset, best_v - offset, &part);
                //fprintf(stderr, "part: %s", part.c_str());
                ass(t < inf);
                parts.push_back(part);
                best_v = v0;
            }
            ass(best_v.x == wnd && best_v.y == wnd);
            reverse(parts.begin(), parts.end());
            *sol = StringJoin(parts, "");
        }
        return best;
    }

}


vector<int> get_commands_1d(int v0, int steps, int v1, int delta0) {
    vector<int> res;
    int delta = max_delta + delta0;
    int vdp = max_vel + v0;
    int v = v1;
    for (int i = steps; i > 0; i--) {
        int t;
        if (dp[vdp][i][delta].first == v)
            t = from[vdp][i][delta].first;
        else if (dp[vdp][i][delta].second == v)
            t = from[vdp][i][delta].second;
        else
            ass(false);
        res.push_back(t);
        delta -= v;
        v -= t;
    }
    ass(delta == max_delta);
    ass(v == v0);
    reverse(res.begin(), res.end());
    return res;
}

inline bool can_overacc(int dist_left, int vel) {
    dist_left -= vel + 1;
    int decc_dist = (vel + max_vel) * (vel - max_vel + 1) / 2;
    return dist_left >= decc_dist;
}

pair<int, IPoint> solve_step(IPoint delta, IPoint vel, IPoint delta2, Solution *sol) {
    ass(vel.x >= -max_vel);
    ass(vel.x <= max_vel);
    ass(vel.y >= -max_vel);
    ass(vel.y <= max_vel);

    int pre_steps = 0;
    while (abs(delta.x) > max_delta || abs(delta.y) > max_delta) {
        // adjust speed
        int ax = 0;
        if (delta.x > 0) {
            if (vel.x < max_vel || can_overacc(delta.x - max_delta, vel.x)) ax = 1;
            else if (vel.x > max_vel) ax = -1;
        } else { // delta.x < 0
            if (vel.x > -max_vel || can_overacc(-max_delta - delta.x, -vel.x)) ax = -1;
            else if (vel.x < -max_vel) ax = 1;
        }
        //if (delta.x > 0 && vel.x < max_vel) ax = 1;
        //else if (delta.x < 0 && vel.x > -max_vel) ax = -1;
        int ay = 0;
        if (delta.y > 0) {
            if (vel.y < max_vel || can_overacc(delta.y - max_delta, vel.y)) ay = 1;
            else if (vel.y > max_vel) ay = -1;
        } else { // delta.y < 0
            if (vel.y > -max_vel || can_overacc(-max_delta - delta.y, -vel.y)) ay = -1;
            else if (vel.y < -max_vel) ay = 1;
        }
        //if (delta.y > 0 && vel.y < max_vel) ay = 1;
        //else if (delta.y < 0 && vel.y > -max_vel) ay = -1;
        vel.x += ax;
        vel.y += ay;
        if (sol)
            sol->push_back(cmd_map[ax+1][ay+1]);
        delta.x -= vel.x;
        delta.y -= vel.y;
        pre_steps++;
    }

    ass(vel.x >= -max_vel);
    ass(vel.x <= max_vel);
    ass(vel.y >= -max_vel);
    ass(vel.y <= max_vel);
    ass(delta.x >= -max_delta);
    ass(delta.x <= max_delta);
    ass(delta.y >= -max_delta);
    ass(delta.y <= max_delta);

    for (int i = 1; i < max_steps; i++) {
        int vx = dp[vel.x + max_vel][i][delta.x + max_delta].first;
        int vy = dp[vel.y + max_vel][i][delta.y + max_delta].first;
        if (vx != inf && vy != inf) {
            if (delta2.x > 0) vx = dp[vel.x + max_vel][i][delta.x + max_delta].second;
            if (delta2.y > 0) vy = dp[vel.y + max_vel][i][delta.y + max_delta].second;
            if (sol) {
                auto cx = get_commands_1d(vel.x, i, vx, delta.x);
                auto cy = get_commands_1d(vel.y, i, vy, delta.y);
                for (int j = 0; j < i; j++)
                    sol->push_back(cmd_map[cx[j]+1][cy[j]+1]);
            }
            return {i + pre_steps, {vx, vy}};
        }
    }
    ass(false);
}

int score_permutation(const Permutation &p, Solution *sol = nullptr) {
    IPoint vel = {0, 0};
    IPoint cur = {0, 0};
    int n = (int)p.size();
    int res = 0;
    if (sol) sol->clear();
    for (int i = 0; i < n; i++) {
        auto d2 = i + 1 < n ? pts[p[i+1]] - pts[p[i]] : IPoint {0, 0};
        auto [steps, nvel] = solve_step(pts[p[i]] - cur, vel, d2, sol);
        res += steps;
        cur = pts[p[i]];
        vel = nvel;
    }
    return res;
}

static vector<vector<int>> nearest;

void init_nearest()
{
    vector<pair<LL, int>> t;
    if (nearest.empty()) {
        int n = pts.size();
        nearest.resize(n);
        for (int i = 0; i < n; i++) {
            t.clear();
            for (int j = 0; j < n; j++) if (j != i)
                t.push_back({pts[i].sqd(pts[j]), j});
            sort(t.begin(), t.end());
            for (auto x : t) {
                nearest[i].push_back(x.second);
                if (nearest.size() >= 10) break;
            }
        }
    }
}

Permutation get_neighbor(const Permutation &base) {
    int n = (int)base.size();
    int t = Rnd(4);
    auto res = base;
    if (t == 0) {
        // reverse range
        int i1 = Rnd(n);
        int i2 = Rnd(n);
        if (i1 > i2) swap(i1, i2);
        if (i1 < i2) {
            reverse(res.begin() + i1, res.begin() + i2 + 1);
            return res;
        }
    }
    if (t == 1) {
        // shift
        int i1 = Rnd(n);
        int i2 = Rnd(n);
        if (i1 > i2) swap(i1, i2);
        if (i1 < i2) {
            rotate(res.begin() + i1, res.begin() + i1 + 1, res.begin() + i2 + 1);
            return res;
        }
    }

    init_nearest();

    if (t == 2) {
        // attract
        int i = Rnd(n - 1);
        int cnt = min(5, (int)nearest[res[i]].size());
        int idx = nearest[res[i]][Rnd(cnt)];
        int j = -1;
        for (int t = 0; t < n; t++) if (res[t] == idx) j = t;
        ass(j != -1);

        if (j > i) {
            rotate(res.begin() + i + 1, res.begin() + j, res.begin() + j + 1);
            //ass(res[i+1] == best_mon);
        } else {
            rotate(res.begin() + j, res.begin() + j + 1, res.begin() + i);
            //ass(res[i-1] == best_mon);
        }
        return res;
    }

    // swap neighbors
    int i = Rnd(n - 1);
    swap(res[i], res[i+1]);
    return res;
}

int user_temp = -1;
int timeout = -1;
int iter = 1000000;


int dx[9] = {-1, -1, -1, 0, 0, 0, 1, 1, 1};
int dy[9] = {-1, 0, 1, -1, 0, 1, -1, 0, 1};
const char *dc = "147258369";

namespace naive {

    map<IPoint, int> pmap;
    vector<int> used;
    int real_n;
    string sol;
    int max_cnt = 0;

    bool M2 = false;

    bool naive_rec(IPoint pos, IPoint vel, int cnt) {
        max_cnt = max(max_cnt, cnt);
        if (cnt >= real_n)
            return true;
        int opts = 0;
        for (int dir = 0; dir < 9; dir++) {
            auto vel2 = vel;
            vel2.x += dx[dir];
            vel2.y += dy[dir];
            auto pos2 = pos + vel2;
            if (pmap.find(pos2) != pmap.end() && !used[pmap[pos2]]) {
                opts++;
                int idx = pmap[pos2];
                sol.push_back(dc[dir]);
                used[idx] = 1;
                if (naive_rec(pos2, vel2, cnt + 1)) return true;
                used[idx] = 0;
                sol.pop_back();
            }
        }
        if (opts == 0 && M2) {
            for (int dir = 0; dir < 9; dir++)
                for (int dir2 = 0; dir2 < 9; dir2++) {
                    auto vel2 = vel;
                    vel2.x += dx[dir];
                    vel2.y += dy[dir];
                    auto pos2 = pos + vel2;
                    vel2.x += dx[dir2];
                    vel2.y += dy[dir2];
                    pos2 = pos2 + vel2;
                    if (pmap.find(pos2) != pmap.end() && !used[pmap[pos2]]) {
                        opts++;
                        int idx = pmap[pos2];
                        sol.push_back(dc[dir]);
                        sol.push_back(dc[dir2]);
                        used[idx] = 1;
                        if (naive_rec(pos2, vel2, cnt + 1)) return true;
                        used[idx] = 0;
                        sol.pop_back();
                        sol.pop_back();
                    }
                }
        }
        return false;
    }

    Solution solve_naive() {

        IPoint pos, vel;
        int n = (int)pts.size();
        for (int i = 0; i < n; i++) {
            if (pmap.find(pts[i]) == pmap.end())
                pmap[pts[i]] = i;
        }
        used = vector<int>(n);
        int cnt = 0;
        real_n = (int)pmap.size();
        fprintf(stderr, "real_n = %d/%d\n", real_n, n);
        if (pmap.find(pos) != pmap.end()) {
            cnt++;
            used[pmap[pos]] = 1;
        }
        sol = "";
        if (!naive_rec({0, 0}, {0, 0}, cnt)) {
            fprintf(stderr, "Failed (%d) :(\n", max_cnt);
            exit(112);
        }
        return sol;
    }

}

bool IDENTITY = false;
bool MST0 = false;
bool PRECISE = false;

Solution solve_big() {
    init_dp();
    int n = (int)pts.size();
    vector<int> a;
    vector<int> used(n);
    int start_idx = -1;
    LL min_d = (LL)inf * inf;
    IPoint c = {0, 0};
    for (int i = 0; i < n; i++) {
        LL d = c.sqd(pts[i]);
        if (d < min_d) {
            min_d = d;
            start_idx = i;
        }
    }
    int cur = start_idx;
    for (int i = 0; i < n; i++) {
        a.push_back(cur);
        used[cur] = 1;
        int next = -1;
        LL min_d = (LL)inf * inf;
        for (int j = 0; j < n; j++) if (!used[j]) {
            LL d = pts[cur].sqd(pts[j]);
            if (d < min_d) {
                min_d = d;
                next = j;
            }
        }
        cur = next;
    }
    Solution res;
    int t = score_permutation(a, &res);
    fprintf(stderr, "final score %d\n", t);
    if (t > 1048576) {
        fprintf(stderr, "solution is too long!\n");
        exit(111);
    }
    return res;
}

const LL INF = (LL)inf * inf;

Permutation get_mst_perm() {
    int n = (int)pts.size();
    vector<int> used (n);
    vector<int> sel_e (n, -1);
    vector<LL> min_e(n, INF);
    vector<vector<pair<int, int>>> edges(n); // euler graph (v, edge_id)
    int edge_id = 0;

    auto add_edge = [&](int u, int v) {
        edges[u].push_back({v, edge_id});
        edges[v].push_back({u, edge_id});
        edge_id++;
    };

    min_e[0] = 0;
    for (int i=0; i<n; ++i) {
        int v = -1;
        for (int j=0; j<n; ++j)
            if (!used[j] && (v == -1 || min_e[j] < min_e[v]))
                v = j;
        if (min_e[v] == inf) {
            ass(false);
        }

        used[v] = true;
        if (sel_e[v] != -1) {
            add_edge(v, sel_e[v]);
            //edges[v].push_back({sel_e[v], edge_id});
            //edges[sel_e[v]].push_back({v, edge_id});
            //edge_id++;
        }

        for (int to=0; to<n; ++to) {
            LL dist = pts[v].sqd(pts[to]);
            if (dist < min_e[to]) {
                min_e[to] = dist;
                sel_e[to] = v;
            }
        }
    }

    int bad_cnt = 0;
    for (int i = 0; i < n; i++) if (edges[i].size() % 2 != 0) bad_cnt++;
    fprintf(stderr, "%d bad vertices\n", bad_cnt);

    for (int i = 0; i < n; i++) if (edges[i].size() % 2 != 0) {
        LL min_d = INF;
        int v = -1;
        for (int j = 0; j < n; j++) if (i != j && edges[j].size() % 2 != 0) {
            LL d = pts[i].sqd(pts[j]);
            if (d < min_d) {
                min_d = d;
                v = j;
            }
        }
        ass(v != -1);
        add_edge(i, v);
    }

    int start_idx = -1;
    LL min_d = (LL)inf * inf;
    IPoint c = {0, 0};
    for (int i = 0; i < n; i++) {
        LL d = c.sqd(pts[i]);
        if (d < min_d) {
            min_d = d;
            start_idx = i;
        }
    }

	stack<int> st;
	st.push(start_idx);
	vector<int> res;
    vector<int> del(edge_id);
    set<int> seen;
	while (!st.empty())
	{
		int v = st.top();
		int other = -1;
        while (!edges[v].empty()) {
            auto e = edges[v].back();
            edges[v].pop_back();
            if (del[e.second]) continue;
            other = e.first;
            del[e.second] = 1;
            break;
        }
		if (other == -1)
		{
            if (seen.find(v) == seen.end()) {
                seen.insert(v);
			    res.push_back(v);
            }
			st.pop();
		}
		else
		{
			st.push(other);
		}
	}
    ass((int)res.size() == n);
    return res;
}

Solution solve_mst() {
    init_dp();
    auto a = get_mst_perm();
    Solution res;
    int t = score_permutation(a, &res);
    fprintf(stderr, "final score %d\n", t);
    if (t > 1048576) {
        fprintf(stderr, "solution is too long!\n");
        exit(111);
    }
    return res;
}

Solution solve_precise() {
    vector<int> a;
    int n = (int)pts.size();
    for (int i = 0; i < n; i++)
        a.push_back(i);
    Solution res;
    int t = precise::score_permutation(a, &res);
    fprintf(stderr, "final score %d\n", t);
    if (t > 1048576) {
        fprintf(stderr, "solution is too long!\n");
        exit(111);
    }
    return res;
}

Solution solve_sa() {
    auto score_perm_f = PRECISE ? precise::score_permutation : score_permutation;
    auto score_perm_fast = score_permutation;
    int best_score = 0;
    vector<int> best_perm;
    vector<int> a;
    int n = (int)pts.size();
    if (MST0) {
        a = get_mst_perm();
    } else if (problem_id == 11 || IDENTITY) {
        for (int i = 0; i < n; i++)
            a.push_back(i);
    } else if (n < 500 && problem_id != 6 && problem_id != 9 && problem_id != 10 && problem_id != 14) {
        for (int i = 0; i < n; i++)
            a.push_back(i);
        shuffle(a.begin(), a.end(), RGEN);
    } else {
        vector<int> used(n);
        int start_idx = -1;
        LL min_d = (LL)inf * inf;
        IPoint c = {0, 0};
        for (int i = 0; i < n; i++) {
            LL d = c.sqd(pts[i]);
            if (d < min_d) {
                min_d = d;
                start_idx = i;
            }
        }
        int cur = start_idx;
        for (int i = 0; i < n; i++) {
            a.push_back(cur);
            used[cur] = 1;
            int next = -1;
            LL min_d = (LL)inf * inf;
            for (int j = 0; j < n; j++) if (!used[j]) {
                LL d = pts[cur].sqd(pts[j]);
                if (d < min_d) {
                    min_d = d;
                    next = j;
                }
            }
            cur = next;
        }
    }

    fprintf(stderr, "Starting hill climbing\n");
    auto cur_score = score_perm_fast(a, nullptr);
    // phase 1: hill climbing
    int hill_iter = 100000 * 100 / n;
    //if (PRECISE) hill_iter /= 100;
    for (int it = 0; it < hill_iter; it++) {
        auto ne = get_neighbor(a);
        auto t = score_perm_fast(ne, nullptr);
        if (t < cur_score) {
            fprintf(stderr, "iter = %d, score = %d\n", it, t);
            cur_score = t;
            a = ne;
        }
    }
    // phase 2: SA
    int temp0 = user_temp < 0 ? int(0.1 * cur_score) : user_temp;
    fprintf(stderr, "base temp = %d\n", temp0);
    int n_acc = 0;
    best_score = cur_score;
    best_perm = a;
    int it = 0;
    auto t0 = std::chrono::system_clock::now();
    double temp = temp0;
    while (true) {
        if (timeout <= 0) {
            if (it > iter) break;
            //temp = (int)(temp0 * (1.0 - (double)it / iter));
            temp = temp0 * pow(1.0 / temp0, (double)it / iter);
        } else {
            int check_period = PRECISE ? 1000 : 10000;
            if (it % check_period == 0) {
                // check timer
                auto t1 = std::chrono::system_clock::now();
                auto diff = (t1 - t0).count();
                double fr = diff / ((double)timeout * 60 * 1e9);
                if (fr >= 1) break;
                //temp = (int)(temp0 * (1.0 - fr));
                temp = temp0 * pow(1.0 / temp0, fr);
            }
        }
        it++;
        auto ne = get_neighbor(a);
        auto t = score_perm_f(ne, nullptr);
        int delta = cur_score - t;
        //if (delta > 0 || temp * RndF() > -delta) {
        if (delta >= 0 || RndF() < exp(delta / temp)) {
            //fprintf(stderr, "score = %d\n", t.first);
            cur_score = t;
            a = ne;
            n_acc++;
            if (cur_score < best_score) {
                best_score = cur_score;
                fprintf(stderr, "it = %d, temp = %d, score = %d\n", it, (int)temp, best_score);
                best_perm = a;
            }
        }
    }
    Solution res;
    if (best_score > 1048576) {
        fprintf(stderr, "solution is too long!\n");
        exit(111);
    }
    int t = score_perm_f(best_perm, &res);
    fprintf(stderr, "final score %d\n", t);
    //return format("solve spaceship%d ", problem_id) + res;
    return res;
}

void solve(const string &infile, const string &solver, const string &fname, ArgParser &args) {
    Problem p;
    FILE *f = fopen(infile.c_str(), "rt");
    int x, y;
    set<pair<int, int>> seen;
    while (fscanf(f, "%d%d", &x, &y) > 0) {
        if (seen.find({x, y}) != seen.end()) continue;
        seen.insert({x, y});
        pts.push_back({x, y});
    }
    fclose(f);
    fprintf(stderr, "%d points\n", (int)pts.size());
    Solution s;
    if (solver == "sa") {
        init_dp();
        s = solve_sa();
    }
    else if (solver == "sap") {
        init_dp();
        PRECISE = true;
        s = solve_sa();
    }
    else if (solver == "naive") {
        s = naive::solve_naive();
    }
    else if (solver == "big") {
        s = solve_big();
    }
    else if (solver == "precise") {
        s = solve_precise();
    }
    else if (solver == "mst") {
        s = solve_mst();
    }
    else if (solver == "naive_x2") {
        naive::M2 = true;
        s = naive::solve_naive();
    }
    else {
        fprintf(stderr, "Invalid solver: %s\n", solver.c_str());
        exit(1);
    }
    writeSolution(p, s, fname);
}

int main(int argc, char *argv[]) {
    ArgParser args = { argc, argv };

    string in_file = "";
    if (auto p = args.get_arg("-pp"))
    {
        in_file = p;
    } else {
        if (auto p = args.get_arg("-p"))
            in_file = format("../../data/in/spaceship/%s.in", p);
    }

    regex prob_id("/(\\d+)\\.in");
    smatch match;
    if (regex_search(in_file, match, prob_id)) {
        string s = match[1].str();
        sscanf(s.c_str(), "%d", &problem_id);
    }

    string solver = "";
    if (auto s = args.get_arg("-s")) {
        solver = s;
    }

    if (auto p = args.get_arg("-time")) {
        sscanf(p, "%d", &timeout);
    }

    if (auto ii = args.get_arg("-iter")) {
        sscanf(ii, "%d", &iter);
    }

    if (auto st = args.get_arg("-temp")) {
        sscanf(st, "%d", &user_temp);
    }

    if (auto st = args.get_arg("-thr")) {
        sscanf(st, "%d", &precise::thr);
    }

    string fname = "";
    if (auto s = args.get_arg("-out")) {
        fname = s;
    }
    if (auto s = args.get_arg("-tag")) {
        fname = format("../../data/out/spaceship/%02d/%s.sol", problem_id, s);
    }
    if (args.has_option("--stdout")) {
        STDOUT = true;
    }
    if (args.has_option("--identity")) {
        fprintf(stdout, "enabled IDENTITY\n");
        IDENTITY = true;
    }
    if (args.has_option("--mst0")) {
        fprintf(stdout, "enabled MST0\n");
        MST0 = true;
    }

    solve(in_file, solver, fname, args);
    return 0;
}
