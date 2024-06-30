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

const char* cmd_map[3] = {"147", "258", "369"};

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
    if (nearest.empty()) {
        int n = pts.size();
        nearest.resize(n);
        for (int i = 0; i < n; i++) {
            vector<pair<LL, int>> t;
            for (int j = 0; j < n; j++) if (j != i)
                t.push_back({pts[i].sqd(pts[j]), j});
            sort(t.begin(), t.end());
            for (auto x : t) nearest[i].push_back(x.second);
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
        // while (cnt < real_n) {
        //     int next_dir = -1;
        //     int options = 0;
        //     for (int dir = 0; dir < 9; dir++) {
        //         auto vel2 = vel;
        //         vel2.x += dx[dir];
        //         vel2.y += dy[dir];
        //         auto pos2 = pos + vel2;
        //         if (pmap.find(pos2) != pmap.end() && !used[pmap[pos2]]) {
        //             if (next_dir == -1)
        //                 next_dir = dir;
        //             options++;
        //         }
        //     }
        //     if (next_dir == -1) {
        //         fprintf(stderr, "Failed at %d/%d :(\n", cnt, n);
        //         exit(112);
        //     }
        //     if (options > 1) {
        //         fprintf(stderr, "%d options at %d/%d :(\n", options, cnt, n);
        //         //exit(112);
        //     }
        //     vel.x += dx[next_dir];
        //     vel.y += dy[next_dir];
        //     sol.push_back(dc[next_dir]);
        //     pos = pos + vel;
        //     used[pmap[pos]] = 1;
        //     cnt++;
        // }
        if (!naive_rec({0, 0}, {0, 0}, cnt)) {
            fprintf(stderr, "Failed (%d) :(\n", max_cnt);
            exit(112);
        }
        return sol;
    }

}

bool IDENTITY = false;

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
    if (t > 1000000) {
        fprintf(stderr, "solution is too long!\n");
        exit(111);
    }
    return res;
}

Solution solve_sa() {
    int best_score = 0;
    vector<int> best_perm;
    vector<int> a;
    int n = (int)pts.size();
    if (problem_id == 11 || IDENTITY) {
        for (int i = 0; i < n; i++)
            a.push_back(i);
    } else if (n < 500) {
        for (int i = 0; i < n; i++)
            a.push_back(i);
        shuffle(a.begin(), a.end(), RGEN);
    } else {
        init_nearest();
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
            for (int t : nearest[cur]) if (!used[t]) {
                next = t;
                break;
            }
            cur = next;
        }
    }

    fprintf(stderr, "Starting hill climbing\n");
    auto cur_score = score_permutation(a);
    // phase 1: hill climbing
    int hill_iter = 100000 * 100 / n;
    for (int it = 0; it < hill_iter; it++) {
        auto ne = get_neighbor(a);
        auto t = score_permutation(ne);
        if (t < cur_score) {
            fprintf(stderr, "score = %d\n", t);
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
            if (it % 10000 == 0) {
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
        auto t = score_permutation(ne);
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
    if (best_score > 1000000) {
        fprintf(stderr, "solution is too long!\n");
        exit(111);
    }
    int t = score_permutation(best_perm, &res);
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
    else if (solver == "naive") {
        s = naive::solve_naive();
    }
    else if (solver == "big") {
        s = solve_big();
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

    solve(in_file, solver, fname, args);
    return 0;
}
