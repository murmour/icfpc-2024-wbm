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
#include "geom2d.h"

void __never(int a){printf("\nOPS %d", a);}
#define ass(s) {if (!(s)) {__never(__LINE__);cout.flush();cerr.flush();abort();}}

template<typename T> T Sqr(const T &x) { return x * x; }
typedef long long LL;

using namespace std;

//random_device random_dev;
mt19937 RGEN(1235321);

constexpr int inf = 1000000000;

int problem_id = -1;

struct Problem {
    int honyarara;

    FLD_BEGIN
        FLD(honyarara)
    FLD_END
};

struct Solution {
    int naninani;

    FLD_BEGIN
        FLD(naninani)
    FLD_END
};


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
        Json::FastWriter fw;
        fprintf(f, "%s", fw.write(serializeJson(sol)).c_str());
        fclose(f);
    } else {
        Json::FastWriter fw;
        printf("%s", fw.write(serializeJson(sol)).c_str());
    }
}

template<typename T>
void writeJsonFile(const T &data, const char *fname) {
    auto f = fopen(fname, "wt");
    if (!f) exit(14);
    Json::FastWriter fw;
    fprintf(f, "%s", fw.write(serializeJson(data)).c_str());
    fclose(f);
}

Solution solve_baka(const Problem &p) {
    return {};
}

void solve(const string &infile, const string &solver, const string &fname, ArgParser &args) {
    Json::Value root, root_s;
    Problem p;
    if (!readJsonFile(infile.c_str(), root)) {
        fprintf(stderr, "Invalid json 1!\n");
        exit(1);
    }
    if (!deserializeJson(p, root)) {
        fprintf(stderr, "Invalid json 3!\n");
        exit(1);
    }
    Solution s;
    if (solver == "baka") {
        s = solve_baka(p);
    }
    // else if (solver == "...") {
    //     s = solve_...(p)
    // }
    else
    {
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
            in_file = format("../../data/in/%s.json", p);
    }

    regex prob_id("/(\\d+)\\.json");
    smatch match;
    if (regex_search(in_file, match, prob_id)) {
        string s = match[1].str();
        sscanf(s.c_str(), "%d", &problem_id);
        // if (problem_id > 25) {
        //     new_scoring = true;
        //     fprintf(stderr, "Using new scoring\n");
        // }
    }

    string solver = "";
    if (auto s = args.get_arg("-s")) {
        solver = s;
    }

    // if (auto ii = args.get_arg("-iter")) {
    // 	sscanf(ii, "%d", &iter);
    // }

    string fname = "";
    if (auto s = args.get_arg("-out")) {
        fname = s;
    }
    if (auto s = args.get_arg("-tag")) {
        fname = format("../../data/out/%03d/%s.sol", problem_id, s);
    }
    if (args.has_option("--stdout")) {
        STDOUT = true;
    }

    solve(in_file, solver, fname, args);
    return 0;
}
