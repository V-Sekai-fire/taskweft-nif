// RECTGTN 'T' — Temporal: ISO 8601 duration parsing, STN consistency, plan timing.
// Mirrors Python ipyhop/temporal/stn.py and temporal_metadata.py.
#pragma once
#include "tw_domain.hpp"
#include <cmath>
#include <sstream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

// Parse ISO 8601 duration string → total seconds. Spec is the Lean module
// `lean/Planner/Iso8601Duration.lean` in multiplayer-fabric-taskweft, which
// in turn mirrors `Timex.Parse.Duration.Parsers.ISO8601Parser` byte-for-byte:
// grammar, error cases, and UTC-elapsed unit normalisation
// (Y=365d, M-before-T=30d, W=7d, D=86400s). Civil time is out of scope.
//
// Recognised: PnY nM nW nD T nH nM nS, with `T` toggling date→time state.
// `M` before `T` is months; `M` after `T` is minutes. Fractional digits
// only allowed on `S` (Lean spec restriction). `P` alone is the zero
// duration. Returns -1.0 on any spec violation.
inline double tw_parse_duration(const std::string &dur) {
    if (dur.empty())     return -1.0;
    if (dur[0] != 'P')   return -1.0;
    if (dur.size() == 1) return 0.0;              // "P" alone = zero

    size_t  i        = 1;
    bool    in_time  = false;
    bool    saw_t    = false;
    bool    saw_w    = false;
    bool    saw_other= false;
    int64_t total_ms = 0;

    while (i < dur.size()) {
        char c = dur[i];

        if (c == 'T') {
            if (saw_t)              return -1.0; // duplicate T
            if (i + 1 >= dur.size())return -1.0; // T at end
            in_time = true;
            saw_t   = true;
            ++i;
            continue;
        }

        if (!std::isdigit((unsigned char)c)) return -1.0; // unexpected token

        // Integer part.
        int64_t whole = 0;
        while (i < dur.size() && std::isdigit((unsigned char)dur[i])) {
            whole = whole * 10 + (dur[i] - '0');
            ++i;
        }

        // Optional fractional part — first 3 digits accumulated as
        // milliseconds, any further digits consumed but discarded
        // (matches the Lean spec's bounded precision).
        int64_t frac_ms = 0;
        bool    had_frac = false;
        if (i < dur.size() && dur[i] == '.') {
            ++i;
            had_frac = true;
            int n = 0;
            while (i < dur.size() && std::isdigit((unsigned char)dur[i])) {
                if (n < 3) {
                    frac_ms = frac_ms * 10 + (dur[i] - '0');
                    ++n;
                }
                ++i;
            }
            for (int k = n; k < 3; ++k) frac_ms *= 10;
        }

        if (i >= dur.size()) return -1.0; // number with no unit

        char unit_c = dur[i++];

        // W is the basic-form-only unit and may not coexist with anything.
        if (unit_c == 'W' && (saw_other || in_time))           return -1.0;
        if (unit_c != 'W' && unit_c != 'T' && saw_w)           return -1.0;

        // Fractional only allowed on S (Lean spec).
        if (had_frac && unit_c != 'S')                          return -1.0;

        int64_t unit_ms = 0;
        if (in_time) {
            switch (unit_c) {
                case 'H': unit_ms = 3600LL * 1000;        saw_other = true; break;
                case 'M': unit_ms =   60LL * 1000;        saw_other = true; break;
                case 'S': unit_ms =          1000;        saw_other = true; break;
                case 'Y': case 'D':                       return -1.0; // dateAfterT
                default:                                  return -1.0; // unexpected
            }
        } else {
            switch (unit_c) {
                case 'Y': unit_ms = 365LL * 86400 * 1000; saw_other = true; break;
                case 'M': unit_ms =  30LL * 86400 * 1000; saw_other = true; break;
                case 'W': unit_ms =   7LL * 86400 * 1000; saw_w     = true; break;
                case 'D': unit_ms =          86400 * 1000;saw_other = true; break;
                case 'H': case 'S':                       return -1.0; // timeBeforeT
                default:                                  return -1.0; // unexpected
            }
        }

        total_ms += whole * unit_ms + (unit_c == 'S' ? frac_ms : 0);
    }

    return static_cast<double>(total_ms) / 1000.0;
}

// Format total seconds → ISO 8601 duration string. Whole-day spans render
// as PnD; sub-day remainders render as the time part PTnHnMnS. PT0S for zero.
inline std::string tw_format_duration(double seconds) {
    if (seconds < 0.0) seconds = 0.0;

    int    days = static_cast<int>(seconds / 86400.0);
    double rem  = seconds - days * 86400.0;
    int    h    = static_cast<int>(rem / 3600.0);
    rem        -= h * 3600.0;
    int    m    = static_cast<int>(rem / 60.0);
    rem        -= m * 60.0;

    bool no_time = (h == 0 && m == 0 && rem == 0.0);
    std::string s = "P";
    if (days > 0) s += std::to_string(days) + "D";
    if (no_time) {
        if (days == 0) s += "T0S";
        return s;
    }
    s += "T";
    if (h > 0) s += std::to_string(h) + "H";
    if (m > 0) s += std::to_string(m) + "M";
    if (rem > 0.0) {
        if (rem == static_cast<double>(static_cast<int>(rem)))
            s += std::to_string(static_cast<int>(rem)) + "S";
        else {
            char buf[32];
            std::snprintf(buf, sizeof(buf), "%.6gS", rem);
            s += buf;
        }
    }
    return s;
}

// STN is an implementation detail — hidden from callers.
// Public interface: TwTemporalStep, TwTemporalResult, tw_check_temporal().
namespace tw_detail {
struct STN {
    static constexpr double INF = std::numeric_limits<double>::infinity();

    std::vector<std::string>                points;
    std::unordered_map<std::string, size_t> idx;
    std::vector<std::vector<double>>        dist;

    void add_point(const std::string &p) {
        if (idx.count(p)) return;
        size_t n = points.size();
        idx[p] = n;
        points.push_back(p);
        for (auto &row : dist) row.push_back(INF);
        dist.push_back(std::vector<double>(n + 1, INF));
        dist[n][n] = 0.0;
    }

    void add_constraint(const std::string &from, const std::string &to,
                        double lo, double hi) {
        add_point(from); add_point(to);
        size_t fi = idx.at(from), ti = idx.at(to);
        if (hi  < dist[fi][ti]) dist[fi][ti] = hi;
        if (-lo < dist[ti][fi]) dist[ti][fi] = -lo;
    }

    bool consistent() const {
        size_t n = points.size();
        if (n == 0) return true;
        std::vector<std::vector<double>> d = dist;
        for (size_t k = 0; k < n; ++k)
            for (size_t i = 0; i < n; ++i) {
                if (d[i][k] == INF) continue;
                for (size_t j = 0; j < n; ++j) {
                    if (d[k][j] == INF) continue;
                    double via = d[i][k] + d[k][j];
                    if (via < d[i][j]) d[i][j] = via;
                }
            }
        for (size_t i = 0; i < n; ++i)
            if (d[i][i] < 0.0) return false;
        return true;
    }
};
} // namespace tw_detail

// Per-step temporal annotation — all time values are ISO 8601 strings.
struct TwTemporalStep {
    std::string action_name;
    std::string duration_iso;   // ISO 8601 duration (e.g. "PT10M"), empty if none
    std::string start_iso;      // ISO 8601 duration from origin (e.g. "PT0S")
    std::string end_iso;        // ISO 8601 duration from origin (e.g. "PT10M")
};

// Result of temporal analysis on a plan — all time in ISO 8601.
struct TwTemporalResult {
    bool        consistent;    // STN consistency check passed
    std::string total_iso;     // total plan duration as ISO 8601 (e.g. "PT20M")
    std::string origin_iso;    // origin offset supplied by caller (default "PT0S")
    std::vector<TwTemporalStep> steps;
};

// Build a sequential STN from a plan and return temporal metadata.
// Sequential assumption: each action starts exactly when the previous ends.
// Actions with no duration entry are treated as PT0S.
// origin_iso: ISO 8601 duration string for the plan start offset (default "PT0S").
// All start_iso/end_iso values are durations measured from the same zero reference.
inline TwTemporalResult tw_check_temporal(
        const std::vector<TwCall> &plan,
        const TwDomain            &domain,
        const std::string         &origin_iso = "PT0S") {
    TwTemporalResult r;
    r.consistent = true;
    r.origin_iso = origin_iso;

    double origin_s = tw_parse_duration(origin_iso);
    if (origin_s < 0.0) origin_s = 0.0;

    if (plan.empty()) {
        r.total_iso = "PT0S";
        return r;
    }

    tw_detail::STN stn;
    stn.add_point("t0");
    std::string prev_end = "t0";
    double current_s     = origin_s;
    double total_s       = 0.0;

    for (size_t i = 0; i < plan.size(); ++i) {
        const std::string &name = plan[i].name;

        double dur_s = 0.0;
        std::string dur_iso;
        std::unordered_map<std::string, std::string>::const_iterator dit =
            domain.action_durations.find(name);
        if (dit != domain.action_durations.end()) {
            dur_iso = dit->second;
            double parsed = tw_parse_duration(dur_iso);
            if (parsed >= 0.0) dur_s = parsed;
        }

        TwTemporalStep step;
        step.action_name  = name;
        step.duration_iso = dur_iso.empty() ? "PT0S" : dur_iso;
        step.start_iso    = tw_format_duration(current_s);
        step.end_iso      = tw_format_duration(current_s + dur_s);
        r.steps.push_back(std::move(step));

        current_s += dur_s;
        total_s   += dur_s;

        std::string a_start = "a" + std::to_string(i) + "_start";
        std::string a_end   = "a" + std::to_string(i) + "_end";
        stn.add_constraint(prev_end, a_start, 0.0, 0.0);
        stn.add_constraint(a_start,  a_end,   dur_s, dur_s);
        prev_end = a_end;
    }

    r.consistent = stn.consistent();
    r.total_iso  = tw_format_duration(total_s);
    return r;
}

// Serialise a plan + TwTemporalResult as a JSON object.
inline std::string tw_temporal_to_json(const std::vector<TwCall> &plan,
                                        const TwTemporalResult   &tr,
                                        const std::string        &plan_json) {
    std::ostringstream o;
    o << "{\n";
    o << "  \"plan\": " << plan_json << ",\n";
    o << "  \"temporal\": {\n";
    o << "    \"consistent\": " << (tr.consistent ? "true" : "false") << ",\n";
    o << "    \"origin\": \"" << tr.origin_iso << "\",\n";
    o << "    \"total\": \"" << tr.total_iso << "\",\n";
    o << "    \"steps\": [";
    for (size_t i = 0; i < tr.steps.size(); ++i) {
        if (i) o << ", ";
        o << "{\"action\": \"" << tr.steps[i].action_name
          << "\", \"duration\": \"" << tr.steps[i].duration_iso
          << "\", \"start\": \"" << tr.steps[i].start_iso
          << "\", \"end\": \"" << tr.steps[i].end_iso << "\"}";
    }
    o << "]\n";
    o << "  }\n";
    o << "}";
    return o.str();
}
