"""Build the LaTeX result tables, the number macros and Supplementary Table S1
from the Diebold-Mariano output of the pipeline.

Usage (from C:/covid):
  python -I manuscript/reproduce/make_tables.py \
      --by-country outputs/diebold_mariano/diebold_mariano_by_country_latest.csv \
      --model-input-info outputs/diebold_mariano/model_input_info.json \
      --out-tables manuscript/tables --out-supp manuscript/supplementary
"""
import argparse
import csv
import json
import math
import os
import statistics

FAMILIES = [
    ("A", "ARIMA vs ARFIMA", "ARIMA vs. ARFIMA"),
    ("R", "Random Forest vs Fractional Random Forest", "RF vs. Fractional RF"),
    ("X", "XGBoost vs Fractional XGBoost", "XGBoost vs. Fractional XGBoost"),
]
PANELS = {"A": "Panel A", "R": "Panel B", "X": "Panel C"}


def fnum(x):
    try:
        v = float(x)
    except (TypeError, ValueError):
        return float("nan")
    return v


def isnan(x):
    return x is None or (isinstance(x, float) and math.isnan(x))


def sci(x, sig=3):
    """LaTeX scientific notation with `sig` significant digits."""
    if isnan(x):
        return "--"
    if x == 0:
        return "0"
    exp = int(math.floor(math.log10(abs(x))))
    mant = x / 10 ** exp
    mant = round(mant, sig - 1)
    if abs(mant) >= 10:
        mant /= 10
        exp += 1
    return r"\mbox{$%s \times 10^{%d}$}" % (("%." + str(sig - 1) + "f") % mant, exp)


def fmt_p(p):
    if isnan(p):
        return "undefined"
    if p < 1e-16:
        return r"\mbox{$<10^{-16}$}"
    if p < 1e-3:
        return sci(p, 2)
    return "%.3f" % p


def fmt_dm(x):
    return "--" if isnan(x) else "%.3f" % x


def fmt_loss(x):
    if isnan(x):
        return "--"
    if abs(x) < 1e4:
        return "{:,.2f}".format(x)
    return sci(x, 3)


def q_type7(v, p):
    v = sorted(v)
    n = len(v)
    h = (n - 1) * p
    lo = math.floor(h)
    hi = math.ceil(h)
    return v[lo] + (h - lo) * (v[hi] - v[lo])


def tex_escape(s):
    return (s.replace("&", r"\&").replace("%", r"\%").replace("_", r"\_").replace("#", r"\#"))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--by-country", required=True)
    ap.add_argument("--model-input-info", default=None)
    ap.add_argument("--out-tables", required=True)
    ap.add_argument("--out-supp", required=True)
    ap.add_argument("--top-n", type=int, default=2, help="illustrative units per side and family")
    ap.add_argument("--arfima-details", default=None, help="CSV written by reproduce/arima_arfima_fit_details.R")
    args = ap.parse_args()

    os.makedirs(args.out_tables, exist_ok=True)
    os.makedirs(args.out_supp, exist_ok=True)

    rows = list(csv.DictReader(open(args.by_country, encoding="utf-8")))
    for r in rows:
        for k in ("n", "dm_statistic", "dm_p_value", "mean_loss_standard", "mean_loss_fractional", "mean_loss_diff"):
            r[k] = fnum(r[k])
        r["significant"] = (not isnan(r["dm_p_value"])) and r["dm_p_value"] < 0.05

    info = {}
    if args.model_input_info and os.path.exists(args.model_input_info):
        info = json.load(open(args.model_input_info, encoding="utf-8"))

    stats = {}
    for key, fam, short in FAMILIES:
        R = [r for r in rows if r["family"] == fam]
        pf = [r["dm_p_value"] for r in R if not isnan(r["dm_p_value"])]
        dmf = [r["dm_statistic"] for r in R if not isnan(r["dm_statistic"])]
        ld = [r["mean_loss_diff"] for r in R if not isnan(r["mean_loss_diff"])]
        sig = [r for r in R if r["significant"]]
        s = {
            "family": fam, "short": short,
            "units": len(R),
            "defined": len(pf),
            "undefined": len(R) - len(pf),
            "sig": len(sig),
            "std_lower": sum(r["better_model"] == "standard" for r in R),
            "frac_lower": sum(r["better_model"] == "fractional" for r in R),
            "ties": sum(r["better_model"] == "tie" for r in R),
            "sig_std": sum(r["better_model"] == "standard" for r in sig),
            "sig_frac": sum(r["better_model"] == "fractional" for r in sig),
            "median_p": statistics.median(pf) if pf else float("nan"),
            "min_p": min(pf) if pf else float("nan"),
            "median_n": statistics.median([r["n"] for r in R]),
            "median_dm": statistics.median(dmf) if dmf else float("nan"),
            "q1_dm": q_type7(dmf, 0.25) if dmf else float("nan"),
            "q3_dm": q_type7(dmf, 0.75) if dmf else float("nan"),
            "median_ld": statistics.median(ld) if ld else float("nan"),
            "mean_ld": statistics.mean(ld) if ld else float("nan"),
            "bartlett": sum(r["dm_varestimator"] == "bartlett" and not isnan(r["dm_p_value"]) for r in R),
        }
        s["sig_pct"] = 100.0 * s["sig"] / s["units"]
        s["std_pct"] = 100.0 * s["std_lower"] / s["units"]
        s["frac_pct"] = 100.0 * s["frac_lower"] / s["units"]
        s["fds"] = s["sig_frac"] / s["defined"] if s["defined"] else float("nan")
        s["label"] = "standard" if s["mean_ld"] < 0 else ("fractional" if s["mean_ld"] > 0 else "tie")
        s["label_by_counts"] = "standard" if s["sig_std"] > s["sig_frac"] else ("fractional" if s["sig_frac"] > s["sig_std"] else "balanced")
        # illustrative units: exclude degenerate (near-zero incidence) series, whose
        # DM statistics are numerically extreme but epidemiologically meaningless
        valid = [r for r in sig if r["mean_loss_standard"] >= 1.0 and r["mean_loss_fractional"] >= 1.0]
        std_sig = sorted([r for r in valid if r["better_model"] == "standard"], key=lambda r: (r["dm_p_value"], -abs(r["mean_loss_diff"])))
        frac_sig = sorted([r for r in valid if r["better_model"] == "fractional"], key=lambda r: (r["dm_p_value"], -abs(r["mean_loss_diff"])))
        s["examples"] = std_sig[: args.top_n] + frac_sig[: args.top_n]
        s["n_degenerate_sig"] = len(sig) - len(valid)
        stats[key] = s

    n_units = stats["A"]["units"]
    n_comp = sum(stats[k]["units"] for k in stats)

    # ---------------- numbers.tex ----------------
    L = []

    def mac(name, val):
        L.append(r"\newcommand{\%s}{%s}" % (name, val))

    mac("DMnUnits", n_units)
    mac("DMnComparisons", n_comp)
    mac("DMstart", info.get("start_text", "22 January 2020"))
    mac("DMend", info.get("end_text", "9 March 2023"))
    mac("DMnDays", info.get("n_days", "1143"))
    mac("DMnFracFeatures", info.get("n_frac_features", "12"))
    mac("DMnStdFeatures", info.get("n_std_features", "20"))
    n_days = int(info.get("n_days", 1143))
    arima_test = int(stats["A"]["median_n"])
    mac("DMtrainDaysArima", n_days - arima_test)
    mac("DMtestDaysArima", arima_test)
    mac("DMtestDaysML", int(stats["R"]["median_n"]))
    for key, s in stats.items():
        mac("DM%ssig" % key, s["sig"])
        mac("DM%ssigPct" % key, "%.2f" % s["sig_pct"])
        mac("DM%sstdWins" % key, s["sig_std"])
        mac("DM%sfracWins" % key, s["sig_frac"])
        mac("DM%sstdLower" % key, s["std_lower"])
        mac("DM%sfracLower" % key, s["frac_lower"])
        mac("DM%sundefined" % key, s["undefined"])
        mac("DM%sdefined" % key, s["defined"])
        mac("DM%snonsig" % key, s["units"] - s["sig"])
        mac("DM%smedP" % key, fmt_p(s["median_p"]))
        mac("DM%smedDM" % key, fmt_dm(s["median_dm"]))
        mac("DM%smedN" % key, "%d" % s["median_n"])
        mac("DM%sFDS" % key, "%.3f" % s["fds"])
        mac("DM%slabel" % key, s["label"])
        mac("DM%slabelCounts" % key, s["label_by_counts"])
        mac("DM%sbartlett" % key, s["bartlett"])
        mac("DM%smedLD" % key, fmt_loss(s["median_ld"]))
        mac("DM%smeanLD" % key, fmt_loss(s["mean_ld"]))
        mac("DM%sqOneDM" % key, fmt_dm(s["q1_dm"]))
        mac("DM%sqThreeDM" % key, fmt_dm(s["q3_dm"]))
        mac("DM%sminP" % key, fmt_p(s["min_p"]))
    mac("DMtotalSig", sum(s["sig"] for s in stats.values()))
    mac("DMtotalFracWins", sum(s["sig_frac"] for s in stats.values()))
    mac("DMtotalStdWins", sum(s["sig_std"] for s in stats.values()))
    tot_sig = sum(s["sig"] for s in stats.values())
    mac("DMtotalFracSharePct", "%.1f" % (100.0 * sum(s["sig_frac"] for s in stats.values()) / tot_sig) if tot_sig else "--")
    for key, s in stats.items():
        mac("DM%sstdScore" % key, "%.3f" % (s["sig_std"] / s["defined"]) if s["defined"] else "--")
        mac("DM%sdegenerateSig" % key, s["n_degenerate_sig"])

    # cross-family agreement (significant outcomes only)
    def outcome(fam):
        d = {}
        for r in rows:
            if r["family"] == fam:
                d[r["country"]] = (r["better_model"] if r["significant"] else "ns")
        return d
    oA, oR, oX = (outcome(FAMILIES[i][1]) for i in range(3))
    units = sorted(oA)
    both_ml_frac = [u for u in units if oR[u] == "fractional" and oX[u] == "fractional"]
    both_ml_std = [u for u in units if oR[u] == "standard" and oX[u] == "standard"]
    ml_opposite = [u for u in units if {oR[u], oX[u]} == {"standard", "fractional"}]
    all3_frac = [u for u in both_ml_frac if oA[u] == "fractional"]
    all3_std = [u for u in both_ml_std if oA[u] == "standard"]
    any_frac = [u for u in units if "fractional" in (oA[u], oR[u], oX[u])]
    any_std = [u for u in units if "standard" in (oA[u], oR[u], oX[u])]
    mac("DMbothMLfrac", len(both_ml_frac))
    mac("DMbothMLstd", len(both_ml_std))
    mac("DMmlOpposite", len(ml_opposite))
    mac("DMallThreeFrac", len(all3_frac))
    mac("DMallThreeStd", len(all3_std))
    mac("DManyFrac", len(any_frac))
    mac("DManyStd", len(any_std))
    mac("DMallThreeFracList", ", ".join(tex_escape(u) for u in all3_frac) if all3_frac else "none")
    mac("DMallThreeStdList", ", ".join(tex_escape(u) for u in all3_std) if all3_std else "none")
    mac("DMbothMLfracList", ", ".join(tex_escape(u) for u in both_ml_frac) if both_ml_frac else "none")
    mac("DMbothMLstdList", ", ".join(tex_escape(u) for u in both_ml_std) if both_ml_std else "none")
    undefined_units = sorted({r["country"] for r in rows if isnan(r["dm_p_value"])})
    mac("DMundefinedList", ", ".join(tex_escape(u) for u in undefined_units) if undefined_units else "none")
    bart = sorted({"%s (%s)" % (tex_escape(r["country"]), {FAMILIES[0][1]: "ARIMA", FAMILIES[1][1]: "RF", FAMILIES[2][1]: "XGBoost"}[r["family"]]) for r in rows if r["dm_varestimator"] == "bartlett" and not isnan(r["dm_p_value"])})
    mac("DMbartlettList", ", ".join(bart) if bart else "none")
    mac("DMbartlettTotal", len(bart))

    # ARIMA / ARFIMA fit details (optional)
    if args.arfima_details and os.path.exists(args.arfima_details):
        det = list(csv.DictReader(open(args.arfima_details, encoding="utf-8")))
        dvals = sorted(fnum(r["arfima_d"]) for r in det if not isnan(fnum(r["arfima_d"])))
        n_fb = sum(r.get("arfima_mean_fallback", "FALSE") == "TRUE" for r in det)
        d_int = [int(float(r["arima_d"])) for r in det if r.get("arima_d") not in ("", "NA", None)]
        mac("ARFIMAfallback", n_fb)
        mac("ARFIMAdMedian", "%.3f" % statistics.median(dvals))
        mac("ARFIMAdQone", "%.3f" % q_type7(dvals, 0.25))
        mac("ARFIMAdQthree", "%.3f" % q_type7(dvals, 0.75))
        mac("ARFIMAdLow", sum(x < 0.05 for x in dvals))
        mac("ARFIMAdHigh", sum(x >= 0.45 for x in dvals))
        mac("ARIMAintegrated", sum(x >= 1 for x in d_int))
        mac("ARIMAstationary", sum(x == 0 for x in d_int))
        stats["_arfima"] = {"fallback": n_fb, "d_median": statistics.median(dvals), "d_low": sum(x < 0.05 for x in dvals), "d_high": sum(x >= 0.45 for x in dvals), "arima_I1": sum(x >= 1 for x in d_int)}
    stats["_agreement"] = {"both_ml_frac": both_ml_frac, "both_ml_std": both_ml_std, "ml_opposite": len(ml_opposite), "all3_frac": all3_frac, "all3_std": all3_std, "any_frac": len(any_frac), "any_std": len(any_std), "undefined": undefined_units, "bartlett": bart}
    with open(os.path.join(args.out_tables, "numbers.tex"), "w", encoding="utf-8") as f:
        f.write("% Generated by reproduce/make_tables.py -- do not edit by hand.\n")
        f.write("\n".join(L) + "\n")

    # ---------------- Table: global counts ----------------
    T = []
    T.append(r"\begin{table}[H]")
    T.append(r"\caption{Global summary of the Diebold--Mariano (DM) comparisons by model family across %d reporting units (squared-error loss, 5\%% significance level). ``Lower loss'' counts compare mean test-set losses irrespective of significance; ``significant wins'' count units with $p<0.05$. Undefined tests are units for which the long-run variance of the loss differential is zero, so that no DM statistic exists.\label{tab:dm-global}}" % n_units)
    T.append(r"\begin{adjustwidth}{-\extralength}{0cm}")
    T.append(r"\small")
    T.append(r"\begin{tabularx}{\fulllength}{lCCCCCCC}")
    T.append(r"\toprule")
    T.append(r"\textbf{Comparison} & \textbf{Units} & \textbf{Significant} & \textbf{Standard} & \textbf{Fractional} & \textbf{Significant} & \textbf{Significant} & \textbf{Undefined}\\")
    T.append(r" & & \textbf{(5\%)} & \textbf{lower loss} & \textbf{lower loss} & \textbf{standard wins} & \textbf{fractional wins} & \textbf{tests}\\")
    T.append(r"\midrule")
    for key, fam, short in FAMILIES:
        s = stats[key]
        T.append("%s & %d & %d (%.2f\\%%) & %d (%.2f\\%%) & %d (%.2f\\%%) & %d & %d & %d\\\\" % (
            short, s["units"], s["sig"], s["sig_pct"], s["std_lower"], s["std_pct"], s["frac_lower"], s["frac_pct"], s["sig_std"], s["sig_frac"], s["undefined"]))
    T.append(r"\bottomrule")
    T.append(r"\end{tabularx}")
    T.append(r"\end{adjustwidth}")
    T.append(r"\end{table}")
    open(os.path.join(args.out_tables, "table_dm_global.tex"), "w", encoding="utf-8").write("\n".join(T) + "\n")

    # ---------------- Table: distributional diagnostics ----------------
    T = []
    T.append(r"\begin{table}[H]")
    T.append(r"\caption{Distributional diagnostics of the Diebold--Mariano comparisons. $n$ is the number of aligned test-set forecast errors per unit; IQR is the interquartile range of the DM statistic; loss differences are mean standard loss minus mean fractional loss (positive values favour the fractional model); FDS is the fractional dominance score (significant fractional wins divided by the number of units with a defined test); ``Majority'' names the model with the larger number of significant wins. The mean loss difference is dominated by a few high-incidence units and is reported for completeness only.\label{tab:dm-diagnostics}}")
    T.append(r"\begin{adjustwidth}{-\extralength}{0cm}")
    T.append(r"\small")
    T.append(r"\begin{tabularx}{\fulllength}{lCCCCCCCC}")
    T.append(r"\toprule")
    T.append(r"\textbf{Comparison} & \textbf{Median $n$} & \textbf{Median DM} & \textbf{IQR of DM} & \textbf{Median $p$} & \textbf{Median loss diff.} & \textbf{Mean loss diff.} & \textbf{FDS} & \textbf{Majority}\\")
    T.append(r"\midrule")
    for key, fam, short in FAMILIES:
        s = stats[key]
        T.append("%s & %d & %s & %s to %s & %s & %s & %s & %.3f & %s\\\\" % (
            short, s["median_n"], fmt_dm(s["median_dm"]), fmt_dm(s["q1_dm"]), fmt_dm(s["q3_dm"]), fmt_p(s["median_p"]),
            fmt_loss(s["median_ld"]), fmt_loss(s["mean_ld"]), s["fds"], s["label_by_counts"]))
    T.append(r"\bottomrule")
    T.append(r"\end{tabularx}")
    T.append(r"\end{adjustwidth}")
    T.append(r"\end{table}")
    open(os.path.join(args.out_tables, "table_dm_diagnostics.tex"), "w", encoding="utf-8").write("\n".join(T) + "\n")

    # ---------------- Table: illustrative units ----------------
    T = []
    T.append(r"\begin{table}[H]")
    T.append(r"\caption{Illustrative reporting units selected by a fixed rule from the full Diebold--Mariano output: for each model family, the %d significant standard-model wins and the %d significant fractional-model wins with the smallest $p$-values among units whose mean losses both exceed one squared case (this excludes series that are essentially zero over the evaluation window). Loss difference is mean standard loss minus mean fractional loss, so positive values favour the fractional model. The complete unit-level output is given in Supplementary Table S1.\label{tab:dm-examples}}" % (args.top_n, args.top_n))
    T.append(r"\begin{adjustwidth}{-\extralength}{0cm}")
    T.append(r"\footnotesize")
    T.append(r"\begin{tabularx}{\fulllength}{llCCCCCCC}")
    T.append(r"\toprule")
    T.append(r"\textbf{Comparison} & \textbf{Reporting unit} & $\boldsymbol{n}$ & \textbf{DM} & $\boldsymbol{p}$\textbf{-value} & \textbf{Mean standard loss} & \textbf{Mean fractional loss} & \textbf{Loss difference} & \textbf{Lower-loss model}\\")
    T.append(r"\midrule")
    for i, (key, fam, short) in enumerate(FAMILIES):
        s = stats[key]
        for r in s["examples"]:
            T.append("%s & %s & %d & %s & %s & %s & %s & %s & %s\\\\" % (
                short, tex_escape(r["country"]), r["n"], fmt_dm(r["dm_statistic"]), fmt_p(r["dm_p_value"]),
                sci(r["mean_loss_standard"], 2), sci(r["mean_loss_fractional"], 2), sci(r["mean_loss_diff"], 2), r["better_model"]))
        if i < len(FAMILIES) - 1:
            T.append(r"\midrule")
    T.append(r"\bottomrule")
    T.append(r"\end{tabularx}")
    T.append(r"\end{adjustwidth}")
    T.append(r"\end{table}")
    open(os.path.join(args.out_tables, "table_dm_examples.tex"), "w", encoding="utf-8").write("\n".join(T) + "\n")

    # ---------------- Supplementary Table S1 ----------------
    header = ["reporting_unit", "model_comparison", "aligned_forecast_errors", "dm_statistic", "dm_p_value",
              "mean_loss_standard", "mean_loss_fractional", "loss_difference_standard_minus_fractional",
              "lower_loss_model", "significant_5pct", "variance_estimator", "note"]
    panels = {}
    for key, fam, short in FAMILIES:
        R = sorted([r for r in rows if r["family"] == fam], key=lambda r: r["country"])
        out = []
        for r in R:
            out.append([r["country"], fam, int(r["n"]),
                        "" if isnan(r["dm_statistic"]) else r["dm_statistic"],
                        "" if isnan(r["dm_p_value"]) else r["dm_p_value"],
                        r["mean_loss_standard"], r["mean_loss_fractional"], r["mean_loss_diff"],
                        r["better_model"], "yes" if r["significant"] else "no", r["dm_varestimator"], r["dm_note"]])
        panels[key] = out
        with open(os.path.join(args.out_supp, "Table_S1_%s_%s.csv" % (PANELS[key].replace(" ", ""), key)), "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f)
            w.writerow(header)
            w.writerows(out)
    try:
        import openpyxl
        from openpyxl.styles import Font
        wb = openpyxl.Workbook()
        ws = wb.active
        ws.title = "README"
        readme = [
            ["Supplementary Table S1. Complete reporting-unit-level Diebold-Mariano output."],
            [""],
            ["Panel A", FAMILIES[0][1]],
            ["Panel B", FAMILIES[1][1]],
            ["Panel C", FAMILIES[2][1]],
            [""],
            ["Columns"],
            ["reporting_unit", "JHU CSSE Country/Region label (countries, territories and special aggregations)"],
            ["aligned_forecast_errors", "number of test-set dates with finite forecasts from both models"],
            ["dm_statistic", "Diebold-Mariano statistic for d_t = L(e_standard) - L(e_fractional), squared loss; positive values favour the fractional model"],
            ["dm_p_value", "two-sided p-value (empty when the long-run variance of d_t is zero and the statistic is undefined)"],
            ["mean_loss_standard / mean_loss_fractional", "mean squared forecast error on the aligned test set"],
            ["loss_difference_standard_minus_fractional", "mean_loss_standard - mean_loss_fractional"],
            ["lower_loss_model", "model with the lower mean loss (irrespective of significance)"],
            ["significant_5pct", "yes if dm_p_value < 0.05"],
            ["variance_estimator", "acf (default) or bartlett (fallback when the acf long-run variance is negative)"],
            ["note", "diagnostic message from the pipeline"],
        ]
        for row in readme:
            ws.append(row)
        ws["A1"].font = Font(bold=True)
        for key, fam, short in FAMILIES:
            w2 = wb.create_sheet("%s %s" % (PANELS[key], short.replace("vs.", "vs").replace(" ", "_"))[:31])
            w2.append(header)
            for c in w2[1]:
                c.font = Font(bold=True)
            for row in panels[key]:
                w2.append(row)
        wb.save(os.path.join(args.out_supp, "Table_S1_Diebold_Mariano_full_output.xlsx"))
    except ImportError:
        print("openpyxl not available: xlsx not written")

    # ---------------- JSON summary for inspection ----------------
    dump = {k: {kk: vv for kk, vv in v.items() if kk != "examples"} for k, v in stats.items()}
    for k, v in stats.items():
        if "examples" in v:
            dump[k]["examples"] = [[r["country"], r["n"], r["dm_statistic"], r["dm_p_value"], r["mean_loss_diff"], r["better_model"]] for r in v["examples"]]
    json.dump(dump, open(os.path.join(args.out_tables, "dm_stats.json"), "w", encoding="utf-8"), indent=1)
    print(json.dumps(dump, indent=1))


if __name__ == "__main__":
    main()
