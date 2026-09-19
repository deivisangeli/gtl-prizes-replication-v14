"""19_awarding_countries.py

Generates r2_2_awarding_country_table.tex (supplementary table) and r2_2_desc_macros.tex:
which countries award the 99 prizes, counted by prize and by 2015-2024
recognition, and where the laureates are based (the countries on their small-team
papers up to the award year, script 17) against where the field's benchmark pool
is based (the 1,000 most-cited researchers per field and award year, weighted by
the recognitions' field-year mix).

Inputs  output/intermediate/prize_awarding_countries.csv     (script 15)
        output/intermediate/r2_2_winner_countries.csv        (script 17)
        output/intermediate/r2_2_home_awards_by_country.csv  (script 17, main pool)
        data/openalex/pool_v2_top1000/top_pool_field_year.csv, top_pool_field_country_year.csv
Run from the repository root.
"""
import os
import re

import pandas as pd

DATA = "data"
INTER = os.path.join("output", "intermediate")
TABLES = os.path.join("output", "tables")
POOL_DIR = os.path.join(DATA, "openalex", "pool_v2_top1000")

# ---- inputs -----------------------------------------------------------------
pc = pd.read_csv(os.path.join(INTER, "prize_awarding_countries.csv"), encoding="utf-8")
w = pd.read_csv(os.path.join(INTER, "r2_2_winner_countries.csv"), encoding="utf-8")
byc = pd.read_csv(os.path.join(INTER, "r2_2_home_awards_by_country.csv"), encoding="utf-8")
pool_n = pd.read_csv(os.path.join(POOL_DIR, "top_pool_field_year.csv"))
pool_c = pd.read_csv(os.path.join(POOL_DIR, "top_pool_field_country_year.csv"))
assert len(pc) == 99, len(pc)
assert w.AwardingCountry.notna().all()

REGION = {"United States": "North America", "Canada": "North America",
          "Sweden": "Europe", "United Kingdom": "Europe", "Spain": "Europe",
          "Germany": "Europe", "Netherlands": "Europe", "Norway": "Europe",
          "Italy": "Europe", "Switzerland": "Europe", "Denmark": "Europe",
          "Belgium": "Europe", "Finland": "Europe",
          "Japan": "Asia and Middle East", "Israel": "Asia and Middle East",
          "Hong Kong": "Asia and Middle East", "Saudi Arabia": "Asia and Middle East",
          "Taiwan": "Asia and Middle East",
          "International": "International"}
assert set(pc.AwardingCountry) <= set(REGION), set(pc.AwardingCountry) - set(REGION)
pc["region"] = pc.AwardingCountry.map(REGION)

# ISO code of each awarding country, for the laureate/pool columns
ISO = {"United States": "US", "Canada": "CA", "Sweden": "SE", "United Kingdom": "GB",
       "Spain": "ES", "Germany": "DE", "Netherlands": "NL", "Norway": "NO", "Italy": "IT",
       "Switzerland": "CH", "Denmark": "DK", "Belgium": "BE", "Finland": "FI",
       "Japan": "JP", "Israel": "IL", "Hong Kong": "HK", "Saudi Arabia": "SA", "Taiwan": "TW"}
# Hong Kong and Taiwan award prizes of their own, so they are awarding rows here and
# their received shares are their own. Only the home-bias test folds them into Greater
# China; folding them here too would list Hong Kong twice.
NAMES = {"FR": "France", "AU": "Australia", "RU": "Russia", "AT": "Austria", "SG": "Singapore",
         "IN": "India", "KR": "South Korea", "ZA": "South Africa", "NZ": "New Zealand", "IE": "Ireland",
         "CZ": "Czech Republic", "PL": "Poland", "BR": "Brazil", "HU": "Hungary", "PT": "Portugal",
         "CN": "China (mainland)", "MO": "Macau"}
N_TOP_OTHER = 5
CN_FOLD = ["CN", "HK", "MO", "TW"]     # Greater China: mainland, Hong Kong, Macau, Taiwan

# ---- 1. awarding countries, by prize and by recognition ----------------------
n_rec_all = len(w)
rec_by_c = w.groupby("AwardingCountry").size().rename("recognitions")
prizes_by_c = pc.groupby("AwardingCountry").size().rename("prizes")
tab = pd.concat([prizes_by_c, rec_by_c], axis=1).fillna(0).astype(int)
tab["rec_pct"] = 100 * tab.recognitions / n_rec_all
tab = tab.sort_values(["recognitions", "prizes"], ascending=False)

reg = pc.groupby("region").size().rename("prizes").to_frame()
reg["recognitions"] = w.groupby(w.AwardingCountry.map(REGION)).size()
reg["rec_pct"] = 100 * reg.recognitions / n_rec_all

n_countries = pc.loc[pc.AwardingCountry != "International", "AwardingCountry"].nunique()

# ---- 2. laureates vs the benchmark pool, by country ---------------------------
# main_field: each prize's field, as 17_home_bias.R defines it
placed = w[w.countries_prior.notna() & (w.countries_prior != "")]
assert placed.main_field.notna().all()
n_placed = len(placed)
mix = placed.groupby(["main_field", "Year"]).size()
pool_size = pool_n.set_index(["field", "year"]).n_pool
pool_cnt = pool_c.set_index(["field", "country", "year"]).n_authors
assert all(k in pool_size.index for k in mix.index)
assert (pool_c.country == "CNX").any(), "pool file has no folded China (CNX) rows"


def pool_share(key):
    """Share of the benchmark pool based in `key`, weighted by the recognitions'
    field-year mix. `key` is a country code, or CNX for the pool's Greater China aggregate."""
    num = sum(k * pool_cnt.get((f, key, y), 0) / pool_size[(f, y)] for (f, y), k in mix.items())
    return 100 * num / mix.sum()


def laureate_share(*ccs_):
    """Share of placeable recognitions whose laureate was based in any of `ccs_`,
    from the UNFOLDED country set (a laureate based in two of them counts once)."""
    want = set(ccs_)
    return 100 * placed.countries_prior_raw.fillna("").str.split(";").apply(
        lambda L: bool(want & set(L))).mean()


pool_ccs = set(pool_c.country) - {"CNX"}          # CNX is the fold, reported separately
laur_ccs = set(c for v in placed.countries_prior_raw.fillna("") for c in v.split(";") if c)
ccs = sorted(set(ISO.values()) | pool_ccs | laur_ccs)
geo = pd.DataFrame({"cc": ccs,
                    "laureate_pct": [laureate_share(c) for c in ccs],
                    "pool_pct": [pool_share(c) for c in ccs]}).set_index("cc")
GREATER_CN = {"laureate_pct": laureate_share(*CN_FOLD), "pool_pct": pool_share("CNX")}

geo["awarding"] = geo.index.isin(set(ISO.values()))
other_top = geo[~geo.awarding].sort_values("pool_pct", ascending=False).head(N_TOP_OTHER).index.tolist()
shown = set(ISO.values()) | set(other_top)
rest = geo[~geo.index.isin(shown)]
OTHER = {cc: NAMES[cc] for cc in sorted(other_top, key=lambda c: -geo.pool_pct[c])}
tab["cc"] = tab.index.map(ISO)
tab["laureate_pct"] = tab.cc.map(geo.laureate_pct)
tab["pool_pct"] = tab.cc.map(geo.pool_pct)


# ---- macros --------------------------------------------------------------------
def slug(s):
    return re.sub(r"[^A-Za-z]", "", s.title())


def fmt(v, d=1):
    return f"{v:.{d}f}"


def thousands(n):
    return f"{n:,}".replace(",", "{,}")


M = {}
M["NCountries"] = n_countries
M["PrizesUnitedStates"] = int(tab.loc["United States", "prizes"])
M["RecPctUnitedStates"] = fmt(tab.loc["United States", "rec_pct"], 0)
for rname in ("Europe", "Asia and Middle East"):
    M["Prizes" + slug(rname)] = int(reg.loc[rname, "prizes"])
    M["RecPct" + slug(rname)] = fmt(reg.loc[rname, "rec_pct"], 0)
for cc in ("US", "GB", "FR"):
    M["LaurPct" + cc] = fmt(geo.laureate_pct[cc])
    M["PoolPct" + cc] = fmt(geo.pool_pct[cc])
# Greater China (mainland, Hong Kong, Macau, Taiwan) laureate and pool shares
M["LaurPctGreaterCN"] = fmt(GREATER_CN["laureate_pct"])
M["PoolPctGreaterCN"] = fmt(GREATER_CN["pool_pct"])

GEN = "% generated by code/19_awarding_countries.py -- do not edit by hand"
lines = [GEN] + [f"\\newcommand{{\\rTwoTwo{k}}}{{{v}}}" for k, v in M.items()]
with open(os.path.join(TABLES, "r2_2_desc_macros.tex"), "w", encoding="utf-8", newline="\n") as f:
    f.write("\n".join(lines) + "\n")


# ---- table ---------------------------------------------------------------------
hb = byc.set_index("AwardingCountry")
n_placed_national = int(byc.n_awards.sum())
rest_n_countries = int((rest.pool_pct > 0).sum())


def stack(*lines):
    """Header cell with one line per argument, so wide labels do not widen the column."""
    return "\\begin{tabular}[c]{@{}c@{}}" + " \\\\ ".join(lines) + "\\end{tabular}"


def cell(v, scale=1.0):
    return "--" if v is None or pd.isna(v) else fmt(scale * v)


def ratio(a, b):
    if a is None or b is None or pd.isna(a) or pd.isna(b) or b == 0:
        return "--"
    return fmt(a / b)


def row(name, prizes, rec, rec_pct, home_act, home_exp, lp, pp):
    return " & ".join([name, str(prizes), str(rec), fmt(rec_pct),
                       cell(home_act, 100), cell(home_exp, 100), ratio(home_act, home_exp),
                       cell(lp), cell(pp), ratio(lp, pp)]) + " \\\\"


rows = []
for c, r in tab.iterrows():
    ha = hb.loc[c, "actual_share"] if c in hb.index else None
    he = hb.loc[c, "expected_share"] if c in hb.index else None
    rows.append(row(c, int(r.prizes), int(r.recognitions), r.rec_pct, ha, he, r.laureate_pct, r.pool_pct))
rows.append("\\midrule")
rows.append("\\multicolumn{10}{l}{\\emph{Countries awarding none of the 99 prizes (the %d with the largest pool shares), and all others:}} \\\\" % N_TOP_OTHER)
for cc, name in OTHER.items():
    rows.append(row(name, 0, 0, 0.0, None, None, geo.laureate_pct[cc], geo.pool_pct[cc]))
rows.append(row("All other countries (%d)" % rest_n_countries, 0, 0, 0.0, None, None,
                rest.laureate_pct.sum(), rest.pool_pct.sum()))

T = [GEN,
     "\\begin{table}[H]", "\\centering \\footnotesize \\setlength{\\tabcolsep}{3pt}",
     "\\caption{Awarding Countries of the 99 Prizes, Home Bias, and Where Laureates Are Based}",
     "\\label{tab:r2_2_awarding_countries}",
     "\\begin{tabular}{lrrrrrrrrr}", "\\toprule",
     "Country & " + stack("Prizes", "given by", "country") + " & \\multicolumn{2}{c}{" + stack("Recognitions awarded", "by the country's", "organizations") + "} & \\multicolumn{3}{c}{" + stack("Share of the country's", "recognitions given to", "scientists based there (\\%)") + "} & \\multicolumn{3}{c}{" + stack("Share of all recognitions", "received by scientists", "based in the country (\\%)") + "} \\\\",
     "\\cmidrule(lr){3-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
     " & & N & \\% of all & Actual & " + stack("Expected", "from pool") + " & Ratio & Actual & " + stack("Share", "of pool") + " & Ratio \\\\",
     "\\midrule"] + rows + ["\\bottomrule", "\\end{tabular}",
     "\\par\\smallskip\\begin{minipage}{\\textwidth}\\footnotesize\\textit{Note}: Prizes and recognitions (2015--2024 award events) "
     "are counted by the country of the awarding organization; the three international prizes (Fields Medal, "
     "ICTP Ramanujan Prize, IABSE Award of Merit) have no national home. Home shares are computed over the "
     f"{thousands(n_placed_national)} recognitions of national prizes with a placeable laureate: actual is the share going "
     "to a laureate who had been based in the awarding country by the award year, expected is the share of the "
     "field's benchmark pool (the 1{,}000 researchers with the most citations to small-team papers, per field and "
     f"award year) based there. The last three columns take all {thousands(n_placed)} placeable recognitions of the 99 "
     "prizes: the share received by laureates based in the country, and the country's share of the benchmark pool "
     "weighted by the recognitions' field-year mix; a researcher based in two countries counts in both. Each ratio "
     "divides the actual share by the expected or pool share. Hong Kong and Taiwan award prizes of their own and "
     "appear as awarding jurisdictions; mainland China awards none of the 99 and appears in the last block. The "
     "home-bias test folds Hong Kong, Macau and Taiwan into Greater China on both sides, so the home columns of "
     "the Hong Kong and Taiwan rows are measured against a pool that includes the mainland: a laureate based in "
     "Beijing counts as local for a prize awarded from Hong Kong. The received-share columns fold nothing; taken "
     f"together, Greater China hosts {fmt(GREATER_CN['laureate_pct'])}\\% of laureates against "
     f"{fmt(GREATER_CN['pool_pct'])}\\% of the pool. The last block lists the {N_TOP_OTHER} non-awarding countries with the largest "
     "pool shares and pools every other country in one row (the count is the number of countries with any pool "
     "member).\\end{minipage}",
     "\\end{table}"]
with open(os.path.join(TABLES, "r2_2_awarding_country_table.tex"), "w", encoding="utf-8", newline="\n") as f:
    f.write("\n".join(T) + "\n")
