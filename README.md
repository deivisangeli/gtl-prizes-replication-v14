# Replication package: "The Missing Nobels"

Replication materials for

> Agarwal, R., Angeli, D., & Gaule, P. "The Missing Nobels."

One command regenerates every table, macro file and figure in the paper from the
data in `data/`; a second checks the result against the paper. Every push to
this repository runs both on GitHub Actions
(`.github/workflows/replicate.yml`).

## Quick start

```bash
# 1. R (4.5.1) and Python (3.11) with pip on the PATH
# 2. R packages, pinned in renv.lock
Rscript -e "install.packages('renv'); renv::restore()"
# 3. Python packages, pinned in requirements.txt
pip install -r requirements.txt
# 4. Reproduce everything (about 5 minutes)
Rscript run_all.R
# 5. Check the outputs against the paper
Rscript tests/test_outputs.R
```

`run_all.R` deletes `output/`, runs the scripts in `code/` in numeric order,
each in a fresh process, and stops at the first failure. Python scripts use the
interpreter named by the `PYTHON` environment variable (default `python3`, or
`python` on Windows).

## What the tests check

`tests/test_outputs.R` compares `output/` with `expected/`:

1. **Tables and macro files.** Every `.tex` file the paper `\input{}`s (9 tables
   and 13 macro files, listed in `expected/paper_exhibits.csv`) is regenerated
   identically to the paper's own copy in `expected/tables/` (line by line,
   ignoring comment lines and line endings).
2. **Figures.** Every figure the paper includes (17 files) is regenerated with
   the same plot data as the original analysis scripts that produced the
   paper's figures. Figure files themselves are not byte-comparable across
   machines (fonts, graphics devices, timestamps), so each script also writes
   the data ggplot2 draws from — every layer's computed positions, labels and
   colours — to `output/plotdata/`, and the test compares that with
   `expected/plotdata/` (character columns exactly, numbers to 1e-6).
3. **No extraneous files.** The run must create exactly the files listed in
   `tests/generated_files.txt`. Any other untracked file in the repository — a
   stray `Rplots.pdf`, a table the paper does not use, a cache — fails the test.

`expected/` is rebuilt by the authors with `tests/refresh_expected.R`, which
reads the exhibit list from the paper's LaTeX source, copies the paper's table
files, and runs the original scripts with `ggsave()` replaced by a plot-data
capture (`tests/capture_original.R`). It needs the authors' project repository
and is not part of the replication run.

## Repository structure

```
├── README.md
├── run_all.R                    Master script
├── _helpers.R                   Shared paths, packages and helpers (sourced by every script)
├── renv.lock, requirements.txt  Pinned R and Python packages
├── data/                        Input data (see Data below)
├── code/                        Analysis scripts, run in numeric order
│   ├── 01_ranking_tables.R        selectedPrizes, summaryStats, selectedECPrizes
│   ├── 02_correlation_table.R     corr
│   ├── 03_scatterplots.R          cum_prize_time, scatter_time_rating,
│   │                              scatter_moneyprize_time_linear_fit, scatter_money_views
│   ├── 04_money_weights.R         moneyPerWinner_prizeLevel / _winnerLevel
│   ├── 05_prizes_per_doctor.R     prizesByField
│   ├── 06_robustness.R            prizeRankRobustnessTable; robustness_macros
│   ├── 07_density_phd.R           prizesDensityByPhD_finest
│   ├── 08_density_vs_academics.R  prizesDensityByVSacademics_finest
│   ├── 09_density_tiers.R         prizesDensityByVS_finest_T1 / _T12
│   ├── 10_density_funding.R       prizesDensityFunding
│   ├── 11_density_works.R         prizesDensityByWorks_finest
│   ├── 12_views_robustness.R      r1_4_macros (index without Wikipedia page views)
│   ├── 13_field_measures.R        Field measures: density, patent-cited share, SDR shares and pay
│   ├── 14_field_tables.R          r2_3_macros
│   ├── 15_pay_grouped.R           r2_3_pay_grouped_macros; r2_3_pay_grouped_scatter
│   ├── 16_funder_types.R          r2_5_macros; funder types and awarding bodies per prize
│   ├── 17_criteria_overlap.R      r2_1_criteria_table; r2_1_macros
│   ├── 18_home_bias.R             Home bias of every prize against its field's benchmark pool
│   ├── 19_home_bias_tables.R      r2_2_macros (+ _share01, _share005); r2_2_prize_table
│   ├── 20_awarding_countries.py   r2_2_awarding_country_table; r2_2_desc_macros
│   ├── 21_teams.R                 r2_6_macros (laureates per award)
│   ├── 22_recipients_by_prestige.py  r2_6_prestige_macros
│   ├── 23_ec_summary.py           r2_9_ec_macros (the early-career list)
│   └── 24_home_bias_figures.R     r2_3_field_measures_scatter,
│                                  r2_2_home_share_by_country, r2_2_home_bias_scatter
├── output/                      Created by run_all.R (not committed)
│   ├── tables/                    .tex tables and macro files, as the paper inputs them
│   ├── figures/                   figure files, as the paper includes them
│   ├── plotdata/                  the data behind every figure (one CSV per figure)
│   └── intermediate/              CSVs passed between scripts
├── expected/                    What the package must reproduce
│   ├── paper_exhibits.csv         every \input and \includegraphics of the paper
│   ├── tables/                    the paper's copies of the .tex files
│   └── plotdata/                  plot data of the paper's figures, from the original scripts
└── tests/
    ├── test_outputs.R             the checks (run after run_all.R)
    ├── generated_files.txt        the files run_all.R is allowed to create
    ├── plotdata.R                 plot-data writer and comparison
    ├── refresh_expected.R         rebuilds expected/ (authors only)
    └── capture_original.R         runs an original script with ggsave() capturing plot data
```

## Data

All inputs are in `data/`. Prize-level files:

| File | Description | Source |
|------|-------------|--------|
| `cleanPrizeList.xlsx` | The 99 prizes with all indicators, the prestige index and rank | Authors' compilation |
| `mainPrizeList_pre-imputation.xlsx` | The same list before the imputation of missing survey ratings | Authors' compilation |
| `cleanEC.xlsx` | The 68 early-career prizes | Authors' compilation |
| `r2_9_ec_stage_by_prize.csv` | Yearly recognition counts of the early-career prizes, with the observed counts of the 14 prizes whose 2020-2024 recipients were collected | Authors' collection from prize websites |
| `all_winners_with_plotFinestField.csv` | Every 2015-2024 recognition of the 99 prizes: winner, OpenAlex author id, OpenAlex subfield/field | Authors' collection from prize websites; fields from OpenAlex |
| `institution_recode/` | One JSON record per prize: awarding organization, its type and country, funder at founding and today, sources, confidence | Authors' coding from prize and funder websites |
| `laureate_birth_years.csv` | Birth years of laureates | Wikidata |
| `nobel_api_cache.json` | All Nobel Prizes in physics, chemistry, medicine and economics with their laureates | Nobel Foundation API (api.nobelprize.org), cached 29 Aug 2026 |

Field-level files:

| File | Description | Source |
|------|-------------|--------|
| `subfield_to_finest_group.csv` | OpenAlex subfields mapped to the paper's 26 field groups | Authors |
| `nsf_field_to_finest_group.csv` | NSF doctorate fields mapped to the 26 field groups | Authors |
| `finest_group_to_funding_field.csv` | The 26 field groups mapped to the categories of the federal funding table | Authors |
| `comte_rungs.csv` | The 26 field groups on the rungs of Comte's hierarchy of the sciences | Authors |
| `nsf2023.xlsx` | US doctorates awarded by field, 2023 | NSF Survey of Earned Doctorates |
| `2022budgetByField.xlsx` | US federal research obligations by field, FY2022 | NSF NCSES Federal Funds for R&D |
| `vs_academics_by_finest_group.csv` | Count of senior research academics by field group | OpenAlex |
| `works_by_subfield.csv` | Count of well-cited works by OpenAlex subfield | OpenAlex |
| `ncses/sdr25321_tab012-003.xlsx` | Employed doctorate holders by field and sector, 2023 (NSF 25-321, Table 12-3) | NCSES Survey of Doctorate Recipients |
| `ncses/sdr25321_tab054.xlsx` | Median salaries of doctorate holders by field and sector, 2023 (NSF 25-321, Table 54) | NCSES Survey of Doctorate Recipients |
| `ncses/doctorate_recipients_2023.zip` | SDR 2023 public-use microdata | NCSES (public-use file) |
| `ncses/pay_paper_groups.csv`, `pay_sdr_groups.csv` | Crosswalk of the field groups and the SDR salary fields to 16 common groups | Authors |

OpenAlex-derived files (`data/openalex/`), built by the authors from a local
copy of the OpenAlex snapshot (partitions 2026-02-01 to 2026-03-30) and, for the
patent citations, from Marx and Fuegi's Reliance on Science (v64, July 2026);
the scanning code that produced them is in the authors' project repository:

| File | Description |
|------|-------------|
| `author_affiliations_smallteam.jsonl` | For every laureate: institutions on their small-team papers (at most 20 authors), with years and work counts |
| `pool_v2_top1000/top_pool_field_year.csv`, `top_pool_field_country_year.csv` | The benchmark pool: per OpenAlex field and year, the 1,000 researchers with the most citations to small-team papers, and how many of them were affiliated with each country (institutions on at least 2 years and 5% of works) |
| `pool_v2_share0.05/`, `pool_v2_share0.1/` | The same pool under the alternative affiliation rules quoted as robustness checks |
| `pop_field_year.csv`, `pop_field_country_year.csv` | The wide reference pool: everyone with 10+ citations |
| `patent_citation_counts.csv` | Per OpenAlex subfield and publication year: articles, and articles cited by at least one US patent at each confidence level |
| `patent_citation_topk.csv` | The most-cited articles of each field group, 2000-2015, with their patent-citation status |

## Software

- R 4.5.1; packages pinned in `renv.lock` (dplyr, tidyr, readxl, ggplot2, ggrepel,
  scales, stargazer, Hmisc, xtable, LaplacesDemon, jsonlite and their dependencies)
- Python 3.11; packages pinned in `requirements.txt` (pandas, numpy, scipy, openpyxl)
- Script 06 uses `set.seed(42)`; results are identical across runs.
- All paths are relative to the repository root; no external service is called.

## Archive

This package is archived on Zenodo (DOI 10.5281/zenodo.22721355) and
mirrored at github.com/deivisangeli/gtl-prizes-replication-v14. The Zenodo
record is the citable, versioned copy; the GitHub copy is the one that runs
the tests on every push.

## License

Data and code are provided for replication purposes. Please cite the paper if
you use these materials.
