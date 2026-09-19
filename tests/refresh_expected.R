# ==============================================================================
# tests/refresh_expected.R -- rebuild expected/ from the paper and the authors'
# original analysis code (NOT part of the replication run; needs the authors'
# project repository, its Dropbox data and the Overleaf clone).
#
#   Rscript tests/refresh_expected.R
#
# expected/ holds what the package must reproduce:
#   expected/tables/<file>.tex   every .tex file the paper \input{}s, copied
#                                from the paper's own source (the Overleaf clone)
#   expected/plotdata/<fig>.csv  the data behind every figure the paper includes,
#                                captured from the ORIGINAL scripts that made the
#                                paper's figures (tests/capture_original.R)
#   expected/paper_exhibits.csv  the list of exhibits, read from the paper's .tex
#   expected/paper_usage.csv    the macros the paper invokes (tests/paper_usage.py, run separately)
#
# Environment variables (defaults are the authors' machine):
#   GTL_PRIZES_REPO  the project repository with analysis/*.R   (needs db_path set)
#   OVERLEAF_REPO    the Overleaf clone of the paper
#   PAPER            the paper's main .tex file inside OVERLEAF_REPO
# ==============================================================================
repo     <- Sys.getenv("GTL_PRIZES_REPO", "C:/Users/deivi/github/gtl-prizes")
overleaf <- Sys.getenv("OVERLEAF_REPO", "C:/Users/deivi/github/gtl-prizes-overleaf")
paper    <- Sys.getenv("PAPER", "prizes/v14.tex")
stopifnot(file.exists("_helpers.R"), dir.exists(repo), dir.exists(overleaf), nzchar(Sys.getenv("db_path")))
here <- normalizePath(".")

# ---- 1. the exhibits the paper uses: every \input{} and \includegraphics{} ----
tex <- readLines(file.path(overleaf, paper), warn = FALSE, encoding = "UTF-8")
tex <- sub("(^|[^\\\\])%.*$", "\\1", tex)                    # drop comments
body <- paste(tex, collapse = "\n")
inputs <- regmatches(body, gregexpr("\\\\input\\{[^}]*\\}", body))[[1]]
inputs <- sub("\\\\input\\{([^}]*)\\}", "\\1", inputs)
figs <- regmatches(body, gregexpr("\\\\includegraphics(\\[[^]]*\\])?\\{[^}]*\\}", body))[[1]]
figs <- sub(".*\\{([^}]*)\\}", "\\1", figs)
inputs <- ifelse(grepl("\\.tex$", inputs), inputs, paste0(inputs, ".tex"))
exhibits <- rbind(
  data.frame(kind = "tex", paper_path = unique(inputs), stringsAsFactors = FALSE),
  data.frame(kind = "figure", paper_path = unique(figs), stringsAsFactors = FALSE))
exhibits$output_path <- ifelse(exhibits$kind == "tex",
                               file.path("output/tables", basename(exhibits$paper_path)),
                               file.path("output/figures", basename(exhibits$paper_path)))
stopifnot(!any(duplicated(exhibits$output_path)))
dir.create("expected/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("expected/plotdata", recursive = TRUE, showWarnings = FALSE)
write.csv(exhibits, "expected/paper_exhibits.csv", row.names = FALSE)
cat(sprintf("paper exhibits: %d tex inputs, %d figures\n", sum(exhibits$kind == "tex"), sum(exhibits$kind == "figure")))

# ---- 2. tables and macro files: the paper's own copies ----
unlink(list.files("expected/tables", full.names = TRUE))
for (p in exhibits$paper_path[exhibits$kind == "tex"]) {
  src <- file.path(overleaf, p)
  stopifnot(file.exists(src))
  file.copy(src, file.path("expected/tables", basename(p)), overwrite = TRUE)
}

# ---- 3. figures: plot data captured from the original scripts ----
# original script (relative to the project repository) -> figures it writes
ORIGINALS <- list(
  "analysis/scatterplots.R" = c("cum_prize_time", "scatter_time_rating",
                                "scatter_moneyprize_time_linear_fit", "scatter_money_views"),
  "analysis/densityByField_moneyWeights.R" = c("moneyPerWinner_prizeLevel", "moneyPerWinner_winnerLevel"),
  "analysis/densityByField_finest.R" = "prizesDensityByPhD_finest",
  "analysis/densityByField_finest_vsAcademics.R" = "prizesDensityByVSacademics_finest",
  "analysis/densityByField_finest_tiers.R" = c("prizesDensityByVS_finest_T1", "prizesDensityByVS_finest_T12"),
  "analysis/densityByField_funding.R" = "prizesDensityFunding",
  "analysis/densityByField_finest_works.R" = "prizesDensityByWorks_finest",
  "analysis/r2_figures.R" = c("r2_2_home_bias_scatter", "r2_2_home_share_by_country", "r2_3_field_measures_scatter"),
  "analysis/r2_3_pay_grouped.R" = "r2_3_pay_grouped_scatter",
  "analysis/r2_5_funder_types.R" = "r2_5_funder_share_by_year")
wanted <- tools::file_path_sans_ext(basename(exhibits$paper_path[exhibits$kind == "figure"]))
covered <- unlist(ORIGINALS, use.names = FALSE)
stopifnot(setequal(wanted, covered))
unlink(list.files("expected/plotdata", full.names = TRUE))
rscript <- file.path(R.home("bin"), "Rscript")
# each original runs in its own R process with the project repository as the
# working directory (the scripts use paths relative to it)
for (script in names(ORIGINALS)) {
  cat("running original", script, "...\n")
  setwd(repo)
  status <- system2(rscript, c(file.path(here, "tests/capture_original.R"),
                               file.path(here, "expected/plotdata"), script, ORIGINALS[[script]]),
                    stdout = FALSE, stderr = "")
  setwd(here)
  if (status != 0) stop(script, " failed")
}
cat("expected/ refreshed\n")
