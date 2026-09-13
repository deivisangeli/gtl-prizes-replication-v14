# ==============================================================================
# tests/test_outputs.R -- does the package reproduce the paper, and nothing else?
#
#   Rscript run_all.R && Rscript tests/test_outputs.R      (from the repo root)
#
# Three checks, all against expected/ (see tests/refresh_expected.R):
#   1. every table and macro file the paper \input{}s is regenerated identically
#      (line by line, ignoring comment lines and line endings);
#   2. every figure the paper includes is regenerated with the same plot data as
#      the original scripts that produced the paper's figures (character
#      columns exact, numbers to 1e-6 relative);
#   3. the run creates exactly the files listed in tests/generated_files.txt:
#      any other untracked file in the repository -- a stray Rplots.pdf, a table
#      the paper does not use, a cache -- fails the test, as does a missing one.
# Exit status 1 on any failure.
# ==============================================================================
source("tests/plotdata.R")
pass <- 0; fail <- 0
check <- function(desc, ok, detail = NULL) {
  if (isTRUE(ok)) { pass <<- pass + 1; cat("  PASS:", desc, "\n") }
  else { fail <<- fail + 1; cat("  FAIL:", desc, "\n"); for (d in detail) cat("        ", d, "\n") }
}
read_lines <- function(path) sub("\r$", "", readLines(path, warn = FALSE, encoding = "UTF-8"))
strip_comments <- function(x) x[!grepl("^\\s*%", x)]

exhibits <- read.csv("expected/paper_exhibits.csv", stringsAsFactors = FALSE)
cat(sprintf("=== %d exhibits in the paper: %d tex inputs, %d figures ===\n",
            nrow(exhibits), sum(exhibits$kind == "tex"), sum(exhibits$kind == "figure")))

# ---------- 1. tables and macro files ----------
cat("\n--- Tables and macro files vs the paper's copies ---\n")
for (i in which(exhibits$kind == "tex")) {
  out <- exhibits$output_path[i]; ref <- file.path("expected/tables", basename(out))
  if (!file.exists(out)) { check(paste(basename(out), "generated"), FALSE); next }
  e <- strip_comments(read_lines(ref)); g <- strip_comments(read_lines(out))
  same <- identical(e, g)
  detail <- if (!same) {
    d <- which(e != g)[1]
    if (length(e) != length(g)) sprintf("%d lines expected, %d generated", length(e), length(g))
    else sprintf("first difference at content line %d:\n          expected:  %s\n          generated: %s", d, e[d], g[d])
  }
  check(paste(basename(out), "matches the paper"), same, detail)
}

# ---------- 2. figures ----------
cat("\n--- Figures vs the originals' plot data ---\n")
for (i in which(exhibits$kind == "figure")) {
  out <- exhibits$output_path[i]
  name <- tools::file_path_sans_ext(basename(out))
  gen <- file.path("output/plotdata", paste0(name, ".csv"))
  ref <- file.path("expected/plotdata", paste0(name, ".csv"))
  if (!file.exists(out) || file.size(out) == 0) { check(paste(basename(out), "generated"), FALSE); next }
  if (!file.exists(gen)) { check(paste(basename(out), "plot data written"), FALSE); next }
  problems <- compare_plot_data(ref, gen)
  # extra/missing columns are informational (a ggplot2 update can add an aesthetic)
  hard <- problems[!grepl("^columns only in", problems)]
  check(paste(basename(out), "has the paper's plot data"), length(hard) == 0, problems)
}

# ---------- 3. no extraneous files ----------
cat("\n--- Generated files: exactly the declared set ---\n")
declared <- read_lines("tests/generated_files.txt")
declared <- declared[nzchar(declared)]
present <- list.files(".", recursive = TRUE, all.files = TRUE, include.dirs = FALSE, no.. = TRUE)
present <- present[!grepl("^\\.git/|^renv/(library|staging|sandbox|local|cellar|python)/", present)]
tracked <- tryCatch(system2("git", c("ls-files"), stdout = TRUE, stderr = FALSE), error = function(e) NULL)
if (is.null(tracked) || length(tracked) == 0) {
  cat("  (git not available: checking output/ only)\n")
  untracked <- present[grepl("^output/", present)]
} else {
  untracked <- setdiff(present, tracked)
}
extra <- setdiff(untracked, declared)
missing <- setdiff(declared, present)
check(sprintf("no undeclared files (%d files generated)", length(untracked)), length(extra) == 0,
      paste("undeclared:", extra))
check("every declared file present", length(missing) == 0, paste("missing:", missing))

cat(sprintf("\n=== Results: %d passed, %d failed ===\n", pass, fail))
if (fail > 0) quit(status = 1)
