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
#      the paper does not use, a cache -- fails the test, as does a missing one
#      (files .gitignore lists, such as .Rhistory, are not counted);
#   4. nothing beyond the paper: every macro the paper invokes is defined, no
#      generated macro is unused by the paper, every generated table and figure
#      is a paper exhibit, and every intermediate file is read by a later script.
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
  ignored <- tryCatch(system2("git", c("ls-files", "--others", "--ignored", "--exclude-standard"),
                              stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  untracked <- untracked[grepl("^output/", untracked) | !(untracked %in% ignored)]
}
extra <- setdiff(untracked, declared)
missing <- setdiff(declared, present)
check(sprintf("no undeclared files (%d files generated)", length(untracked)), length(extra) == 0,
      paste("undeclared:", extra))
check("every declared file present", length(missing) == 0, paste("missing:", missing))

# ---------- 4. nothing beyond the paper ----------
cat("\n--- Nothing beyond the paper ---\n")
usage <- read.csv("expected/paper_usage.csv", stringsAsFactors = FALSE)
usage <- usage[usage$document == "paper", ]
paper_macros <- usage[usage$kind == "macro", ]
paper_macros$file <- file.path("output/tables", basename(paper_macros$defined_in))
defs <- function(path) {
  if (!file.exists(path)) return(character(0))
  x <- regmatches(read_lines(path), regexpr("\\\\newcommand\\{\\\\[A-Za-z]+\\}", read_lines(path)))
  sub("\\\\newcommand\\{\\\\([A-Za-z]+)\\}", "\\1", x)
}
generated <- do.call(rbind, lapply(list.files("output/tables", full.names = TRUE), function(f)
  if (length(defs(f))) data.frame(file = f, macro = defs(f), stringsAsFactors = FALSE)))
undefined <- paper_macros[!mapply(function(m, f) m %in% defs(f), paper_macros$name, paper_macros$file), ]
check(sprintf("every macro the paper invokes is generated (%d macros)", nrow(paper_macros)),
      nrow(undefined) == 0, paste(basename(undefined$file), undefined$name))
unused <- generated[!paste(generated$file, generated$macro) %in% paste(paper_macros$file, paper_macros$name), ]
check(sprintf("no generated macro is unused by the paper (%d generated)", nrow(generated)),
      nrow(unused) == 0, paste(basename(unused$file), unused$macro))
tables <- list.files("output/tables"); figures <- list.files("output/figures")
extra_tables <- setdiff(tables, basename(exhibits$output_path[exhibits$kind == "tex"]))
extra_figures <- setdiff(figures, basename(exhibits$output_path[exhibits$kind == "figure"]))
check("every generated table and macro file is a paper input", length(extra_tables) == 0, extra_tables)
check("every generated figure is in the paper", length(extra_figures) == 0, extra_figures)
scripts <- sort(list.files("code", full.names = TRUE))
code <- lapply(scripts, function(s) paste(readLines(s, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
# a script names the file literally, or builds it from a stem and a suffix
# (r2_2_home_awards_by_country + _share01), so the stem counts as a match
readers <- function(f) {
  stem <- sub("_[A-Za-z0-9]+\\.csv$", "", f)
  sum(vapply(code, function(txt) grepl(f, txt, fixed = TRUE) || grepl(stem, txt, fixed = TRUE), logical(1)))
}
intermediates <- list.files("output/intermediate")
orphans <- intermediates[vapply(intermediates, readers, numeric(1)) < 2]   # the writer and at least one reader
check(sprintf("every intermediate file is read by a later script (%d files)", length(intermediates)),
      length(orphans) == 0, orphans)

cat(sprintf("\n=== Results: %d passed, %d failed ===\n", pass, fail))
if (fail > 0) quit(status = 1)
