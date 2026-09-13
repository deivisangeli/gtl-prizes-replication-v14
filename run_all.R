# ==============================================================================
# run_all.R -- the one command that reproduces every table, macro file and
# figure of "The Missing Nobels" from the data in data/.
#
#   Rscript run_all.R
#
# Deletes output/ and runs every script in code/ in numeric order, each in a
# fresh R or Python process, stopping at the first failure. Python scripts use
# the interpreter named by the PYTHON environment variable (default: python3,
# or python on Windows).
# ==============================================================================

if (!file.exists("_helpers.R")) stop("run_all.R must be executed from the repository root directory.")

python <- Sys.getenv("PYTHON", unset = if (.Platform$OS.type == "windows") "python" else "python3")
rscript <- file.path(R.home("bin"), "Rscript")

unlink("output", recursive = TRUE)
dir.create("output")

scripts <- sort(list.files("code", pattern = "^\\d+_.*\\.(R|py)$", full.names = TRUE))
cat(sprintf("=== The Missing Nobels: running %d scripts ===\n\n", length(scripts)))
t0 <- Sys.time()
for (s in scripts) {
  cat(sprintf("%-32s ", basename(s)))
  t1 <- Sys.time()
  cmd <- if (grepl("\\.py$", s)) python else rscript
  args <- s   # the project .Rprofile activates renv in each R child process
  log <- tempfile(fileext = ".log")
  status <- system2(cmd, args, stdout = log, stderr = log)
  if (status != 0) {
    cat("FAILED\n\n"); cat(readLines(log, warn = FALSE), sep = "\n")
    stop(sprintf("%s failed (exit status %d)", basename(s), status))
  }
  cat(sprintf("done (%.0fs)\n", as.numeric(difftime(Sys.time(), t1, units = "secs"))))
}
cat(sprintf("\n=== All scripts completed in %.0f seconds ===\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
