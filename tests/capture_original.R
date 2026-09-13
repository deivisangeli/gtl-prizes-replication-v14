# ==============================================================================
# tests/capture_original.R -- run ONE original analysis script of the authors'
# project repository with ggsave() replaced by a capture: instead of writing the
# figure file, the plot data of every figure named on the command line is written
# to expected/plotdata/<figure>.csv. Used by tests/refresh_expected.R; not part
# of the replication run.
#
#   Rscript tests/capture_original.R <expected/plotdata dir> <script> <figure> [<figure> ...]
#
# Run with the authors' repository as the working directory (the scripts use
# paths relative to it and the db_path environment variable).
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
plotdata_dir <- args[1]; script <- args[2]; wanted <- args[-(1:2)]
plotdata_source <- file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), "plotdata.R")

pdf(NULL)
.capture <- new.env()
.capture$wanted <- wanted
.capture$dir <- plotdata_dir
.capture$seen <- character(0)
.capture$script <- normalizePath(script)
.capture$write_plot_data <- local({ source(plotdata_source, local = TRUE); write_plot_data })

# ggsave replacement inside the ggplot2 namespace, so every call -- from any
# environment, after any rm(list = ls()) -- reaches it
capture_ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  name <- tools::file_path_sans_ext(basename(filename))
  if (name %in% .capture$wanted && !(name %in% .capture$seen)) {
    .capture$write_plot_data(plot, file.path(.capture$dir, paste0(name, ".csv")))
    .capture$seen <- c(.capture$seen, name)
    cat("captured", name, "\n")
  }
  invisible(filename)
}
environment(capture_ggsave) <- asNamespace("ggplot2")
suppressPackageStartupMessages(library(ggplot2))
assignInNamespace("ggsave", capture_ggsave, "ggplot2")
# the attached package environment keeps its own copy of the export; replace it too
pkg <- as.environment("package:ggplot2")
unlockBinding("ggsave", pkg); assign("ggsave", capture_ggsave, envir = pkg); lockBinding("ggsave", pkg)
# scripts that call ggsave() before attaching ggplot2 themselves find it here
ggsave <- ggplot2::ggsave

# Some originals locate themselves through commandArgs(FALSE) ("--file=");
# hand them their own path.
commandArgs <- function(trailingOnly = FALSE) {
  a <- base::commandArgs(trailingOnly)
  if (!trailingOnly) a <- c(a[!grepl("^--file=", a)], paste0("--file=", .capture$script))
  a
}

source(script, echo = FALSE)
missing <- setdiff(.capture$wanted, .capture$seen)
if (length(missing)) stop("figures not captured: ", paste(missing, collapse = ", "))
