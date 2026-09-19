# ==============================================================================
# tests/plotdata.R -- the data behind a ggplot, written as one CSV
#
# Figure files (PDF/PNG) are not byte-comparable across machines: fonts, the
# graphics device and embedded timestamps all differ. What can be compared is the
# data ggplot2 draws from: ggplot_build() returns, for every layer, the table of
# computed aesthetics (positions, labels, colours, sizes). write_plot_data() writes
# those tables, stacked with a `layer` column, so a figure produced by the
# package can be checked against the same figure produced by the authors' analysis scripts.
#
# Sourced by _helpers.R (the package) and by tests/capture_original.R (the
# authors' analysis scripts), so both sides write the identical format.
# ==============================================================================

write_plot_data <- function(plot, path) {
  built <- ggplot2::ggplot_build(plot)
  layers <- lapply(seq_along(built$data), function(i) {
    d <- as.data.frame(built$data[[i]])
    for (col in names(d)) {
      if (is.list(d[[col]])) d[[col]] <- vapply(d[[col]], function(v) paste(format(v), collapse = "|"), "")
      if (!is.numeric(d[[col]])) d[[col]] <- as.character(d[[col]])
    }
    d$layer <- i
    d
  })
  # a column numeric in one layer and not in another (alpha, label, size...) is
  # written as character in every layer
  mixed <- unique(unlist(lapply(layers, function(d) names(d)[!vapply(d, is.numeric, TRUE)])))
  layers <- lapply(layers, function(d) { for (col in intersect(mixed, names(d))) d[[col]] <- as.character(d[[col]]); d })
  out <- dplyr::bind_rows(layers)
  out <- out[, c("layer", setdiff(names(out), "layer")), drop = FALSE]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb"); on.exit(close(con))
  utils::write.csv(out, con, row.names = FALSE, na = "")
  invisible(out)
}

# Compare two plot-data CSVs. Character columns must match exactly, numeric
# columns up to `tol` (relative), and the two files must agree on the number of
# rows in every layer. Columns present on one side only are reported but do not
# fail the comparison, so a ggplot2 minor update that adds an aesthetic column
# does not break the test. Returns a character vector of problems (empty = same).
compare_plot_data <- function(expected_file, generated_file, tol = 1e-6) {
  e <- utils::read.csv(expected_file, stringsAsFactors = FALSE, check.names = FALSE)
  g <- utils::read.csv(generated_file, stringsAsFactors = FALSE, check.names = FALSE)
  problems <- character(0)
  if (!identical(table(e$layer), table(g$layer)))
    return(sprintf("layers/rows differ: expected %s, generated %s",
                   paste(table(e$layer), collapse = "/"), paste(table(g$layer), collapse = "/")))
  only_e <- setdiff(names(e), names(g)); only_g <- setdiff(names(g), names(e))
  if (length(only_e)) problems <- c(problems, paste("columns only in expected:", paste(only_e, collapse = ", ")))
  if (length(only_g)) problems <- c(problems, paste("columns only in generated:", paste(only_g, collapse = ", ")))
  for (col in setdiff(intersect(names(e), names(g)), "layer")) {
    x <- e[[col]]; y <- g[[col]]
    if (is.numeric(x) && is.numeric(y)) {
      both_na <- is.na(x) & is.na(y)
      if (any(is.na(x) != is.na(y))) { problems <- c(problems, sprintf("column %s: NA pattern differs", col)); next }
      x <- x[!both_na]; y <- y[!both_na]
      bad <- abs(x - y) > tol * pmax(1, abs(x))
      if (any(bad)) problems <- c(problems, sprintf("column %s: %d of %d values differ (max abs diff %.3g)",
                                                   col, sum(bad), length(x), max(abs(x - y))))
    } else {
      x <- ifelse(is.na(x), "", as.character(x)); y <- ifelse(is.na(y), "", as.character(y))
      # a character column whose non-empty values all parse as numbers (a column
      # numeric in one layer, character in another) is compared numerically
      xn <- suppressWarnings(as.numeric(x)); yn <- suppressWarnings(as.numeric(y))
      numeric_like <- all(is.na(xn) == (x == "")) && all(is.na(yn) == (y == "")) && any(x != "")
      bad <- if (numeric_like) {
        (is.na(xn) != is.na(yn)) | (!is.na(xn) & !is.na(yn) & abs(xn - yn) > tol * pmax(1, abs(xn)))
      } else x != y
      if (any(bad)) problems <- c(problems, sprintf("column %s: %d of %d values differ (e.g. %s vs %s)",
                                                   col, sum(bad), length(x), x[bad][1], y[bad][1]))
    }
  }
  problems
}
