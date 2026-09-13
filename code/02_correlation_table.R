# ==============================================================================
# 02_correlation_table.R
# Generates: Table S2 (corr.tex)
# ==============================================================================
source("_helpers.R")

df <- readxl::read_excel(file.path(data_dir, "mainPrizeList_pre-imputation.xlsx"))

varsofinteres <- df[, c("Rating", "lnPageViews", "lnAge", "lnMoneyPerPrize", "lnNewsMentions")]

data <- varsofinteres

# Correlation matrix with p-values and standard errors
corr_res <- rcorr(as.matrix(data))

cor_mat <- corr_res$r
p_mat <- corr_res$P
n <- corr_res$n

calc_se <- function(r, n) sqrt((1 - r^2) / (n - 2))

# Each pair's own sample size: rcorr()$n is the matrix of pairwise complete
# observations (76 for pairs involving the survey rating, 99 otherwise).
se_mat <- calc_se(cor_mat, n)

add_stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

output_mat <- matrix(nrow = nrow(cor_mat), ncol = ncol(cor_mat))
for (i in 1:nrow(cor_mat)) {
  for (j in 1:ncol(cor_mat)) {
    if (i != j) {
      output_mat[i, j] <- paste0(formatC(cor_mat[i, j], format = "f", digits = 2),
                                  " (", formatC(se_mat[i, j], format = "f", digits = 2), ")",
                                  add_stars(p_mat[i, j]))
    } else {
      output_mat[i, j] <- ""
    }
  }
}

output_mat[upper.tri(output_mat, diag = TRUE)] <- ""
diag(output_mat) <- 1

rownames(output_mat) <- c("Survey Rating", "Daily Page Views", "Prize Age", "Money per Prize", "News Mentions")
colnames(output_mat) <- rownames(output_mat)

output_df <- as.data.frame(output_mat)

corr_table <- print(xtable(output_df, digits = 3),
                    type = "latex", file = NULL, print.results = FALSE)

corr_table <- gsub("\\\\begin\\{table\\}\\[ht\\]",
                   "\\\\begin{table}[ht]\n\\\\caption{Correlation Between Normalized Prestige Indicators} \\\\label{corrTable}",
                   corr_table)

corr_table <- gsub("\\{r[lrc]+\\}", "{lccccc}", corr_table)

# Scaled to \textwidth as in the paper (analysis/correlation between prestige indicators.R).
corr_table <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}",
                   corr_table, fixed = TRUE)
corr_table <- gsub("\\end{tabular}", "\\end{tabular}}", corr_table, fixed = TRUE)

note <- "\\noindent \\justify \\footnotesize \\textit{Note}: The table shows estimates of the Pearson correlation coefficients between each pair of the five prestige indicators, after normalization. Standard errors between parenthesis. * p<0.05, ** p<0.01, *** p<0.001."

corr_table_lines <- strsplit(corr_table, "\n")[[1]]
end_table_pos <- grep("\\\\end\\{table\\}", corr_table_lines)
corr_table_lines <- c(corr_table_lines[1:(end_table_pos - 1)],
                      note,
                      corr_table_lines[end_table_pos])

corr_table <- paste(corr_table_lines, collapse = "\n")

write_tex(corr_table, file.path(table_dir, "corr.tex"))
