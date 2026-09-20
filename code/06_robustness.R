# ==============================================================================
# 06_robustness.R
# Generates: prizeRankRobustnessTable.tex (Table \ref{mainRankingRobustness}:
#   simulated rank and tier of each prize under 1,000 random weightings) and
#   robustness_macros.tex (\robustStableFifty, \robustStableNinety)
# Inputs: data/cleanPrizeList.xlsx
# ==============================================================================
source("_helpers.R")

prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx"))
prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)

prizeList <- prizeList %>%
  mutate(Tier = case_when(cumWinners <= 0.1 * totalWinners ~ 1,
                          cumWinners <= 0.3 * totalWinners ~ 2,
                          TRUE ~ 3))

# Put the survey rating on the same scale as the other four indicators. The *Std
# columns of cleanPrizeList.xlsx are centered and scaled with the mean and
# population SD of the PCA fitting sample (the 76 complete cases); RatingNorm
# (observed rating, imputed where missing) is not.
pcaCols <- c("Rating", "lnPageViews", "lnAge", "lnMoneyPerPrize", "lnNewsMentions")
fitRating <- prizeList$Rating[complete.cases(prizeList[, pcaCols])]
stopifnot(length(fitRating) == 76)
ratingMean <- mean(fitRating)
ratingSD <- sqrt(mean((fitRating - ratingMean)^2))
prizeList$RatingStd <- (prizeList$RatingNorm - ratingMean) / ratingSD

calculateNewRanking <- function(df, w) {
  df$newRating <- w[1] * df$lnPageViewsStd + w[2] * df$RatingStd +
    w[3] * df$lnAgeStd + w[4] * df$lnMoneyPerPrizeStd + w[5] * df$lnNewsMentionsStd

  df <- df[order(df$newRating, decreasing = TRUE), ]
  df$newRank <- 1:nrow(df)
  totalWinners <- sum(df$`Yearly Winners`)
  df$cumWinners <- cumsum(df$`Yearly Winners`)
  df <- df %>%
    mutate(Tier = case_when(cumWinners <= 0.1 * totalWinners ~ 1,
                            cumWinners <= 0.3 * totalWinners ~ 2,
                            TRUE ~ 3))
  newRank <- df %>% select(`Award Name`, newRank, Tier)
  return(newRank)
}

set.seed(42)  # For reproducibility
newRankings <- data.frame()
newTiers <- data.frame()
reps <- 1000

for (i in 1:reps) {
  w <- rdirichlet(1, c(1, 1, 1, 1, 1))
  newRank <- calculateNewRanking(prizeList, w)
  newRank <- t(newRank)
  colnames(newRank) <- newRank[1, ]
  newRank <- newRank[-1, ]
  newRank <- as.data.frame(apply(newRank, 2, as.numeric))

  newRankings <- rbind(newRankings, newRank[1, ])
  newTiers <- rbind(newTiers, newRank[2, ])
}

# Quantiles for each prize
quantiles <- data.frame()
for (prize in colnames(newRankings)) {
  q <- data.frame(q05 = NA, q5 = NA, q95 = NA)
  q$q05 <- quantile(newRankings[, prize], 0.05)[[1]]
  q$q5 <- quantile(newRankings[, prize], 0.5)[[1]]
  q$q95 <- quantile(newRankings[, prize], 0.95)[[1]]
  q$`Award Name` <- prize
  quantiles <- rbind(quantiles, q)
}

prizeList <- left_join(prizeList, quantiles, by = "Award Name")

# Tier frequency table
tierTable <- data.frame()
for (prize in colnames(newTiers)) {
  t <- data.frame(tier1 = NA, tier2 = NA, tier3 = NA)
  t$tier1 <- sum(newTiers[, prize] == 1) / reps
  t$tier2 <- sum(newTiers[, prize] == 2) / reps
  t$tier3 <- sum(newTiers[, prize] == 3) / reps
  t$`Award Name` <- prize
  tierTable <- rbind(tierTable, t)
}

prizeList <- left_join(prizeList, tierTable, by = "Award Name")

prizeList$`Freq. in right Tier` <- NA
prizeList$`Freq. in right Tier`[prizeList$Tier == 1] <- round(prizeList$tier1[prizeList$Tier == 1] * 100, 1)
prizeList$`Freq. in right Tier`[prizeList$Tier == 2] <- round(prizeList$tier2[prizeList$Tier == 2] * 100, 1)
prizeList$`Freq. in right Tier`[prizeList$Tier == 3] <- round(prizeList$tier3[prizeList$Tier == 3] * 100, 1)

prizeList$P5 <- round(prizeList$q05, 0)
prizeList$P50 <- round(prizeList$q5, 0)
prizeList$P95 <- round(prizeList$q95, 0)

forTable <- prizeList %>% select(`Award Name`, pcaRank, P5, P50, P95, Tier, `Freq. in right Tier`)
forTable$`Freq. in right Tier` <- sprintf("%.1f", forTable$`Freq. in right Tier`)
colnames(forTable) <- c("Award Name", "PCA Rank", "P5", "P50", "P95", "PCA Tier", "% in right Tier")

table <- stargazer(forTable, type = "latex",
                   summary = FALSE, digits = 1, rownames = FALSE,
                   title = "List of Selected Prizes")

# Rewrite the tabular as a longtable inside a \begingroup\small ... \endgroup group.
table <- gsub("\\begin{tabular}", "\\begingroup\\small\\setlength{\\tabcolsep}{4pt} \\begin{longtable}", table, fixed = TRUE)
table <- gsub("\\end{tabular}", "\\end{longtable}\\endgroup", table, fixed = TRUE)
table <- gsub("ccc}",
              "ccc} \\caption{Robustness Exercise -- Distribution of Prize Ranking under Random Weighting} \\label{mainRankingRobustness}",
              table, fixed = TRUE)
table <- gsub("{@{\\extracolsep{5pt}} ccccccc}",
              "{>{\\raggedright\\arraybackslash}p{6.6cm}cccccc}", table, fixed = TRUE)
table <- table[-c(1, 2, 3, 4, 5, 6, length(table))]
table <- longtable_heads(table, "mainRankingRobustness", 7)

# Blank line, then the table note.
table[length(table) + 1] <- ""
table[length(table) + 1] <-
  paste("\\noindent \\footnotesize \\textit{Note}: This table shows the rank and tier of each of the 99 prizes across 1,000 rankings built with random weights over the five standardized indicators.",
        "PCA Rank and PCA Tier are the original ranking and tier; P5, P50 and P95 the 5th, 50th and 95th percentiles of the simulated rank;",
        "\\% in right Tier the share of simulations that place the prize in its original tier.",
        sep = " ")

write_tex(table, file.path(table_dir, "prizeRankRobustnessTable.tex"))

# The two shares the paper quotes (prizes keeping their tier in over 50% / 90% of
# the simulations)
write_tex(c(mac("robustStableFifty",  sprintf("%.0f", 100 * mean(prizeList$`Freq. in right Tier` > 50))),
            mac("robustStableNinety", sprintf("%.0f", 100 * mean(prizeList$`Freq. in right Tier` > 90)))),
          file.path(table_dir, "robustness_macros.tex"))
