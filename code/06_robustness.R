# ==============================================================================
# 06_robustness.R
# Generates: Table S4 (prizeRankRobustnessTable.tex)
# ==============================================================================
source("_helpers.R")

prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx"))
prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)

prizeList <- prizeList %>%
  mutate(Tier = case_when(cumWinners <= 0.1 * totalWinners ~ 1,
                          cumWinners <= 0.3 * totalWinners ~ 2,
                          TRUE ~ 3),
         pcaRatingNormRound = round(pcaRatingNorm, 1))

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
weights <- data.frame()
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
  weights <- rbind(weights, w)
}

# Quantiles for each prize
quantiles <- data.frame(q025 = numeric(), q5 = numeric(), q975 = numeric(), `Award Name` = character())
for (prize in colnames(newRankings)) {
  q <- data.frame(q025 = NA, q05 = NA, q975 = NA, prize = "")
  q$q025 <- quantile(newRankings[, prize], 0.025)[[1]]
  q$q05 <- quantile(newRankings[, prize], 0.05)[[1]]
  q$q5 <- quantile(newRankings[, prize], 0.5)[[1]]
  q$q95 <- quantile(newRankings[, prize], 0.95)[[1]]
  q$q975 <- quantile(newRankings[, prize], 0.975)[[1]]
  q$`Award Name` <- prize
  quantiles <- rbind(quantiles, q)
}

quantiles <- quantiles[order(quantiles$q5), ]
quantiles$qRank <- 1:nrow(quantiles)
quantiles <- quantiles %>%
  group_by(q5) %>%
  mutate(qRank = mean(qRank))

prizeList <- left_join(prizeList, quantiles, by = "Award Name")

# Tier frequency table
tierTable <- data.frame()
for (prize in colnames(newTiers)) {
  t <- data.frame(tier1 = NA, tier2 = NA, tier3 = NA, prize = "")
  t$tier1 <- sum(newTiers[, prize] == 1) / reps
  t$tier2 <- sum(newTiers[, prize] == 2) / reps
  t$tier3 <- sum(newTiers[, prize] == 3) / reps
  t$`Award Name` <- prize
  tierTable <- rbind(tierTable, t)
}

prizeList <- left_join(prizeList, tierTable, by = "Award Name")
prizeList$CI <- paste0("[", round(prizeList$q025, 0), ",", round(prizeList$q975, 0), "]")

prizeList$`Freq. in right Tier` <- NA
prizeList$`Freq. in right Tier`[prizeList$Tier == 1] <- round(prizeList$tier1[prizeList$Tier == 1] * 100, 1)
prizeList$`Freq. in right Tier`[prizeList$Tier == 2] <- round(prizeList$tier2[prizeList$Tier == 2] * 100, 1)
prizeList$`Freq. in right Tier`[prizeList$Tier == 3] <- round(prizeList$tier3[prizeList$Tier == 3] * 100, 1)

prizeList$P5 <- round(prizeList$q05, 0)
prizeList$P50 <- round(prizeList$q5, 0)
prizeList$P95 <- round(prizeList$q95, 0)

forTable <- prizeList %>% select(`Award Name`, pcaRank, P5, P50, P95, Tier, `Freq. in right Tier`)
colnames(forTable) <- c("Award Name", "PCA Rank", "P5", "P50", "P95", "PCA Tier", "% in right Tier")

table <- stargazer(forTable, type = "latex",
                   summary = FALSE, digits = 1, rownames = FALSE,
                   title = "List of Selected Prizes")

table <- gsub("\\textbackslash &", "\\&", table, fixed = TRUE)
# Until 2026-09-03 the longtable was wrapped in a "{ ... }" group whose closing brace was
# never written, leaving an unbalanced "{" that made the paper fail with "Missing } inserted".
# \small stays scoped to the table; \begingroup/\endgroup sit on the same lines as the delimiters.
table <- gsub("\\begin{tabular}", "\\begingroup\\small \\begin{longtable}", table, fixed = TRUE)
table <- gsub("\\end{tabular}", "\\end{longtable}\\endgroup", table, fixed = TRUE)
table <- gsub("ccc}",
              "ccc} \\caption{Robustness Exercise -- Distribution of Prize Ranking under Random Weighting} \\label{mainRankingRobustness}",
              table, fixed = TRUE)
table <- gsub("{@{\\extracolsep{5pt}} ccccc}", "{lcccc}", table, fixed = TRUE)
table <- table[-c(1, 2, 3, 4, 5, 6, length(table))]

# Blank line, then the note (writing straight to length + 2 left a literal "NA" line).
table[length(table) + 1] <- ""
table[length(table) + 1] <-
  paste("\\noindent \\footnotesize \\textit{Note}: This table shows statistics about the distribution of rank and tier for our list of 99 most prestigious prizes in 1,000 simulations using random weights over the five indicators, each centered and scaled with the PCA fitting sample.",
        "The original ranking is shown in the PCA Rank column. The P5, P50, and P95 columns show the 5th, 50th, and 95th percentiles of the simulated ranks.",
        "The PCA Tier column shows the original tier. The \\% in right Tier column shows how often the simulated tier coincides with the original tier.",
        sep = " ")

write_tex(table, file.path(table_dir, "prizeRankRobustnessTable.tex"))

# The two shares the paper quotes (prizes keeping their tier in over 50% / 90% of
# the simulations)
write_tex(c(mac("robustStableFifty",  sprintf("%.0f", 100 * mean(prizeList$`Freq. in right Tier` > 50))),
            mac("robustStableNinety", sprintf("%.0f", 100 * mean(prizeList$`Freq. in right Tier` > 90)))),
          file.path(table_dir, "robustness_macros.tex"))
