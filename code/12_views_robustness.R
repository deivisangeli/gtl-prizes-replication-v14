# ==============================================================================
# 12_views_robustness.R
# Generates: r1_4_macros.tex -- robustness of the prestige index to dropping
#            Wikipedia page views (numbers quoted in the paper's robustness
#            paragraph on the attention indicators)
#
# Variant A recomputes the PCA index with lnPageViews excluded from the PCA.
# Variant B additionally re-runs the rating imputation without the page-view
# quartiles, so Wikipedia data enters nowhere. The PCA mirrors the data
# preparation: fit on complete cases with the raw rating (correlation matrix,
# i.e. standardized indicators), scores for all prizes from the imputed rating
# with the fitting-sample means and standard deviations.
# ==============================================================================
source("_helpers.R")

prizeList <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`))
cat("Rows:", nrow(prizeList), "\n")

# Baseline ranking and tiers
prizeList <- add_tiers(prizeList, "pcaRank")
baseTier <- prizeList %>% select(`Award Name`, pcaRank, Tier)

# Wikipedia is also an inclusion criterion (C4: at least 40 daily views). Prizes
# clearing only that bar would leave the list without Wikipedia.
c4only <- prizeList %>%
  filter(C4 == "x", is.na(C1) | C1 != "x", is.na(C2) | C2 != "x", is.na(C3) | C3 != "x")
cat("\nPrizes clearing only the page-view inclusion bar:", nrow(c4only), "\n")
print(c4only %>% select(`Award Name`, pcaRank, Tier))
stopifnot(all(c4only$Tier == 3))

# PCA index: fit on complete cases with the raw rating, score everyone with
# RatingNorm using the fitting-sample means and population SDs
pcaIndex <- function(df, vars, ratingScoreCol = "RatingNorm") {
  fit <- df[, vars] %>% na.omit()
  mu <- colMeans(fit)
  sd0 <- apply(fit, 2, function(x) sqrt(mean((x - mean(x))^2)))
  Z <- scale(as.matrix(fit), center = mu, scale = sd0)
  pc <- prcomp(Z, center = FALSE, scale. = FALSE)
  load1 <- pc$rotation[, 1]
  if (load1["Rating"] < 0) load1 <- -load1
  cat("PC1 loadings [", paste(vars, collapse = ", "), "]:",
      round(load1, 4), "| var share:", round(pc$sdev[1]^2 / sum(pc$sdev^2), 3), "\n")
  score_vars <- df[, vars]
  score_vars$Rating <- df[[ratingScoreCol]]
  Zs <- scale(as.matrix(score_vars), center = mu, scale = sd0)
  as.numeric(Zs %*% load1)
}

# Replication check: the full five-indicator PCA reproduces pcaRank
vars5 <- c("Rating", "lnPageViews", "lnAge", "lnMoneyPerPrize", "lnNewsMentions")
prizeList$score5 <- pcaIndex(prizeList, vars5)
repl_rho <- cor(rank(-prizeList$score5), prizeList$pcaRank, method = "spearman")
cat("Replication check (recomputed 5-indicator index vs pcaRank): rho =", round(repl_rho, 4), "\n")
stopifnot(repl_rho > 0.999)

# --- Variant A: drop lnPageViews from the PCA ---
vars4 <- c("Rating", "lnAge", "lnMoneyPerPrize", "lnNewsMentions")
prizeList$scoreA <- pcaIndex(prizeList, vars4)
prizeList$rankA <- rank(-prizeList$scoreA, ties.method = "first")
tierA <- add_tiers(prizeList, "rankA") %>% select(`Award Name`, rankA, TierA = Tier)

# --- Variant B: also re-impute ratings without the page-view quartiles ---
# Original imputation: Rating ~ i.viewsQuart + i.ageQuart + i.moneyQuart + i.Field
# + C1 + C3, predicted/2 for missing. Here viewsQuart is dropped. Dummies are
# built by hand so an NA quartile becomes the reference category.
imp <- prizeList %>%
  mutate(C1n = as.integer(!is.na(C1) & C1 == "x"), C3n = as.integer(!is.na(C3) & C3 == "x"))
dum <- function(x, lvls) sapply(lvls, function(l) as.numeric(!is.na(x) & x == l))
X <- cbind(1,
           dum(imp$`4 quantiles of age`, 2:4),
           dum(imp$`4 quantiles of moneyPerPeriod`, 2:4),
           dum(imp$Field, sort(unique(imp$Field))[-1]),
           imp$C1n, imp$C3n)
train <- !is.na(imp$Rating)
fitB <- lm.fit(X[train, , drop = FALSE], imp$Rating[train])
beta <- ifelse(is.na(fitB$coefficients), 0, fitB$coefficients)  # unseen-level dummies -> 0
imp$RatingHatB <- as.numeric(X %*% beta) / 2
prizeList$RatingNormB <- ifelse(is.na(prizeList$Rating), imp$RatingHatB, prizeList$Rating)

prizeList$scoreB <- pcaIndex(prizeList, vars4, ratingScoreCol = "RatingNormB")
prizeList$rankB <- rank(-prizeList$scoreB, ties.method = "first")
tierB <- add_tiers(prizeList, "rankB") %>% select(`Award Name`, rankB, TierB = Tier)

# --- Compare ---
cmp <- baseTier %>% left_join(tierA, by = "Award Name") %>% left_join(tierB, by = "Award Name")

stats_for <- function(cmp, rk, tr) {
  list(rho   = cor(cmp$pcaRank, cmp[[rk]], method = "spearman"),
       moved = sum(cmp$Tier != cmp[[tr]]),
       t1    = sum(cmp$Tier == 1 & cmp[[tr]] != 1) + sum(cmp$Tier != 1 & cmp[[tr]] == 1),
       med   = median(abs(cmp$pcaRank - cmp[[rk]])),
       max   = max(abs(cmp$pcaRank - cmp[[rk]])))
}
A <- stats_for(cmp, "rankA", "TierA")
B <- stats_for(cmp, "rankB", "TierB")
cat(sprintf("\nVariant A (views out of PCA):        rho=%.3f, tier changes=%d (T1: %d), median |move|=%.0f, max=%d\n",
            A$rho, A$moved, A$t1, A$med, A$max))
cat(sprintf("Variant B (views out of everything): rho=%.3f, tier changes=%d (T1: %d), median |move|=%.0f, max=%d\n",
            B$rho, B$moved, B$t1, B$med, B$max))

write_tex(c(
  mac("rOneFourSpearmanA",   sprintf("%.3f", A$rho)),
  mac("rOneFourTierMovesA",  A$moved),
  mac("rOneFourTierOneMovesA", A$t1),
  mac("rOneFourMedianMoveA", sprintf("%.0f", A$med)),
  mac("rOneFourMaxMoveA",    A$max),
  mac("rOneFourSpearmanB",   sprintf("%.3f", B$rho)),
  mac("rOneFourTierMovesB",  B$moved),
  mac("rOneFourTierOneMovesB", B$t1),
  mac("rOneFourMedianMoveB", sprintf("%.0f", B$med)),
  mac("rOneFourMaxMoveB",    B$max),
  mac("rOneFourNPrizes",     nrow(cmp)),
  mac("rOneFourCFourOnly",   nrow(c4only)),
  mac("rOneFourViewsZeroPrizes", sum(prizeList$`Daily Page Views` == 0)),
  mac("rOneFourAnnualPrizes",    sum(prizeList$Period == 1))
), file.path(table_dir, "r1_4_macros.tex"))
