# ==============================================================================
# 01_ranking_tables.R
# Generates: summaryStats.tex, selectedPrizes.tex, selectedECPrizes.tex
# Inputs: data/cleanPrizeList.xlsx, data/cleanEC.xlsx
# ==============================================================================
source("_helpers.R")

# Load data
prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx"))

################################################################################
# selectedPrizes.tex: the 99 prizes ranked by the prestige index, with tiers
# (label listOfPrizes)
################################################################################

prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)

prizeList <- prizeList %>%
  mutate(Tier = case_when(cumWinners <= 0.1 * totalWinners ~ 1,
                          cumWinners <= 0.3 * totalWinners ~ 2,
                          TRUE ~ 3),
         # formatted as text so that 100, 20 or -41 print with one decimal like the rest
         pcaRatingNormRound = sprintf("%.1f", pcaRatingNorm))

forTable <- prizeList %>% select("Award Name", "Tier", "pcaRank", "pcaRatingNormRound", "plotFinestField")

# Field labels as in prizesByField.tex
forTable$plotFinestField <- gsub("&", "and", forTable$plotFinestField)
forTable$plotFinestField <- gsub("^Math$", "Mathematics", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Life Sciences and Medicine", "Life and Health Sciences", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Materials and mining engineering", "Materials and Mining Eng.", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Bioengineering and biomedical engineering", "Bio. and Biomedical Eng.", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Computing, electrical", "Computing, Electrical", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Civil, environmental, and transportation engineering", "Civil, Env., and Transp. Eng.", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Mechanical engineering", "Mechanical Eng.", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Business and Econ", "Business and Economics", forTable$plotFinestField)
forTable$plotFinestField <- gsub("Ag. and natural resources", "Ag. and Natural Resources", forTable$plotFinestField)
colnames(forTable)[5] <- "Field"
colnames(forTable)[3] <- "Rank"
colnames(forTable)[4] <- "Rating"

# Equalize ranks for tied ratings
for (i in 2:nrow(forTable)) {
  if (forTable$Rating[i] == forTable$Rating[i - 1]) {
    forTable$Rank[i] <- forTable$Rank[i - 1]
  }
}

table <- stargazer(forTable,
                   summary = FALSE, digits = 1, type = "latex", rownames = FALSE,
                   title = "List of Selected Prizes")

table <- gsub("\\begin{tabular}", "\\begin{longtable}", table, fixed = TRUE)
table <- gsub("\\end{tabular}", "\\end{longtable}", table, fixed = TRUE)
table <- gsub("ccc}",
              "ccc} \\caption{Most Prestigious Prizes, Ranked} \\label{listOfPrizes}", table, fixed = TRUE)
table <- gsub("{@{\\extracolsep{5pt}} ccccc}", "{lcccc}", table, fixed = TRUE)
table <- table[-c(seq(1, 6), length(table))]

table[length(table) + 2] <-
  paste("\\noindent \\footnotesize \\textit{Note}: This table lists the 99 prizes that our methodology has identified as",
        "``most prestigious,'' ranked. Prizes are sorted by Rating, which is the",
        "sum of the prestige indicators weighted by the first principal component",
        "loadings, rescaled so that the Nobel Prize in Physics equals 100 and the mean",
        sprintf("of the %d prizes with observed survey ratings (the PCA fitting sample) equals 0.", sum(!is.na(prizeList$Rating))),
        "Prizes with the same Rating up to the first decimal are shown",
        "under the same Rank. Tiers are defined according to the cumulative sum",
        "of the yearly most prestigious recognition events",
        "(1 event = 1 person being recognized with 1 prize): Tier 1 includes the top",
        "10\\% recognition events, Tier 2 the next 20\\%, and Tier 3 the remaining.",
        sep = " ")
table[length(table) - 1] <- ""
table <- longtable_heads(table, "listOfPrizes", 5)

write_tex(table, file.path(table_dir, "selectedPrizes.tex"))

################################################################################
# summaryStats.tex: summary statistics of the 99 prizes (label summaryStats)
################################################################################

prizeList$money_per_prize <- prizeList$moneyPerPrize / 1000
prizeList$money_per_winner <- prizeList$moneyPerWinner / 1000
prizeList$money_per_year <- prizeList$moneyPerYear / 1000

summaryStats <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(summaryStats) <- c("Variable", "Median", "Mean", "SD", "Min", "Max", "N")

vars <- c("Rating", "Daily Page Views", "article_count", "age", "Period", "Yearly Winners",
          "money_per_year", "money_per_prize", "money_per_winner")
labels <- c("Survey Rating", "Daily Page Views", "News Mentions", "Prize Age", "Period (Years)",
            "Yearly Winners", "Money per Year", "Money per Prize", "Money per Winner")

i <- 1
for (var in vars) {
  median_val <- round(median(prizeList[[var]], na.rm = TRUE), 2)
  mean_val <- round(mean(prizeList[[var]], na.rm = TRUE), 2)
  sd_val <- round(sd(prizeList[[var]], na.rm = TRUE), 2)
  min_val <- round(min(prizeList[[var]], na.rm = TRUE), 2)
  max_val <- round(max(prizeList[[var]], na.rm = TRUE), 2)
  n <- sum(!is.na(prizeList[[var]]))
  summaryStats[i, ] <- c(labels[i], median_val, mean_val, sd_val, min_val, max_val, n)
  i <- i + 1
}

summaryStatsTable <- stargazer(summaryStats, summary = FALSE, digits = 1, type = "latex", rownames = FALSE,
                               title = "Summary Statistics", label = "summaryStats")
summaryStatsTable <- gsub("{@{\\extracolsep{5pt}} ccccccc}", "{lcccccc}", summaryStatsTable, fixed = TRUE)

summaryStatsTable[length(summaryStatsTable)] <-
  paste("\\noindent \\justify \\footnotesize \\textit{Note}: This table provides summary statistics for the 99 recognition",
        "prizes that our methodology identifies as ``most prestigious.'' Survey Rating",
        "stands for the expert ratings of prize importance in relation to the Nobel by",
        "\\cite{zheng2015mapping, jiang2018hierarchical}. Daily Page Views is the average number of daily",
        "Wikipedia page views, 2020--2025. News Mentions is the number of unique news articles mentioning",
        "the prize in the Media Cloud archive, January 2020 to December 2025.",
        "Prize Age is the number of years since the prize was first given, as of 2025.",
        "Period refers to the periodicity of the prize (e.g., yearly = 1, given once",
        "every two years = 2, and so on). Yearly Winners is the number of recipients per award period",
        "divided by the period. Money per Year is the prize money paid out per award period divided by the period;",
        "Money per Prize is the money per award period divided by the number of prizes given per period",
        "(some awards give several prizes per period); Money per Winner is the money per award period divided",
        "by the number of recipients per period. Money values are in thousands of USD.")

summaryStatsTable <- c(summaryStatsTable, "\\end{table}")

write_tex(summaryStatsTable, file.path(table_dir, "summaryStats.tex"))

################################################################################
# selectedECPrizes.tex: the 68 early-career prizes with tiers (label listOfECPrizes)
################################################################################

ECList <- readxl::read_excel(file.path(data_dir, "cleanEC.xlsx")) %>%
  filter(Field != "Humanities")

ECList <- ECList[order(ECList$Rank), ]

ECList$Tier <- 3
ECList$Tier[1:10] <- 1
ECList$Tier[11:30] <- 2

ECList <- ECList[order(ECList$Tier, ECList$`Award Name`), ]
forTable <- ECList %>% select("Award Name", "Tier", "Field")

# The asterisk marks research fellowships (cohort programs that fund a period of
# research rather than a single award)
fellowships <- c("Sloan Research Fellowship", "Amelia Earhart Fellowship")
stopifnot(all(fellowships %in% forTable$`Award Name`))
forTable$`Award Name`[forTable$`Award Name` %in% fellowships] <-
  paste0(forTable$`Award Name`[forTable$`Award Name` %in% fellowships], "*")

ECtable <- stargazer(forTable,
                     summary = FALSE, digits = 1, type = "latex", rownames = FALSE,
                     title = "List of Selected Early Career Prizes")

# \begingroup\small ... \endgroup keeps \small scoped to the longtable
ECtable <- gsub("\\begin{tabular}", "\\begingroup\\small \\begin{longtable}", ECtable, fixed = TRUE)
ECtable <- gsub("\\end{tabular}", "\\end{longtable}\\endgroup", ECtable, fixed = TRUE)
ECtable <- gsub("cc}",
                "cc} \\caption{Selected Early Career Prizes, Ranked} \\label{listOfECPrizes}", ECtable, fixed = TRUE)
ECtable <- gsub("{@{\\extracolsep{5pt}} ccc}", "{lcc}", ECtable, fixed = TRUE)
ECtable <- ECtable[-c(seq(1, 6), length(ECtable))]

ECtable[length(ECtable) + 2] <- paste(
  "\\noindent \\footnotesize \\textit{Note}: This table lists the 68 most prestigious early",
  "career prizes. We construct prize Tiers by calculating a prestige index that puts 80\\% weight on daily page views and",
  "20\\% on prize age. Tier 1 contains the top 10 prizes, Tier 2",
  "the next 20. Within Tiers, prizes are listed alphabetically.",
  "\\textasteriskcentered{} Research fellowship: a cohort program that funds a period of research rather than a single award.",
  sep = " ")
ECtable[length(ECtable) - 1] <- ""
ECtable <- longtable_heads(ECtable, "listOfECPrizes", 3)

write_tex(ECtable, file.path(table_dir, "selectedECPrizes.tex"))
