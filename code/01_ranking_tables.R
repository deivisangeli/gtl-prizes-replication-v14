# ==============================================================================
# 01_ranking_tables.R
# Generates: Tables 1, S1, S3 (summaryStats.tex, selectedPrizes.tex, selectedECPrizes.tex)
# ==============================================================================
source("_helpers.R")

# Load data
prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`))

################################################################################
# Table 1: Most Prestigious Prizes, by Field
################################################################################

prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)

prizeList <- prizeList %>%
  mutate(Tier = case_when(cumWinners <= 0.1 * totalWinners ~ 1,
                          cumWinners <= 0.3 * totalWinners ~ 2,
                          TRUE ~ 3),
         pcaRatingNormRound = round(pcaRatingNorm, 1))

forTable <- prizeList %>% select("Award Name", "Tier", "pcaRank", "pcaRatingNormRound", "plotFinestField")

# Clean field labels to match Table 2 (prizesByField)
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

table <- gsub("\\textbackslash &", "\\&", table, fixed = TRUE)
# Until 2026-09-03 the longtable was wrapped in a "{ ... }" group whose closing brace was
# never written, leaving an unbalanced "{" that made the paper fail with "Missing } inserted".
# The group served no purpose here.
table <- gsub("\\begin{tabular}", "\\begin{longtable}", table, fixed = TRUE)
table <- gsub("\\end{tabular}", "\\end{longtable}", table, fixed = TRUE)
table <- gsub("ccc}",
              "ccc} \\caption{Most Prestigious Prizes, by Field} \\label{listOfPrizes}", table, fixed = TRUE)
table <- gsub("{@{\\extracolsep{5pt}} ccccc}", "{lcccc}", table, fixed = TRUE)
table <- table[-c(seq(1, 6), length(table))]

table[length(table) + 2] <-
  paste("\\noindent \\footnotesize \\textit{Note}: This table lists the 99 prizes that our methodology has identified as",
        "``most prestigious,'' ranked. Prizes are sorted by Rating, which is the",
        "sum of the prestige indicators weighted by the first principal component",
        "loadings. Prizes with the same Rating up to the first decimal are shown",
        "under the same Rank. Tiers are defined according to the cumulative sum",
        "of the yearly most prestigious recognition events",
        "(1 event = 1 personbeing recognized with 1 prize): Tier 1 includes the top",
        "10\\% recognition events, Tier 2 the next 20\\%, and Tier 3 the remaining.",
        sep = " ")
table[length(table) - 1] <- ""
table <- gsub("ccccc", "lcccc", table)

write_tex(table, file.path(table_dir, "selectedPrizes.tex"))

################################################################################
# Table 2: Summary Statistics
################################################################################

prizeList$money_per_prize <- prizeList$moneyPerPrize / 1000
prizeList$money_per_winner <- prizeList$moneyPerWinner / 1000
prizeList$money_per_year <- prizeList$moneyPerYear / 1000

summaryStats <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(summaryStats) <- c("Variable", "Median", "Mean", "SD", "Min", "Max", "N")

vars <- c("Rating", "Daily Page Views", "age", "Period", "Yearly Winners",
          "money_per_year", "money_per_prize", "money_per_winner")
labels <- c("Survey Rating", "Daily Page views", "Prize Age", "Period (Years)",
            "Yearly Winners", "Money per Year", "Money per Prize",
            "Money Prize per Winner (Average)")

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
        "\\cite{zheng2015mapping, jiang2018hierarchical}. Daily Page views refers to Wikipedia",
        "page views. Prize Age is with reference to the year the prize was first given.",
        "Period refers to the periodicity of the prize (e.g., yearly = 1, given once",
        "every two years = 2, and so on). Money values are in thousands of USD.")

summaryStatsTable <- c(summaryStatsTable, "\\end{table}")

write_tex(summaryStatsTable, file.path(table_dir, "summaryStats.tex"))

################################################################################
# Table S1: Selected Early Career Prizes
################################################################################

ECList <- readxl::read_excel(file.path(data_dir, "cleanEC.xlsx")) %>%
  filter(Field != "Humanities")

ECList <- ECList[order(ECList$Rank), ]
ECList$cumWinners <- cumsum(ECList$`Yearly Winners`)

ECList$Tier <- 3
ECList$Tier[1:10] <- 1
ECList$Tier[11:30] <- 2

ECList <- ECList[order(ECList$Tier, ECList$`Award Name`), ]
forTable <- ECList %>% select("Award Name", "Tier", "Field")

ECtable <- stargazer(forTable,
                     summary = FALSE, digits = 1, type = "latex", rownames = FALSE,
                     title = "List of Selected Early Career Prizes")

# \small stays scoped to the table; \begingroup/\endgroup sit on the same lines as the longtable
# delimiters so they cannot be lost separately (see the note on the main table above).
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
  sep = " ")
ECtable[length(ECtable) - 1] <- ""

write_tex(ECtable, file.path(table_dir, "selectedECPrizes.tex"))
