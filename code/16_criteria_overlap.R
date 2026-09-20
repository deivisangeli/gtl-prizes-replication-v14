# ==============================================================================
# 16_criteria_overlap.R
# Generates: r2_1_criteria_table.tex (overlap of the four inclusion criteria)
#            and r2_1_macros.tex (prizes meeting each criterion; Gini coefficient
#            of field densities with and without the prizes that enter through
#            the citation criterion alone)
# Inputs: data/cleanPrizeList.xlsx, data/nsf2023.xlsx,
#         data/nsf_field_to_finest_group.csv, data/vs_academics_by_finest_group.csv,
#         and through winners_by_group() data/all_winners_with_plotFinestField.csv
#         and data/subfield_to_finest_group.csv
# ==============================================================================
source("_helpers.R")

## ---- Prize list, tiers, criteria flags --------------------------------------
prizeList <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  arrange(pcaRank)
stopifnot(nrow(prizeList) == 99)
totW <- sum(prizeList$`Yearly Winners`)
prizeList <- prizeList %>%
  mutate(cumW = cumsum(`Yearly Winners`),
         Tier = case_when(cumW <= 0.1 * totW ~ 1, cumW <= 0.3 * totW ~ 2, TRUE ~ 3),
         across(c(C1, C2, C3, C4), ~ as.integer(!is.na(.x) & .x == "x")),
         Cs = C1 + C2 + C3 + C4)
stopifnot(all(prizeList$Cs >= 1))

nC   <- colSums(prizeList[, c("C1", "C2", "C3", "C4")])
only <- sapply(c("C1", "C2", "C3", "C4"), function(c) sum(prizeList[[c]] == 1 & prizeList$Cs == 1))

# The prizes that enter through the citation criterion alone are all Tier 3
c3only <- prizeList %>% filter(C3 == 1, Cs == 1)
stopifnot(all(c3only$Tier == 3))

## ---- Field densities with and without the C3-only prizes --------------------
# Winner matching and field-size denominators as in the density figures (scripts 07 and 08)
normalize_name <- function(x) {
  x <- gsub("–", "-", x); x <- gsub("—", "-", x); x <- gsub("ö", "o", x); trimws(x)
}
prize_key <- prizeList %>% mutate(C3only = Cs == 1 & C3 == 1) %>%
  select(`Award Name`, C3only) %>%
  mutate(prize_norm = normalize_name(`Award Name`))
prize_key$prize_norm[prize_key$prize_norm == "Balzan Prizes"] <- "Balzan Prize"
prize_key$prize_norm[prize_key$prize_norm == "Gold Medal for Astronomy"] <- "Gold Medal of the Royal Astronomical Society"
prize_key$prize_norm[prize_key$prize_norm == "Crafoord prize in Polyarthritis"] <- "Crafoord Prize in Polyarthritis"
winners <- winners_by_group() %>% mutate(prize_norm = normalize_name(Prize))
w <- winners %>% inner_join(prize_key %>% select(prize_norm, C3only), by = "prize_norm")
stopifnot(nrow(w) == nrow(winners),                        # every recognition matched a prize
          all(prize_key$prize_norm %in% w$prize_norm))     # every prize has winners

nsf_map <- read.csv(file.path(data_dir, "nsf_field_to_finest_group.csv"), stringsAsFactors = FALSE)
nsf_raw <- read_excel(file.path(data_dir, "nsf2023.xlsx"))
phd_by_group <- nsf_raw %>% inner_join(nsf_map, by = c("Field of doctorate" = "nsf_field")) %>%
  group_by(finest_group) %>% summarise(size = sum(DoctoratesIn2023), .groups = "drop")
vs_raw <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE)
vs_by_group <- vs_raw[vs_raw$group_name != "Humanities", ] %>%
  select(finest_group = group_name, size = academic_count)

density_by <- function(wdf, denom) {
  re <- wdf %>% group_by(finest_group) %>% summarise(yearlyRE = n() / 10, .groups = "drop")
  denom %>% full_join(re, by = "finest_group") %>%
    mutate(size = coalesce(size, 0), yearlyRE = coalesce(yearlyRE, 0)) %>%
    filter(size > 0) %>% mutate(density = yearlyRE / size * 1000)
}
compare <- function(denom, label) {
  full <- density_by(w, denom) %>% select(finest_group, dens_full = density)
  red  <- density_by(w %>% filter(!C3only), denom) %>% select(finest_group, dens_noC3 = density)
  full %>% inner_join(red, by = "finest_group") %>% mutate(denominator = label)
}
cmp <- bind_rows(compare(phd_by_group, "PhD"), compare(vs_by_group, "VS"))

gini <- function(x) { x <- sort(x); n <- length(x); if (sum(x) == 0) return(NA); sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x)) }
summ <- cmp %>% group_by(denominator) %>%
  summarise(gini_full = gini(dens_full), gini_noC3 = gini(dens_noC3), .groups = "drop")

## ---- Macros and table --------------------------------------------------------
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
m <- c(
  mac("rTwoOneCOne",   nC["C1"]), mac("rTwoOneCTwo", nC["C2"]),
  mac("rTwoOneCThree", nC["C3"]), mac("rTwoOneCFour", nC["C4"]),
  mac("rTwoOneCThreeOnly", only["C3"]),
  mac("rTwoOneGiniFullVS", fmt(summ$gini_full[summ$denominator == "VS"])), mac("rTwoOneGiniNoCThreeVS", fmt(summ$gini_noC3[summ$denominator == "VS"])),
  mac("rTwoOneGiniFullPhD", fmt(summ$gini_full[summ$denominator == "PhD"])), mac("rTwoOneGiniNoCThreePhD", fmt(summ$gini_noC3[summ$denominator == "PhD"]))
)
write_tex(m, file.path(table_dir, "r2_1_macros.tex"))

# Overlap table: rows = criterion, cols = prizes meeting it, unique to it
tab <- c("% generated by code/16_criteria_overlap.R -- do not edit by hand",
         "\\begin{table}[H]", "\\centering \\small",
         "\\caption{Inclusion Criteria: Prizes Meeting Each Criterion and Prizes Meeting No Other}",
         "\\label{tab:criteria_overlap}",
         "\\begin{tabular}{p{8.6cm}cc}", "\\toprule",
         "Inclusion criterion & Prizes meeting it & Meeting no other \\\\", "\\midrule",
         sprintf("1. Appears on at least three of seven authoritative lists & %d & %d \\\\", nC["C1"], only["C1"]),
         sprintf("2. Expert rating $\\geq 0.5$ of a Nobel, and on at least one list & %d & %d \\\\", nC["C2"], only["C2"]),
         sprintf("3. Over 50\\%% of recent winners on Clarivate's HCR list & %d & %d \\\\", nC["C3"], only["C3"]),
         sprintf("4. At least 40 daily Wikipedia page views, 2020--2025 & %d & %d \\\\", nC["C4"], only["C4"]),
         "\\midrule",
         sprintf("At least one criterion (the 99 prizes) & %d & \\\\", nrow(prizeList)),
         sprintf("Two or more criteria & %d & \\\\", sum(prizeList$Cs >= 2)),
         sprintf("All four criteria & %d & \\\\", sum(prizeList$Cs == 4)),
         "\\bottomrule", "\\end{tabular}",
         "\\par\\vspace{3pt}\\begin{minipage}{\\linewidth}\\footnotesize",
         paste0("\\textit{Note}: A prize enters the list if it meets at least one criterion; the citation ",
                "criterion is evaluated over prizes that appear on at least one of the seven authoritative ",
                "lists or were rated in either expert survey."),
         "\\end{minipage}", "\\end{table}")
write_tex(tab, file.path(table_dir, "r2_1_criteria_table.tex"))
