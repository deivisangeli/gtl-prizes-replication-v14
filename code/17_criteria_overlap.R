# ==============================================================================
# 17_criteria_overlap.R
# Generates: r2_1_criteria_table.tex (Table S7, overlap of the four inclusion
#            criteria) and r2_1_macros.tex -- how binding the citation-based
#            criterion (C3: over 50% of recent winners on Clarivate's Highly Cited
#            Researchers list) is: the prizes that enter only through it, field
#            densities with and without them, and their profile against the rest
# Reads the intermediate files of scripts 16 (funder types, awarding bodies).
# ==============================================================================
source("_helpers.R")

## ---- Prize list, tiers, criteria flags --------------------------------------
prizeList <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`)) %>%
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
cat("Criterion counts:\n"); print(nC); cat("Unique to criterion:\n"); print(only)

c3only <- prizeList %>% filter(C3 == 1, Cs == 1)
cat("\nC3-only prizes:", nrow(c3only), "\n")
stopifnot(all(c3only$Tier == 3))
c3only_events_share <- sum(c3only$`Yearly Winners`) / totW
c3only_fields <- c3only %>% count(Field, sort = TRUE)

# Tier-1 prizes failing the citation bar
t1 <- prizeList %>% filter(Tier == 1)
t1_notC3 <- t1 %>% filter(C3 == 0)

# Tiers recomputed on the prizes that remain without the C3-only ones (same
# 10%/30% cumulative-recognition rule, same pcaRank order)
rest <- prizeList %>% filter(!(Cs == 1 & C3 == 1)) %>% arrange(pcaRank)
totW76 <- sum(rest$`Yearly Winners`)
rest <- rest %>% mutate(cumW76 = cumsum(`Yearly Winners`),
                        Tier76 = case_when(cumW76 <= 0.1 * totW76 ~ 1, cumW76 <= 0.3 * totW76 ~ 2, TRUE ~ 3))
tier_changes <- rest %>% filter(Tier != Tier76)

# Early-career prizes among the 99 (intersection with the EC list)
ec <- read_excel(file.path(data_dir, "cleanEC.xlsx")) %>%
  filter(Field != "Humanities")
norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
ec_in99 <- prizeList %>% filter(norm(`Award Name`) %in% norm(ec$`Award Name`))

## ---- Field densities with and without the C3-only prizes --------------------
# Winner matching and denominators as in the density figures (scripts 07-09)
sf_map  <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)
nsf_map <- read.csv(file.path(data_dir, "nsf_field_to_finest_group.csv"), stringsAsFactors = FALSE)
winners <- read.csv(file.path(data_dir, "all_winners_with_plotFinestField.csv"),
                    stringsAsFactors = FALSE, encoding = "UTF-8")

normalize_name <- function(x) {
  x <- gsub("–", "-", x); x <- gsub("—", "-", x); x <- gsub("ö", "o", x); trimws(x)
}
prize_key <- prizeList %>% mutate(C3only = Cs == 1 & C3 == 1) %>%
  select(`Award Name`, Tier, C3only) %>%
  mutate(prize_norm = normalize_name(`Award Name`))
prize_key$prize_norm[prize_key$prize_norm == "Balzan Prizes"] <- "Balzan Prize"
prize_key$prize_norm[prize_key$prize_norm == "Gold Medal for Astronomy"] <- "Gold Medal of the Royal Astronomical Society"
prize_key$prize_norm[prize_key$prize_norm == "Crafoord prize in Polyarthritis"] <- "Crafoord Prize in Polyarthritis"
winners$prize_norm <- normalize_name(winners$Prize)
w <- winners %>% inner_join(prize_key %>% select(prize_norm, Tier, C3only), by = "prize_norm") %>%
  inner_join(sf_map %>% select(subfield_name, finest_group) %>% distinct(),
             by = c("Best_Subfield" = "subfield_name"))
cat("\nWinners matched:", nrow(w), "of", nrow(winners), "; from C3-only prizes:", sum(w$C3only), "\n")
stopifnot(length(setdiff(prize_key$prize_norm, winners$prize_norm)) == 0)  # every prize has winners

nsf_raw <- read_excel(file.path(data_dir, "nsf2023.xlsx")) %>%
  filter(!(plotFinestField %in% c("Humanities", "Education", "Other non-science")))
phd_by_group <- nsf_raw %>% inner_join(nsf_map, by = c("Field of doctorate" = "nsf_field")) %>%
  group_by(finest_group) %>% summarise(size = sum(DoctoratesIn2023), .groups = "drop")
vs_raw <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE)
vs_by_group <- vs_raw[vs_raw$academic_count > 0 & vs_raw$group_name != "Humanities", ] %>%
  select(finest_group = group_name, size = academic_count)

density_by <- function(wdf, denom) {
  re <- wdf %>% group_by(finest_group) %>% summarise(yearlyRE = n() / 10, .groups = "drop")
  denom %>% full_join(re, by = "finest_group") %>%
    mutate(size = coalesce(size, 0), yearlyRE = coalesce(yearlyRE, 0)) %>%
    filter(size > 0) %>% mutate(density = yearlyRE / size * 1000)
}
compare <- function(denom, label) {
  full <- density_by(w, denom) %>% select(finest_group, yearlyRE_full = yearlyRE, dens_full = density)
  red  <- density_by(w %>% filter(!C3only), denom) %>% select(finest_group, yearlyRE_noC3 = yearlyRE, dens_noC3 = density)
  full %>% inner_join(red, by = "finest_group") %>%
    mutate(rank_full = rank(-dens_full), rank_noC3 = rank(-dens_noC3),
           rank_move = rank_noC3 - rank_full, denominator = label) %>%
    arrange(rank_full)
}
cmp <- bind_rows(compare(phd_by_group, "PhD"), compare(vs_by_group, "VS"))

gini <- function(x) { x <- sort(x); n <- length(x); if (sum(x) == 0) return(NA); sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x)) }
summ <- cmp %>% group_by(denominator) %>%
  summarise(spearman = cor(dens_full, dens_noC3, method = "spearman"),
            max_rank_move = max(abs(rank_move)),
            n_fields = n(),
            gini_full = gini(dens_full), gini_noC3 = gini(dens_noC3),
            top3_share_full = sum(sort(yearlyRE_full, decreasing = TRUE)[1:3]) / sum(yearlyRE_full),
            top3_share_noC3 = sum(sort(yearlyRE_noC3, decreasing = TRUE)[1:3]) / sum(yearlyRE_noC3),
            .groups = "drop")
print(as.data.frame(summ))
# Top and bottom five fields under each definition
bottom <- cmp %>% group_by(denominator) %>%
  summarise(same_bottom5 = setequal(finest_group[order(-rank_full)][1:5], finest_group[order(-rank_noC3)][1:5]),
            .groups = "drop")
# Fields that lose the most recognition events (share) when C3-only prizes drop
loss <- cmp %>% filter(denominator == "PhD") %>%
  mutate(lost_share = 1 - yearlyRE_noC3 / yearlyRE_full) %>% arrange(desc(lost_share)) %>%
  select(finest_group, yearlyRE_full, yearlyRE_noC3, lost_share)

## ---- Laureate age at award, C3-only prizes vs the rest ------------------------
# Wikidata birth years (data/laureate_birth_years.csv)
by <- read.csv(file.path(data_dir, "laureate_birth_years.csv"), stringsAsFactors = FALSE, encoding = "UTF-8") %>%
  filter(!is.na(birth_year)) %>% distinct(Winner, Wikipedia_URL, .keep_all = TRUE)
wa <- w %>% left_join(by %>% select(Winner, Wikipedia_URL, birth_year), by = c("Winner", "Wikipedia_URL")) %>%
  mutate(age = Year - birth_year)
age_cov <- mean(!is.na(wa$age))
age_by <- wa %>% filter(!is.na(age)) %>% group_by(C3only) %>%
  summarise(n = n(), med_age = median(age), mean_age = mean(age), sh_under45 = mean(age < 45), .groups = "drop")
cat("\nAge at award (Wikidata coverage", round(100 * age_cov), "% of recognitions):\n"); print(as.data.frame(age_by))

## ---- Profile of the C3-only prizes vs the other 76 --------------------------
fund <- read.csv(file.path(intermediate_dir, "r2_5_funder_by_prize.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
awd  <- read.csv(file.path(intermediate_dir, "prize_awarding_countries.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
# a name key: lower case, accents stripped, everything but letters and digits removed
key  <- function(x) gsub("[^a-z0-9]", "", tolower(chartr("àáâäèéêëìíîïòóôöùúûüñç",
                                                        "aaaaeeeeiiiioooouuuunc", x)))
prof <- prizeList %>% mutate(C3only = Cs == 1 & C3 == 1, k = key(`Award Name`)) %>%
  mutate(k = recode(k, balzanprizes = "balzanprize",
                    goldmedalforastronomy = "goldmedaloftheroyalastronomicalsociety")) %>%
  left_join(fund %>% mutate(k = key(Prize)) %>% select(k, funder_origin_type, funder_current_type), by = "k") %>%
  left_join(awd %>% mutate(k = key(Prize)) %>% select(k, AwardingOrgType, AwardingCountry), by = "k")
stopifnot(sum(is.na(prof$funder_origin_type)) == 0, sum(is.na(prof$AwardingOrgType)) == 0)
grp <- prof %>% group_by(C3only) %>%
  summarise(n = n(),
            med_first = median(`First awarded`, na.rm = TRUE),
            sh_post1980 = mean(`First awarded` >= 1980, na.rm = TRUE),
            med_money = median(moneyPerWinner, na.rm = TRUE),
            med_views = median(`Daily Page Views`, na.rm = TRUE),
            n_gov_origin = sum(funder_origin_type == "Government"),
            sh_philanthropy_origin = mean(funder_origin_type == "Philanthropy"),
            n_society_awd = sum(AwardingOrgType == "Society"),
            n_university_awd = sum(AwardingOrgType == "University"),
            n_philanthropy_awd = sum(AwardingOrgType == "Philanthropy"),
            n_us = sum(AwardingCountry == "United States", na.rm = TRUE),
            n_sweden = sum(AwardingCountry == "Sweden", na.rm = TRUE),
            n_lifephys = sum(plotField %in% c("Life Sciences & Medicine", "Physics & Astronomy")),
            n_math_cs_env = sum(plotField %in% c("Math", "CS & Engineering", "Environment", "Ag. & natural resources")),
            .groups = "drop")
g23 <- grp[grp$C3only, ]; g76 <- grp[!grp$C3only, ]

## ---- Macros and table --------------------------------------------------------
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
m <- c(
  mac("rTwoOneCOne",   nC["C1"]), mac("rTwoOneCTwo", nC["C2"]),
  mac("rTwoOneCThree", nC["C3"]), mac("rTwoOneCFour", nC["C4"]),
  mac("rTwoOneCOneOnly",   only["C1"]), mac("rTwoOneCTwoOnly", only["C2"]),
  mac("rTwoOneCThreeOnly", only["C3"]), mac("rTwoOneCFourOnly", only["C4"]),
  mac("rTwoOneMultiCriteria", sum(prizeList$Cs >= 2)),
  mac("rTwoOneAllFour", sum(prizeList$Cs == 4)),
  mac("rTwoOneCThreeOnlyEventsPct", fmt(100 * c3only_events_share, 0)),
  mac("rTwoOneCThreeOnlyLifeSci", c3only_fields$n[c3only_fields$Field == "Life Sciences and Medicine"]),
  mac("rTwoOneCThreeOnlyAstro", c3only_fields$n[c3only_fields$Field == "Astronomy"]),
  mac("rTwoOneCThreeOnlyMinRank", min(c3only$pcaRank)),
  mac("rTwoOneTierOne", nrow(t1)), mac("rTwoOneTierOneNotCThree", nrow(t1_notC3)),
  mac("rTwoOneECinNinetyNine", nrow(ec_in99)),
  mac("rTwoOneECviaCThreeOnly", sum(ec_in99$Cs == 1 & ec_in99$C3 == 1)),
  mac("rTwoOneSpearmanPhD", fmt(summ$spearman[summ$denominator == "PhD"])),
  mac("rTwoOneSpearmanVS",  fmt(summ$spearman[summ$denominator == "VS"])),
  mac("rTwoOneMaxRankMovePhD", summ$max_rank_move[summ$denominator == "PhD"]),
  mac("rTwoOneMaxRankMoveVS",  summ$max_rank_move[summ$denominator == "VS"]),
  mac("rTwoOneNFields", summ$n_fields[1]),
  mac("rTwoOneWinnersMatched", nrow(w)), mac("rTwoOneWinnersCThreeOnly", sum(w$C3only)),
  mac("rTwoOneBottomFiveSame", ifelse(all(bottom$same_bottom5), "unchanged", "changed")),
  mac("rTwoOneRemaining", nrow(rest)),
  mac("rTwoOneTierChangesWithout", nrow(tier_changes)),
  mac("rTwoOneTierOneToTwoWithout", sum(tier_changes$Tier == 1 & tier_changes$Tier76 == 2)),
  mac("rTwoOneTierTwoToThreeWithout", sum(tier_changes$Tier == 2 & tier_changes$Tier76 == 3)),
  mac("rTwoOneTierUpWithout", sum(tier_changes$Tier76 < tier_changes$Tier)),
  mac("rTwoOneLossMicro", fmt(100 * loss$lost_share[loss$finest_group == "Microbiology & Immunology"], 0)),
  mac("rTwoOneLossGeo",   fmt(100 * loss$lost_share[loss$finest_group == "Geosciences"], 0)),
  mac("rTwoOneLossEcon",  fmt(100 * loss$lost_share[loss$finest_group == "Economics"], 0)),
  mac("rTwoOneLossNeuro", fmt(100 * loss$lost_share[loss$finest_group == "Neuroscience"], 0)),
  mac("rTwoOneLossCS",    fmt(100 * loss$lost_share[loss$finest_group == "Computer Science"], 0)),
  mac("rTwoOneFieldsNoLoss", sum(loss$yearlyRE_full > 0 & loss$lost_share == 0)),
  mac("rTwoOneFieldsWithEvents", sum(loss$yearlyRE_full > 0)),
  # profile of the 23 vs the 76
  mac("rTwoOneMedFirstCThree", g23$med_first), mac("rTwoOneMedFirstRest", g76$med_first),
  mac("rTwoOnePostEightyPctCThree", fmt(100 * g23$sh_post1980, 0)), mac("rTwoOnePostEightyPctRest", fmt(100 * g76$sh_post1980, 0)),
  mac("rTwoOneMedMoneyCThree", formatC(round(g23$med_money, -3), format = "d", big.mark = ",")),
  mac("rTwoOneMedMoneyRest",   formatC(round(g76$med_money, -3), format = "d", big.mark = ",")),
  mac("rTwoOneNoMoneyCThree", sum(coalesce(prof$moneyPerWinner[prof$C3only], 0) == 0)),
  mac("rTwoOneNoMoneyRest",   sum(coalesce(prof$moneyPerWinner[!prof$C3only], 0) == 0)),
  mac("rTwoOneMedViewsCThree", fmt(g23$med_views, 0)), mac("rTwoOneMedViewsRest", fmt(g76$med_views, 0)),
  mac("rTwoOneGovOriginCThree", g23$n_gov_origin), mac("rTwoOneGovOriginRest", g76$n_gov_origin),
  mac("rTwoOnePhilOriginPctCThree", fmt(100 * g23$sh_philanthropy_origin, 0)), mac("rTwoOnePhilOriginPctRest", fmt(100 * g76$sh_philanthropy_origin, 0)),
  mac("rTwoOneSocietyAwdCThree", g23$n_society_awd), mac("rTwoOneUniversityAwdCThree", g23$n_university_awd),
  mac("rTwoOnePhilanthropyAwdCThree", g23$n_philanthropy_awd), mac("rTwoOnePhilanthropyAwdRest", g76$n_philanthropy_awd),
  mac("rTwoOneUniversityAwdRest", g76$n_university_awd),
  mac("rTwoOneUSCThree", g23$n_us), mac("rTwoOneUSRest", g76$n_us),
  mac("rTwoOneSwedenCThree", g23$n_sweden), mac("rTwoOneSwedenRest", g76$n_sweden),
  mac("rTwoOneLifePhysCThree", g23$n_lifephys), mac("rTwoOneLifePhysRest", g76$n_lifephys),
  mac("rTwoOneMathCSEnvCThree", g23$n_math_cs_env), mac("rTwoOneMathCSEnvRest", g76$n_math_cs_env),
  mac("rTwoOneGiniFullVS", fmt(summ$gini_full[summ$denominator == "VS"])), mac("rTwoOneGiniNoCThreeVS", fmt(summ$gini_noC3[summ$denominator == "VS"])),
  mac("rTwoOneGiniFullPhD", fmt(summ$gini_full[summ$denominator == "PhD"])), mac("rTwoOneGiniNoCThreePhD", fmt(summ$gini_noC3[summ$denominator == "PhD"])),
  mac("rTwoOneTopThreeShareFull", fmt(100 * summ$top3_share_full[summ$denominator == "VS"], 0)),
  mac("rTwoOneTopThreeShareNoCThree", fmt(100 * summ$top3_share_noC3[summ$denominator == "VS"], 0)),
  mac("rTwoOneAgeCoveragePct", fmt(100 * age_cov, 0)),
  mac("rTwoOneMedAgeCThree", age_by$med_age[age_by$C3only]), mac("rTwoOneMedAgeRest", age_by$med_age[!age_by$C3only]),
  mac("rTwoOneMeanAgeCThree", fmt(age_by$mean_age[age_by$C3only], 1)), mac("rTwoOneMeanAgeRest", fmt(age_by$mean_age[!age_by$C3only], 1)),
  mac("rTwoOneUnderFortyFivePctCThree", fmt(100 * age_by$sh_under45[age_by$C3only], 0)),
  mac("rTwoOneUnderFortyFivePctRest", fmt(100 * age_by$sh_under45[!age_by$C3only], 0))
)
stopifnot(all(bottom$same_bottom5))   # the under-served fields are unchanged
write_tex(m, file.path(table_dir, "r2_1_macros.tex"))

# Overlap table: rows = criterion, cols = prizes meeting it, unique to it
tab <- c("% generated by code/17_criteria_overlap.R -- do not edit by hand",
         "\\begin{table}[H]", "\\centering \\small",
         paste0("\\caption{Inclusion criteria: prizes meeting each criterion and prizes meeting no other. ",
                "A prize enters the list if it meets at least one criterion; the citation criterion is ",
                "evaluated over prizes that appear on at least one of the seven authoritative lists or ",
                "were rated in either expert survey.}"),
         "\\label{tab:criteria_overlap}",
         "\\begin{tabular}{p{8.6cm}cc}", "\\toprule",
         "Inclusion criterion & Prizes meeting it & Meeting no other \\\\", "\\midrule",
         sprintf("1. Appears on at least three of seven authoritative lists & %d & %d \\\\", nC["C1"], only["C1"]),
         sprintf("2. Expert rating $\\geq 0.5$ of a Nobel, and on at least one list & %d & %d \\\\", nC["C2"], only["C2"]),
         sprintf("3. Over 50\\%% of recent winners on Clarivate's Highly Cited Researchers list & %d & %d \\\\", nC["C3"], only["C3"]),
         sprintf("4. At least 40 daily Wikipedia page views, 2020--2023 & %d & %d \\\\", nC["C4"], only["C4"]),
         "\\midrule",
         sprintf("At least one criterion (the 99 prizes) & %d & \\\\", nrow(prizeList)),
         sprintf("Two or more criteria & %d & \\\\", sum(prizeList$Cs >= 2)),
         sprintf("All four criteria & %d & \\\\", sum(prizeList$Cs == 4)),
         "\\bottomrule", "\\end{tabular}", "\\end{table}")
write_tex(tab, file.path(table_dir, "r2_1_criteria_table.tex"))
cat("\nWrote r2_1_macros.tex and r2_1_criteria_table.tex\n")
