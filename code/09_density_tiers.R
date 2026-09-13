# ==============================================================================
# 09_density_tiers.R
# Generates: Figures S3-S4 (prizesDensityByVS_finest_T1.pdf,
#            prizesDensityByVS_finest_T12.pdf)
# ==============================================================================
source("_helpers.R")

sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)

# Load prize list and compute tiers
prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`))

prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)
prizeList$Tier <- ifelse(prizeList$cumWinners <= 0.1 * totalWinners, 1,
                  ifelse(prizeList$cumWinners <= 0.3 * totalWinners, 2, 3))

prize_tier <- prizeList %>% select(`Award Name`, Tier)

# Load winners and match to tiers
winners <- read.csv(file.path(data_dir, "all_winners_with_plotFinestField.csv"), stringsAsFactors = FALSE)

normalize_name <- function(x) {
  x <- gsub("\u2013", "-", x)
  x <- gsub("\u2014", "-", x)
  x <- gsub("\u00f6", "o", x)
  trimws(x)
}

winners$prize_norm <- normalize_name(winners$Prize)
prize_tier$prize_norm <- normalize_name(prize_tier$`Award Name`)

prize_tier$prize_norm[prize_tier$prize_norm == "Balzan Prizes"] <- "Balzan Prize"
prize_tier$prize_norm[prize_tier$prize_norm == "Gold Medal for Astronomy"] <-
  "Gold Medal of the Royal Astronomical Society"
prize_tier$prize_norm[prize_tier$prize_norm == "Crafoord prize in Polyarthritis"] <-
  "Crafoord Prize in Polyarthritis"

winners <- winners %>%
  inner_join(prize_tier %>% select(prize_norm, Tier), by = "prize_norm")

winners_with_group <- winners %>%
  inner_join(sf_map %>% select(subfield_name, finest_group) %>% distinct(),
             by = c("Best_Subfield" = "subfield_name"))

# Load field-size denominator (VS academics)
vs_raw <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE)
vs_raw <- vs_raw[vs_raw$academic_count > 0 & vs_raw$group_name != "Humanities", ]
vs_by_group <- vs_raw %>%
  select(finest_group = group_name, size = academic_count)
total_vs <- sum(vs_by_group$size)

# Generate tier figures
tier_configs <- list(
  list(max_tier = 1, suffix = "T1",  label = "Tier 1 Only"),
  list(max_tier = 2, suffix = "T12", label = "Tier 1+2")
)

for (tc in tier_configs) {
  w_tier <- winners_with_group %>% filter(Tier <= tc$max_tier)
  re_tier <- w_tier %>%
    group_by(finest_group) %>%
    summarise(yearlyRE = n() / 10, .groups = "drop")

  results <- vs_by_group %>%
    rename(field = finest_group) %>%
    full_join(re_tier, by = c("field" = "finest_group")) %>%
    mutate(size = ifelse(is.na(size), 0, size),
           yearlyRE = ifelse(is.na(yearlyRE), 0, yearlyRE))
  results <- results[results$size > 0, ]
  results$density <- results$yearlyRE / results$size * 1000

  p <- make_density_plot(results, "1,000 research academics", 1000, total_vs,
                         ylab = "Award density (yearly recognitions/1,000 research academics)",
                         xlab = "Field size (% of research academics)")

  fname <- sprintf("prizesDensityByVS_finest_%s.pdf", tc$suffix)
  save_figure(fname, p, width = 10, height = 5)
}
