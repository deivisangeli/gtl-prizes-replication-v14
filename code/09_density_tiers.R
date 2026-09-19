# ==============================================================================
# 09_density_tiers.R
# Generates: prizesDensityByVS_finest_T1.pdf and prizesDensityByVS_finest_T12.pdf
#            (award density by field, Tier 1 and Tier 1-2 prizes, research-academics denominator)
# Inputs: data/cleanPrizeList.xlsx, data/vs_academics_by_finest_group.csv; via winners_by_group():
#         data/all_winners_with_plotFinestField.csv, data/subfield_to_finest_group.csv
# ==============================================================================
source("_helpers.R")

# Prize tiers: Tier 1 = prizes making up the top 10% of yearly recognitions by
# prestige rank, Tier 2 = the next 20%, Tier 3 = the rest
prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx"))

prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)
prizeList$Tier <- ifelse(prizeList$cumWinners <= 0.1 * totalWinners, 1,
                  ifelse(prizeList$cumWinners <= 0.3 * totalWinners, 2, 3))

prize_tier <- prizeList %>% select(`Award Name`, Tier)

# Winners by field group, matched to their prize's tier
winners_with_group <- winners_by_group()

normalize_name <- function(x) {
  x <- gsub("\u2013", "-", x)
  x <- gsub("\u00f6", "o", x)
  trimws(x)
}

winners_with_group$prize_norm <- normalize_name(winners_with_group$Prize)
prize_tier$prize_norm <- normalize_name(prize_tier$`Award Name`)

prize_tier$prize_norm[prize_tier$prize_norm == "Balzan Prizes"] <- "Balzan Prize"
prize_tier$prize_norm[prize_tier$prize_norm == "Gold Medal for Astronomy"] <-
  "Gold Medal of the Royal Astronomical Society"
prize_tier$prize_norm[prize_tier$prize_norm == "Crafoord prize in Polyarthritis"] <-
  "Crafoord Prize in Polyarthritis"

n_winners <- nrow(winners_with_group)
winners_with_group <- winners_with_group %>%
  inner_join(prize_tier %>% select(prize_norm, Tier), by = "prize_norm")
stopifnot(nrow(winners_with_group) == n_winners)  # every winner's prize has a tier

# Field size: research academics at the top 150 US universities
vs_raw <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE)
vs_raw <- vs_raw[vs_raw$group_name != "Humanities", ]
vs_by_group <- vs_raw %>%
  select(finest_group = group_name, size = academic_count)
total_vs <- sum(vs_by_group$size)

# One density figure per tier cut-off
tier_configs <- list(
  list(max_tier = 1, suffix = "T1"),
  list(max_tier = 2, suffix = "T12")
)

for (tc in tier_configs) {
  re_tier <- winners_with_group %>%
    filter(Tier <= tc$max_tier) %>%
    group_by(finest_group) %>%
    summarise(yearlyRE = n() / 10, .groups = "drop")

  results <- vs_by_group %>%
    rename(field = finest_group) %>%
    left_join(re_tier, by = c("field" = "finest_group")) %>%
    mutate(yearlyRE = coalesce(yearlyRE, 0),
           density = yearlyRE / size * 1000)

  p <- make_density_plot(results, "1,000 research academics", 1000, total_vs,
                         ylab = "Award density (yearly recognitions/1,000 research academics)",
                         xlab = "Field size (% of research academics)")

  fname <- sprintf("prizesDensityByVS_finest_%s.pdf", tc$suffix)
  save_figure(fname, p, width = 10, height = 5)
}
