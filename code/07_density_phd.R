# ==============================================================================
# 07_density_phd.R
# Generates: Figure 5 (prizesDensityByPhD_finest.pdf)
# ==============================================================================
source("_helpers.R")

sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)
nsf_map <- read.csv(file.path(data_dir, "nsf_field_to_finest_group.csv"), stringsAsFactors = FALSE)

nsf_raw <- readxl::read_excel(file.path(data_dir, "nsf2023.xlsx"))
nsf_raw <- nsf_raw %>%
  filter(!(plotFinestField %in% c("Humanities", "Education", "Other non-science")))

nsf_with_group <- nsf_raw %>%
  inner_join(nsf_map, by = c("Field of doctorate" = "nsf_field"))

phd_by_group <- nsf_with_group %>%
  group_by(finest_group) %>%
  summarise(phds = sum(DoctoratesIn2023), .groups = "drop")

total_phds <- sum(phd_by_group$phds)

winners <- read.csv(file.path(data_dir, "all_winners_with_plotFinestField.csv"), stringsAsFactors = FALSE)

winners_with_group <- winners %>%
  inner_join(sf_map %>% select(subfield_name, finest_group) %>% distinct(),
             by = c("Best_Subfield" = "subfield_name"))

re_by_group <- winners_with_group %>%
  group_by(finest_group) %>%
  summarise(yearlyRE = n() / 10, .groups = "drop")

results <- phd_by_group %>%
  full_join(re_by_group, by = "finest_group") %>%
  rename(field = finest_group) %>%
  mutate(phds = ifelse(is.na(phds), 0, phds),
         yearlyRE = ifelse(is.na(yearlyRE), 0, yearlyRE))

results <- results[results$phds > 0, ]
results$fieldSize <- results$phds / sum(results$phds) * 100
results$density <- results$yearlyRE / results$phds * 1000

# Prepare for make_density_plot
results$size <- results$phds

p <- make_density_plot(results, "1,000 new PhDs", 1000, total_phds,
                       ylab = "Award density (yearly recognitions/1000 PhDs)",
                       xlab = "Field size (% of PhDs in field)",
                       bracket_y = c(-0.15, -0.25, -0.5), ylim_low = -0.7)

save_figure("prizesDensityByPhD_finest.pdf", p, width = 10, height = 5)
