# ==============================================================================
# 08_density_vs_academics.R
# Generates: Figure 4 (prizesDensityByVSacademics_finest.pdf)
# ==============================================================================
source("_helpers.R")

sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)

vs_raw <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE)
vs_raw <- vs_raw[vs_raw$academic_count > 0 & vs_raw$group_name != "Humanities", ]
total_vs <- sum(vs_raw$academic_count)

winners <- read.csv(file.path(data_dir, "all_winners_with_plotFinestField.csv"), stringsAsFactors = FALSE)

winners_with_group <- winners %>%
  inner_join(sf_map %>% select(subfield_name, finest_group) %>% distinct(),
             by = c("Best_Subfield" = "subfield_name"))

re_by_group <- winners_with_group %>%
  group_by(finest_group) %>%
  summarise(yearlyRE = n() / 10, .groups = "drop")

results <- vs_raw %>%
  select(field = group_name, vs_academics = academic_count) %>%
  full_join(re_by_group, by = c("field" = "finest_group")) %>%
  mutate(vs_academics = ifelse(is.na(vs_academics), 0, vs_academics),
         yearlyRE = ifelse(is.na(yearlyRE), 0, yearlyRE))

results <- results[results$vs_academics > 0, ]
results$fieldSize <- results$vs_academics / sum(results$vs_academics) * 100
results$density <- results$yearlyRE / results$vs_academics * 1000

# Prepare for make_density_plot
results$size <- results$vs_academics

p <- make_density_plot(results, "1,000 research academics", 1000, total_vs,
                       ylab = "Award density (yearly recognitions per 1,000 research academics)",
                       xlab = "Field size (% of research academics)",
                       bracket_y = c(-0.05, -0.10, -0.20), ylim_low = -0.3)

save_figure("prizesDensityByVSacademics_finest.pdf", p, width = 10, height = 5)
