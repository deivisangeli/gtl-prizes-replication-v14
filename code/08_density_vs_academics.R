# ==============================================================================
# 08_density_vs_academics.R
# Generates: prizesDensityByVSacademics_finest.pdf (award density by field, research-academics denominator)
# Inputs: data/vs_academics_by_finest_group.csv; via winners_by_group(): data/all_winners_with_plotFinestField.csv, data/subfield_to_finest_group.csv
# ==============================================================================
source("_helpers.R")

vs_raw <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE)
vs_raw <- vs_raw[vs_raw$academic_count > 0 & vs_raw$group_name != "Humanities", ]
total_vs <- sum(vs_raw$academic_count)

# Yearly recognitions per field group: recognitions over 2015-2024 divided by 10
re_by_group <- winners_by_group() %>%
  group_by(finest_group) %>%
  summarise(yearlyRE = n() / 10, .groups = "drop")

results <- vs_raw %>%
  select(field = group_name, size = academic_count) %>%
  left_join(re_by_group, by = c("field" = "finest_group")) %>%
  mutate(yearlyRE = coalesce(yearlyRE, 0),
         density = yearlyRE / size * 1000)

p <- make_density_plot(results, "1,000 research academics", 1000, total_vs,
                       ylab = "Award density (yearly recognitions per 1,000 research academics)",
                       xlab = "Field size (% of research academics)",
                       bracket_y = c(-0.05, -0.10, -0.20), ylim_low = -0.3)

save_figure("prizesDensityByVSacademics_finest.pdf", p, width = 10, height = 5)
