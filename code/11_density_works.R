# ==============================================================================
# 11_density_works.R
# Generates: figures/prizesDensityByWorks_finest.pdf (award density by field
#   group, denominator = OpenAlex works with >= 5 citations, 2018-2020 window)
# Inputs: data/subfield_to_finest_group.csv, data/works_by_subfield.csv,
#   data/all_winners_with_plotFinestField.csv
# ==============================================================================
source("_helpers.R")

sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)

works_raw <- read.csv(file.path(data_dir, "works_by_subfield.csv"), stringsAsFactors = FALSE)
# works_by_subfield uses full URLs ("https://openalex.org/subfields/NNNN");
# sf_map uses short IDs ("subfields/NNNN") -- strip base URL before joining
works_raw$subfield_short <- sub("https://openalex.org/", "", works_raw$subfield_id)
stopifnot(all(works_raw$min_citations == "cit5"), all(works_raw$window == "2018_2020"))

works_by_group <- works_raw %>%
  left_join(sf_map %>% select(subfield_id, finest_group) %>% distinct(),
            by = c("subfield_short" = "subfield_id")) %>%
  filter(!is.na(finest_group), finest_group != "Humanities") %>%
  group_by(finest_group) %>%
  summarise(size = sum(work_count, na.rm = TRUE), .groups = "drop")

total_works <- sum(works_by_group$size)

# yearly recognitions per field group (2015-2024 recognition events / 10)
re_by_group <- winners_by_group() %>%
  group_by(finest_group) %>%
  summarise(yearlyRE = n() / 10, .groups = "drop")

results <- works_by_group %>%
  full_join(re_by_group, by = "finest_group") %>%
  mutate(
    size     = ifelse(is.na(size),     0, size),
    yearlyRE = ifelse(is.na(yearlyRE), 0, yearlyRE)
  ) %>%
  filter(size > 0) %>%
  rename(field = finest_group)

results$density <- results$yearlyRE / results$size * 10000  # per 10k papers

p <- make_density_plot(results, "10,000 well-cited papers", 10000, total_works,
                       ylab = "Award density (yearly recognitions per 10,000 well-cited papers)",
                       xlab = "Field size (% of well-cited papers)",
                       bracket_y = c(-0.05, -0.10, -0.20), ylim_low = -0.3)

save_figure("prizesDensityByWorks_finest.pdf", p, width = 10, height = 5)
