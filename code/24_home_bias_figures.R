# ==============================================================================
# 24_home_bias_figures.R
# Generates, from the intermediate files of scripts 13 and 18:
#   r2_2_home_bias_scatter.pdf       Figure S11: each prize's home share against
#                                    the field's benchmark pool share
#   r2_2_home_share_by_country.pdf   Figure S10: home share vs benchmark share
#                                    by awarding country, recognition-weighted
#   r2_3_field_measures_scatter.pdf  Figure S7: recognition density against the
#                                    patent-cited share of the field's articles
# ==============================================================================
source("_helpers.R")

# ---- home share vs the field's researcher pool, by prize ----
# Points above the 45-degree line award locals more often than the pool predicts.
hb <- read.csv(file.path(intermediate_dir, "r2_2_prize_home_bias.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
hb <- hb[!is.na(hb$excess_pop), ]
p1 <- ggplot(hb, aes(benchmark_pop, home_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  geom_point(aes(size = n_winners), shape = 21, fill = "steelblue4",
             colour = "white", stroke = .3, alpha = .75) +
  geom_text_repel(data = subset(hb, excess_pop > .30 | excess_pop < -.15),
                  aes(label = Prize), size = 2.4, max.overlaps = Inf,
                  min.segment.length = 0, box.padding = .35, seed = 1,
                  segment.colour = "grey60", segment.size = .25) +
  # Area-proportional sizing, so a prize awarded twice in the decade reads as a
  # tenth of one awarded twenty times
  scale_size_area(max_size = 11, breaks = c(2, 5, 10, 20, 30),
                  name = "Recognitions\n2015-24") +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Benchmark: share of the field's 1,000 most-cited researchers (small-team citations) based in the awarding country",
       y = "Share of the prize's own laureates based in the awarding country") +
  theme_minimal(base_size = 11)
save_figure("r2_2_home_bias_scatter.pdf", p1, width = 8.5, height = 5.5)

# ---- home share vs benchmark share, by awarding country ----
# Recognition-weighted: the share of a country's recognitions going to a laureate
# based there, against the share expected from the field pools of its prizes.
cs <- read.csv(file.path(intermediate_dir, "r2_2_home_awards_by_country.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
p2b <- ggplot(cs, aes(expected_share, actual_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  geom_point(aes(size = n_awards), shape = 21, fill = "firebrick", colour = "white", stroke = .3, alpha = .75) +
  geom_text_repel(aes(label = AwardingCountry), size = 2.6, max.overlaps = Inf, min.segment.length = 0,
                  box.padding = .35, seed = 1, segment.colour = "grey60", segment.size = .25) +
  scale_size_area(max_size = 11, breaks = c(10, 50, 100, 400), name = "Recognitions\n2015-24") +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent) + scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  coord_fixed() +
  labs(x = "Expected share of recognitions to laureates based in the awarding country",
       y = "Actual share") +
  theme_minimal(base_size = 11)
save_figure("r2_2_home_share_by_country.pdf", p2b, width = 7, height = 6)

# ---- recognition density vs the patent-cited share ----
fm <- read.csv(file.path(intermediate_dir, "r2_3_field_measures.csv"), stringsAsFactors = FALSE)
social <- c("Economics", "Psychology", "Business", "Anthropology",
            "Political Science & Sociology", "Other Social Sci.")
stopifnot(all(social %in% fm$field))
labs4 <- c(
  ros_cited_share     = "Share of the field's cited 2000-15 articles\ncited by a US patent (Reliance on Science)")
long4 <- fm |>
  select(field, density, all_of(names(labs4))) |>
  tidyr::pivot_longer(all_of(names(labs4)), names_to = "measure", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(measure = factor(labs4[measure], levels = labs4))
p3 <- ggplot(long4, aes(value, density)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey60", linewidth = .6) +
  geom_point(aes(color = field %in% social), size = 2) +
  geom_text_repel(aes(label = field), size = 2.1, max.overlaps = 18,
                  segment.colour = "grey70", segment.size = .2) +
  scale_color_manual(values = c("FALSE" = "steelblue4", "TRUE" = "firebrick"),
                     guide = "none") +
  facet_wrap(~measure, scales = "free_x") +
  labs(x = NULL, y = "Yearly recognitions per 1,000 top researchers (2015-2024)") +
  theme_minimal(base_size = 10)
save_figure("r2_3_field_measures_scatter.pdf", p3, width = 5.5, height = 4.2)
cat("figures written\n")
