# ==============================================================================
# 23_home_bias_figures.R
# Produces three figures from the intermediate files of scripts 12 and 17:
#   r2_2_home_bias_scatter.pdf       each prize's home share against the field's
#                                    benchmark pool share
#   r2_2_home_share_by_country.pdf   home share vs benchmark share by awarding
#                                    country, recognition-weighted
#   r2_3_field_measures_scatter.pdf  recognition density against the patent-cited
#                                    share of the field's articles
# Inputs: output/intermediate/r2_2_prize_home_bias.csv,
#         r2_2_home_awards_by_country.csv, r2_3_field_measures.csv
# ==============================================================================
source("_helpers.R")

# ---- home share vs the field's researcher pool, by prize ----
# Points above the 45-degree line award locals more often than the pool predicts.
hb <- read.csv(file.path(intermediate_dir, "r2_2_prize_home_bias.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
hb <- hb[!is.na(hb$excess_pop), ]
lab <- subset(hb, excess_pop > .30 | excess_pop < -.15)
p1 <- ggplot(hb, aes(benchmark_pop, home_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50") +
  geom_point(aes(size = n_winners), shape = 21, fill = "steelblue4",
             colour = "white", stroke = .3, alpha = .75) +
  geom_text_repel(data = lab, aes(label = Prize), size = 2.4, max.overlaps = Inf,
                  min.segment.length = 0, box.padding = .6, force = 3, seed = 1,
                  nudge_y = ifelse(lab$home_share == 1, .06, 0),
                  segment.colour = "grey60", segment.size = .25) +
  # Area-proportional sizing, so a prize awarded twice in the decade reads as a
  # tenth of one awarded twenty times
  scale_size_area(max_size = 11, breaks = c(2, 5, 10, 20, 30),
                  name = "Recognitions\n2015-24") +
  scale_x_continuous(limits = c(0, 1.05), breaks = seq(0, 1, .25)) +
  scale_y_continuous(limits = c(0, 1.12), breaks = seq(0, 1, .25)) +
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
# Social-science fields are filled red; the x axis is the share in percent.
fm$pct <- 100 * fm$ros_cited_share
fm$social <- fm$field %in% social
p3 <- ggplot(fm, aes(pct, density)) +
  geom_smooth(method = "lm", se = FALSE, color = "#CBC3E3", linewidth = 0.5) +
  geom_point(aes(fill = social), shape = 21, size = 2, alpha = 0.6) +
  geom_label_repel(aes(label = field), box.padding = 0.2, point.padding = 0.3,
                   min.segment.length = 0, force = 15, max.overlaps = Inf,
                   segment.color = "grey50", segment.size = 0.4, segment.alpha = 0.6,
                   fill = alpha("white", 0.7), label.size = NA, size = 2.3, seed = 1,
                   max.time = 5, max.iter = 1e5) +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(.06, .1))) +
  scale_y_continuous(expand = expansion(mult = c(.08, .1))) +
  labs(x = "Share of Cited 2000-15 Articles Cited by a US Patent",
       y = "Recognitions per 1,000 Top Researchers") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", linewidth = 0.1))
save_figure("r2_3_field_measures_scatter.pdf", p3, width = 6, height = 3.8)
