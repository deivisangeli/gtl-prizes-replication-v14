# ==============================================================================
# 03_scatterplots.R
# Generates: Figures 2a-c and S1 (cum_prize_time.png, scatter_time_rating.png,
#            scatter_moneyprize_time_linear_fit.png, scatter_money_views.png)
# ==============================================================================
source("_helpers.R")

prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`), `Award Name` != "Max Planck Research Award")
prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$first_awarded <- 2025 - prizeList$age

prizeList <- prizeList %>%
  arrange(first_awarded) %>%
  mutate(cumMoneyYear = cumsum(moneyPerYear))

prizes_to_label <- c("Nobel Prize in Physics", "Nobel Prize in Chemistry",
                     "Copley Medal", "A. M. Turing Award",
                     "Max Planck-Humboldt Research Award",
                     "Queen Elizabeth Prize for Engineering",
                     "Breakthrough Prize in Fundamental Physics",
                     "Fields Medal")

prizes_to_label_view <- c("Nobel Prize in Physics", "Nobel Prize in Chemistry",
                          "Copley Medal", "A. M. Turing Award",
                          "Queen Elizabeth Prize for Engineering",
                          "Breakthrough Prize in Fundamental Physics",
                          "Fields Medal")

prizeList$ln10PageViews <- log(prizeList$`Daily Page Views` + 1, base = 10)
prizeList$ln10MoneyPerPeriod <- log(prizeList$moneyPerPeriod + 1, base = 10)
prizeList$ln10MoneyPerWinner <- log(prizeList$moneyPerWinner + 1, base = 10)
prizeList$ln10MoneyPerPrize <- log(prizeList$moneyPerPrize + 1, base = 10)

labels_data <- prizeList %>% filter(`Award Name` %in% prizes_to_label)
labels_without_zero_views <- prizeList %>% filter(`Award Name` %in% prizes_to_label_view)

################################################################################
# Figure 1a: Cumulative Prize money and number of prizes
################################################################################

base_prizes <- prizeList %>%
  filter(`Award Name` != "A. M. Turing Award") %>%
  select(`Award Name`, first_awarded, moneyPerYear)

# Increments to the cumulative purse: 250k in 2007, then the 750k that lifts it
# to the current 1M in 2013
turing_increments <- tibble(
  `Award Name`  = "A. M. Turing Award",
  first_awarded = c(1966, 2007, 2013),
  moneyPerYear  = c(0, 250000, 750000)
)

prize_adj <- bind_rows(base_prizes, turing_increments) %>%
  arrange(first_awarded) %>%
  mutate(
    cumNumPrizes = cumsum(!duplicated(`Award Name`)),
    cumMoneyYear = cumsum(moneyPerYear)
  )

value_1925 <- prize_adj %>% filter(first_awarded >= 1925) %>% slice(1)
prizes_1925 <- value_1925$cumNumPrizes
money_1925 <- value_1925$cumMoneyYear

prize_adj <- prize_adj %>%
  mutate(
    transformed_prizes = (cumNumPrizes / prizes_1925) * 100,
    transformed_money  = (cumMoneyYear / money_1925) * 100
  )

max_y <- max(max(prize_adj$transformed_prizes), max(prize_adj$transformed_money))

plot2 <- ggplot(prize_adj, aes(x = first_awarded)) +
  geom_line(aes(y = transformed_prizes), color = "#1A237E", size = 0.8) +
  geom_line(aes(y = transformed_money), color = "#8B0000", size = 0.8) +
  scale_y_continuous(
    name = "",
    limits = c(0, max_y),
    breaks = seq(0, ceiling(max_y / 100) * 100, by = 300),
    labels = function(y) round(y * prizes_1925 / 100, 0),
    sec.axis = sec_axis(~ ., name = "",
      breaks = seq(0, ceiling(max_y / 100) * 100, by = 300),
      labels = function(y) {
        scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)(y * money_1925 / 100)
      })
  ) +
  scale_x_continuous(limits = c(1925, 2025),
                     breaks = c(seq(1925, 2000, by = 20), 2025), expand = c(0, 0)) +
  labs(x = "Year First Awarded") +
  theme_minimal() +
  theme(
    axis.title.y       = element_text(color = "#1A237E"),
    axis.text.y        = element_text(color = "#1A237E"),
    axis.title.y.right = element_text(color = "#8B0000"),
    axis.text.y.right  = element_text(color = "#8B0000"),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position    = "none"
  ) +
  annotate("text", x = 1940, y = 65, label = "Cumulative Number of Prizes",
           hjust = 0, vjust = 0.5, size = 3, color = "#1A237E") +
  annotate("text", x = 1940, y = 175, label = "Cumulative Prize Money per Year",
           angle = 10, hjust = 0, vjust = 0.5, size = 3, color = "#8B0000")

save_figure("cum_prize_time.png", plot2, width = 6, height = 3.5, dpi = 1080)

################################################################################
# Figure 2b: Prize Age vs Survey Rating
################################################################################

rated <- prizeList %>% filter(!is.na(Rating))

p_time_rating <- ggplot(rated, aes(x = first_awarded, y = Rating)) +
  geom_point(shape = 21, size = 2, fill = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#CBC3E3", size = 0.5, alpha = 0.2) +
  geom_label_repel(
    data = rated %>% filter(`Award Name` %in% prizes_to_label_view,
                            `Award Name` != "Breakthrough Prize in Fundamental Physics"),
    aes(label = `Award Name`),
    box.padding = 0.2, point.padding = 0.3, min.segment.length = 0,
    force = 15, force_pull = 0, max.overlaps = Inf, direction = "both",
    segment.color = "grey50", segment.size = 0.5, segment.alpha = 0.6,
    position = position_dodge(width = 0.9),
    fill = scales::alpha("white", 0.7), label.size = NA, size = 2.5
  ) +
  # Breakthrough Prize placed manually: label left of dot with explicit connector
  annotate("segment", x = 1983, xend = 2011, y = 0.62, yend = 0.555,
           color = "grey50", size = 0.5, alpha = 0.6) +
  geom_text(
    data = data.frame(x = 1983, y = 0.62, label = "Breakthrough Prize in Fundamental Physics"),
    aes(x = x, y = y, label = label),
    size = 2.5, hjust = 1, inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.05) +
  scale_x_continuous(
    limits = c(min(prizeList$first_awarded), max(prizeList$first_awarded)),
    breaks = c(1731, min(prizeList$first_awarded), seq(1800, 2000, by = 100))
  ) +
  scale_y_continuous(name = "Survey Rating (Relative to Nobel)") +
  labs(title = "", x = "Year First Awarded") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1)
  ) +
  annotate("text", x = 1800, y = 0.3, label = "Linear Fit",
           hjust = 0, vjust = 0.5, size = 2.5, color = "#CBC3E3", angle = 0)

save_figure("scatter_time_rating.png", p_time_rating, width = 6, height = 3.5, dpi = 1080)

################################################################################
# Figure 1c: Prize money per winner over time
################################################################################

plot_linear <- ggplot(prizeList, aes(x = first_awarded)) +
  geom_point(aes(y = moneyPerPrize), shape = 21, size = 2, fill = "blue", alpha = 0.3) +
  geom_smooth(aes(y = moneyPerPrize), method = "lm", se = FALSE, color = "#CBC3E3", size = 1, alpha = 0.8) +
  geom_label_repel(
    data = labels_data,
    aes(y = moneyPerPrize, label = `Award Name`),
    box.padding = 1, point.padding = 0.2, min.segment.length = 0,
    force = 20, force_pull = 0.5, max.overlaps = Inf, direction = "both",
    segment.color = "grey50", segment.size = 0.5, segment.alpha = 0.8,
    position = position_dodge(width = 1),
    fill = scales::alpha("white", 0.7), label.size = NA, size = 2.5
  ) +
  scale_y_continuous(name = "Money per Prize (USD)",
                     labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(
    limits = c(min(prizeList$first_awarded), max(prizeList$first_awarded)),
    breaks = c(min(prizeList$first_awarded), seq(1800, 1950, by = 50), 2019)
  ) +
  labs(x = "Year First Awarded") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1)
  ) +
  annotate("text", x = 1830, y = 200000, label = "Linear Fit",
           hjust = 0, vjust = 0.5, size = 2.5, color = "#CBC3E3", angle = 0)

save_figure("scatter_moneyprize_time_linear_fit.png", plot_linear, width = 6, height = 3.5, dpi = 1080)

################################################################################
# Figure 1d: Money vs Online Visibility
################################################################################

p_money_views <- ggplot(prizeList %>% filter(ln10PageViews > 0),
                        aes(x = moneyPerPrize, y = ln10PageViews)) +
  geom_point(shape = 21, size = 2, fill = "blue", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "#CBC3E3", size = 1, alpha = 0.8) +
  geom_label_repel(
    data = labels_data %>% filter(ln10PageViews > 0),
    aes(label = `Award Name`),
    box.padding = 1, point.padding = 0.2, min.segment.length = 0,
    force = 20, force_pull = 0.5, max.overlaps = Inf, direction = "both",
    segment.color = "grey50", segment.size = 0.5, segment.alpha = 0.8,
    position = position_dodge(width = 1),
    fill = scales::alpha("white", 0.7), label.size = NA, size = 2.5
  ) +
  scale_x_continuous(name = "Money per Prize (USD)",
                     labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(name = "Daily Page Views (Log Scale)",
                     limits = c(0, 3.5),
                     breaks = c(0, 1, 2, 3), labels = c("0", "10", "100", "1000")) +
  labs(title = "") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "black", size = 0.1)
  ) +
  annotate("text", x = 2000000, y = 1.9, label = "Linear Fit",
           hjust = 0, vjust = 0.5, size = 2.5, color = "#CBC3E3", angle = 5)

save_figure("scatter_money_views.png", p_money_views, width = 6, height = 3.5, dpi = 1080)
