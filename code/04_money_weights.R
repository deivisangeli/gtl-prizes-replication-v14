# ==============================================================================
# 04_money_weights.R
# Generates: moneyPerWinner_prizeLevel.png, moneyPerWinner_winnerLevel.png (prize money per winner)
# Inputs: data/cleanPrizeList.xlsx
# ==============================================================================
source("_helpers.R")

prizeList <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`), `Award Name` != "Max Planck Research Award")

prizeList$moneyPerWinner <- prizeList$moneyPerWinner / 10^6

# Money per winner (USD million), one count per prize (98 prizes)
ggplot(prizeList, aes(x = moneyPerWinner)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "", x = "Money per Winner (USD millions)", y = "Prizes") +
  scale_x_continuous(breaks = seq(0, 20, by = 0.5), labels = function(x) format(x, drop0trailing = TRUE)) +
  theme_minimal(base_size = 20, base_line_size = 0.5, base_rect_size = 0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

save_figure("moneyPerWinner_prizeLevel.png", width = 9, height = 6, dpi = 300)

# Money per winner (USD million), each prize weighted by its yearly number of winners
ggplot(prizeList, aes(x = moneyPerWinner, weight = `Yearly Winners`)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.5) +
  labs(x = "Money per Winner (USD millions)", y = "Winners") +
  scale_x_continuous(breaks = seq(0, 20, by = 0.5), labels = function(x) format(x, drop0trailing = TRUE)) +
  theme_minimal(base_size = 20, base_line_size = 0.5, base_rect_size = 0.5) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

save_figure("moneyPerWinner_winnerLevel.png", width = 9, height = 6, dpi = 300)
