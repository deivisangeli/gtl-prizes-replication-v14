# ==============================================================================
# 04_money_weights.R
# Generates: Figure 6, S2 (moneyPerWinner_prizeLevel.png, moneyPerWinner_winnerLevel.png)
# ==============================================================================
source("_helpers.R")

prizeList <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`), `Award Name` != "Max Planck Research Award")

prizeList$moneyPerYear <- prizeList$moneyPerYear / 10^6
prizeList$moneyPerPrize <- prizeList$moneyPerPrize / 10^6
prizeList$moneyPerWinner <- prizeList$moneyPerWinner / 10^6

# Figure S1: Distribution at prize level
ggplot(prizeList, aes(x = moneyPerWinner)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "", x = "Money Per Winner (in millions)", y = "Prize count") +
  scale_x_continuous(breaks = seq(0, 20, by = 0.25)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_figure("moneyPerWinner_prizeLevel.png", width = 9, height = 6, dpi = 300)

# Figure S2: Distribution at winner level (weighted by Yearly Winners)
ggplot(prizeList, aes(x = moneyPerWinner, weight = `Yearly Winners`)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.5) +
  labs(x = "Money Per Winner (in millions)", y = "Winners") +
  scale_x_continuous(breaks = seq(0, 20, by = 0.25)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_figure("moneyPerWinner_winnerLevel.png", width = 9, height = 6, dpi = 300)
