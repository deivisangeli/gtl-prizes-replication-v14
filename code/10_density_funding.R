# ==============================================================================
# 10_density_funding.R
# Produces: figures/prizesDensityFunding.pdf (award density by funding)
# Inputs: data/finest_group_to_funding_field.csv, data/2022budgetByField.xlsx,
#         and through winners_by_group() data/all_winners_with_plotFinestField.csv,
#         data/subfield_to_finest_group.csv
#
# Recognition density with US federal research funding as the field-size
# denominator. Numerator: 2015-2024 recognitions, assigned to the paper's 26 field
# groups through the winner's OpenAlex subfield and aggregated to the eight
# categories of the NSF federal-funding table (data/finest_group_to_funding_field.csv).
# Denominator: FY2022 federal research obligations by field
# (data/2022budgetByField.xlsx), Humanities and "Other non-science" excluded.
# ==============================================================================
source("_helpers.R")

################################################################################
# Recognitions per funding field
################################################################################

sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)
fund_map <- read.csv(file.path(data_dir, "finest_group_to_funding_field.csv"), stringsAsFactors = FALSE)
stopifnot(!anyDuplicated(fund_map$finest_group),
          setequal(fund_map$finest_group, setdiff(unique(sf_map$finest_group), "Humanities")))

# The figure's 98-prize sample excludes the discontinued Max Planck Research Award
winners_with_group <- winners_by_group() %>%
  filter(Prize != "Max Planck Research Award") %>%
  filter(finest_group != "Humanities") %>%
  inner_join(fund_map, by = "finest_group")

re_by_funding <- winners_with_group %>%
  group_by(funding_field) %>%
  summarise(recognitions = n(), yearlyWinners = n() / 10, .groups = "drop")  # 2015-2024

################################################################################
# Federal research funding by field
################################################################################

federalRnD <- read_excel(file.path(data_dir, "2022budgetByField.xlsx")) %>%
  filter(!is.na(plotFundingField), plotFundingField != "NA",
         !(plotFundingField %in% c("Humanities", "Other non-science"))) %>%
  group_by(plotFundingField) %>%
  summarise(researchBudget2022 = sum(researchBudget2022, na.rm = TRUE), .groups = "drop")
stopifnot(setequal(federalRnD$plotFundingField, unique(fund_map$funding_field)))

fundingFieldStats <- federalRnD %>%
  left_join(re_by_funding, by = c("plotFundingField" = "funding_field")) %>%
  mutate(recognitions = coalesce(recognitions, 0L),
         yearlyWinners = coalesce(yearlyWinners, 0),
         fieldSize = 100 * researchBudget2022 / sum(researchBudget2022),
         winnersPerBillionUSD = yearlyWinners / (researchBudget2022 / 1e6)) %>%  # budget in thousands USD
  arrange(desc(winnersPerBillionUSD)) %>%
  as.data.frame()
stopifnot(sum(fundingFieldStats$recognitions) == nrow(winners_with_group))

################################################################################
# Figure
################################################################################

fundingFieldStats$AccFieldSize <- cumsum(fundingFieldStats$fieldSize)
fundingFieldStats$lagAccFieldSize <- dplyr::lag(fundingFieldStats$AccFieldSize)
fundingFieldStats$lagAccFieldSize[1] <- 0
fundingFieldStats$plotFundingField[fundingFieldStats$plotFundingField == "Life Sciences & Medicine"] <-
  "Life & Health Sciences"

# Step-plot ribbon: one horizontal segment per field over its share of the funding
create_ribbon_data_funding <- function(data, var) {
  result <- data.frame()
  for (i in 1:(nrow(data) - 1)) {
    result <- rbind(result, data.frame(
      x = c(data$lagAccFieldSize[i], data$lagAccFieldSize[i + 1]),
      y = c(data[i, var], data[i, var]),
      plotFundingField = data$plotFundingField[i]
    ))
  }
  last_row <- nrow(data)
  result <- rbind(result, data.frame(
    x = c(data$lagAccFieldSize[last_row], data$AccFieldSize[last_row]),
    y = data[last_row, var],
    plotFundingField = data$plotFundingField[last_row]
  ))
  return(result)
}

funding_ribbon_data <- create_ribbon_data_funding(fundingFieldStats, "winnersPerBillionUSD")

# Fill and outline colors by broad field
custom_colors_funding <- c(
  "Ag. & natural resources" = "#8B4577",
  "Business & Econ"         = "#E7298A",
  "CS & Engineering"        = "#8B4577",
  "Life & Health Sciences"  = "#00A5A5",
  "Math"                    = "#1A237E",
  "Other Social Sciences"   = "#E7298A",
  "Physical Sciences"       = "#1A237E",
  "Psychology"              = "#E0E0E2"
)

maxHeight <- round(max(fundingFieldStats$winnersPerBillionUSD), 0)
yMax <- max(fundingFieldStats$winnersPerBillionUSD) * 1.08
avgDensity <- weighted.mean(fundingFieldStats$winnersPerBillionUSD, fundingFieldStats$fieldSize)
tallThreshold <- avgDensity * 2
labelBuffer <- yMax * 0.04

fundingFieldStats$labelInside <- fundingFieldStats$winnersPerBillionUSD > tallThreshold
fundingFieldStats$manualOffset <- ifelse(fundingFieldStats$labelInside, -labelBuffer, labelBuffer)
# Field labels: fields under 3% of funding get a horizontal label from the bar's
# right edge; fields at 15% or more get a horizontal label inside the bar (above
# it when the bar is low); the rest get a vertical label.
fundingFieldStats$labelStyle <- with(fundingFieldStats, case_when(
  fieldSize < 3 ~ "narrow",
  fieldSize >= 15 & winnersPerBillionUSD > 1.2 ~ "wide-inside",
  fieldSize >= 15 ~ "wide-above",
  TRUE ~ "vertical"))

legStep <- yMax * 0.05
legBase <- yMax * 0.70

p <- ggplot() +
  geom_ribbon(data = funding_ribbon_data,
              aes(x = x, ymin = 0, ymax = y, fill = plotFundingField),
              alpha = alpha) +
  geom_line(data = funding_ribbon_data,
            aes(x = x, y = y, color = plotFundingField), linewidth = 1) +
  geom_segment(data = fundingFieldStats,
               aes(x = AccFieldSize, xend = AccFieldSize,
                   y = 0, yend = winnersPerBillionUSD),
               linetype = "dashed", color = "gray80", linewidth = 0.3) +
  geom_text(data = fundingFieldStats[fundingFieldStats$labelStyle == "vertical", ],
    aes(x = (lagAccFieldSize + AccFieldSize) / 2,
        y = winnersPerBillionUSD + manualOffset,
        label = plotFundingField, hjust = ifelse(labelInside, 1, 0)),
    size = 3.5, angle = 90, color = "black") +
  geom_text(data = fundingFieldStats[fundingFieldStats$labelStyle == "narrow", ],
    aes(x = AccFieldSize + 0.6,
        y = winnersPerBillionUSD + labelBuffer * 0.6,
        label = plotFundingField),
    size = 3.5, hjust = 0, vjust = 0, color = "black") +
  geom_text(data = fundingFieldStats[fundingFieldStats$labelStyle == "wide-inside", ],
    aes(x = (lagAccFieldSize + AccFieldSize) / 2,
        y = winnersPerBillionUSD / 2,
        label = plotFundingField),
    size = 3.5, color = "black") +
  geom_text(data = fundingFieldStats[fundingFieldStats$labelStyle == "wide-above", ],
    aes(x = (lagAccFieldSize + AccFieldSize) / 2,
        y = winnersPerBillionUSD + labelBuffer * 0.6,
        label = plotFundingField),
    size = 3.5, vjust = 0, color = "black") +
  geom_segment(data = fundingFieldStats,
               aes(x = lagAccFieldSize, xend = lagAccFieldSize,
                   y = -0.2, yend = -0.3), color = "black", linewidth = 0.1) +
  geom_segment(data = fundingFieldStats,
               aes(x = AccFieldSize, xend = AccFieldSize,
                   y = -0.2, yend = -0.3), color = "black", linewidth = 0.1) +
  geom_segment(data = fundingFieldStats,
               aes(x = lagAccFieldSize, xend = AccFieldSize,
                   y = -0.3, yend = -0.3), color = "black", linewidth = 0.3) +
  geom_text(data = fundingFieldStats,
            aes(x = (lagAccFieldSize + AccFieldSize) / 2,
                y = -0.13 * maxHeight, angle = 90,
                label = paste0(round(fieldSize, 0), "%")),
            size = 3.3) +
  theme_minimal() +
  ylab("Award density (yearly recognitions/billion USD R&D)") +
  xlab("Field size (% of federal research budget)") +
  scale_x_continuous(breaks = c(0, 100), labels = c("0%", "100%")) +
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5, 10, 12.5)) +
  coord_cartesian(ylim = c(-yMax * 0.14, yMax)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10, margin = margin(t = -10)),
        axis.text.y = element_text(face = "bold", size = 9, margin = margin(t = -10)),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors_funding, aesthetics = c("fill", "color")) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase + legStep * 3, ymax = legBase + legStep * 4,
    fill = "#1A237E", alpha = alpha) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase + legStep * 2, ymax = legBase + legStep * 3,
    fill = "#8B4577", alpha = alpha) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase + legStep * 1, ymax = legBase + legStep * 2,
    fill = "#00A5A5", alpha = alpha) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase, ymax = legBase + legStep * 1,
    fill = "#E7298A", alpha = alpha) +
  annotate("segment", x = 85,
    y = legBase + legStep * 3, yend = legBase + legStep * 4,
    color = "#1A237E", linewidth = 1) +
  annotate("segment", x = 85,
    y = legBase + legStep * 2, yend = legBase + legStep * 3,
    color = "#8B4577", linewidth = 1) +
  annotate("segment", x = 85,
    y = legBase + legStep * 1, yend = legBase + legStep * 2,
    color = "#00A5A5", linewidth = 1) +
  annotate("segment", x = 85,
    y = legBase, yend = legBase + legStep * 1,
    color = "#E7298A", linewidth = 1) +
  annotate("text", x = 86, y = legBase + legStep * 3.5,
           label = "Math & Phys. sci", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 2.5,
           label = "Applied sci", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 1.5,
           label = "Life & Health", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 0.5,
           label = "Social sci", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 4.3,
           label = "Broad fields", hjust = 0, size = 3.3, fontface = "bold") +
  geom_hline(yintercept = avgDensity, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  annotate("text", x = 97, y = avgDensity, label = "Avg.",
           hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  theme(legend.position = "none")

save_figure("prizesDensityFunding.pdf", p, width = 9, height = 4.7)
