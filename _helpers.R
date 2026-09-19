# ==============================================================================
# Shared configuration for "The Missing Nobels" replication package
# Sourced by every script in code/. Run everything from the repository root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readxl); library(ggplot2); library(scales)
  library(ggrepel); library(stargazer); library(Hmisc); library(xtable)
  library(LaplacesDemon); library(jsonlite)
})

# Paths (relative to the repository root)
data_dir         <- "data"
output_dir       <- "output"
table_dir        <- file.path(output_dir, "tables")        # .tex tables and macro files the paper inputs
figure_dir       <- file.path(output_dir, "figures")       # figure files the paper includes
plotdata_dir     <- file.path(output_dir, "plotdata")      # the data behind every figure (see tests/plotdata.R)
intermediate_dir <- file.path(output_dir, "intermediate")  # CSVs passed between scripts
for (d in c(table_dir, figure_dir, plotdata_dir, intermediate_dir))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Never let R open its default Rplots.pdf device (a printed ggplot would create it)
pdf(NULL)

source("tests/plotdata.R")

# Write a .tex file with LF line endings on every platform, so the generated
# tables are byte-identical to the paper's copies.
write_tex <- function(lines, path) {
  con <- file(path, open = "wb"); on.exit(close(con))
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
}

# Save a ggplot to figure_dir and its plot data to plotdata_dir.
save_figure <- function(filename, plot = ggplot2::last_plot(), ...) {
  ggplot2::ggsave(file.path(figure_dir, filename), plot, ...)
  write_plot_data(plot, file.path(plotdata_dir, paste0(tools::file_path_sans_ext(filename), ".csv")))
  invisible(plot)
}

# Repeat the column header on every continuation page of a longtable: the
# first-page head carries the caption, later pages a "(Table X continued)" line.
longtable_heads <- function(tab, label, ncol) {
  header <- grep("^Award Name &", tab)[1]
  rule <- header + 1  # the "\hline \\[-1.8ex]" line under the header row
  c(tab[1:rule],
    "\\endfirsthead",
    sprintf("\\multicolumn{%d}{l}{\\small\\textit{(Table~\\ref{%s} continued)}} \\\\[2pt]", ncol, label),
    "\\hline \\hline \\\\[-1.8ex]",
    tab[header], tab[rule],
    "\\endhead",
    tab[(rule + 1):length(tab)])
}

# A LaTeX \newcommand line
mac <- function(name, value) sprintf("\\newcommand{\\%s}{%s}", name, value)

# Winners matched to the paper's 26 field groups through the winner's OpenAlex
# subfield (every row of the winner file is one recognition)
winners_by_group <- function() {
  sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)
  winners <- read.csv(file.path(data_dir, "all_winners_with_plotFinestField.csv"),
                      stringsAsFactors = FALSE, encoding = "UTF-8")
  winners %>%
    inner_join(sf_map %>% select(subfield_name, finest_group) %>% distinct(),
               by = c("Best_Subfield" = "subfield_name"))
}

# Broad-area colors of the density figures by field group
color_map <- c(
  "Math & Phys. sci" = "#1A237E",
  "Applied sci"      = "#8B4577",
  "Life & Health"    = "#00A5A5",
  "Social sci"       = "#E7298A",
  "Other"            = "gray50"
)

# Broad-area assignment function
assign_broad_area <- function(field) {
  dplyr::case_when(
    field %in% c("Mathematics","Physics & Astronomy",
                 "Chemistry & Chemical Eng.") ~ "Math & Phys. sci",
    field %in% c("Computer Science","Computer & Electrical Eng.",
                 "Mechanical & Aerospace Eng.",
                 "Civil & Env. Eng.","Biomedical Eng.",
                 "Other Engineering",
                 "Geosciences","Climate & Ocean Sci.",
                 "Agriculture & Food Sci.") ~ "Applied sci",
    field %in% c("Molecular Bio & Genetics",
                 "Microbiology & Immunology","Neuroscience",
                 "Ecology & Evolution",
                 "Public Health",
                 "Medicine",
                 "Dentistry & Allied Health") ~ "Life & Health",
    field %in% c("Psychology","Economics","Business",
                 "Political Science & Sociology","Anthropology",
                 "Other Social Sci.") ~ "Social sci",
    TRUE ~ "Other"
  )
}

# Default transparency for density plots
alpha <- 0.4

# Shared density plotting function (scripts 07-09 and 11). `results` needs the
# columns field, size, density. ref_label names the reference square
# ("1,000 new PhDs"); denom_unit / total_size give its width. bracket_y are the
# y positions of the field-size brackets (tick top, tick bottom and bracket line,
# percentage label) and ylim_low the lower axis limit; both default to fractions
# of the tallest bar.
make_density_plot <- function(results, ref_label, denom_unit, total_size, ylab, xlab,
                              bracket_y = NULL, ylim_low = NULL) {
  results <- results[order(-results$density), ]
  results$fieldSize_pct <- results$size / sum(results$size) * 100
  results$AccFieldSize <- cumsum(results$fieldSize_pct)
  results$lagAccFieldSize <- dplyr::lag(results$AccFieldSize)
  results$lagAccFieldSize[1] <- 0
  results$broadArea <- assign_broad_area(results$field)
  results$midX <- (results$lagAccFieldSize + results$AccFieldSize) / 2

  yMax <- max(results$density) * 1.08
  avgDensity <- weighted.mean(results$density, w = results$fieldSize_pct)
  labelBuffer <- yMax * 0.04

  tallThreshold <- avgDensity * 2
  results$labelInside <- results$density > tallThreshold
  results$labelY <- ifelse(results$labelInside,
                           results$density - labelBuffer,
                           results$density + labelBuffer)

  legStep <- yMax * 0.05
  legBase <- yMax * 0.70
  refBase <- legBase - legStep * 2

  ref_width <- 100 * denom_unit / total_size
  if (is.null(bracket_y)) bracket_y <- c(-yMax * 0.01, -yMax * 0.02, -yMax * 0.04)
  if (is.null(ylim_low)) ylim_low <- -yMax * 0.06

  ggplot() +
    geom_rect(data = results,
      aes(xmin = lagAccFieldSize, xmax = AccFieldSize,
          ymin = 0, ymax = density, fill = broadArea),
      alpha = alpha, color = NA) +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = AccFieldSize,
          y = density, yend = density, color = broadArea),
      linewidth = 0.8) +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = lagAccFieldSize,
          y = 0, yend = density, color = broadArea),
      linewidth = 0.4) +
    geom_segment(data = results,
      aes(x = AccFieldSize, xend = AccFieldSize, y = 0, yend = density),
      linetype = "dashed", color = "gray80", linewidth = 0.3) +
    geom_text(data = results[!results$labelInside, ],
      aes(x = midX, y = labelY, label = field),
      size = 2.3, angle = 90, hjust = 0, color = "black") +
    geom_text(data = results[results$labelInside, ],
      aes(x = midX, y = labelY, label = field),
      size = 2.3, angle = 90, hjust = 1, color = "black") +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = lagAccFieldSize,
          y = bracket_y[1], yend = bracket_y[2]),
      color = "black", linewidth = 0.1) +
    geom_segment(data = results,
      aes(x = AccFieldSize, xend = AccFieldSize,
          y = bracket_y[1], yend = bracket_y[2]),
      color = "black", linewidth = 0.1) +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = AccFieldSize,
          y = bracket_y[2], yend = bracket_y[2]),
      color = "black", linewidth = 0.3) +
    geom_text(data = results,
      aes(x = midX, y = bracket_y[3],
          label = paste0(round(fieldSize_pct, 0), "%")),
      size = 2.3, angle = 90) +
    geom_hline(yintercept = avgDensity, linetype = "dashed",
               color = "gray50", linewidth = 0.3) +
    geom_text(aes(x = 97, y = avgDensity, label = "Avg."),
      hjust = 1, vjust = -0.5, size = 2.5, color = "gray50") +
    geom_rect(aes(xmin = 60, xmax = 60 + ref_width,
                  ymin = refBase, ymax = refBase + legStep),
      fill = "black", alpha = 0.1, color = "grey") +
    geom_text(aes(x = 60 + ref_width + 1,
                  y = refBase + legStep / 2,
                  label = paste0("= 1 recognition per ", ref_label)),
      color = "black", size = 2.5, hjust = 0, vjust = 0.5) +
    theme_minimal() +
    ylab(ylab) +
    xlab(xlab) +
    scale_x_continuous(breaks = c(0, 100), labels = c("0%", "100%")) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ".")) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(face = "bold", size = 10, margin = margin(t = -10)),
      axis.text.y = element_text(face = "bold", size = 9),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 11)
    ) +
    scale_fill_manual(values = color_map, aesthetics = c("fill", "color")) +
    # Manual legend
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase + legStep * 3, ymax = legBase + legStep * 4,
      fill = "#1A237E", alpha = alpha) +
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase + legStep * 2, ymax = legBase + legStep * 3,
      fill = "#8B4577", alpha = alpha) +
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase + legStep * 1, ymax = legBase + legStep * 2,
      fill = "#00A5A5", alpha = alpha) +
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase, ymax = legBase + legStep * 1,
      fill = "#E7298A", alpha = alpha) +
    annotate("segment", x = 84,
      y = legBase + legStep * 3, yend = legBase + legStep * 4,
      color = "#1A237E", linewidth = 1) +
    annotate("segment", x = 84,
      y = legBase + legStep * 2, yend = legBase + legStep * 3,
      color = "#8B4577", linewidth = 1) +
    annotate("segment", x = 84,
      y = legBase + legStep * 1, yend = legBase + legStep * 2,
      color = "#00A5A5", linewidth = 1) +
    annotate("segment", x = 84,
      y = legBase, yend = legBase + legStep * 1,
      color = "#E7298A", linewidth = 1) +
    annotate("text", x = 85, y = legBase + legStep * 3.5,
      label = "Math & Phys. sci", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 2.5,
      label = "Applied sci", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 1.5,
      label = "Life & Health", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 0.5,
      label = "Social sci", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 4.3,
      label = "Broad fields", hjust = 0, size = 2.8, fontface = "bold") +
    coord_cartesian(ylim = c(ylim_low, yMax)) +
    theme(legend.position = "none")
}
