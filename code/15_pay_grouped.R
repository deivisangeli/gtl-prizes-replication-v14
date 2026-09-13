# ==============================================================================
# 15_pay_grouped.R
# Generates: r2_3_pay_grouped_macros.tex and Figure S8
#            (r2_3_pay_grouped_scatter.pdf) -- recognition density against the
#            private-sector pay of a field's doctorate holders
#
# The paper's 26 field groups are coarsened to 16 groups that share a
# classification with the Survey of Doctorate Recipients' salary fields
# (data/ncses/pay_paper_groups.csv, pay_sdr_groups.csv). Business has no SDR
# counterpart; Dentistry & Allied Health lacks an adequate doctorate match.
#
# Salary is a survey-weighted median of individual public-use records (SDR 2023,
# data/ncses/doctorate_recipients_2023.zip): US residents, employed, private
# for-profit sector (including incorporated self-employment), working more than
# 35 hours (the public hours variable groups 21-35 hours together). The
# published Table 54 medians are used only as a validation check.
# ==============================================================================
source("_helpers.R")

paper_map <- read.csv(file.path(data_dir, "ncses/pay_paper_groups.csv"), stringsAsFactors = FALSE)
sdr_map <- read.csv(file.path(data_dir, "ncses/pay_sdr_groups.csv"), stringsAsFactors = FALSE,
                    colClasses = "character")
stopifnot(!anyDuplicated(paper_map$field), !anyDuplicated(sdr_map$NSDRMENTOD),
          setequal(paper_map$pay_group, sdr_map$pay_group))

# Recognitions and faculty denominators from their source files
faculty <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE) |>
  filter(academic_count > 0, group_name != "Humanities") |>
  transmute(field = group_name, academic_count)
winners <- winners_by_group()
raw_density <- winners |> count(finest_group, name = "recognitions") |>
  right_join(faculty, by = c("finest_group" = "field")) |>
  transmute(field = finest_group, academic_count,
            recognitions = coalesce(recognitions, 0L))
stopifnot(!anyDuplicated(raw_density$field), nrow(raw_density) == 26L,
          setequal(setdiff(raw_density$field, paper_map$field),
                   c("Business", "Dentistry & Allied Health")))
included <- inner_join(raw_density, paper_map, by = "field")
density <- included |> group_by(pay_group) |>
  summarise(academic_count = sum(academic_count), recognitions = sum(recognitions),
            n_paper_fields = n(), paper_fields = paste(sort(field), collapse = "; "),
            .groups = "drop") |>
  mutate(yearlyRE = recognitions / 10, density = 1000 * yearlyRE / academic_count)
# Conservation checks catch duplicate mappings and averaging of field densities
stopifnot(sum(density$recognitions) == sum(included$recognitions),
          sum(density$academic_count) == sum(included$academic_count),
          sum(density$n_paper_fields) == 24L, nrow(density) == 16L)

archive <- file.path(data_dir, "ncses/doctorate_recipients_2023.zip")
cols <- c("NSDRMENTOD", "SALARYP", "WTSURVY", "HRSWKP", "LFSTAT", "EMSECDT", "FNINUS")
headers <- names(read.csv(unz(archive, "psd23Public/epsd23.csv"), nrows = 0))
classes <- ifelse(headers %in% cols, "character", "NULL")
micro <- read.csv(unz(archive, "psd23Public/epsd23.csv"), colClasses = classes,
                  stringsAsFactors = FALSE) |>
  mutate(across(c(SALARYP, WTSURVY, HRSWKP), as.numeric))
stopifnot(setequal(unique(micro$NSDRMENTOD), sdr_map$NSDRMENTOD),
          all(is.finite(micro$WTSURVY)), all(micro$WTSURVY > 0))
eligible <- micro |>
  filter(FNINUS == "Y", LFSTAT == "1", EMSECDT == "21", HRSWKP %in% c(3, 4))
stopifnot(all(is.finite(eligible$SALARYP)),
          all(eligible$SALARYP >= 0 & eligible$SALARYP < 9999998))
pay_records <- eligible |> left_join(sdr_map, by = "NSDRMENTOD")
stopifnot(nrow(pay_records) == nrow(eligible), !anyNA(pay_records$pay_group))
weighted_median <- function(x, w) {
  ord <- order(x)
  x[ord][which(cumsum(w[ord]) >= sum(w) / 2)[1]]
}
pay <- pay_records |> group_by(pay_group) |>
  summarise(salary = weighted_median(SALARYP, WTSURVY),
            salary_n = n(), salary_population = sum(WTSURVY), .groups = "drop")
fm <- inner_join(density, pay, by = "pay_group") |> mutate(ind_k = salary / 1000)
stopifnot(nrow(fm) == nrow(density), !anyDuplicated(fm$pay_group),
          all(is.finite(fm$salary)), all(is.finite(fm$density)))
test <- cor.test(fm$salary, fm$density, method = "spearman", exact = FALSE)
write.csv(fm, file.path(intermediate_dir, "r2_3_pay_grouped_sample.csv"), row.names = FALSE)

# Validate against the 26 published Table 54 salary categories before grouping.
# Exact equality is not expected: Table 54 includes exactly-35-hour workers,
# while the public file cannot distinguish them from the 21-34-hour group; its
# salary values are also recoded.
source_check <- pay_records |> group_by(NSDRMENTOD, sdr_field, pay_group) |>
  summarise(n = n(), population = sum(WTSURVY),
            median_salary = weighted_median(SALARYP, WTSURVY), .groups = "drop")
published <- read_excel(file.path(data_dir, "ncses/sdr25321_tab054.xlsx"), skip = 5, col_names = FALSE,
                        .name_repair = "minimal") |>
  select(published_field = 1, published_median = 8, published_se = 9) |>
  filter(!is.na(published_field)) |>
  mutate(published_field = tolower(published_field),
         across(c(published_median, published_se), ~suppressWarnings(as.numeric(.x))))
validation <- source_check |>
  mutate(published_field = ifelse(NSDRMENTOD == "43",
           "geosciences, atmospheric sciences, and ocean sciences", tolower(sdr_field))) |>
  left_join(published, by = "published_field") |>
  mutate(difference = median_salary - published_median,
         pct_difference = 100 * difference / published_median)
stopifnot(nrow(validation) == 26L, !anyNA(validation$published_median),
          all(is.finite(validation$published_se)), all(validation$published_se > 0))
cat(sprintf(paste0("Published salary check: %d/26 exact; median absolute difference $%.0f; ",
                   "max $%.0f (%.2f%%); rank correlation %.6f\n"),
            sum(validation$difference == 0), median(abs(validation$difference)),
            max(abs(validation$difference)), max(abs(validation$pct_difference)),
            cor(validation$median_salary, validation$published_median, method = "spearman")))

write_tex(c("% generated by code/15_pay_grouped.R -- do not edit",
            sprintf("\\newcommand{\\mIndGroupedN}{%d}", nrow(fm)),
            sprintf("\\newcommand{\\mIndGroupedFields}{%d}", nrow(included)),
            sprintf("\\newcommand{\\mIndGroupedRho}{%+.2f}", test$estimate),
            sprintf("\\newcommand{\\mIndGroupedP}{%.3f}", test$p.value)),
          file.path(table_dir, "r2_3_pay_grouped_macros.tex"))

labels <- c("Agriculture & Environmental Sci." = "Agriculture &\nenvironmental sci.",
            "Other Biological & Health Sci." = "Other biological\n& health sci.",
            "Anthropology & Other Social Sci." = "Anthropology &\nother social sci.",
            "Mechanical & Aerospace Eng." = "Mechanical &\naerospace eng.",
            "Computer & Electrical Eng." = "Computer &\nelectrical eng.",
            "Political Science & Sociology" = "Political science\n& sociology",
            "Microbiology & Immunology" = "Microbiology &\nimmunology",
            "Chemistry & Chemical Eng." = "Chemistry &\nchemical eng.",
            "Biomedical & Other Eng." = "Biomedical &\nother eng.")
fm$plot_label <- ifelse(fm$pay_group %in% names(labels), labels[fm$pay_group], fm$pay_group)
p <- ggplot(fm, aes(ind_k, density)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey60", linewidth = .5) +
  geom_point(colour = "steelblue4", size = 2) +
  geom_text_repel(aes(label = plot_label), size = 2.6, lineheight = .95,
                  colour = "grey20", max.overlaps = Inf, box.padding = .35,
                  segment.colour = "grey70", segment.size = .2, seed = 1,
                  max.time = 5, max.iter = 100000) +
  scale_x_continuous(expand = expansion(mult = c(.2, .18))) +
  scale_y_continuous(expand = expansion(mult = c(.08, .18))) +
  labs(x = "Median annual private-sector salary (USD thousands)",
       y = "Yearly recognitions per 1,000 top researchers") +
  theme_minimal(base_size = 9) + theme(panel.grid.minor = element_blank())
save_figure("r2_3_pay_grouped_scatter.pdf", p, width = 5.5, height = 3.8)
cat(sprintf("Common groups: N=%d, covering %d paper fields; rho=%+.6f, p=%.6f\n",
            nrow(fm), nrow(included), test$estimate, test$p.value))
