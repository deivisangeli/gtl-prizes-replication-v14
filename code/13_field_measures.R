# ==============================================================================
# 13_field_measures.R
# Generates the field-level measures behind the "Gaps across fields" results and
# the SI section on distance to application (intermediate CSVs read by scripts
# 14, 16 and 25):
#   output/intermediate/r2_3_field_measures.csv    one row per field group
#   output/intermediate/r2_3_correlations.csv      Spearman/Pearson with density
#   output/intermediate/r2_3_proxy_agreement.csv   agreement between the measures
#   output/intermediate/r2_3_ros_windows.csv       patent-cited share by window
#
# Measures per field group (26 groups, Humanities excluded):
#   density              yearly recognitions 2015-2024 per 1,000 top researchers
#                        (the construction behind the paper's academic-density figure)
#   sdr_private_share    share of employed US-residing doctorate holders working in
#                        business or industry, 2023 (NCSES SDR, NSF 25-321 Table 12-3)
#   sdr_salary_*         median annual salary of doctorate holders, all sectors /
#                        education / industry (SDR 2023, NSF 25-321 Table 54)
#   ros_cited_share      share of the field's cited 2000-2015 articles that a US
#                        patent cites (Reliance on Science x OpenAlex), with
#                        denominator, confidence and top-k robustness variants
# ==============================================================================
source("_helpers.R")

# ---------------------------------------------------------------- prize density
sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)
vs <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE) |>
  filter(academic_count > 0, group_name != "Humanities")
winners <- winners_by_group()
dens <- winners |> count(finest_group, name = "re10") |>
  right_join(vs, by = c("finest_group" = "group_name")) |>
  mutate(re10 = ifelse(is.na(re10), 0, re10),
         yearlyRE = re10 / 10,
         density = yearlyRE / academic_count * 1000) |>
  select(field = finest_group, academic_count, yearlyRE, density)

# Comte's hierarchy of the sciences (Cours de philosophie positive, 1830-42):
# comte_rungs.csv places every group but Dentistry & Allied Health on a rung,
# placement == "Comte" for the groups Comte's own scheme names, "ours" for the
# groups placed by interpretation.
comte <- read.csv(file.path(data_dir, "comte_rungs.csv"), stringsAsFactors = FALSE)
stopifnot(all(comte$field %in% dens$field), !any(duplicated(comte$field)))
dens <- dens |> left_join(comte, by = "field")

# ------------------------------------------- SDR private-sector share of PhDs
# Table 12-3 columns: 1 field, 2 all employed, 3 SE, 4 education, 5 SE,
# 6 business or industry, 7 SE, 8 government, 9 SE.
sdr_raw <- read_excel(file.path(data_dir, "ncses/sdr25321_tab012-003.xlsx"), skip = 5, col_names = FALSE) |>
  select(field = 1, employed = 2, education = 4, business = 6, government = 8) |>
  filter(!is.na(field), !is.na(employed)) |>
  mutate(across(c(employed, education, business, government), as.numeric))
sdr_total <- sdr_raw$employed[sdr_raw$field == "All fields"]

# Leaf rows only: the table nests subfields under aggregates, and summing both
# would double count. "Political science and government" appears as an aggregate
# and again as a leaf; the leaf is the later row.
sdr_xw <- c(
  "Agricultural sciences" = "Agriculture & Food Sci.",
  "Animal sciences" = "Agriculture & Food Sci.",
  "Food sciences and technology" = "Agriculture & Food Sci.",
  "Plant sciences" = "Agriculture & Food Sci.",
  "Soil sciences" = "Agriculture & Food Sci.",
  "Biochemistry" = "Molecular Bio & Genetics",
  "Biophysics" = "Molecular Bio & Genetics",
  "Cell, cellular biology, and molecular biology" = "Molecular Bio & Genetics",
  "Genetics" = "Molecular Bio & Genetics",
  "Immunology" = "Microbiology & Immunology",
  "Microbiological sciences" = "Microbiology & Immunology",
  "Fish, fisheries, wildlife, and wildlands science and management" = "Environmental Science",
  "Forestry" = "Environmental Science",
  "Natural resource conservation, research, management, and policy" = "Environmental Science",
  "Zoology" = "Ecology & Evolution",
  "Botany and plant biology" = "Ecology & Evolution",
  "Epidemiology, ecology, and population biology" = "Ecology & Evolution",
  "Biomathematics, bioinformatics, and computational biology" = "Medicine",
  "Neurobiology and neuroscience" = "Neuroscience",
  "Nutrition sciences" = "Dentistry & Allied Health",
  "Pharmacology and toxicology" = "Medicine",
  "Physiology, pathology, and related sciences" = "Medicine",
  "Biological and biomedical sciences, general" = "Medicine",
  "Biological and biomedical sciences, other" = "Medicine",
  "Computer science" = "Computer Science",
  "Information science, studies" = "Computer Science",
  "Computer and information sciences, other" = "Computer Science",
  "Applied mathematics" = "Mathematics",
  "Mathematics" = "Mathematics",
  "Statistics" = "Mathematics",
  "Mathematics and statistics, other" = "Mathematics",
  "Astronomy and astrophysics" = "Physics & Astronomy",
  "Physics" = "Physics & Astronomy",
  "Inorganic chemistry" = "Chemistry & Chemical Eng.",
  "Organic chemistry" = "Chemistry & Chemical Eng.",
  "Chemistry, other, except biochemistry" = "Chemistry & Chemical Eng.",
  "Atmospheric sciences and meteorology" = "Climate & Ocean Sci.",
  "Ocean sciences and marine sciences" = "Climate & Ocean Sci.",
  "Oceanography, chemical and physical" = "Climate & Ocean Sci.",
  "Geological and earth sciences, geosciences" = "Geosciences",
  "Clinical psychology" = "Psychology",
  "Counseling and applied psychology" = "Psychology",
  "Educational and school psychology" = "Psychology",
  "Industrial and organizational psychology" = "Psychology",
  "Research and experimental psychology" = "Psychology",
  "Psychology, general" = "Psychology",
  "Psychology, other" = "Psychology",
  "Economics" = "Economics",
  "Political science and government" = "Political Science & Sociology",
  "Public policy analysis" = "Political Science & Sociology",
  "Sociology, demography, and population studies" = "Political Science & Sociology",
  "Anthropology" = "Anthropology",
  "Area, ethnic, cultural, gender, and group studies" = "Other Social Sci.",
  "Geography and cartography" = "Other Social Sci.",
  "International relations and national security studies" = "Other Social Sci.",
  "Linguistics" = "Other Social Sci.",
  "Urban studies, affairs" = "Other Social Sci.",
  "Social sciences, other" = "Other Social Sci.",
  "Aerospace, aeronautical, and astronautical engineering" = "Mechanical & Aerospace Eng.",
  "Mechanical engineering" = "Mechanical & Aerospace Eng.",
  "Chemical engineering" = "Chemistry & Chemical Eng.",
  "Metallurgical and materials engineering" = "Chemistry & Chemical Eng.",
  "Civil engineering" = "Civil & Env. Eng.",
  "Computer engineering" = "Computer & Electrical Eng.",
  "Electrical, electronics, and communications engineering" = "Computer & Electrical Eng.",
  "Bioengineering and biomedical engineering" = "Biomedical Eng.",
  "Agricultural engineering" = "Other Engineering",
  "Engineering mechanics, physics, and science" = "Other Engineering",
  "Industrial and manufacturing engineering" = "Other Engineering",
  "Nuclear engineering" = "Other Engineering",
  "Engineering, other" = "Other Engineering",
  "Communication disorders sciences and services" = "Dentistry & Allied Health",
  "Hospital and medical administration services" = "Dentistry & Allied Health",
  "Registered nursing, nursing administration, nursing research" = "Dentistry & Allied Health",
  "Health sciences, other" = "Dentistry & Allied Health",
  "Pharmacy, pharmaceutical sciences, and administration" = "Medicine",
  "Public health" = "Public Health"
)
stopifnot(all(names(sdr_xw) %in% sdr_raw$field))

sdr_leaf <- sdr_raw |>
  mutate(row = row_number()) |>
  filter(field %in% names(sdr_xw)) |>
  # keep the later row where a name appears both as aggregate and as leaf
  group_by(field) |> slice_max(row, n = 1) |> ungroup() |>
  mutate(group = unname(sdr_xw[field]))
# every leaf accounted for: leaves must sum to the published all-fields total
stopifnot(abs(sum(sdr_leaf$employed) - sdr_total) <= 0.005 * sdr_total)

sdr_g <- sdr_leaf |> group_by(field = group) |>
  summarise(sdr_employed = sum(employed), sdr_business = sum(business),
            sdr_education = sum(education), .groups = "drop") |>
  mutate(sdr_private_share = sdr_business / sdr_employed,
         sdr_academic_share = sdr_education / sdr_employed)

# --------------------------------------------- SDR pay of doctorate holders
# Table 54 reports about 30 broad fields, not the fine fields of Table 12-3, so
# the group values use the same fine-field crosswalk as the industry share: each
# fine field takes the median of the broad field it nests under in Table 12-3
# (the aggregate row above it), and a group's value is the employment-weighted
# mean over its fine fields. Columns: 1 field, 2 all full-time employed,
# 4 four-year educational institutions, 8 private for-profit.
sal_raw <- read_excel(file.path(data_dir, "ncses/sdr25321_tab054.xlsx"), skip = 5, col_names = FALSE) |>
  select(field = 1, sal_all = 2, sal_edu = 4, sal_ind = 8) |>
  filter(!is.na(field)) |>
  mutate(across(c(sal_all, sal_edu, sal_ind), ~suppressWarnings(as.numeric(.x))))
# fine field -> broad field, walking Table 12-3 in row order
broad_of <- character(0); current <- NA_character_
for (f in sdr_raw$field) {
  if (f %in% sal_raw$field) current <- f
  broad_of[f] <- current
}
sal_g <- sdr_leaf |>
  mutate(broad = unname(broad_of[field])) |>
  inner_join(sal_raw, by = c("broad" = "field")) |>
  group_by(field = group) |>
  summarise(sdr_salary_all = weighted.mean(sal_all, employed, na.rm = TRUE),
            sdr_salary_edu = weighted.mean(sal_edu, employed, na.rm = TRUE),
            sdr_salary_ind = weighted.mean(sal_ind, employed, na.rm = TRUE),
            sdr_salary_broad = paste(sort(unique(broad)), collapse = "; "),
            .groups = "drop")
stopifnot(nrow(sal_g) == nrow(sdr_g), all(!is.na(sal_g$sdr_salary_all)))

# ------------------------------ patent citations to the field's articles
# data/openalex/patent_citation_counts.csv gives, per OpenAlex subfield and
# publication year, the number of articles and the number cited by at least one
# US patent in Marx & Fuegi's Reliance on Science (v64, matches at confidence
# >= 4 as shipped; >= 7, = 10, front-page-only and applicant-only are robustness
# cuts). Articles published 2000-2015, so each has had ten or more years to be
# cited by patents granted through 2025. Subfields map to the 26 groups through
# the paper's own crosswalk. HIGHER = MORE CITED BY PATENTS = MORE APPLIED.
ROS_WINDOW <- c(2000, 2015)
ros_all <- read.csv(file.path(data_dir, "openalex/patent_citation_counts.csv")) |>
  filter(is_article == 1, subfield_id > 0) |>
  mutate(subfield_id = paste0("subfields/", subfield_id)) |>
  inner_join(distinct(sf_map, subfield_id, finest_group), by = "subfield_id") |>
  filter(finest_group != "Humanities")
stopifnot(all(c("cite_bucket", "n_cited_c4", "n_cited_c7", "n_cited_c10", "n_front_c4",
                "n_app_c4", "sum_pat_c4") %in% names(ros_all)))
# Denominators: cite_bucket is the lower bound of the work's OpenAlex citation
# bucket (0, 1, 10, 50), so min_cites = 1 keeps works with at least one citation.
ros_agg <- function(d, min_cites = 0, suffix = "") {
  out <- d |> filter(cite_bucket >= min_cites) |> group_by(field = finest_group) |>
    summarise(n_articles  = sum(n_works),
              cited_share = sum(n_cited_c4) / sum(n_works),
              cited_share_c7  = sum(n_cited_c7) / sum(n_works),
              cited_share_c10 = sum(n_cited_c10) / sum(n_works),
              front_share = sum(n_front_c4) / sum(n_works),
              app_share   = sum(n_app_c4) / sum(n_works),
              pat_per_k   = 1000 * sum(sum_pat_c4) / sum(n_works), .groups = "drop")
  names(out)[-1] <- paste0("ros_", names(out)[-1], suffix)
  out
}
ros_w <- filter(ros_all, year >= ROS_WINDOW[1], year <= ROS_WINDOW[2])
# Headline denominator: articles with at least one OpenAlex citation (unsuffixed
# columns). All articles ("_all") and articles with >= 10 citations ("_ge10") are
# robustness denominators, as are the top-k most-cited articles below.
ros <- ros_agg(ros_w, 1) |>
  left_join(ros_agg(ros_w, 0, "_all") |> select(field, ros_n_articles_all, ros_cited_share_all),
            by = "field") |>
  left_join(ros_agg(ros_w, 10, "_ge10") |> select(field, ros_n_articles_ge10, ros_cited_share_ge10),
            by = "field")
# top-k most-cited articles of each group in the window
topk <- read.csv(file.path(data_dir, "openalex/patent_citation_topk.csv"), stringsAsFactors = FALSE)
stopifnot(all(topk$year >= ROS_WINDOW[1] & topk$year <= ROS_WINDOW[2]),
          !any(duplicated(topk$work_id)))
top_share <- function(k) topk |> filter(rank <= k) |> group_by(field) |>
  summarise(!!paste0("ros_top", k / 1000, "k_share") := mean(max_conf >= 4),
            !!paste0("ros_top", k / 1000, "k_min_cites") := min(cited_by_count), .groups = "drop")
ros <- ros |> left_join(top_share(5000), by = "field") |>
  left_join(top_share(10000), by = "field") |> left_join(top_share(20000), by = "field")
stopifnot(setequal(ros$field, dens$field), all(ros$ros_n_articles > 1000))
# window robustness
ros_win <- bind_rows(lapply(list(c(1995, 2010), c(2000, 2015), c(2005, 2015), c(2010, 2019)),
  function(w) ros_agg(filter(ros_all, year >= w[1], year <= w[2]), 1) |>
    left_join(ros_agg(filter(ros_all, year >= w[1], year <= w[2]), 0, "_all") |>
                select(field, ros_cited_share_all), by = "field") |>
    mutate(window = paste(w, collapse = "-"))))
write.csv(ros_win, file.path(intermediate_dir, "r2_3_ros_windows.csv"), row.names = FALSE)

# ------------------------------------------------------------------- assemble
res <- dens |>
  left_join(select(sdr_g, field, sdr_employed, sdr_private_share, sdr_academic_share),
            by = "field") |>
  left_join(select(sal_g, -sdr_salary_broad), by = "field") |>
  left_join(ros, by = "field") |>
  arrange(desc(density))
stopifnot(all(!is.na(res$ros_cited_share)))
write.csv(res, file.path(intermediate_dir, "r2_3_field_measures.csv"), row.names = FALSE)

# ---------------------------------------------------------------- correlations
# Pooled over all groups, and within the science / engineering / health groups
meas <- c("sdr_private_share", "ros_cited_share",
          "ros_cited_share_all", "ros_cited_share_ge10",
          "ros_top5k_share", "ros_top10k_share", "ros_top20k_share",
          "ros_cited_share_c7", "ros_cited_share_c10", "ros_front_share", "ros_app_share",
          "ros_pat_per_k",
          "sdr_salary_ind", "sdr_salary_all")
social <- c("Economics", "Political Science & Sociology", "Anthropology",
            "Psychology", "Business", "Other Social Sci.")
stopifnot(all(social %in% res$field))
cor_block <- function(d, label) bind_rows(lapply(meas, function(m) {
  ok <- !is.na(d[[m]]) & !is.na(d$density)
  s <- suppressWarnings(cor.test(d$density[ok], d[[m]][ok], method = "spearman",
                                 exact = FALSE))
  p <- cor.test(d$density[ok], d[[m]][ok])
  data.frame(sample = label, measure = m, n = sum(ok), spearman = unname(s$estimate),
             p_spearman = s$p.value, pearson = unname(p$estimate), p_pearson = p$p.value)
}))
# Dentistry & Allied Health does not map onto the SDR's field options, so it is
# out of the pay measure; the correlation is computed on the same sample.
res_pay <- res
res_pay$sdr_salary_ind[res_pay$field == "Dentistry & Allied Health"] <- NA
cors <- bind_rows(cor_block(res_pay, "all"),
                  cor_block(res_pay[!res_pay$field %in% social, ], "science"))
write.csv(cors, file.path(intermediate_dir, "r2_3_correlations.csv"), row.names = FALSE)
cat("\n--- correlation of prize density with each field measure ---\n")
print(cors |> mutate(across(where(is.numeric), ~round(.x, 3))), row.names = FALSE)

agree <- cor(res[meas], method = "spearman", use = "pairwise.complete.obs")
write.csv(as.data.frame(agree), file.path(intermediate_dir, "r2_3_proxy_agreement.csv"))
