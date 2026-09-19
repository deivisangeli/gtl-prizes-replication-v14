# ==============================================================================
# 12_field_measures.R
# Writes the field-level inputs of the distance-to-application results (read by
# scripts 13, 15 and 23):
#   output/intermediate/r2_3_field_measures.csv  per field group: recognition
#     density (yearly recognitions 2015-2024 per 1,000 top researchers) and the
#     share of the group's cited 2000-2015 articles cited by a US patent
#     (Reliance on Science x OpenAlex), with the article counts
#   output/intermediate/r2_3_correlations.csv  Spearman correlation of density
#     with the patent-cited share, all groups and excluding the social sciences
# Inputs: data/subfield_to_finest_group.csv, data/vs_academics_by_finest_group.csv,
#   data/all_winners_with_plotFinestField.csv, data/openalex/patent_citation_counts.csv
# ==============================================================================
source("_helpers.R")

# ---------------------------------------------------------------- prize density
sf_map <- read.csv(file.path(data_dir, "subfield_to_finest_group.csv"), stringsAsFactors = FALSE)
vs <- read.csv(file.path(data_dir, "vs_academics_by_finest_group.csv"), stringsAsFactors = FALSE) |>
  filter(group_name != "Humanities")
winners <- winners_by_group()
dens <- winners |> count(finest_group, name = "re10") |>
  right_join(vs, by = c("finest_group" = "group_name")) |>
  mutate(re10 = ifelse(is.na(re10), 0, re10),
         density = re10 / 10 / academic_count * 1000) |>
  select(field = finest_group, density)

# ------------------------------ patent citations to the field's articles
# data/openalex/patent_citation_counts.csv gives, per OpenAlex subfield and
# publication year, the number of articles and the number cited by at least one
# US patent in Marx & Fuegi's Reliance on Science (confidence >= 4). Articles
# published 2000-2015 have had at least ten years to be cited by patents
# granted through 2025. Subfields map to the 26 groups through the paper's
# crosswalk.
ROS_WINDOW <- c(2000, 2015)
ros_all <- read.csv(file.path(data_dir, "openalex/patent_citation_counts.csv")) |>
  filter(is_article == 1, subfield_id > 0) |>
  mutate(subfield_id = paste0("subfields/", subfield_id)) |>
  inner_join(distinct(sf_map, subfield_id, finest_group), by = "subfield_id") |>
  filter(finest_group != "Humanities")
# Denominators: cite_bucket is the lower bound of the work's OpenAlex citation
# bucket (0, 1, 10, 50), so min_cites = 1 keeps works with at least one citation.
ros_agg <- function(d, min_cites = 0, suffix = "") {
  out <- d |> filter(cite_bucket >= min_cites) |> group_by(field = finest_group) |>
    summarise(cited_share = sum(n_cited_c4) / sum(n_works),
              n_articles  = sum(n_works), .groups = "drop")
  names(out)[-1] <- paste0("ros_", names(out)[-1], suffix)
  out
}
ros_w <- filter(ros_all, year >= ROS_WINDOW[1], year <= ROS_WINDOW[2])
# Denominator: articles with at least one OpenAlex citation; the all-articles
# count is reported alongside it.
ros <- ros_agg(ros_w, 1) |>
  left_join(ros_agg(ros_w, 0, "_all") |> select(field, ros_n_articles_all), by = "field")
stopifnot(setequal(ros$field, dens$field))

# ------------------------------------------------------------------- assemble
res <- dens |>
  left_join(ros, by = "field") |>
  arrange(desc(density))
stopifnot(all(!is.na(res$ros_cited_share)))
write.csv(res, file.path(intermediate_dir, "r2_3_field_measures.csv"), row.names = FALSE)

# ---------------------------------------------------------------- correlations
# All 26 groups, and the 20 groups outside the social sciences
social <- c("Economics", "Political Science & Sociology", "Anthropology",
            "Psychology", "Business", "Other Social Sci.")
stopifnot(all(social %in% res$field))
cor_block <- function(d, label) {
  s <- suppressWarnings(cor.test(d$density, d$ros_cited_share, method = "spearman",
                                 exact = FALSE))
  data.frame(sample = label,
             spearman = unname(s$estimate), p_spearman = s$p.value)
}
cors <- bind_rows(cor_block(res, "all"),
                  cor_block(res[!res$field %in% social, ], "science"))
write.csv(cors, file.path(intermediate_dir, "r2_3_correlations.csv"), row.names = FALSE)
