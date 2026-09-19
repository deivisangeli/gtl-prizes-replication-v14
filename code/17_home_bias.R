# ==============================================================================
# 17_home_bias.R
# Home bias in prize recognitions (Discussion, "Prizes as signals"). Writes the
# intermediate files behind r2_2_macros*.tex, r2_2_prize_table.tex,
# r2_2_desc_macros.tex, r2_2_awarding_country_table.tex,
# r2_2_home_bias_scatter.pdf and r2_2_home_share_by_country.pdf (scripts 18, 19, 23).
# For each 2015-2024 recognition, the laureate counts as "home" if the awarding
# country appears among the countries of the institutions on their small-team
# papers (at most 20 authors) up to and including the award year, an institution
# counting if it appears in at least two distinct years and on at least 5% of
# the author's small-team works by then. Hong Kong, Macau and Taiwan are folded
# into China on both sides. The benchmark is the share of the field's reference
# pool -- the 1,000 researchers in the prize's modal OpenAlex field with the most
# citations to small-team papers as of the award year -- affiliated with the
# awarding country by that year. The prize-table note also reports the ratio
# under two benchmarks whose size scales with the field: the top 0.1% and top
# 0.05% of the field's cited authors (pools pool_v2_share0.1, pool_v2_share0.05).
#
# Inputs: data/all_winners_with_plotFinestField.csv
#         data/openalex/author_affiliations_smallteam.jsonl   (laureate affiliation histories)
#         data/openalex/<pool>/top_pool_field_year.csv, top_pool_field_country_year.csv
#         output/intermediate/prize_awarding_countries.csv    (script 15)
# Outputs (output/intermediate/): r2_2_winner_countries.csv, r2_2_prize_home_bias.csv,
#         r2_2_home_awards_by_country<sfx>.csv for sfx = "" (top-1,000), "_share01", "_share005"
# ==============================================================================
source("_helpers.R")

oa_dir <- file.path(data_dir, "openalex")
POOLS <- list(list(dir = "pool_v2_top1000",    sfx = ""),
              list(dir = "pool_v2_share0.1",  sfx = "_share01"),
              list(dir = "pool_v2_share0.05", sfx = "_share005"))
COUNTRY_MIN_YEARS <- 2      # a country counts if an affiliation there appears in >= this many distinct years by the award year
AFF_MIN_SHARE <- 0.05       # ... and on at least this share of the author's small-team works by then

# The two collaboration rows (EHT 2020, Oxford-AstraZeneca 2022) carry a team
# leader's OpenAlex id for field allocation only; person-level analyses use
# individual rows.
winners <- read.csv(file.path(data_dir, "all_winners_with_plotFinestField.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8") |>
  filter(laureate_type == "individual") |>
  mutate(author_url = ifelse(Corrected_OpenAlex_URL != "", Corrected_OpenAlex_URL, OpenAlex_URL),
         author_id = sub(".*/(A\\d+)$", "\\1", author_url)) |>
  filter(grepl("^A\\d+$", author_id))

# ---- author affiliation histories, from the OpenAlex snapshot ----
aff <- lapply(readLines(file.path(oa_dir, "author_affiliations_smallteam.jsonl"), encoding = "UTF-8"),
              function(l) fromJSON(l, simplifyVector = FALSE))

# per author: one row per (institution country, years)
aff_tab <- bind_rows(lapply(aff, function(a) {
  bind_rows(lapply(a$affiliations, function(x) {
    cc <- x$institution$country_code
    if (is.null(cc)) return(NULL)
    data.frame(author_id = sub(".*/", "", a$id), country = cc,
               years = I(list(as.integer(unlist(x$years)))),
               n_works = x$n_works, n_total = a$n_works_total)
  }))
}))

# countries of the institutions on the author's small-team papers up to the
# award year, under the two-years / 5%-of-works rule
countries_upto <- function(aid, yr) {
  rows <- aff_tab[aff_tab$author_id == aid, ]
  if (nrow(rows) == 0) return(character(0))
  keep <- vapply(seq_len(nrow(rows)), function(i) {
    y <- rows$years[[i]]
    length(unique(y[y <= yr])) >= COUNTRY_MIN_YEARS &&
      rows$n_works[i] >= AFF_MIN_SHARE * rows$n_total[i]
  }, logical(1))
  unique(rows$country[keep])
}
winners$countries_prior <- mapply(function(a, y) paste(countries_upto(a, y), collapse = ";"),
                                  winners$author_id, winners$Year)

# ---- Greater-China fold, on both sides of the home-bias test ----
CN_FOLD <- c("HK" = "CN", "MO" = "CN", "TW" = "CN")
fold_cn <- function(x) { y <- unname(CN_FOLD[x]); ifelse(is.na(y), x, y) }
fold_set <- function(v) vapply(strsplit(v, ";"), function(cs) {
  if (length(cs) == 0) return("")
  paste(unique(fold_cn(cs)), collapse = ";")
}, character(1))
# the unfolded set feeds the received-share columns of the awarding-country table (script 19)
winners$countries_prior_raw <- winners$countries_prior
winners$countries_prior <- fold_set(winners$countries_prior)

# ---- awarding countries (script 15) ----
pc <- read.csv(file.path(intermediate_dir, "prize_awarding_countries.csv"), check.names = FALSE,
               stringsAsFactors = FALSE, encoding = "UTF-8") |>
  select(Prize, AwardingCountry)
name_fix <- c("Frontiers of Knowledge Award in Economics, Finance and Management" =
                "Frontiers of Knowledge Award in Economics Finance and Management")
winners$PrizeKey <- ifelse(winners$Prize %in% names(name_fix), name_fix[winners$Prize], winners$Prize)
stopifnot(all(winners$PrizeKey %in% pc$Prize))
iso2 <- c("United States" = "US", "Norway" = "NO", "Mexico" = "MX", "Japan" = "JP",
          "Denmark" = "DK", "Canada" = "CA", "United Kingdom" = "GB", "Sweden" = "SE",
          "Italy" = "IT", "Spain" = "ES", "Israel" = "IL", "Netherlands" = "NL",
          "Belgium" = "BE", "Germany" = "DE", "Finland" = "FI", "Saudi Arabia" = "SA",
          "Hong Kong" = "HK", "Taiwan" = "TW", "Switzerland" = "CH", "France" = "FR",
          "Austria" = "AT", "China" = "CN", "Australia" = "AU", "International" = NA)
unmapped <- setdiff(unique(pc$AwardingCountry), names(iso2))
if (length(unmapped) > 0) stop("awarding countries with no ISO-2 mapping: ", paste(unmapped, collapse = ", "))
pc$home_cc <- fold_cn(iso2[pc$AwardingCountry])
winners <- left_join(winners, pc, by = c("PrizeKey" = "Prize"))

hit_of <- function(cs, hc) { if (is.na(hc) || cs == "") return(NA); hc %in% strsplit(cs, ";")[[1]] }
winners$home_hit_prior <- mapply(hit_of, winners$countries_prior, winners$home_cc)

write.csv(winners |> select(Prize, Year, AwardingCountry, Best_Field, countries_prior, countries_prior_raw),
          file.path(intermediate_dir, "r2_2_winner_countries.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- per-prize home share vs the pool benchmark ----
w <- winners |> filter(!is.na(home_hit_prior))

prize_stats <- w |>
  group_by(Prize, AwardingCountry, home_cc) |>
  summarise(n_winners = n(),
            home_share = mean(home_hit_prior),
            main_field = names(sort(table(Best_Field), decreasing = TRUE))[1],
            .groups = "drop")
w <- w |> left_join(prize_stats |> select(Prize, main_field), by = "Prize")

# pool benchmark: a pool is a (field, year) denominator table plus a
# (field, country, year) numerator table; the snapshot emits the Greater-China
# fold as the pseudo-country CNX
make_pool <- function(dir, den_file, num_file, den_col) {
  den <- read.csv(file.path(dir, den_file), stringsAsFactors = FALSE)
  num <- read.csv(file.path(dir, num_file), stringsAsFactors = FALSE)
  list(den = den, num = num, den_col = den_col,
       den_key = paste(den$field, den$year),
       num_key = paste(num$field, num$country, num$year))
}
pop_cc <- function(cc) ifelse(cc == "CN", "CNX", cc)
share_of <- function(pool, field, cc, year) {
  d <- pool$den[[pool$den_col]][match(paste(field, year), pool$den_key)]
  n <- pool$num$n_authors[match(paste(field, pop_cc(cc), year), pool$num_key)]
  if (is.na(d) || d == 0) return(NA_real_)
  if (is.na(n)) n <- 0
  n / d
}

for (pool in POOLS) {
  pool_top <- make_pool(file.path(oa_dir, pool$dir), "top_pool_field_year.csv", "top_pool_field_country_year.csv", "n_pool")
  w$pop_share_row <- mapply(function(f, cc, y) share_of(pool_top, f, cc, y), w$main_field, w$home_cc, w$Year)

  if (pool$sfx == "") {
    pop_bench <- w |>
      group_by(Prize) |>
      summarise(benchmark_pop = mean(pop_share_row, na.rm = TRUE), .groups = "drop")
    ps <- prize_stats |> left_join(pop_bench, by = "Prize")
    ps$excess_pop <- ps$home_share - ps$benchmark_pop
    write.csv(ps |> arrange(desc(excess_pop)) |>
                select(Prize, AwardingCountry, n_winners, home_share, benchmark_pop, excess_pop),
              file.path(intermediate_dir, "r2_2_prize_home_bias.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  }

  # recognition-weighted: each recognition contributes 1 actual home award if the
  # laureate had an awarding-country affiliation by the award year, and its pool
  # share as the expected number under the null
  by_cc <- w |>
    filter(!is.na(pop_share_row), !is.na(home_hit_prior)) |>
    group_by(AwardingCountry, home_cc) |>
    summarise(n_awards = n(),
              actual = sum(home_hit_prior),
              expected = sum(pop_share_row),
              .groups = "drop") |>
    arrange(desc(actual - expected))
  by_cc <- if (pool$sfx == "") {
    by_cc |> mutate(actual_share = actual / n_awards, expected_share = expected / n_awards) |>
      select(AwardingCountry, n_awards, actual, expected, actual_share, expected_share)
  } else by_cc |> select(actual, expected)
  write.csv(by_cc, file.path(intermediate_dir, paste0("r2_2_home_awards_by_country", pool$sfx, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}
