# ==============================================================================
# 18_home_bias.R
# Home bias in prize recognitions (Discussion, "Prizes as indicators of merit").
# For each 2015-2024 recognition, the laureate counts as "home" if the awarding
# country appears among the countries of the institutions on their small-team
# papers (at most 20 authors) up to and including the award year, an institution
# counting if it appears in at least two distinct years and on at least 5% of
# the author's small-team works by then. Hong Kong, Macau and Taiwan are folded
# into China on both sides. The benchmark is the share of the field's reference
# pool -- the 1,000 researchers in the prize's modal OpenAlex field with the most
# citations to small-team papers as of the award year -- affiliated with the
# awarding country by that year. A wider pool (everyone with 10+ citations)
# and two alternative affiliation rules (institutions on at least 10% / 5% of
# works, pools pool_v2_share0.1 / pool_v2_share0.05) are robustness variants.
#
# Inputs: data/all_winners_with_plotFinestField.csv
#         data/openalex/author_affiliations_smallteam.jsonl   (laureate affiliation histories)
#         data/openalex/<pool>/top_pool_field_year.csv, top_pool_field_country_year.csv
#         data/openalex/pop_field_year.csv, pop_field_country_year.csv (10+-citation pool)
#         output/intermediate/prize_awarding_countries.csv    (script 16)
# Outputs (output/intermediate/): r2_2_winner_countries.csv,
#         r2_2_prize_home_bias<sfx>.csv, r2_2_home_awards_by_country<sfx>.csv
#         for sfx = "" (main pool), "_share01", "_share005"
# ==============================================================================
source("_helpers.R")

oa_dir <- file.path(data_dir, "openalex")
POOLS <- list(list(dir = "pool_v2_top1000",    sfx = ""),
              list(dir = "pool_v2_share0.05", sfx = "_share005"),
              list(dir = "pool_v2_share0.1",  sfx = "_share01"))
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
ids <- unique(winners$author_id)
cat("winners:", nrow(winners), "unique authors:", length(ids), "\n")

# ---- author affiliation histories, from the OpenAlex snapshot ----
aff <- lapply(readLines(file.path(oa_dir, "author_affiliations_smallteam.jsonl"), encoding = "UTF-8"),
              function(l) fromJSON(l, simplifyVector = FALSE))
dumped_ids <- vapply(aff, function(a) a$id, character(1))
missing <- setdiff(ids, dumped_ids)
if (length(missing) > 0)
  cat("WARNING:", length(missing), "laureate ids not in the affiliation file (dropped):", paste(missing, collapse = " "), "\n")

aff_years <- function(x) { y <- x$years; if (is.null(names(y))) as.integer(unlist(y)) else as.integer(names(y)) }
aff_counts <- function(x) { y <- x$years; if (is.null(names(y))) rep(NA_real_, length(unlist(y))) else as.numeric(unlist(y)) }
works_upto <- function(a, yr) { w <- a$works_by_year; if (is.null(w)) return(NA_real_); yy <- as.integer(names(w)); sum(as.numeric(unlist(w))[yy <= yr]) }

# per author: one row per (institution country, years)
aff_tab <- bind_rows(lapply(aff, function(a) {
  rows <- lapply(a$affiliations, function(x) {
    cc <- x$institution$country_code
    if (is.null(cc)) return(NULL)
    data.frame(author_id = sub(".*/", "", a$id), country = cc,
               years = I(list(aff_years(x))), counts = I(list(aff_counts(x))),
               n_works = ifelse(is.null(x$n_works), NA_real_, x$n_works),
               n_total = ifelse(is.null(a$n_works_total), NA_real_, a$n_works_total),
               wby = I(list(a$works_by_year)))
  })
  lk <- unique(unlist(lapply(a$last_known_institutions, function(i) i$country_code)))
  out <- bind_rows(rows)
  if (nrow(out) == 0 && length(lk) > 0)
    out <- data.frame(author_id = sub(".*/", "", a$id), country = lk,
                      years = I(rep(list(integer(0)), length(lk))))
  out
}))

# country set for an author in a given year: institutions active that year;
# fallback nearest earlier year, then nearest later, then any listed country
countries_in_year <- function(aid, yr) {
  rows <- aff_tab[aff_tab$author_id == aid, ]
  if (nrow(rows) == 0) return(character(0))
  active <- rows$country[vapply(rows$years, function(y) yr %in% y, logical(1))]
  if (length(active) > 0) return(unique(active))
  gap <- vapply(rows$years, function(y) {
    if (length(y) == 0) return(Inf)
    prev <- y[y <= yr]
    if (length(prev) > 0) yr - max(prev) else 1000 + (min(y) - yr)
  }, numeric(1))
  if (all(is.infinite(gap))) return(unique(rows$country))
  unique(rows$country[gap == min(gap)])
}
winners$countries <- mapply(function(a, y) paste(countries_in_year(a, y), collapse = ";"),
                            winners$author_id, winners$Year)

# MAIN measure: every country the author is observed in up to and including the
# award year, under the two-years / 5%-of-works rule. Institutions whose year
# list is empty come from the last_known_institutions fallback and are retained.
countries_upto <- function(aid, yr) {
  rows <- aff_tab[aff_tab$author_id == aid, ]
  if (nrow(rows) == 0) return(character(0))
  keep <- vapply(seq_len(nrow(rows)), function(i) {
    y <- rows$years[[i]]; cnt <- rows$counts[[i]]; keep <- y <= yr
    n_inst <- if (all(is.na(cnt))) rows$n_works[i] else sum(cnt[keep])
    n_tot <- if (!is.null(rows$wby[[i]])) works_upto(list(works_by_year = rows$wby[[i]]), yr) else rows$n_total[i]
    length(unique(y[keep])) >= COUNTRY_MIN_YEARS &&
      (is.na(n_inst) || is.na(n_tot) || n_inst >= AFF_MIN_SHARE * n_tot)
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
# the unfolded set is kept for the descriptive table (script 21)
winners$countries_prior_raw <- winners$countries_prior
winners$countries <- fold_set(winners$countries)
winners$countries_prior <- fold_set(winners$countries_prior)

# ---- awarding countries (script 16) ----
pc <- read.csv(file.path(intermediate_dir, "prize_awarding_countries.csv"), check.names = FALSE,
               stringsAsFactors = FALSE, encoding = "UTF-8")
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
winners$home_hit <- mapply(hit_of, winners$countries, winners$home_cc)
winners$home_hit_prior <- mapply(hit_of, winners$countries_prior, winners$home_cc)

write.csv(winners |> select(Prize, Year, Winner, author_id, countries, countries_prior,
                            countries_prior_raw, AwardingOrg, AwardingCountry, home_cc,
                            home_hit, home_hit_prior, Best_Field),
          file.path(intermediate_dir, "r2_2_winner_countries.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- per-prize home share vs benchmarks ----
w <- winners |> filter(!is.na(home_hit_prior))
w$in_home <- w$home_hit_prior

# leave-one-prize-out benchmark: among winners of OTHER prizes in the same
# Best_Field, the share whose country set includes this prize's awarding country
bench <- function(prize, field, hc, col = "countries_prior") {
  pool <- w[w$Prize != prize & w$Best_Field == field, ]
  if (nrow(pool) < 20) pool <- w[w$Prize != prize, ]  # small-field fallback: all fields
  pool <- pool[pool[[col]] != "", ]
  mean(vapply(strsplit(pool[[col]], ";"), function(cs) hc %in% cs, logical(1)))
}
used_fallback <- function(prize, field) sum(w$Prize != prize & w$Best_Field == field) < 20

prize_stats <- w |>
  group_by(Prize, AwardingOrg, AwardingCountry, home_cc) |>
  summarise(n_winners = n(),
            home_share = mean(in_home),
            home_share_awardyear = mean(home_hit, na.rm = TRUE),
            main_field = names(sort(table(Best_Field), decreasing = TRUE))[1],
            .groups = "drop")
prize_stats$benchmark <- mapply(bench, prize_stats$Prize, prize_stats$main_field, prize_stats$home_cc, "countries_prior")
prize_stats$excess <- prize_stats$home_share - prize_stats$benchmark
prize_stats$benchmark_awardyear <- mapply(bench, prize_stats$Prize, prize_stats$main_field, prize_stats$home_cc, "countries")
prize_stats$excess_awardyear <- prize_stats$home_share_awardyear - prize_stats$benchmark_awardyear
prize_stats$bench_all_fields <- mapply(used_fallback, prize_stats$Prize, prize_stats$main_field)
w <- w |> left_join(prize_stats |> select(Prize, main_field), by = "Prize")

# population benchmark: a pool is a (field, year) denominator table plus a
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
# robustness pool: everyone with 10+ citations (a much wider net)
pool_c10 <- make_pool(oa_dir, "pop_field_year.csv", "pop_field_country_year.csv", "n_authors")
w$pop_share_row_c10 <- mapply(function(f, cc, y) share_of(pool_c10, f, cc, y), w$main_field, w$home_cc, w$Year)

for (pool in POOLS) {
  pool_top <- make_pool(file.path(oa_dir, pool$dir), "top_pool_field_year.csv", "top_pool_field_country_year.csv", "n_pool")
  w$pop_share_row <- mapply(function(f, cc, y) share_of(pool_top, f, cc, y), w$main_field, w$home_cc, w$Year)

  pop_bench <- w |>
    group_by(Prize) |>
    summarise(benchmark_pop = mean(pop_share_row, na.rm = TRUE),
              benchmark_pop_c10 = mean(pop_share_row_c10, na.rm = TRUE),
              n_pop_rows = sum(!is.na(pop_share_row)), .groups = "drop")
  ps <- prize_stats |> left_join(pop_bench, by = "Prize")
  ps$excess_pop <- ps$home_share - ps$benchmark_pop
  ps$excess_pop_c10 <- ps$home_share - ps$benchmark_pop_c10
  write.csv(ps |> arrange(desc(excess_pop)),
            file.path(intermediate_dir, paste0("r2_2_prize_home_bias", pool$sfx, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")

  # recognition-weighted: each recognition contributes 1 actual home award if the
  # laureate had an awarding-country affiliation by the award year, and its pool
  # share as the expected number under the null
  agg <- w |> filter(!is.na(pop_share_row), !is.na(home_hit_prior))
  cat(sprintf("\n== pool %s: %d recognitions, actual home %d, expected %.1f (%.2fx) ==\n",
              pool$dir, nrow(agg), sum(agg$home_hit_prior), sum(agg$pop_share_row),
              sum(agg$home_hit_prior) / sum(agg$pop_share_row)))
  by_cc <- agg |>
    group_by(AwardingCountry, home_cc) |>
    summarise(n_awards = n(),
              actual = sum(home_hit_prior),
              expected = sum(pop_share_row),
              expected_c10 = sum(pop_share_row_c10),
              .groups = "drop") |>
    mutate(excess_awards = actual - expected,
           ratio = ifelse(expected > 0, actual / expected, NA_real_),
           actual_share = actual / n_awards,
           expected_share = expected / n_awards) |>
    arrange(desc(excess_awards))
  write.csv(by_cc, file.path(intermediate_dir, paste0("r2_2_home_awards_by_country", pool$sfx, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}
