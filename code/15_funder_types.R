# ==============================================================================
# 15_funder_types.R
# Generates: r2_5_macros.tex -- the privately funded share of prizes by founding
#            era, the patent-cited share of winners' fields for privately vs
#            otherwise funded prizes, and the total yearly purse against US
#            federal R&D obligations;
#            r2_5_funder_share_by_year.pdf -- the prizes existing in each year
#            split by funder type at founding;
#            plus output/intermediate/prize_awarding_countries.csv (awarding
#            country per prize, read by the home-bias scripts).
# Inputs: data/institution_recode/roster.json and <slug>.json,
#         data/cleanPrizeList.xlsx, output/intermediate/r2_3_field_measures.csv
#         (script 12), and the winner file through winners_by_group().
#
# The awarding country and the funder types come from
# data/institution_recode/<slug>.json, one record per prize with the awarding
# organization, its type and country, and the funder type at founding and in
# the latest award cycle.
# Funder types: Society, Philanthropy, Government, Corporate, University, Mixed
# (no single source above 50% of the purse). International is a geography, not a
# funder type; a prize whose awarding organization is International has no
# national home in the home-bias analysis.
# ==============================================================================
source("_helpers.R")

recode_dir <- file.path(data_dir, "institution_recode")
roster <- fromJSON(file.path(recode_dir, "roster.json"))
recode <- lapply(seq_len(nrow(roster)), function(i)
  fromJSON(file.path(recode_dir, paste0(roster$slug[i], ".json"))))

# ---- awarding country (the home-bias scripts' input) ----
# The recode records a seat for every prize, even supranational ones (the IMU
# secretariat is in Berlin, so the Fields Medal's country is Germany). For home
# bias that seat is meaningless, so a prize whose awarding_org_type is
# International is written with AwardingCountry = "International", which the
# home-bias script maps to NA and drops.
awarding <- bind_rows(lapply(seq_along(recode), function(i) {
  d <- recode[[i]]
  data.frame(Prize = roster$prize[i],
             AwardingCountry = if (d$awarding_org_type == "International") "International" else d$awarding_org_country,
             stringsAsFactors = FALSE)
}))
stopifnot(nrow(awarding) == 99)
con <- file(file.path(intermediate_dir, "prize_awarding_countries.csv"), open = "w", encoding = "UTF-8")
write.csv(awarding, con, row.names = FALSE); close(con)

# ---- funder types ----
ft <- bind_rows(lapply(seq_along(recode), function(i) {
  d <- recode[[i]]
  data.frame(Prize = roster$prize[i], origin = d$funder_origin_type,
             current = d$funder_current_type, stringsAsFactors = FALSE)
}))

pl <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) |>
  select(Prize = `Award Name`, year1 = `year 1st awarded`, moneyPerYear)
# prize names in the recode vs cleanPrizeList
fix <- c("Lasker–DeBakey Clinical Medical Research Award" = "Lasker-DeBakey Clinical Medical Research Award",
         "Balzan Prizes" = "Balzan Prize",
         "Gold Medal for Astronomy" = "Gold Medal of the Royal Astronomical Society",
         "Gödel Prize" = "Godel Prize",
         "Frontiers of Knowledge Award in Economics, Finance and Management" = "Frontiers of Knowledge Award in Economics Finance and Management",
         "Crafoord prize in Polyarthritis" = "Crafoord Prize in Polyarthritis")
pl$Prize <- ifelse(pl$Prize %in% names(fix), fix[pl$Prize], pl$Prize)
x <- inner_join(ft, pl, by = "Prize")
stopifnot(nrow(x) == 99)
PRIV <- c("Philanthropy", "Corporate")
x$private_origin <- x$origin %in% PRIV
x$private_now <- x$current %in% PRIV
x$era <- cut(x$year1, c(-Inf, 1979, 1999, Inf), labels = c("pre-1980", "1980-1999", "2000+"))

era <- x |> group_by(era) |> summarise(n = n(), private = mean(private_origin), .groups = "drop")

# ---- patent-cited share of the winners' fields (script 12), recognition-
# weighted, for prizes privately funded in the latest award cycle vs the rest ----
bs <- read.csv(file.path(intermediate_dir, "r2_3_field_measures.csv")) |> select(field, ros_cited_share)
w <- winners_by_group() |>
  inner_join(bs, by = c("finest_group" = "field"))
w$PrizeKey <- ifelse(w$Prize == "Frontiers of Knowledge Award in Economics, Finance and Management",
                     "Frontiers of Knowledge Award in Economics Finance and Management", w$Prize)
w <- inner_join(w, x |> select(Prize, current, private_now), by = c("PrizeKey" = "Prize"))
w$grp <- ifelse(w$private_now, "private", "other")
# the patent-cited share is reported in percent
tiltp <- w |> group_by(grp) |>
  summarise(ros_cited_share = 100 * mean(ros_cited_share, na.rm = TRUE), .groups = "drop")

# ---- total yearly purse against federal R&D ----
# Total federal R&D and R&D plant obligations, FY2023, $192.1 billion (NCSES,
# Science & Engineering Indicators, NSB-2025-7, "Federal Support for U.S. R&D").
FED_RD_FY2023_USD <- 192.1e9
purse_total <- sum(x$moneyPerYear, na.rm = TRUE)

# ---- prizes existing in each year, by funder type at founding ----
# For each year 1900-2025, the share of the prizes already founded whose
# founding funder was of each type.
TYPES <- c("Philanthropy", "Corporate", "Society", "Government", "University", "Mixed")
stopifnot(!any(c(ft$origin, ft$current) == "International"))
years <- 1900:2025
series <- bind_rows(lapply(years, function(y) {
  d <- x[x$year1 <= y, ]
  if (nrow(d) == 0) return(NULL)
  d |> group_by(type = origin) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(year = y, share_n = n / sum(n))
})) |>
  tidyr::complete(year, type = TYPES, fill = list(n = 0, share_n = 0)) |>
  mutate(type = factor(type, levels = TYPES))

# ---- figure: the prizes existing in each year by funder type at founding ----
# Philanthropy and Corporate are the first two factor levels, so geom_area
# stacks them at the top and the privately funded share reads as one band.
pal <- c(Philanthropy = "#377eb8", Corporate = "#e41a1c", Society = "#4daf4a",
         Government = "#ff7f00", University = "#984ea3", Mixed = "#999999")
plot_df <- series |>
  select(year, type, value = share_n) |>
  # legend only for types that actually founded a prize (no University founders)
  filter(type %in% unique(type[value > 0])) |> mutate(type = droplevels(type))
p <- ggplot(plot_df, aes(year, value, fill = type)) +
  geom_area(position = "stack", alpha = .95, colour = "white", linewidth = .15) +
  scale_fill_manual(values = pal, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Share of the top prizes existing in each year,\nby funder at founding") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
save_figure("r2_5_funder_share_by_year.pdf", p, width = 7, height = 4.4)

# ---- macros ----
pct <- function(v) sprintf("%.0f", 100 * v)
write_tex(c(
  "% generated by code/15_funder_types.R -- do not edit by hand",
  mac("fPrivPreEighty", pct(era$private[era$era == "pre-1980"])),
  mac("fPrivEighties", pct(era$private[era$era == "1980-1999"])),
  mac("fPrivPostTwoK", pct(era$private[era$era == "2000+"])),
  mac("fTiltPrivRos", sprintf("%.1f", tiltp$ros_cited_share[tiltp$grp == "private"])),
  mac("fTiltOtherRos", sprintf("%.1f", tiltp$ros_cited_share[tiltp$grp == "other"])),
  mac("fPurseTotalM", sprintf("%.0f", purse_total / 1e6)),
  mac("fPurseOverFederalPct", sprintf("%.2f", 100 * purse_total / FED_RD_FY2023_USD))
), file.path(table_dir, "r2_5_macros.tex"))
