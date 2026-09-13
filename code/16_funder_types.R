# ==============================================================================
# 16_funder_types.R
# Generates: r2_5_macros.tex -- who finances the 99 prizes (funder type at
#            founding and today, by founding era), whether privately funded
#            prizes tilt toward applied fields, and the size of the purse
#            against federal R&D; plus two intermediate files read later:
#   output/intermediate/r2_5_funder_by_prize.csv   funder types per prize
#   output/intermediate/prize_awarding_countries.csv awarding body and country
#
# Both come from the institution recode: data/institution_recode/<slug>.json,
# one reviewed record per prize with the awarding organization, its type and
# country, the funder at founding and today, sources and a confidence grade.
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

# ---- awarding organization and country (the home-bias analysis's input) ----
# The recode records a seat for every prize, even supranational ones (the IMU
# secretariat is in Berlin, so the Fields Medal's country is Germany). For home
# bias that seat is meaningless, so a prize whose awarding_org_type is
# International is written with AwardingCountry = "International", which the
# home-bias script maps to NA and drops.
awarding <- bind_rows(lapply(seq_along(recode), function(i) {
  d <- recode[[i]]
  data.frame(Prize = roster$prize[i], AwardingOrg = d$awarding_org,
             AwardingOrgType = d$awarding_org_type,
             AwardingCountry = if (d$awarding_org_type == "International") "International" else d$awarding_org_country,
             Confidence = d$confidence, stringsAsFactors = FALSE)
}))
stopifnot(nrow(awarding) == 99)
con <- file(file.path(intermediate_dir, "prize_awarding_countries.csv"), open = "w", encoding = "UTF-8")
write.csv(awarding, con, row.names = FALSE); close(con)

# ---- funder types ----
ft <- bind_rows(lapply(seq_along(recode), function(i) {
  d <- recode[[i]]
  data.frame(Prize = roster$prize[i], origin = d$funder_origin_type,
             current = d$funder_current_type, changed = isTRUE(d$funder_changed),
             Confidence = d$confidence, judgment = isTRUE(d$judgment_call),
             n_sources = length(d$sources), stringsAsFactors = FALSE)
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

TYPES <- c("Philanthropy", "Corporate", "Society", "Government", "University", "Mixed")
stopifnot(!any(c(ft$origin, ft$current) == "International"))
cat("== ORIGIN funder type by founding era ==\n")
tab <- table(x$era, factor(x$origin, levels = TYPES)); print(tab)
era <- x |> group_by(era) |> summarise(n = n(), private = mean(private_origin), .groups = "drop")
print(era |> mutate(private = round(private, 2)))
cat("\n== funder changed hands since founding:", sum(x$changed), "of 99 ==\n")

# ---- applied tilt: recognition-weighted mean of the patent-cited share of the
# winner's field, by CURRENT funder type (the measure from script 13) ----
MEAS <- c("ros_cited_share")
bs <- read.csv(file.path(intermediate_dir, "r2_3_field_measures.csv")) |> select(field, all_of(MEAS))
w <- winners_by_group() |>
  inner_join(bs, by = c("finest_group" = "field"))
w$PrizeKey <- ifelse(w$Prize == "Frontiers of Knowledge Award in Economics, Finance and Management",
                     "Frontiers of Knowledge Award in Economics Finance and Management", w$Prize)
w <- inner_join(w, x |> select(Prize, current, private_now, year1), by = c("PrizeKey" = "Prize"))
w$grp <- ifelse(w$private_now, "private", "other")
w$era2 <- cut(w$year1, c(-Inf, 1999, Inf), labels = c("pre-2000", "2000+"))
# the patent-cited share is reported in percent
msum <- function(d) summarise(d, events = n(), across(all_of(MEAS), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(ros_cited_share = 100 * ros_cited_share)
tilt  <- w |> group_by(current) |> msum()
tiltp <- w |> group_by(grp) |> msum()
tilt2 <- w |> group_by(grp, era2) |> msum()
cat("\n== recognition-weighted mean patent-cited share (percent) by CURRENT funder type ==\n")
print(tilt |> mutate(across(where(is.double), ~round(.x, 3))))
print(tiltp |> mutate(across(where(is.double), ~round(.x, 3))))

# ---- how much money is this? total yearly purse against federal R&D ----
# Total federal R&D and R&D plant obligations, FY2023, $192.1 billion (NCSES,
# Science & Engineering Indicators, NSB-2025-7, "Federal Support for U.S. R&D").
FED_RD_FY2023_USD <- 192.1e9
purse_total <- sum(pl$moneyPerYear, na.rm = TRUE)
cat(sprintf("\n== total yearly purse of the 99 prizes: USD %.1f million = %.3f%% of FY2023 federal R&D obligations ==\n",
            purse_total / 1e6, 100 * purse_total / FED_RD_FY2023_USD))

# ---- how the patent-cited share relates to density (from script 13) ----
cors  <- read.csv(file.path(intermediate_dir, "r2_3_correlations.csv"))
ros_dens <- function(s, col) cors[[col]][cors$measure == "ros_cited_share" & cors$sample == s]

write.csv(x |> select(Prize, funder_origin_type = origin, funder_current_type = current,
                      funder_changed = changed, Confidence, year1, era),
          file.path(intermediate_dir, "r2_5_funder_by_prize.csv"), row.names = FALSE)

# ---- the yearly-money share by funder type over time ----
# For each year, the prizes already founded by then and the yearly money they
# distribute (current purse values), split by the type of funder that FOUNDED
# each prize ("origin") and by today's funder ("current"; only its 2025 endpoint
# is quoted, because the recode has no year of change).
years <- 1900:2025
series <- bind_rows(lapply(c("origin", "current"), function(k) {
  bind_rows(lapply(years, function(y) {
    d <- x[x$year1 <= y, ]
    if (nrow(d) == 0) return(NULL)
    d |> group_by(type = .data[[k]]) |>
      summarise(n = n(), money = sum(moneyPerYear, na.rm = TRUE), .groups = "drop") |>
      mutate(year = y, share = money / sum(money), share_n = n / sum(n), basis = k)
  }))
})) |>
  tidyr::complete(basis, year, type = TYPES, fill = list(n = 0, money = 0, share = 0, share_n = 0)) |>
  mutate(type = factor(type, levels = TYPES),
         basis = factor(basis, levels = c("origin", "current"),
                        labels = c("Funder at founding", "Funder today")))
priv_share <- series |> filter(basis == "Funder at founding", type %in% PRIV) |>
  group_by(year) |> summarise(s = sum(share), .groups = "drop")
paid_now <- series |> filter(basis == "Funder today", year == 2025, type %in% PRIV) |> pull(share) |> sum()
cat(sprintf("private share of yearly money by FOUNDING funder: 1950 %.0f%%, 2000 %.0f%%, 2025 %.0f%%; paid by private funders TODAY: %.0f%%\n",
            100 * priv_share$s[priv_share$year == 1950], 100 * priv_share$s[priv_share$year == 2000],
            100 * priv_share$s[priv_share$year == 2025], 100 * paid_now))

# ---- macros ----
pct <- function(v) sprintf("%.0f", 100 * v)
SUF <- c(ros_cited_share = "Ros")
FMT <- c(ros_cited_share = "%.1f")
tilt_macros <- unlist(lapply(MEAS, function(m) {
  f <- function(v) sprintf(FMT[[m]], v)
  g <- function(t) f(tilt[[m]][tilt$current == t])
  c(mac(paste0("fTiltPriv", SUF[[m]]),  f(tiltp[[m]][tiltp$grp == "private"])),
    mac(paste0("fTiltOther", SUF[[m]]), f(tiltp[[m]][tiltp$grp == "other"])),
    mac(paste0("fTiltGov", SUF[[m]]), g("Government")), mac(paste0("fTiltSoc", SUF[[m]]), g("Society")),
    mac(paste0("fTiltPhil", SUF[[m]]), g("Philanthropy")), mac(paste0("fTiltCorp", SUF[[m]]), g("Corporate")),
    mac(paste0("fTiltPrivPre", SUF[[m]]),  f(tilt2[[m]][tilt2$grp == "private" & tilt2$era2 == "pre-2000"])),
    mac(paste0("fTiltPrivPost", SUF[[m]]), f(tilt2[[m]][tilt2$grp == "private" & tilt2$era2 == "2000+"])))
}))
chg <- x[x$changed, ]
write_tex(c(
  "% generated by code/16_funder_types.R -- do not edit by hand",
  mac("fPrivPreEighty", pct(era$private[era$era == "pre-1980"])),
  mac("fPrivEighties", pct(era$private[era$era == "1980-1999"])),
  mac("fPrivPostTwoK", pct(era$private[era$era == "2000+"])),
  mac("fSocPreEighty", tab["pre-1980", "Society"]), mac("fSocPostTwoK", tab["2000+", "Society"]),
  mac("fCorpPreEighty", tab["pre-1980", "Corporate"]), mac("fCorpPostTwoK", tab["2000+", "Corporate"]),
  mac("fGovPostTwoK", tab["2000+", "Government"]),
  mac("fChanged", sum(x$changed)),
  tilt_macros,
  mac("fEvents", nrow(w)),
  mac("fPurseTotalM", sprintf("%.0f", purse_total / 1e6)),
  mac("fPurseOverFederalPct", sprintf("%.2f", 100 * purse_total / FED_RD_FY2023_USD)),
  mac("fRosDensRho", sprintf("%+.2f", ros_dens("all", "spearman"))),
  mac("fRosDensP",   sprintf("%.3f", ros_dens("all", "p_spearman"))),
  mac("fRosDensRhoSci", sprintf("%+.2f", ros_dens("science", "spearman"))),
  mac("fRosDensPSci",   sprintf("%.3f", ros_dens("science", "p_spearman"))),
  mac("fMoneyPrivNineteenFifty", sprintf("%.0f", 100 * priv_share$s[priv_share$year == 1950])),
  mac("fMoneyPrivTwoK", sprintf("%.0f", 100 * priv_share$s[priv_share$year == 2000])),
  mac("fMoneyPrivNow", sprintf("%.0f", 100 * priv_share$s[priv_share$year == 2025])),
  mac("fMoneyPaidPrivNow", sprintf("%.0f", 100 * paid_now)),
  # coverage of the coding itself
  mac("fConfHigh", sum(ft$Confidence == "high")),
  mac("fConfMedium", sum(ft$Confidence == "medium")),
  mac("fConfLow", sum(ft$Confidence == "low")),
  mac("fJudgmentCalls", sum(ft$judgment)),
  mac("fSourcesMedian", sprintf("%.0f", median(ft$n_sources))),
  # era sizes
  mac("fNPreEighty", era$n[era$era == "pre-1980"]),
  mac("fNEighties", era$n[era$era == "1980-1999"]),
  mac("fNPostTwoK", era$n[era$era == "2000+"]),
  mac("fMedianYear", sprintf("%.0f", median(x$year1))),
  # net movements among the prizes whose funder changed
  mac("fChgPrivToOther", sum(chg$private_origin & !chg$private_now)),
  mac("fChgOtherToPriv", sum(!chg$private_origin & chg$private_now)),
  mac("fChgWithin", sum(chg$private_origin == chg$private_now))
), file.path(table_dir, "r2_5_macros.tex"))
cat("wrote r2_5_macros.tex\n")
