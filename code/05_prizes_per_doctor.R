# ==============================================================================
# 05_prizes_per_doctor.R
# Generates: Table 2 (prizesByField.tex)
# ==============================================================================
source("_helpers.R")

prizeList <- readxl::read_excel(file.path(data_dir, "cleanPrizeList.xlsx"))
prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)

prizeList <- prizeList %>%
  mutate(Tier = case_when(cumWinners <= 0.1 * totalWinners ~ 1,
                          cumWinners <= 0.3 * totalWinners ~ 2,
                          TRUE ~ 3),
         pcaRatingNormRound = round(pcaRatingNorm, 1))

nsf2023 <- readxl::read_excel(file.path(data_dir, "nsf2023.xlsx")) %>%
  filter(!(plotFinestField %in% c("Humanities", "Education", "Other non-science")))

nsf2023 <- nsf2023 %>%
  mutate(`Field of doctorate` = ifelse(`Field of doctorate` == "Industrial engineering and operations research",
    "Engineering, other", `Field of doctorate`),
    plotFinestField = ifelse(plotFinestField == "Industrial engineering and operations research",
      "Engineering, other", plotFinestField)) %>%
  group_by(`Field of doctorate`, nsfBroadField, plotPosition, plotFinestField, PrizeField) %>%
  dplyr::summarize(DoctoratesIn2023 = sum(DoctoratesIn2023), .groups = "drop")

nsf2023 <- nsf2023 %>% group_by(plotFinestField) %>%
  summarise(`Doctoral Degrees in 2023` = sum(DoctoratesIn2023, na.rm = TRUE),
            plotPosition = first(plotPosition))

prize_list_doc <- merge(nsf2023, prizeList, by = "plotFinestField", all = TRUE)

prizesPerDoc <- prize_list_doc %>%
  group_by(plotFinestField) %>%
  summarise(
    `Prizes` = n(),
    `Yearly Winners` = sum(`Yearly Winners`, na.rm = TRUE),
    `Average Award Rank` = mean(pcaRank, na.rm = TRUE, weights = `Yearly Winners`),
    `Doctoral Degrees in 2023 (000's in the US)` = mean(`Doctoral Degrees in 2023`, na.rm = TRUE) / 1000
  )

prizesPerDoc <- prizesPerDoc %>%
  mutate(
    `Prizes` = if_else(plotFinestField %in% c("Psychology", "Engineering, other"),
      NA_integer_, `Prizes`),
    `Yearly Winners` = if_else(plotFinestField %in% c("Psychology", "Engineering, other"),
      NA_real_, `Yearly Winners`),
    `Average Award Rank` = if_else(plotFinestField %in% c("Psychology", "Engineering, other"),
      NA_real_, `Average Award Rank`)
  )

prizesPerDoc$`Prizes per thousand degrees` <-
  prizesPerDoc$`Yearly Winners` / prizesPerDoc$`Doctoral Degrees in 2023 (000's in the US)`

prizesPerDoc$plotFinestField <- gsub("&", "and", prizesPerDoc$plotFinestField)

prizesPerDoc$`Broad Field` <- case_when(
  prizesPerDoc$plotFinestField %in% c("Math", "Chemistry", "Physics and Astronomy") ~ "Math and Physical Sciences",
  prizesPerDoc$plotFinestField %in% c("Environment", "Multidisciplinary") ~ "Environment and Multidisciplinary",
  prizesPerDoc$plotFinestField %in% c("Business and Econ", "Other Social Sciences") ~ "Social Sciences",
  prizesPerDoc$plotFinestField %in% c("Life Sciences and Medicine") ~ "Life and Health Sciences",
  prizesPerDoc$plotFinestField %in% c("Psychology") ~ "Psychology",
  prizesPerDoc$plotFinestField %in% c("Computing, electrical", "Earth Sciences",
    "Materials and mining engineering", "Bioengineering and biomedical engineering",
    "Mechanical engineering", "Civil, environmental, and transportation engineering",
    "Ag. and natural resources", "Engineering, other") ~ "Applied Sciences",
  TRUE ~ prizesPerDoc$plotFinestField
)

prizesPerDocBroad <- prizesPerDoc %>% group_by(`Broad Field`) %>%
  summarise(Prizes = sum(`Prizes`, na.rm = TRUE),
            `Yearly Winners` = sum(`Yearly Winners`, na.rm = TRUE),
            `Doctoral Degrees in 2023 (000's in the US)` = sum(`Doctoral Degrees in 2023 (000's in the US)`, na.rm = TRUE),
            `Prizes per thousand degrees` = sum(`Yearly Winners`, na.rm = TRUE) / sum(`Doctoral Degrees in 2023 (000's in the US)`, na.rm = TRUE),
            `Average Award Rank` = mean(`Average Award Rank`, na.rm = TRUE, weights = `Yearly Winners`))

colnames(prizesPerDocBroad)[1] <- "plotFinestField"
prizesPerDoc <- prizesPerDoc %>% select(-`Broad Field`)
prizesPerDoc <- rbind(prizesPerDoc, prizesPerDocBroad)

prizesPerDoc <- prizesPerDoc %>% add_row(
  plotFinestField = "Total",
  `Prizes` = nrow(prizeList),
  `Yearly Winners` = totalWinners,
  `Doctoral Degrees in 2023 (000's in the US)` = sum(nsf2023$`Doctoral Degrees in 2023`, na.rm = TRUE) / 1000,
  `Prizes per thousand degrees` = totalWinners / 57.862,
  `Average Award Rank` = mean(prizeList$pcaRank, na.rm = TRUE, weights = `Yearly Winners`))

prizesPerDoc <- prizesPerDoc %>%
  filter(!(plotFinestField %in% c("Environment", "Multidisciplinary", "Life Sciences and Medicine")))

prizesPerDoc$sort <- factor(prizesPerDoc$plotFinestField,
  levels = c("Math and Physical Sciences", "Math", "Physics and Astronomy", "Chemistry",
    "Applied Sciences", "Earth Sciences", "Ag. and natural resources",
    "Materials and mining engineering", "Bioengineering and biomedical engineering",
    "Computing, electrical", "Civil, environmental, and transportation engineering",
    "Mechanical engineering", "Engineering, other",
    "Life and Health Sciences", "Life Sciences and Medicine",
    "Psychology",
    "Social Sciences", "Business and Econ", "Other Social Sciences",
    "Environment and Multidisciplinary", "Total"))

prizesPerDoc <- prizesPerDoc[order(prizesPerDoc$sort), ]

prizesPerDoc$plotFinestField <- ifelse(
  prizesPerDoc$plotFinestField %in% c("Math and Physical Sciences", "Applied Sciences",
    "Life and Health Sciences", "Psychology", "Social Sciences",
    "Environment and Multidisciplinary", "Total"),
  prizesPerDoc$plotFinestField,
  paste("\\hspace{3mm}", prizesPerDoc$plotFinestField))

prizesPerDoc$`Yearly Winners` <- round(prizesPerDoc$`Yearly Winners`, 1)
prizesPerDoc$`Doctoral Degrees in 2023 (000's in the US)` <- round(prizesPerDoc$`Doctoral Degrees in 2023 (000's in the US)`, 1)
prizesPerDoc$`Prizes per thousand degrees` <- round(prizesPerDoc$`Prizes per thousand degrees`, 1)
prizesPerDoc$`Average Award Rank` <- round(prizesPerDoc$`Average Award Rank`, 1)

prizesPerDoc$`Prizes per thousand degrees`[prizesPerDoc$`Prizes per thousand degrees` == Inf] <- NA
prizesPerDoc$`Doctoral Degrees in 2023 (000's in the US)`[prizesPerDoc$`Doctoral Degrees in 2023 (000's in the US)` == 0] <- NA

prizesPerDoc <- prizesPerDoc %>%
  dplyr::select(-sort, -`Prizes per thousand degrees`) %>%
  rename(`Avg. Rank` = `Average Award Rank`) %>%
  rename(`Awards/year` = `Yearly Winners`) %>%
  select(-`Doctoral Degrees in 2023 (000's in the US)`)

prizesPerDoc <- prizesPerDoc[!duplicated(prizesPerDoc$plotFinestField), ]

prizesPerDocTable <- stargazer(prizesPerDoc, summary = FALSE, digits = 1, type = "latex", rownames = FALSE,
                               title = "Awards By Field", label = "byField")

prizesPerDocTable <- gsub("\\textbackslash hspace\\{3mm\\}", "\\hspace{3mm}", prizesPerDocTable, fixed = TRUE)
prizesPerDocTable <- gsub("ccc", "lcc", prizesPerDocTable, fixed = TRUE)
prizesPerDocTable <- gsub("NA", "--", prizesPerDocTable, fixed = TRUE)
prizesPerDocTable <- gsub("NaN", "--", prizesPerDocTable, fixed = TRUE)
prizesPerDocTable <- gsub("Total", "\\hline Total", prizesPerDocTable, fixed = TRUE)
prizesPerDocTable <- gsub("plotFinestField", "Field", prizesPerDocTable)
prizesPerDocTable <- gsub("Life Sciences and Medicine", "Life and Health Sciences", prizesPerDocTable)
prizesPerDocTable <- gsub("Math ", "Mathematics ", prizesPerDocTable)
prizesPerDocTable <- gsub("Materials and mining engineering", "Materials and Mining Eng.", prizesPerDocTable)
prizesPerDocTable <- gsub("Bioengineering and biomedical engineering", "Bio and Biomedical Eng.", prizesPerDocTable)
prizesPerDocTable <- gsub("Computing, electrical", "Computing, Electrical", prizesPerDocTable)
prizesPerDocTable <- gsub("Civil, environmental, and transportation engineering", "Civil, Env., and Transp. Eng.", prizesPerDocTable)
prizesPerDocTable <- gsub("Mechanical engineering", "Mechanical Eng.", prizesPerDocTable)
prizesPerDocTable <- gsub("Business and Econ", "Business and Economics", prizesPerDocTable)
prizesPerDocTable <- gsub("Ag. and natural resources", "Ag. and Natural Resources", prizesPerDocTable)

note_text <- "\\noindent \\justify \\footnotesize \\textit{Note}: This table summarizes the number of prizes, recognition events per year, and average rank (PCA-based) for the 99 most prestigious prizes, grouped by field."
prizesPerDocTable <- gsub("\\end{table}",
                          paste0(note_text, "\n\\end{table}"),
                          prizesPerDocTable, fixed = TRUE)

write_tex(prizesPerDocTable, file.path(table_dir, "prizesByField.tex"))
