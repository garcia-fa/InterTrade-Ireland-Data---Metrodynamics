# ==========================================================
# Libraries
# ==========================================================
library(tidyverse)
library(readxl)
library(writexl)
library(arrow)
library(stringr)

# ==========================================================
# Set Working Directory
# ==========================================================
setwd("C:\\Users\\FatimaGarcia\\OneDrive - The Data City\\Desktop\\InterTradeIreland\\InterTradeIreland\\TDC_Explore2026-02-03_11h05m56s")

# ==========================================================
# Load Norther Ireland companies download from Industry Engine (in repository)
# ==========================================================
companies  <- read_csv("Companies.csv")
locations  <- read_csv("Locations.csv")

# Keep only registered postcodes
locations <- locations %>%
  filter(IsRegisteredPostcode == TRUE)

# ==========================================================
# Reshape Company Employee & Turnover Data
# ==========================================================
companies <- companies %>%
  pivot_longer(
    cols = c(Reported_Numberofemployees, Reported_Turnover),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from  = c(variable, Yearending),
    values_from = value,
    names_sep   = "_"
  )

# ==========================================================
# Order Employee & Turnover Columns
# ==========================================================
companies <- companies %>%
  relocate(
    {
      emp_cols <- grep("^Reported_Numberofemployees_", names(.), value = TRUE)
      emp_cols[order(
        as.numeric(str_extract(emp_cols, "(?<=_)\\d+$")),
        decreasing = TRUE
      )]
    },
    {
      turn_cols <- grep("^Reported_Turnover_", names(.), value = TRUE)
      turn_cols[order(
        as.numeric(str_extract(turn_cols, "(?<=_)\\d+$")),
        decreasing = TRUE
      )]
    },
    .after = everything()
  )

# Move most recent employee column next to Countryoforigin
if ("Reported_Numberofemployees_2025" %in% names(companies)) {
  companies <- companies %>%
    relocate(Reported_Numberofemployees_2025, .after = Countryoforigin)
}

# Rename Company Name column
names(companies)[2] <- "Companyname"

# Remove unwanted columns (by index)
companies <- companies[, -c(23, 24, 27)]

# ==========================================================
# Join Location Data for NI companies
# ==========================================================
companies <- companies %>%
  left_join(locations, by = "Companynumber") %>%
  filter(!is.na(IsRegisteredPostcode))

# ==========================================================
# Load ROI Companies
# ==========================================================
companies_RoI <- read_parquet("FlatProcessedCompanies_Ireland.parquet") %>%
  select(-28)



# ==========================================================
# Join RTIC Vertical Data for RoI companies
# ==========================================================
rtic_verticals <- read_parquet("Companies_RTIC_IEs_Scores_20260114.parquet") %>%
  rename(CreditsafeID = 1) %>%
  select(1, 2)


verticals_final <- companies_RoI %>%
  left_join(rtic_verticals, by = "CreditsafeID")

# ==========================================================
# Clean RTIC verticals for RoI (same column separated by commas)
# ==========================================================
verticals_final <- verticals_final %>%
  group_by(Companynumber) %>%
  summarise(
    RTICsubsectorCodes = RTICsubsectorCodes %>%
      str_c(collapse = ",") %>%
      str_split(",") %>%
      unlist() %>%
      str_trim() %>%
      unique() %>%
      str_c(collapse = ","),
    across(-RTICsubsectorCodes, first),
    .groups = "drop"
  )



# ==========================================================
# Standardise Employee & Turnover Column Names
# ==========================================================
verticals_final <- verticals_final %>%
  rename_with(
    ~ ifelse(
      str_detect(.x, "^Employees_\\d{4}$"),
      paste0("Reported_Numberofemployees_", str_extract(.x, "\\d{4}$")),
      .x
    )
  ) %>%
  rename_with(
    ~ ifelse(
      str_detect(.x, "^Turnover_\\d{4}$"),
      paste0("Reported_Turnover_", str_extract(.x, "\\d{4}$")),
      .x
    )
  )

# Reorder columns again
verticals_final2 <- verticals_final %>%
  relocate(
    {
      emp_cols <- grep("^Reported_Numberofemployees_", names(.), value = TRUE)
      emp_cols[order(as.numeric(sub(".*_", "", emp_cols)), decreasing = TRUE)]
    },
    {
      turn_cols <- grep("^Reported_Turnover_", names(.), value = TRUE)
      turn_cols[order(as.numeric(sub(".*_", "", turn_cols)), decreasing = TRUE)]
    },
    .after = everything()
  )

# ==========================================================
# Combine NI & ROI Data
# ==========================================================
final <- bind_rows(companies, verticals_final)



# ==========================================================
# Export Outputs
# ==========================================================
write_csv(final, "FullIrelandData.csv")
write_csv(final_filtered, "FullIrelandData_NL.csv")