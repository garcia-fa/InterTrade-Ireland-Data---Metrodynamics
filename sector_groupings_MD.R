library(dplyr)
library(stringr)
library(dbscan)
library(readr)
library(readxl)

# ==========================================================
# Setup
# ==========================================================
setwd("C:\\Users\\fatima.garcia\\Desktop\\InterTradeIreland")

final <- read_csv("FullIrelandData.csv")

# ==========================================================
# Advanced Precision Manufacturing
# ==========================================================
Adv_Prec_Man <- c("RTIC0065")

rtic_pattern <- paste0("(^|,)(", paste(Adv_Prec_Man, collapse = "|"), ")")

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Adv_Man    = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Advanced Materials and Composites
# ==========================================================
Adv_Mat_Comp <- c("RTIC0034")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Adv_Mat_Comp, collapse = "|"),
  ")[^,]*"
)

final <- final %>%
  mutate(
    RTIC_clean   = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Adv_Mat_Comp = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Aerospace and Security
# ==========================================================
Aerospace_Security_RTICs <- c(
  "RTIC006516", "RTIC009601", "RTIC009602",
  "RTIC009603", "RTIC009604"
)

Aerospace_Security_SICs <- c("3030", "3040", "2540")

sic_pattern <- paste0(
  "(^|,)(",
  paste(Aerospace_Security_SICs, collapse = "|"),
  ")"
)

# Separate 8-digit and 10-digit codes
rtic_8  <- Aerospace_Security_RTICs[nchar(Aerospace_Security_RTICs) == 8]
rtic_10 <- Aerospace_Security_RTICs[nchar(Aerospace_Security_RTICs) == 10]

# Build regex components
pattern_8 <- if (length(rtic_8) > 0) {
  paste0("(", paste(rtic_8, collapse = "|"), ")[^,]*")
} else NULL

pattern_10 <- if (length(rtic_10) > 0) {
  paste0("(", paste(rtic_10, collapse = "|"), ")(,|$)")
} else NULL

# Combine safely
rtic_pattern <- paste0(
  "(^|,)",
  paste(na.omit(c(pattern_10, pattern_8)), collapse = "|")
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Aerospace_Security = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Agritech & FoodTech
# ==========================================================
Agr_Food <- c("RTIC0001", "RTIC0057")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Agr_Food, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Agr_Food   = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Artificial Intelligence
# ==========================================================
AI <- c("RTIC0095")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(AI, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    AI         = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Biotechnology and Life Sciences
# ==========================================================
Biotech_LS_RTICs <- c("RTIC0078")
Biotech_LS_SICs  <- c("7211")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Biotech_LS_RTICs, collapse = "|"),
  ")"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Biotech_LS_SICs, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Biotech_LS = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Clean Technology
# ==========================================================
CleanTech <- c("RTIC0047")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(CleanTech, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    CleanTech  = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Construction and Built Environment Technologies
# ==========================================================
Constr_RTICs <- c("RTIC0063", "RTIC002005")
Constr_SICs  <- c("41", "42", "43")

# Split by length
rtic_8  <- Constr_RTICs[nchar(Constr_RTICs) == 8]
rtic_10 <- Constr_RTICs[nchar(Constr_RTICs) == 10]

# Build RTIC patterns
pattern_8 <- if (length(rtic_8) > 0) {
  paste0("(", paste(rtic_8, collapse = "|"), ")[^,]*")
} else NULL

pattern_10 <- if (length(rtic_10) > 0) {
  paste0("(", paste(rtic_10, collapse = "|"), ")(,|$)")
} else NULL

rtic_pattern <- paste0(
  "(^|,)",
  paste(na.omit(c(pattern_10, pattern_8)), collapse = "|")
)

# SIC pattern (prefix logic)
sic_pattern <- paste0(
  "(^|,)(",
  paste(Constr_SICs, collapse = "|"),
  ")\\d*(,|$)"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Constrc    = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Creative Industries
# ==========================================================
Creative_SICs <- c(
  "3212", "58", "59", "60", "6201", "6202",
  "7021", "7111", "731", "741", "742", "743",
  "8552", "90", "9101", "9102"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Creative_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    SIC_clean = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Creative  = as.integer(str_detect(SIC_clean, sic_pattern))
  ) %>%
  select(-SIC_clean)

# ==========================================================
# Cyber Security
# ==========================================================
Cyber <- c("RTIC0006")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Cyber, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Cyber      = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Electronics Manufacturing
# ==========================================================
Electr_Man_RTICs <- c("RTIC0067")
Electr_Man_SICs  <- c("26", "27")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Electr_Man_RTICs, collapse = "|"),
  ")[^,]*"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Electr_Man_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Electr_Man = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Energy Storage and Hydrogen
# ==========================================================
Energy_Stg_H <- c("RTIC0013", "RTIC0097", "RTIC001103")

rtic_8  <- Energy_Stg_H[nchar(Energy_Stg_H) == 8]
rtic_10 <- Energy_Stg_H[nchar(Energy_Stg_H) == 10]

pattern_8 <- if (length(rtic_8) > 0) {
  paste0("(", paste(rtic_8, collapse = "|"), ")[^,]*")
} else NULL

pattern_10 <- if (length(rtic_10) > 0) {
  paste0("(", paste(rtic_10, collapse = "|"), ")(,|$)")
} else NULL

rtic_pattern <- paste0(
  "(^|,)",
  paste(na.omit(c(pattern_10, pattern_8)), collapse = "|")
)

final <- final %>%
  mutate(
    RTIC_clean    = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Energy_Stg_H  = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Fintech
# ==========================================================
Fintech <- c("RTIC0052")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Fintech, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Fintech    = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Food and Beverage Manufacturing
# ==========================================================
Food_Bev_Man <- c("10", "11")

sic_pattern <- paste0(
  "(^|,)(",
  paste(Food_Bev_Man, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    SIC_clean    = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Food_Bev_Man = as.integer(str_detect(SIC_clean, sic_pattern))
  ) %>%
  select(-SIC_clean)

# ==========================================================
# High Performance Computing and Data
# ==========================================================
HP_Computing_Data <- c("RTIC0007", "RTIC0051", "RTIC0087")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(HP_Computing_Data, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean        = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    HP_Computing_Data = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# ICT & Digital Services
# ==========================================================
Digital <- c(
  "261", "262", "263", "263", "264", "268", "465", "582",
  "611", "612", "613", "614", "615", "616", "617", "618",
  "619", "62", "631", "632", "636", "634", "635", "636",
  "637", "638", "639", "951"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Digital, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    SIC_clean = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Digital   = as.integer(str_detect(SIC_clean, sic_pattern))
  ) %>%
  select(-SIC_clean)

# ==========================================================
# Immersive and Spatial Technologies
# ==========================================================
Immer_Spatial <- c("RTIC0008", "RTIC0018")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Immer_Spatial, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean   = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Immer_Spatial = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Industrial Automation and Robotics
# ==========================================================
IndAut_Robotics <- c(
  "RTIC009110", "RTIC009111", "RTIC009112", "RTIC009113",
  "RTIC009119", "RTIC009120", "RTIC009122", "RTIC009124",
  "RTIC009125", "RTIC009126", "RTIC009127", "RTIC009128",
  "RTIC009129", "RTIC009130"
)

rtic_8  <- IndAut_Robotics[nchar(IndAut_Robotics) == 8]
rtic_10 <- IndAut_Robotics[nchar(IndAut_Robotics) == 10]

pattern_8 <- if (length(rtic_8) > 0) {
  paste0("(", paste(rtic_8, collapse = "|"), ")[^,]*")
} else NULL

pattern_10 <- if (length(rtic_10) > 0) {
  paste0("(", paste(rtic_10, collapse = "|"), ")(,|$)")
} else NULL

rtic_pattern <- paste0(
  "(^|,)",
  paste(na.omit(c(pattern_10, pattern_8)), collapse = "|")
)

final <- final %>%
  mutate(
    RTIC_clean      = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    IndAut_Robotics = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Marine and Maritime
# ==========================================================
Marine_Mar_RTICs <- c("RTIC0031")
Marine_Mar_SICs  <- c("0311", "0321", "3011", "3012", "3315", "5010", "5020", "5222", "7734")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Marine_Mar_RTICs, collapse = "|"),
  ")[^,]*"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Marine_Mar_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Marine_Mar = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Medtech & Diagnostics
# ==========================================================
Medtech_Diag <- c("RTIC0058", "RTIC008606", "RTIC008604", "RTIC009115", "RTIC003004", "RTIC003304")

rtic_8  <- Medtech_Diag[nchar(Medtech_Diag) == 8]
rtic_10 <- Medtech_Diag[nchar(Medtech_Diag) == 10]

pattern_8 <- if (length(rtic_8) > 0) {
  paste0("(", paste(rtic_8, collapse = "|"), ")[^,]*")
} else NULL

pattern_10 <- if (length(rtic_10) > 0) {
  paste0("(", paste(rtic_10, collapse = "|"), ")(,|$)")
} else NULL

rtic_pattern <- paste0(
  "(^|,)",
  paste(na.omit(c(pattern_10, pattern_8)), collapse = "|")
)

final <- final %>%
  mutate(
    RTIC_clean    = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Medtech_Diags = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Pharma and Biopharmaceutical
# ==========================================================
Pharma_Biph_RTICs <- c("RTIC0062", "RTIC0083")
Pharma_Biph_SICs  <- c("21", "7211")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Pharma_Biph_RTICs, collapse = "|"),
  ")[^,]*"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Pharma_Biph_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    RTIC_clean  = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean   = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Pharma_Biph = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Photonics
# ==========================================================
Photonics <- c("RTIC0027")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Photonics, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Photonics  = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Professional, Financial and Technical Services
# ==========================================================
ProfFinTech_Services <- c("69", "70", "71", "72", "73", "74", "77", "78", "82")

sic_pattern <- paste0(
  "(^|,)(",
  paste(ProfFinTech_Services, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    SIC_clean = str_remove_all(coalesce(SICs, ""), "\\s+"),
    ProfFinTech_Services = as.integer(str_detect(SIC_clean, sic_pattern))
  ) %>%
  select(-SIC_clean)

# ==========================================================
# Renewable Energy
# ==========================================================
Renewable_Energy <- c(
  "RTIC0011", "RTIC0013", "RTIC0047", "RTIC0055",
  "RTIC0012", "RTIC006704", "RTIC002002", "RTIC008507"
)

rtic_8  <- Renewable_Energy[nchar(Renewable_Energy) == 8]
rtic_10 <- Renewable_Energy[nchar(Renewable_Energy) == 10]

pattern_8 <- if (length(rtic_8) > 0) {
  paste0("(", paste(rtic_8, collapse = "|"), ")[^,]*")
} else NULL

pattern_10 <- if (length(rtic_10) > 0) {
  paste0("(", paste(rtic_10, collapse = "|"), ")(,|$)")
} else NULL

rtic_pattern <- paste0(
  "(^|,)",
  paste(na.omit(c(pattern_10, pattern_8)), collapse = "|")
)

final <- final %>%
  mutate(
    RTIC_clean       = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Renewable_Energy = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Screen Industries
# ==========================================================
Screen_Ind_RTICs <- c("RTIC0101")
Screen_Ind_SICs  <- c("5911", "5912", "5913")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Screen_Ind_RTICs, collapse = "|"),
  ")[^,]*"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Screen_Ind_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Screen_Ind = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Semiconductors
# ==========================================================
Semiconductors <- c("RTIC0099")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Semiconductors, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean     = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Semiconductors = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Software
# ==========================================================
Software_RTICs <- c("RTIC0079", "RTIC0029")
Software_SICs  <- c("5911", "5912", "5913")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Software_RTICs, collapse = "|"),
  ")[^,]*"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Software_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean  = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Software   = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Space Economy
# ==========================================================
Space <- c("RTIC0098")

rtic_pattern <- paste0(
  "(^|,)(",
  paste(Space, collapse = "|"),
  ")"
)

final <- final %>%
  mutate(
    RTIC_clean = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    Space      = as.integer(str_detect(RTIC_clean, rtic_pattern))
  ) %>%
  select(-RTIC_clean)

# ==========================================================
# Transport and Future Mobility
# ==========================================================
Transp_Mobility_RTICs <- c("RTIC003005", "RTIC009124", "RTIC009125", "RTIC009126")
Transp_Mobility_SICs  <- c("49", "50", "51", "52", "53")

# NOTE: In your paste, pattern_10 referenced an undefined object (rtic_10).
# This keeps the same intent (match those RTICs) without changing the approach.
rtic_pattern <- paste0(
  "(^|,)(",
  paste(Transp_Mobility_RTICs, collapse = "|"),
  ")(,|$)"
)

sic_pattern <- paste0(
  "(^|,)(",
  paste(Transp_Mobility_SICs, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    RTIC_clean       = str_remove_all(coalesce(RTICsubsectorsCodes, ""), "\\s+"),
    SIC_clean        = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Transp_Mobility  = as.integer(
      str_detect(RTIC_clean, rtic_pattern) |
        str_detect(SIC_clean, sic_pattern)
    )
  ) %>%
  select(-RTIC_clean, -SIC_clean)

# ==========================================================
# Engineering
# ==========================================================
Engineering <- read_xlsx("Longlist_definitions.xlsx", sheet = 2)
Engineering <- Engineering$SIC

sic_pattern <- paste0(
  "(^|,)(",
  paste(Engineering, collapse = "|"),
  ")\\d*"
)

final <- final %>%
  mutate(
    SIC_clean   = str_remove_all(coalesce(SICs, ""), "\\s+"),
    Engineering = as.integer(str_detect(SIC_clean, sic_pattern))
  ) %>%
  select(-SIC_clean)

# ==========================================================
# Final cleanup & export
# ==========================================================
final <- final[c(-55)]

write_csv(final, "Irishdata_sectors_MD.csv")