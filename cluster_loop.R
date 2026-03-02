library(dbscan)
library(tidyverse)
library(sf)

setwd("C:\\Users\\fatima.garcia\\Desktop\\InterTradeIreland")

final <- read_csv("Irishdata_sectors_MD.csv")

# Force numeric
final$Longitude <- as.numeric(final$Longitude)
final$Latitude  <- as.numeric(final$Latitude)

# Remove invalid coordinates
final <- final %>%
  filter(
    is.finite(Longitude),
    is.finite(Latitude),
    !(Longitude == 0 & Latitude == 0)   # REMOVE 0,0 firms
  )

cat("Remaining rows after removing 0,0:", nrow(final), "\n")


sector_minpts <- c(
  Adv_Man = 5,
  Adv_Mat_Comp = 5,
  Aerospace_Security = 5,
  Agr_Food = 5,
  AI = 5,
  Biotech_LS = 5,
  CleanTech = 5,
  Constrc = 5,
  Creative = 10,
  Cyber = 5,
  Electr_Man = 5,
  Energy_Stg_H = 5,
  Fintech = 5,
  Food_Bev_Man = 5,
  HP_Computing_Data = 5,
  Digital = 10,
  Immer_Spatial = 5,
  IndAut_Robotics = 5,
  Marine_Mar = 5,
  Medtech_Diags = 5,
  Pharma_Biph = 5,
  Photonics = 5,
  Renewable_Energy = 5,
  Screen_Ind = 5,
  Semiconductors = 5,
  Software = 5,
  Space = 5,
  Transp_Mobility = 5,
  ProfFinTech_Services = 10,
  Engineering = 10)


############################################################
#  RUN HDBSCAN FOR LARGE SECTORS
############################################################

large_sectors <- c("ProfFinTech_Services", "Engineering")

for (col in large_sectors) {
  
  cat("\nFixing duplicates in:", col, "\n")
  
  sector_index <- which(final[[col]] == 1)
  
  coords <- final[sector_index, c("Longitude","Latitude")]
  
  dup_mask <- duplicated(coords) | duplicated(coords, fromLast = TRUE)
  
  cat("Duplicates found:", sum(dup_mask), "\n")
  
  # Jitter duplicates slightly (small amount in degrees)
  final$Longitude[sector_index[dup_mask]] <-
    jitter(final$Longitude[sector_index[dup_mask]], amount = 0.00001)
  
  final$Latitude[sector_index[dup_mask]] <-
    jitter(final$Latitude[sector_index[dup_mask]], amount = 0.00001)
}

############################################################
#  RUN HDBSCAN FOR ALL SECTORS
############################################################

for (col in names(sector_minpts)) {
  
  cat("\n====================================\n")
  cat("Processing:", col, "\n")
  
  min_pts_value <- sector_minpts[[col]]
  
  # Convert 0 → NA
  final[[col]][final[[col]] == 0] <- NA
  
  sector_index <- which(!is.na(final[[col]]))
  n_sector <- length(sector_index)
  
  cat("Sector size:", n_sector, "\n")
  
  if (n_sector < min_pts_value) {
    cat("Too few observations → skipping\n")
    next
  }
  
  coords <- final[sector_index, c("Longitude","Latitude")]
  
  coords <- coords[complete.cases(coords), ]
  
  if (nrow(coords) < min_pts_value) {
    cat("Too few valid coordinates → skipping\n")
    next
  }
  
  cat("Running HDBSCAN on", nrow(coords), "rows\n")
  
  cl <- tryCatch({
    hdbscan(coords, minPts = min_pts_value)
  }, error = function(e) {
    cat("Error in sector:", col, "\n")
    return(NULL)
  })
  
  if (is.null(cl)) next
  
  final[[col]][sector_index] <- cl$cluster
  
  cat("Finished:", col, "\n")
}

sector_cols <- c(
  "Adv_Man","Adv_Mat_Comp","Aerospace_Security","Agr_Food","AI",
  "Biotech_LS","CleanTech","Constrc","Creative","Cyber","Electr_Man",
  "Energy_Stg_H","Fintech","Food_Bev_Man","HP_Computing_Data","Digital",
  "Immer_Spatial","IndAut_Robotics","Marine_Mar","Medtech_Diags",
  "Pharma_Biph","Photonics","Renewable_Energy","Screen_Ind",
  "Semiconductors","Software","Space","Transp_Mobility",
  "ProfFinTech_Services","Engineering"
)

final <- final %>%
  mutate(across(any_of(sector_cols), ~ as.integer(.)))
readr::write_excel_csv(final, "Irishdata_sectors_MD.csv", na = "")


