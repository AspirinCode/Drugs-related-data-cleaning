rm(list = ls())

# Add libraries
if("openxlsx" %in% rownames(installed.packages()) == FALSE) {
  install.packages("openxlsx")
}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr")
}
if("tidyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("tidyr")
}
library(openxlsx)
library(dplyr)
library(tidyr)

fileName = "My work.xlsx"

# read sheets
known.associates2 <- read.xlsx(fileName, sheet = 3, startRow = 1, colNames = TRUE)
drug.substructure <- read.xlsx(fileName, sheet = 4, startRow = 1, colNames = TRUE)
drug.sideEffect   <- read.xlsx(fileName, sheet = 5, startRow = 1, colNames = TRUE)
drug.gene         <- read.xlsx(fileName, sheet = 9, startRow = 1, colNames = TRUE)

# Convert the drug names to lower cases
known.associates2$Drug  <- tolower(known.associates2$Drug)
drug.substructure$X1    <- tolower(drug.substructure$X1)
names(drug.substructure)[1] <- "Drug"
known.associates2$id    <- 1:nrow(known.associates2)
names(drug.sideEffect)[1] <- "Drug"
drug.sideEffect$Drug <- tolower(drug.sideEffect$Drug)

drug.gene$Drug.Name <- tolower(drug.gene$Drug.Name)
names(drug.gene)[2] <- "Drug"
# Merge known.associates 2 and drug substructure by drug as the common variable
df <- merge(known.associates2, drug.substructure, by = "Drug", sort = FALSE)

# Merge df with drug side effect
df1 <- merge(df, drug.sideEffect, by = "Drug", sort = FALSE)

# transform drug.gene data in to wide format with 1 and 0 for variables
drug.gene <- unique(drug.gene)
temp <- drug.gene %>%
  gather(Gene.Symbol,name,starts_with("Gene.Symbol")) %>%
  mutate(present = 1) %>%
  select(-Gene.Symbol) %>%
  spread(name,present,fill = 0)

# Merge known associates 2, drug substructure and drug gene into one data structure
df2 <- merge(df1, temp, by = "Drug", sort = FALSE)
df2 <- df2[order(df2$id), ]
df2 <- subset(df2, select = -id)

write.xlsx(df2, "combine_disease_drug_se_gene.xlsx")
