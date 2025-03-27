# Script to save holiday excel file as a RDS

library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

holidays = read_excel("data/Holidays.xlsx")
saveRDS(holidays, "data/holidays.Rds")
