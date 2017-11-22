# boilerplate -------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
if (!require(janitor)) install.packages("janitor")
p_load("tidyverse",
       "data.table")

# get p3 data -------------------------------------------------------------
saveas <- c("grants.csv",
            "people.csv",
            "collab.csv")

# if update ---------------------------------------------------------------
# p3url <- c(
#   # "http://p3.snf.ch/P3Export/P3_GrantExport_with_abstracts.csv",
#   "http://p3.snf.ch/P3Export/P3_GrantExport.csv",
#   "http://p3.snf.ch/P3Export/P3_PersonExport.csv",
#   "http://p3.snf.ch/P3Export/P3_CollaborationExport.csv")
# walk2(p3url, saveas, download.file, quiet = TRUE)
# ------------------------------------------------------------------------
# > p3 csv files are surprisingly difficult to parse on linux
# > open and (re-)save as in loffice for proper encoding

get.csv <- function(x) janitor::clean_names(fread(x))

grants <- get.csv(saveas[1])
people <- get.csv(saveas[2])
collab <- get.csv(saveas[3])

