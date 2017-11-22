# boilerplate -------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
if (!require(janitor)) install.packages("janitor")
p_load("tidyverse", "data.table")

YEAR <- 2017

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


# find collab for a given YEAR --------------------------------------------

p_load("lubridate", "stringr")

trange <- ymd(c(
  str_c(YEAR, "-01-01"),
  str_c(YEAR, "-12-31")))

grants <- grants %>%
  select(project_number, discipline_number, start_date, end_date) %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date),
    domain = as.integer(str_extract_all(discipline_number, "^[0-9]"))) %>%
  # starting before the end of the year
  # and ending before the beginning of the year
  filter(start_date <= trange[2], end_date >= trange[1]) %>%
  drop_na() # for there remain some parsing errors

people <- people %>%
  # unite all projects together
  select(person_id_snsf, starts_with("projects")) %>%
  select(-projects_as_responsible_applicant) %>%
  # put all projects together
  unite(project_number, starts_with("projects"), sep = ";") %>%
  mutate(
    project_number = str_replace_all(project_number, "[;]+", ";"),
    project_number = str_replace_all(project_number, "^[;]|[;]$", ""),
    project_number = str_split(project_number, ";")) %>%
  unnest() %>%
  mutate(project_number = as.integer(project_number)) %>%
  semi_join(grants)

collab <- collab %>%
  select(project_number, group_person, country) %>%
  semi_join(grants)


