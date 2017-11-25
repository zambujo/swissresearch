# boilerplate -------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
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
# > even better: use MS Excel to fix p3 csv files
# p_load("readr", "rio")
# grants_ok <- rio::import("grants.xlsx") %>%
#   janitor::clean_names() %>%
#   select(-starts_with("x_"))
# people_ok <- rio::import("people.xlsx") %>%
#   janitor::clean_names()
# collab_ok <- rio::import("collab.xlsx") %>%
#   janitor::clean_names()
# write_csv(grants_ok, path = saveas[1])
# write_csv(people_ok, path = saveas[2])
# write_csv(collab_ok, path = saveas[3])

grants <- fread(saveas[1])
people <- fread(saveas[2])
collab <- fread(saveas[3])

# find collab for a given YEAR --------------------------------------------

p_load("lubridate", "stringr")
trange <- ymd(c(str_c(YEAR, "-01-01"), str_c(YEAR, "-12-31")))

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
  select(person_id_snsf,
         institute_place,
         starts_with("projects")) %>%
  filter(institute_place != "") %>%
  select(-projects_as_responsible_applicant) %>%
  unite(project_number, starts_with("projects"), sep = ";") %>%
  mutate(
    institute_place = str_replace_all(institute_place, " Cedex(.*)?| [0-9]{1,2}", ""),
    project_number = str_replace_all(project_number, "[;]+", ";"),
    project_number = str_replace_all(project_number, "^[;]|[;]$", ""),
    project_number = str_split(project_number, ";")) %>%
  unnest() %>%
  mutate(project_number = as.integer(project_number)) %>%
  semi_join(grants)

collab <- collab %>%
  select(project_number, group_person, country) %>%
  unite(affil, group_person, country, sep = ", ") %>%
  semi_join(grants)

# building the graph ------------------------------------------------------

# projects linking more than one place
places <- group_by(people, project_number) %>%
  summarise(n_places = n_distinct(institute_place)) %>%
  filter(n_places > 1)

# projects running in multiple places
core <- people %>%
  select(-person_id_snsf) %>%
  semi_join(places) %>%
  arrange(project_number) %>%
  distinct()


# takes a few minutes to update geocodes
update_geocodes <- FALSE
if (update_geocodes) {
  # geocodes for the places
  geocodes <- tibble(
    place = sort(unique(core$institute_place)),
    lat = rep(NA_real_, n_distinct(core$institute_place)),
    lon = rep(NA_real_, n_distinct(core$institute_place)),
    addr = rep(NA_character_, n_distinct(core$institute_place)),
    id = rep(NA_character_, n_distinct(core$institute_place)))

  # https://gis.stackexchange.com/questions/158328/batch-geocoding-in-r
  # https://github.com/hrbrmstr/nominatim
  p_load_gh("hrbrmstr/nominatim")
  # https://developer.mapquest.com/user/me/apps
  osm_key <- readLines("mapquest.key")

  # openstreetmap api
  osm <- function(query, osm_key) {
    r <- osm_search_spatial(query, limit = 1, key = osm_key)
    if (!is.null(r[[1]])) {
      c(r[[1]]$place_id, r[[1]]$display_name,
        r[[1]]$lat, r[[1]]$lon)
    } else NA
  }

  # googlemaps api: 2500 reqs/day, 50 reqs/sec max
  p_load("ggmap") # requires `libpng16-dev` in ubuntu
  google <- function(query) {
    r <- geocode(query, output = "all")
    if (r$status == "OK") {
      c(r$results[[1]]$place_id, r$results[[1]]$formatted_address,
        r$results[[1]]$geometry$location$lat, r$results[[1]]$geometry$location$lng)
    } else NA
  }

  # takes a couple of minutes
  for (k in seq_along(geocodes$place)) {
    if (k %% 5 == 0) cat("........ ", k, ": ", geocodes$place[k], "\n")
    info <- osm(geocodes$place[k], osm_key)
    # fallback
    if (is.na(info)) info <- google(geocodes$place[k])
    # store info in meta
    if (!is.na(info)) {
      geocodes$lat[k] = as.numeric(info[3])
      geocodes$lon[k] = as.numeric(info[4])
      geocodes$addr[k] = info[2]
      geocodes$id[k] = info[1]
    }
  }

  # TODO : run for loop step by step
  geocodes <- drop_na(geocodes)
  write_csv(geocodes, path = "geocodes.csv")
}

geocodes <- read_csv("geocodes.csv")
core <- left_join(core, geocodes, by = c("institute_place" = "place"))

# distinguish between national and international projects ...

core_ch <- filter(core, str_detect(addr, "Switzerland$"))

# find projects on more than one place in Switzerland
projects_places <- group_by(core_ch, project_number) %>%
  summarise(n_places = n_distinct(institute_place)) %>%
  filter(n_places > 1)
core_ch <- semi_join(core_ch, projects_places)



# plot graph --------------------------------------------------------------
# > http://kateto.net/network-visualization

# nodes as places
nodes <- group_by(core_ch, institute_place) %>%
  summarise(y = head(lat, 1), x = head(lon, 1), size = n()) %>%
  arrange(desc(size))

p_load("maps", "mapdata", "geosphere")



# edges defined by the projects
p_load("magrittr")
project_edges <- function(number, df) {
  edges <- filter(df, project_number == number) %>%
    select(institute_place) %$%
    combn(institute_place, m = 2)
  tibble(from = edges[1, ],
         to = edges[2,]) %>%
    mutate(number = number) %>%
    select(number, everything())
}

edges <- purrr::map(unique(core_ch$project_number), project_edges, df = core_ch)
edges <- do.call(bind_rows, edges)

edges <- edges %>%
  left_join(select(nodes, -size), by = c("from" = "institute_place")) %>%
  rename(x1 = x, y1 = y)
  left_join(select(nodes, -size), by = c("to" = "institute_place")) %>%
  rename(x2 = x, y2 = y) %>%
  # add domain to the edges
  left_join(select(grants, project_number, domain),
            by = c("number" = "project_number"))

snsf_colors <-
  c("#FFB24C", "#194CB2", "#666666") %>%
  adjustcolor(alpha = 0.1)


# map of Switzerland
maps::map(database = "worldHires", regions = "Switzerland",
          fill = FALSE, col = rgb(0, 0, 0, .33))

# map edges
for (k in 1:nrow(edges))  {
  arc <- gcIntermediate(
    c(edges$x1[k], edges$y1[k]),
    c(edges$x2[k], edges$y2[k]),
    n = 100, addStartEnd = TRUE)
  lines(arc, col = snsf_colors[edges$domain[k]], lwd = 3)
}

# main cities
cities <- c("Zürich", "Lausanne", "Bern", "Genève", "Basel", "Fribourg",
            "Neuchâtel", "St. Gallen", "Lugano", "Luzern", "Winterthur")
city_nodes <- filter(nodes, institute_place %in% cities)
graphics::text(city_nodes$x, city_nodes$y, labels = city_nodes$institute_place,
               pos = 3, cex = .7, col = rgb(0, 0, 0, .66), font = 2)

# > show all nodes
points(x = nodes$x, y = nodes$y, pch = 16,
  cex = log(nodes$size)/2, col = rgb(0, 0, 0, .33))

