library(readxl)
library(dplyr)
library(sf)
library(rnaturalearth)

wpp_file <- "WPP2024_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES.xlsx"

wpp <- read_excel(
  wpp_file,
  sheet = "Medium variant",
  skip = 16,
  col_types = "text"
)

names(wpp)[3] <- "country"
names(wpp)[6] <- "iso3"
names(wpp)[7] <- "iso2"
names(wpp)[9] <- "type"

wpp$Year <- as.numeric(wpp$Year)

age_cols <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
  "40-44","45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79",
  "80-84","85-89","90-94","95-99","100+"
)

wpp[age_cols] <- lapply(wpp[age_cols], as.numeric)

wpp_2026 <- wpp %>%
  filter(type == "Country/Area", Year == 2026)

age6 <- wpp_2026 %>%
  transmute(
    country_wpp = country,
    iso3,
    iso2,
    age_0_14 = rowSums(across(c("0-4","5-9","10-14")), na.rm = TRUE),
    age_15_24 = rowSums(across(c("15-19","20-24")), na.rm = TRUE),
    age_25_44 = rowSums(across(c("25-29","30-34","35-39","40-44")), na.rm = TRUE),
    age_45_64 = rowSums(across(c("45-49","50-54","55-59","60-64")), na.rm = TRUE),
    age_65_79 = rowSums(across(c("65-69","70-74","75-79")), na.rm = TRUE),
    age_80_plus = rowSums(across(c("80-84","85-89","90-94","95-99","100+")), na.rm = TRUE)
  ) %>%
  filter(!is.na(iso3), nchar(iso3) == 3)

age_groups <- c("age_0_14","age_15_24","age_25_44","age_45_64","age_65_79","age_80_plus")

age6[age_groups] <- age6[age_groups] / rowSums(age6[age_groups], na.rm = TRUE)

age6$population_millions <- rowSums(wpp_2026[wpp_2026$iso3 %in% age6$iso3, age_cols], na.rm = TRUE) / 1000

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Natural Earth uses non-standard ISO3 codes for some entities.
# Reconcile them with UN WPP ISO3 codes before joining.
world$iso_a3[world$name == "France"] <- "FRA"
world$iso_a3[world$name == "Norway"] <- "NOR"
world$name[world$name == "United States"] <- "United States of America"

world <- world %>%
  filter(iso_a3 %in% age6$iso3, iso_a3 != "-99") %>%
  select(name, iso_a3, continent, geometry)

cent <- suppressWarnings(sf::st_centroid(world))
coords <- sf::st_coordinates(cent)

world_index <- world %>%
  st_drop_geometry() %>%
  mutate(
    iso3 = iso_a3,
    lng = coords[,1],
    lat = coords[,2]
  ) %>%
  select(iso3, country_map = name, continent, lng, lat)

country_master <- age6 %>%
  inner_join(world_index, by = "iso3") %>%
  mutate(
    country = country_map,
    annual_outbound_trips_millions = pmax(0.1, population_millions * 0.05)
  ) %>%
  select(
    country,
    country_wpp,
    iso3,
    iso2,
    continent,
    lng,
    lat,
    population_millions,
    annual_outbound_trips_millions,
    all_of(age_groups)
  ) %>%
  arrange(country)

dir.create("docs/roadmap", showWarnings = FALSE, recursive = TRUE)

write.csv(
  country_master,
  "docs/roadmap/country_master_2026_common_iso.csv",
  row.names = FALSE
)

saveRDS(
  country_master,
  "docs/roadmap/country_master_2026_common_iso.rds"
)

world_full <- world %>%
  mutate(
    sim_country = name,
    iso3 = iso_a3
  ) %>%
  select(sim_country, iso3, geometry)

saveRDS(
  world_full,
  "docs/roadmap/world_countries_simplified_common_iso.rds"
)

cat("Countries in master:", nrow(country_master), "\n")
cat("World polygons:", nrow(world_full), "\n")
cat("Saved outputs in docs/roadmap/\n")
