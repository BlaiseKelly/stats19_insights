# this script downloads the STATS19 data from the DfT and creates some tables and plots

library(sf)
library(mapview)
library(dplyr)
library(stats19)
library(geographr)
library(reshape2)
library(ggplot2)
library(readODS)
library(cols4all)
library(gt)
library(waffle)

# create directory for the plots
dir.create("plots/")

# get collision data
cra_L5Y <- get_stats19("5 years", type = "collision")

# get casualty data
cas_L5Y <- get_stats19("5 years", type = "casualty") |>
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0))

# get vehicle data
veh_L5Y <- get_stats19("5 years", type = "vehicle") |>
  mutate(vehicle_type = if_else(escooter_flag == "Vehicle was an e-scooter", "e-scooter", vehicle_type))

# HGVs
# summarise the casualty data to one row per collision to reduce double counting errors
cas_summary <- cas_L5Y |>
  #filter(collision_year == 2023) |>
  select(collision_index, casualty_type, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  group_by(collision_index) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE))

# trim the data frame to just the columns needed
cra_mway <- select(cra_L5Y, collision_year, collision_index, first_road_class, speed_limit)

# calculate casualties from HGVs
goods_mway <- veh_L5Y |>
  select(collision_index, vehicle_type) |>
  group_by(collision_index, vehicle_type) |>
  #filter(grepl("Goods", vehicle_type)) |> 
  filter(vehicle_type %in% c("Goods over 3.5t. and under 7.5t", "Goods 7.5 tonnes mgw and over")) |> # HGV definition
  mutate(number_vehicles = 1) |>
  summarise(number_vehicles = sum(number_vehicles)) |>
  tidyr::pivot_wider(names_from = "vehicle_type", values_from = "number_vehicles") |> 
  inner_join(cra_mway) |> 
  inner_join(cas_summary) |> 
  filter(first_road_class == "Motorway") |> 
  ungroup() |> 
  group_by(collision_year) |> 
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious),
            Slight = sum(Slight))|> 
  mutate(veh_class = "HGV (over 3.5t)")

# calculate casualties from none HGVs
not_goods_mway <- veh_L5Y |>
  select(collision_index, vehicle_type) |>
  group_by(collision_index, vehicle_type) |>
  #filter(!grepl("Goods", vehicle_type)) |> 
  filter(!vehicle_type %in% c("Goods over 3.5t. and under 7.5t", "Goods 7.5 tonnes mgw and over")) |> # NOT HGV
  mutate(number_vehicles = 1) |>
  summarise(number_vehicles = sum(number_vehicles)) |>
  tidyr::pivot_wider(names_from = "vehicle_type", values_from = "number_vehicles") |> 
  inner_join(cra_mway) |> 
  inner_join(cas_summary) |> 
  filter(first_road_class == "Motorway") |> 
  ungroup() |> 
  group_by(collision_year) |> 
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious),
            Slight = sum(Slight)) |> 
  mutate(veh_class = "not HGV")

# create folder for data
dir.create("data/")

#download vehicle miles by road
download.file("https://assets.publishing.service.gov.uk/media/6848503389d002b886e7880f/road-traffic-estimates-in-great-britain-2024.zip", destfile = "data/road-traffic-estimates-in-great-britain-2024.zip")

#unzip it
unzip("data/road-traffic-estimates-in-great-britain-2024.zip", exdir = "data/")

# read in vehicle miles by road
veh_miles <- read_ods("data/(TRA41) Strategic road network traffic/tra4105-miles-england-by-road-and-vehicle-type.ods", sheet = "TRA4105", skip = 4) |>
  select(Year, Vehicle, Measure, mways = `National Highways managed roads: Motorways`) |> 
  filter(Measure == "Motor vehicle traffic" & !Vehicle == "All Motor Vehicles") |> 
  group_by(Year) |> 
  mutate(All_vehicles = sum(mways)) |> 
  rowwise() |> 
  mutate(veh_pc = (mways/All_vehicles)*100) |> 
  filter(Vehicle == "Heavy Goods Vehicles") |> 
  select(Year, Vehicle, veh_pc)

# combine to one table for HGVs and Not
mway_veh <- rbind(goods_mway, not_goods_mway) |> 
  group_by(collision_year) |> 
  mutate(Fatal_tot = sum(Fatal),
         Serious_tot = sum(Serious)) |> 
  rowwise() |> 
  mutate(Fat_pc = (Fatal/Fatal_tot)*100,
         Ser_pc = (Serious/Serious_tot)*100) |> 
  left_join(veh_miles, by = c("collision_year" = "Year")) |> 
  filter(veh_class == "HGV (over 3.5t)") 

# create table to be plotted by chart 0
chart_0 <- mway_veh |> 
  select(year = collision_year, '% of Fatalities' = Fat_pc, '% of vehicle miles' = veh_pc) |> 
  melt("year")

# define the colours for the plot
cols <- rev(c("black", "red"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
chart_theme <- list(cust_theme, scale_color_manual(values = cols))

# plot chart
chart_0 %>%
  ggplot(aes(year, value, color = variable)) +
  geom_line(size = 2, alpha = 1) +
  ylab("%")+
  chart_theme+
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle(paste0("% of all motorway fatalities involving HGVs compared with % of vehicle miles HGVs accounted for from 2020 to 2024")) +
  scale_x_continuous(name = NULL,
                     breaks = seq(2020, 2024, by = 1)  # Add more tick marks
  ) +
  labs(caption = "Source: Stats19")+
  theme(panel.border = element_blank())

# save to plots folder
ggsave(paste0("plots/hgvs_mway.png"))


# pavements
# summarise casualties to one row per collision to avoid double counting
cas_summary_all <- cas_L5Y |>
  filter(collision_year == 2024) |>
  select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  filter(!pedestrian_location == "Not a Pedestrian") |>
  group_by(collision_index, pedestrian_location) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) 

# create table showing breakdown of pedestrian location
cas_ped_loc_tot <- cas_summary_all |>
  ungroup() |>
  group_by(pedestrian_location) |> 
  summarise(Fatal = round(sum(Fatal)),
            Serious = round(sum(Serious)),
            Slight = round(sum(Slight))) |> 
  rowwise() |> 
  mutate(KSI = sum(Fatal,Serious)) |> 
  ungroup() |> 
  mutate(pc_KSI = round(KSI/sum(KSI)*100,1)) |> 
  arrange(desc(pc_KSI))

# table 0
t0 <- gt(cas_ped_loc_tot,auto_align = TRUE) |> 
  cols_width(pedestrian_location ~px(300)) |> 
  cols_label(pedestrian_location = md("**Pedestrian Location**"),
             Fatal = md("**Fatal**"),
             Serious = md("**Serious**"),
             Slight = md("**Slight**"),
             KSI = md("**KSI**"),
             pc_KSI = md("**KSI (%)**")) |> 
  tab_footnote(md("**Source: DfT STATS19**")) |> 
  tab_header(
    title = md(paste0("**Reported Pedestrian casualties by location, GB: 2024**"))) |> 
  tab_options(heading.align = "left",
              column_labels.border.top.style = "none",
              table.border.top.style = "none",
              column_labels.border.bottom.style = "none",
              column_labels.border.bottom.width = 1,
              column_labels.border.bottom.color = "black",
              table_body.border.top.style = "none",
              table_body.border.bottom.color = "white",
              heading.border.bottom.style = "none",
              table.border.bottom.style = "none",) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(pedestrian_location)),
      cells_body(columns = c(pedestrian_location))
    )) |> 
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = everything())
  )

# save table as a png
gtsave(t0, "plots/ped_location_table.png")

# summarise casualties again
cas_summary <- cas_L5Y |>
  filter(collision_year == 2024) |>
  select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  filter(pedestrian_location == "On footway or verge") |>
  group_by(collision_index) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) 


# summary of pavement casualties for the entire period
cas_pave_tot <- cas_summary |>
  ungroup() |>
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious),
            Slight = sum(Slight))

# pick out only columns needed from crash data
cra_sum_L5Y <- cra_L5Y |> 
  select(collision_index, number_of_vehicles)

# summarise vehicle table so only one row per collision
veh_summary <- veh_L5Y |>
  select(collision_index, vehicle_type) |>
  group_by(collision_index, vehicle_type) |>
  mutate(number_vehicles = 1) |>
  summarise(number_vehicles = sum(number_vehicles)) |>
  tidyr::pivot_wider(names_from = "vehicle_type", values_from = "number_vehicles")

# join cas, crash and veh together
cas_cra_veh <- cas_summary |>
  inner_join(cra_sum_L5Y) |>
  inner_join(veh_summary)

# pedestriains pavement single vehicle collisions
one_car <- cas_cra_veh |>
  filter(number_of_vehicles == 1) |> 
  select(-number_of_vehicles) |> 
  melt(c("collision_index", "Fatal", "Serious", "Slight"), variable.name = "vehicle_type") |> 
  filter(!is.na(value)) |> 
  group_by(vehicle_type) |>
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious),
            Slight = sum(Slight))

# collisions with more than one vehicle totals
more_cars_tot <- cas_cra_veh |>
  filter(!number_of_vehicles == 1) |> 
  #group_by(vehicle_type) |>
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious),
            Slight = sum(Slight))

# breakdown of >1 vehicles by vehicle type, pretty sure no double counting is here
more_cars_veh <- cas_cra_veh |>
  filter(!number_of_vehicles == 1) |>
  select(-number_of_vehicles) |> 
  melt(c("collision_index", "Fatal", "Serious", "Slight")) |> 
  filter(!is.na(value)) |> 
  group_by(variable) |> 
  summarise(Fatal = round(sum(Fatal)),
            Serious = round(sum(Serious)),
            Slight = round(sum(Slight))) |> 
  mutate(pc_fat = round(Fatal/sum(Fatal)*100,1))

Fat_pav <- one_car |> 
  filter(Fatal > 0) |> 
  select(vehicle_type, Fatal)

KSI_pav <- one_car |> 
  rowwise() |>
  mutate(KSI =round(sum(Fatal, Serious))) |> 
  filter(KSI > 0) |> 
  select(vehicle_type, KSI)

# get enough colours for the variables
colz <- c4a("poly.sky24", n = NROW(Fat_pav))

# create waffle plot
waffle(Fat_pav, rows = 3, colors = colz,legend_pos = "bottom", title = paste0("Single vehicle Pavement fatalities in 2024 by driver vehicle type"))

# write out with ggplot2
ggsave("plots/fatal_pave.png")

# same again for KSI
colz <- c4a("poly.sky24", n = NROW(KSI_pav))

waffle(KSI_pav, rows = 25, colors = colz, title = paste0("Singe vehicle KSI casualties on pavements by driver vehicle type"))

ggsave("plots/KSI_pave.png")

### England, Wales, Scotland
# import country boundaries
countries <- geographr::boundaries_countries20 |>
  st_transform(27700)

# make the crash data sf to intersect with 
cra_country <- format_sf(cra_L5Y) |>
  st_join(countries) |>
  select(collision_index, urban_or_rural_area, country = country20_name)

# some points are just outside the border, buffer the border to include
countries_buff <- st_buffer(countries, 2000)

# pick out those which didn't match and use the buffered country
cc_na <- cra_country |>
  filter(is.na(country)) |>
  st_join(countries_buff) |>
  select(collision_index, urban_or_rural_area, country = country20_name)

# join them together and use this data for the crashes
cc_filled <- cra_country |>
  filter(!is.na(country)) |>
  rbind(cc_na) |>
  st_set_geometry(NULL)

cas_sum_L5Y <- cas_L5Y |>
  #filter(collision_year == 2024) |>
  select(collision_index, casualty_type, pedestrian_location, fatal_count, casualty_adjusted_severity_serious, casualty_adjusted_severity_slight) |>
  #filter(pedestrian_location == "On footway or verge") |>
  group_by(collision_index) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) 


# define the colours for the plot
cols <- rev(c("#00ab3d", "#005bb2","#c81329"))
cust_theme <- theme(panel.grid.major = element_line(size = 2))
# put the elements in a list
dft_theme <- list(cust_theme, scale_color_manual(values = cols))

# define the base year that will be indexed
base_year <- 2020
# variables to change
urb_rural <- c("Urban", "Rural")

casualties <- c("Pedestrians & Cyclists", "all casualties")

# loop through for all casualties and pedestrians and urban and rural areas to plot
for (c in casualties){
  
  if(c == "Pedestrians & Cyclists"){
  c_types <- c("Pedestrian", "Cyclist")
  } 
  if (c == "all casualties"){
    c_types <- unique(cas_L5Y$casualty_type)
  }

for (ur in urb_rural){

cc <- cas_L5Y |>
  filter(collision_year >= base_year & casualty_type %in% c_types) |>
  group_by(collision_index, collision_year) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |> 
  inner_join(cc_filled) |>
  #filter(casualty_type == "Pedestrian") |>
  filter(urban_or_rural_area == ur) |>
  group_by(collision_year,country) |>
  summarise(Fatal = sum(Fatal),
            Serious = sum(Serious,na.rm = TRUE),
            Slight = sum(Slight,na.rm = TRUE)) |>
  ungroup() |>
  rowwise() |>
  mutate(KSI = sum(c(Fatal, Serious))) |>
  select(collision_year, country, KSI) |>
  tidyr::pivot_wider(names_from = "country", values_from = "KSI")

# baseline values for index plot
bm_vals <- cc |> filter(collision_year == base_year)

# calaute table of indexes
rates <- cc %>%
  mutate(England = England/bm_vals$England*100,
         Scotland = Scotland/bm_vals$Scotland*100,
         Wales = Wales/bm_vals$Wales*100)

# prep for plot
chart_1 <- rates |> melt("collision_year")

# plot
chart_1 %>%
  ggplot(aes(collision_year, value, color = variable)) +
  geom_line(size = 2, alpha = .8) +
  dft_theme+
  theme(panel.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_hline(yintercept=100, linetype='dotted', col = 'black')+
  ggtitle(paste0("Index of ", c, " KSI in ", ur," areas by country: ", base_year, " to 2024 (Index ", base_year, "=100)")) +
  scale_x_continuous(name = NULL,
                     breaks = seq(2020, 2024, by = 1)  # Add more tick marks
  ) +
  ylab("index")+
  labs(caption = "Source: Stats19")+
  theme(panel.border = element_blank())

fn <- gsub(" \\&\\ ", "_", c)
fn <- gsub(" ", "_", c)

ggsave(paste0("plots/country_KSI_",fn, "_", ur,".png"))

}
  
}

# casualties by country and year for writing out
country_table <- cas_L5Y |>
  filter(collision_year >= base_year) |>
  group_by(collision_index, collision_year) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |> 
  inner_join(cc_filled) |>
  #filter(casualty_type == "Pedestrian") |>
  #filter(urban_or_rural_area == "Urban") |>
  group_by(collision_year,country) |>
  summarise(Fatal = round(sum(Fatal)),
            Serious = round(sum(Serious,na.rm = TRUE)),
            Slight = round(sum(Slight,na.rm = TRUE))) |> 
  ungroup() |>
  rowwise() |>
  mutate(KSI = sum(c(Fatal, Serious)))

# difference 
diff_2022_2024 <- round(country_table[country_table$country == "Wales",]$KSI[3]-country_table[country_table$country == "Wales",]$KSI[5])

# country table
t1 <- gt(country_table,auto_align = TRUE) |> 
  cols_width(collision_year ~px(60)) |> 
  cols_label(collision_year = md("**Year**"),
             country = md("**Country**"),
             Fatal = md("**Fatal**"),
             Serious = md("**Serious**"),
             Slight = md("**Slight**"),
             KSI = md("**KSI**")) |> 
  tab_footnote(md("**Source: DfT STATS19**")) |> 
  tab_header(
    title = md(paste0("**Number of reported road casualties by country, GB: 2020 to 2024**"))) |> 
  tab_options(heading.align = "left",
              column_labels.border.top.style = "none",
              table.border.top.style = "none",
              column_labels.border.bottom.style = "none",
              column_labels.border.bottom.width = 1,
              column_labels.border.bottom.color = "black",
              table_body.border.top.style = "none",
              table_body.border.bottom.color = "white",
              heading.border.bottom.style = "none",
              table.border.bottom.style = "none",) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(collision_year)),
      cells_body(columns = c(collision_year))
    )) |> 
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = everything())
  )

gtsave(t1, "plots/country_table.png")

# casualties by country and year for writing out
country_table_urban <- cas_L5Y |>
  filter(collision_year >= base_year) |>
  group_by(collision_index, collision_year) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |> 
  inner_join(cc_filled) |>
  #filter(casualty_type == "Pedestrian") |>
  filter(urban_or_rural_area == "Urban") |>
  group_by(collision_year,country) |>
  summarise(Fatal = round(sum(Fatal)),
            Serious = round(sum(Serious,na.rm = TRUE)),
            Slight = round(sum(Slight,na.rm = TRUE))) |> 
  ungroup() |>
  rowwise() |>
  mutate(KSI = sum(c(Fatal, Serious)))

# difference 
diff_2022_2024_urb <- ((country_table_urban[country_table_urban$country == "Wales",]$KSI[3]-country_table_urban[country_table_urban$country == "Wales",]$KSI[5])/country_table_urban[country_table_urban$country == "Wales",]$KSI[3])*100



# casualties by country and year for writing out
country_table_urb_ped_cy <- cas_L5Y |>
  filter(collision_year >= base_year & casualty_type %in% c("Pedestrian", "Cyclist")) |>
  group_by(collision_index, collision_year) |>
  summarise(Fatal = sum(fatal_count),
            Serious = sum(casualty_adjusted_severity_serious,na.rm = TRUE),
            Slight = sum(casualty_adjusted_severity_slight,na.rm = TRUE)) |> 
  inner_join(cc_filled) |>
  #filter(casualty_type == "Pedestrian") |>
  filter(urban_or_rural_area == "Urban") |>
  group_by(collision_year,country) |>
  summarise(Fatal = round(sum(Fatal)),
            Serious = round(sum(Serious,na.rm = TRUE)),
            Slight = round(sum(Slight,na.rm = TRUE))) |> 
  ungroup() |>
  rowwise() |>
  mutate(KSI = sum(c(Fatal, Serious)))

# difference 
diff_2022_2024_urb <- ((country_table_urb_ped_cy[country_table_urb_ped_cy$country == "Wales",]$KSI[3]-country_table_urb_ped_cy[country_table_urb_ped_cy$country == "Wales",]$KSI[5])/country_table_urb_ped_cy[country_table_urb_ped_cy$country == "Wales",]$KSI[3])*100
# trips and CAs

#   # get trip data NTS0303 https://www.gov.uk/government/statistical-data-sets/nts03-modal-comparisons
#   download.file("https://assets.publishing.service.gov.uk/media/681b717ee26cd2f713d8703e/nts0303_ca.ods", destfile = "nts0303_ca.ods", mode = "wb")
# 
# 
#   trips <- read_ods("nts0303_ca.ods", sheet = 6, skip = 5) |> 
#     mutate(colsplit(Year," ", c("Year", "y2"))) |> 
#     select(-y2) |> 
#     melt(c("Year", "Combined authority [notes 2, 11]")) |> 
#     mutate(variable = as.character(variable),
#            value = as.numeric(value))
#   
#   names(trips) <- c("year", "combined_authority", "mode", "miles_ppp_yr")
#   
#   combined_authorities_trips <- unique(trips$combined_authority)
#   
#   modes <- unique(trips$mode)[1:15]
#   
#   for (m in modes){
#   
#   base_year <- 2021
#   
#   rate_list <- list()
#   for (ca in combined_authorities_trips){ 
#     
#     bm_vals <- trips |> 
#       filter(combined_authority == ca & mode == m & year == base_year)
#     
#     rates_df <- trips %>%
#       filter(combined_authority == ca & mode == m, year >= base_year) |> 
#       mutate(miles_ppp_yr = miles_ppp_yr/bm_vals$miles_ppp_yr*100)
#     
#     rate_list[[ca]] <- rates_df
#     
#   }
#   
#   chart_0 <- do.call(rbind,rate_list)
#   
#   
#   chart_0 %>%
#     ggplot(aes(year, miles_ppp_yr, color = combined_authority)) +
#     geom_line(size = 2, alpha = .8) +
#     dft_theme+
#     theme(panel.background = element_blank(),
#           legend.position = "top",
#           legend.title = element_blank()) +
#     scale_x_continuous(expand = c(0, 0)) +
#     geom_hline(yintercept=100, linetype='dotted', col = 'black')+
#     ggtitle(paste0("Index of estimated travel distance for ", m," by Combined Authority: ", base_year, " to 2024 (Index ", base_year, "=100)")) +
#     scale_x_continuous(name = NULL,
#                        breaks = seq(2020, 2024, by = 1)  # Add more tick marks
#     ) +
#     labs(caption = "Source: Stats19")+
#     theme(panel.border = element_blank())
#   
#   ggsave(chart_1, paste0("plots/country_KSI_", ur,".png"))
# 
# # load in 
# LADs <- geographr::boundaries_ltla24
# 
# geographr::
# 
# LADs_lu <- geographr::lookup_ltla23_cauth23
# 
# LADs_lu_gov <- read.csv("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/ba77701e67bd4f3e80a97fb9a5fd4885/csv?layers=0")
# 
# LADs_lu_gov_sf <- st_read("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/6b0ebe015596484ebd81e372da32db46/geojson?layers=0")
# 
# CA_sf <- st_read("https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/b74cd1fd931846289a0927e6653a7c16/geoPackage?layers=0")
# 
# L_sf <- st_read("C:/Users/blais/Pictures/Local_Authority_District_to_Combined_Authority_(May_2025)_Lookup_in_EN/Local_Authority_District_to_Combined_Authority_(May_2025)_Lookup_in_EN.shp")
# 
# mapview(CA_sf)
# 
# cra_CA <- cra_L5Y |>
#   left_join(LADs_lu, by = c("local_authority_ons_district" = "ltla23_name"))
# 
# cra_CA <- cra_L5Y |>
#   left_join(LADs_lu_gov, by = c("local_authority_ons_district" = "LAD25NM"))
# 
# cra_CA_na <- filter(cra_CA, is.na(CAUTH25NM))
# 
# base_year <- 2020
# 
# combined_authorities <- na.omit(unique(cra_CA$cauth23_name))
# 
# cols <- cols4all::c4a("brewer.paired", n = NROW(combined_authorities))
# cust_theme <- theme(panel.grid.major = element_line(size = 2))
# # put the elements in a list
# dft_theme <- list(cust_theme, scale_color_manual(values = cols))
# 
# 
#   cc <- cas_L5Y |>
#     filter(collision_year >= base_year) |> 
#     select(collision_index, collision_year, casualty_type, fatal = fatal_count, serious = casualty_adjusted_severity_serious, slight = casualty_adjusted_severity_slight) |>
#     inner_join(cra_CA) |> 
#     filter(urban_or_rural_area == "Urban", casualty_type == "Cyclist") |> 
#     group_by(collision_year,cauth23_name) |>
#     summarise(Fatal = sum(fatal),
#               Serious = sum(serious,na.rm = TRUE),
#               Slight = sum(slight,na.rm = TRUE)) |>
#     ungroup() |>
#     rowwise() |>
#     mutate(KSI = sum(c(Fatal, Serious))) |>
#     select(collision_year, cauth23_name, KSI) |> 
#     filter(!is.na(cauth23_name))
#     #tidyr::pivot_wider(names_from = "cauth23_name", values_from = "KSI") |> 
# 
#   rate_list <- list()
#   for (ca in combined_authorities){      
#   
#   bm_vals <- cc |> 
#     filter(cauth23_name == ca & collision_year == base_year)
#   
#   rates_df <- cc %>%
#     filter(cauth23_name == ca) |> 
#     mutate(KSI = KSI/bm_vals$KSI*100)
#   
#   rate_list[[ca]] <- rates_df
#   
#   }
#   
#   chart_1 <- do.call(rbind,rate_list)
#   
#   #chart_1 <- rates |> melt("collision_year")
#   
#   chart_1 %>%
#     ggplot(aes(collision_year, KSI, color = cauth23_name)) +
#     geom_line(size = 2, alpha = .8) +
#     dft_theme+
#     theme(panel.background = element_blank(),
#           legend.position = "top",
#           legend.title = element_blank()) +
#     scale_x_continuous(expand = c(0, 0)) +
#     geom_hline(yintercept=100, linetype='dotted', col = 'black')+
#     ggtitle(paste0("Index of KSI casualties by Combined Authority: ", base_year, " to 2024 (Index ", base_year, "=100)")) +
#     scale_x_continuous(name = NULL,
#                        breaks = seq(2020, 2024, by = 1)  # Add more tick marks
#     ) +
#     labs(caption = "Source: Stats19")+
#     theme(panel.border = element_blank())
#   
#   ggsave(chart_1, paste0("plots/country_KSI_", ur,".png"))
#   
# }

