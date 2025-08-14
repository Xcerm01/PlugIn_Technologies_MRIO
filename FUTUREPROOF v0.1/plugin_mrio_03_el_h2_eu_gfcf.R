##########################################################################
# 03_el_h2_eu_gfcf

# create gross fixed capital formation vectors for the modelled technologies
# including implementation of geographical origin

# input data:
# exiobase 3.9.5 monetary ixi 2020: https://zenodo.org/records/14869924
# parsed data: set by user
# disaggregated IO objects by modelled technologies: set by user (from code 01)
# detailed tech data and scenarios: set by user (input file in the repository)

# libraries:
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(tibble)



##########################################################################
# 0. set path and load functions
##########################################################################

##########################################################################
# 0.1 set path

# parsedpath set by user
# exiopath set by user
# exiopath set by user


##########################################################################
# 0.2 load functions

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

`%!in%` = Negate(`%in%`)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))



##########################################################################
# 1. define basic sets
##########################################################################

# scenario versions
versions <- c("CentMod", "CentHt", "DispMod", "DispHt") # modelled versions

# years considered in the scenarios
years = c("2020", "2030", "2050")  # years considered in the analysis

# country lists
countries <- c("AT","BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT",
               "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")
countries_all <- c("AT","BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT",
                   "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "GB", "US", "JP", "CN", "CA", "KR", "BR",
                   "IN", "MX", "RU", "AU", "CH", "TR", "TW", "NO", "ID", "ZA", "WA", "WL", "WE", "WF", "WM")
countries_row <- c("GB", "US", "JP", "CN", "CA", "KR", "BR", "IN", "MX", "RU", "AU", "CH", "TR", "TW", "NO", "ID", "ZA", "WA",
                   "WL", "WE", "WF", "WM")
countries_names <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Spain",
                     "Finland", "France", "Greece", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia",
                     "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")
countries_names_all <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", "Denmark", "Estonia", "Spain",
                         "Finland", "France", "Greece", "Croatia", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia",
                         "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia", "United Kingdom",
                         "Australia", "Brazil", "Canada", "China", "Chile", "Indonesia", "India", "Japan", "South Korea", "Mexico",
                         "Norway", "Russia", "Turkey", "Taiwan", "United States", "Asia and Pacific", "Rest of Europe", "Africa", "America", 
                         "Middle East", "South Africa")

# modelled tech-spec sectors
technologies <- c(
  "i40.11.e.1.i.turb", "i40.11.e.1.i.airrig", "i40.11.e.2.i.turb", "i40.11.e.2.ii.turb",
  "i40.11.h.1.i.sil", "i40.11.h.1.i.tand", "i40.11.h.1.ii.sil", "i40.11.h.1.ii.tand",
  "i40.11.h.1.iii.sil", "i40.11.h.1.iii.tand"
)


##########################################################################
# 2. prepare GFCF shares and perform disaggregations
##########################################################################

##########################################################################
# 2.1 prepare adjusted IOsupdet.codes_global

IOsupdet.codes_global <- readRDS(file = paste0(exiopath, "IOsupdet.codes.rds"))
IOsupdet.codes_global <- IOsupdet.codes_global %>%
  group_by(Industry.Code, Industry.Name) %>%
  summarise()
IOsupdet.codes_global$Index <- seq_len(nrow(IOsupdet.codes_global))
IOsupdet.codes_global <- IOsupdet.codes_global[,c(3,2,1)]


##########################################################################
# 2.2 prepare separate GFCF columns with sectoral origin of inputs

IOsupdet.codes <- readRDS(file = paste0(exiopath, "IOsupdet.codes.rds"))

capex <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "3_inpdir_sec")
capex <- capex[grepl("Capex breakdown",capex$Category),c(1:4,9:14)]

# spread capex
industry_code_order <- capex %>%
  distinct(Industry.Code) %>%
  pull(Industry.Code)

capex_wide <- capex %>%
  group_by(Industry.Inp.Dir.Name, Industry.Inp.Dir.Code, Category.Breakdown.Item, Geo.Orig.Split, Industry.Code) %>%
  summarise(Alloc = sum(Alloc.Inp.Dir.20, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Industry.Code,
    values_from = Alloc,
    values_fill = 0
  )

capex_wide <- capex_wide %>%
  arrange(as.numeric(str_extract(Industry.Inp.Dir.Code, "(?<=i)\\d+")))
capex_wide <- capex_wide %>%
  relocate(Industry.Inp.Dir.Name, Industry.Inp.Dir.Code, Category.Breakdown.Item, Geo.Orig.Split, all_of(industry_code_order))

# join IOsupdet.codes_global with capex
GFCF_capex <- IOsupdet.codes_global %>%
  left_join(capex_wide, by = c("Industry.Code" = "Industry.Inp.Dir.Code")) %>%
  select(-Industry.Inp.Dir.Name) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

colSums(GFCF_capex[,c(6:15)])


##########################################################################
# 2.3 split GFCF_capex into part allocated geographically with capex_geog, and part allocated domestically
# these two will be eventually overlayed

# GFCF_capex_geog where values are later allocated with GFCF_geog
GFCF_capex_geog <- GFCF_capex %>%
  mutate(across(
    all_of(technologies),
    ~ case_when(
      Geo.Orig.Split == "Yes" ~ .x, # keep value
      TRUE ~ 0 # set everything else to 0
    )
  ))

# GFCF_capex_dom where values are allocated into domestic countries
GFCF_capex_dom <- GFCF_capex %>%
  mutate(across(
    all_of(technologies),
    ~ case_when(
      Geo.Orig.Split == "Yes" ~ 0,
      TRUE ~ .x # preserve everything else as-is
    )
  ))


##########################################################################
# 2.4 prepare the part of GFCF columns with modelled geographical origin

capex_geog <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "3_inpdir_geo_int")
capex_geog <- capex_geog[,c(1:2,5:11)]
capex_geog <- capex_geog[!grepl("alk|pem", capex_geog$Industry.Code),] # delete later when green h2 is prepared

# spread capex_geog
industry_code_order <- capex_geog %>%
  distinct(Industry.Code) %>%
  pull(Industry.Code)

capex_geog_wide <- capex_geog %>%
  group_by(Country.Code, Industry.Inp.Dir.Name, Industry.Inp.Dir.Code, Category.Breakdown.Item.Techecon, Industry.Code) %>%
  summarise(Alloc = sum(Alloc.20, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Industry.Code,
    values_from = Alloc,
    values_fill = 0
  )

capex_geog_wide <- capex_geog_wide %>%
  arrange(as.numeric(str_extract(Industry.Inp.Dir.Code, "(?<=i)\\d+")))
capex_geog_wide <- capex_geog_wide %>%
  relocate(Country.Code,
           Industry.Inp.Dir.Name,
           Industry.Inp.Dir.Code,
           Category.Breakdown.Item.Techecon,
           all_of(industry_code_order))

# join IOsupdet.codes with capex_geog_wide
GFCF_geog <- IOsupdet.codes %>%
  left_join(capex_geog_wide,
            by = c(
              "Industry.Code" = "Industry.Inp.Dir.Code",
              "Industry.Name" = "Industry.Inp.Dir.Name",
              "Country.Code" = "Country.Code")
            ) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

colSums(GFCF_geog[,c(7:16)])


##########################################################################
# 2.5 merge geographical and sectoral split for the part of GFCF columns with modelled geographical origin

GFCF_shares_geog <- GFCF_geog %>%
  left_join(
    GFCF_capex_geog,
    by = c(
      "Industry.Code" = "Industry.Code",
      "Industry.Name" = "Industry.Name",
      "Category.Breakdown.Item.Techecon" = "Category.Breakdown.Item"
    ),
    suffix = c(".geog", ".base")
  )

# multiply matching tech-spec columns row-wise
for (col in technologies) {
  geog_col <- paste0(col, ".geog")
  base_col <- paste0(col, ".base")
  GFCF_shares_geog[[col]] <- coalesce(GFCF_shares_geog[[geog_col]], 0) * coalesce(GFCF_shares_geog[[base_col]], 0)
}

# sum rows with disaggregated sectors that are split due to different Category.Breakdown.Item
country_order <- GFCF_shares_geog %>%
  distinct(Country.Code) %>%
  pull(Country.Code)
GFCF_shares_geog <- GFCF_shares_geog %>%
  select(-ends_with(".geog"), -ends_with(".base"), -Category.Breakdown.Item.Techecon) %>%
  group_by(Country.Code, Industry.Code, Industry.Name) %>%
  summarise(across(all_of(technologies), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(Country.Code = factor(Country.Code, levels = country_order)) %>%
  arrange(Country.Code)

# expand to get columns with the same shares for each modelled country
expanded_cols <- lapply(countries, function(country) {
  GFCF_shares_geog %>%
    select(all_of(technologies)) %>%
    setNames(paste0(country, ".", technologies))
})
GFCF_shares_geog <- bind_cols(
  GFCF_shares_geog %>% select(Country.Code, Industry.Code, Industry.Name),
  do.call(bind_cols, expanded_cols)
)

colSums(GFCF_shares_geog[,c(4:273)])


##########################################################################
# 2.6 prepare and perform geographical and sectoral split for the part of GFCF columns with domestic allocation

# allocate the remaining (=not specified by geog origin in the source data) shares domestically
# columns to retain
meta_cols <- setdiff(names(GFCF_capex_dom), c(technologies, "Index", "Geo.Orig.Split"))

# expand rows for all countries
expanded_rows <- expand_grid(
  Country.Code = countries_all,
  GFCF_capex_dom %>% select(all_of(meta_cols), all_of(technologies))
)

# create new column names for EU27 countries
expanded_tech_colnames <- expand_grid(
  country = countries,
  tech = technologies
) %>%
  mutate(colname = paste0(country, ".", tech)) %>%
  pull(colname)

# initialize new columns with NA
GFCF_shares_dom <- expanded_rows %>%
  mutate(across(everything(), ~ .)) %>%
  bind_cols(
    as_tibble(matrix(NA, nrow = nrow(expanded_rows), ncol = length(expanded_tech_colnames),
                     dimnames = list(NULL, expanded_tech_colnames)))
  )

# fill new columns with original values where Country.Code matches
for (country in countries) {
  for (tech in technologies) {
    new_col <- paste0(country, ".", tech)
    GFCF_shares_dom[[new_col]] <- ifelse(
      GFCF_shares_dom$Country.Code == country,
      GFCF_shares_dom[[tech]],
      GFCF_shares_dom[[new_col]]  # keep as NA
    )
  }
}

# drop original i40.* columns
GFCF_shares_dom <- GFCF_shares_dom %>%
  select(-all_of(technologies)) %>%
  relocate(Country.Code)

# replace NAs in numeric columns with 0
GFCF_shares_dom <- GFCF_shares_dom %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# sum rows with disaggregated sectors that are split due to different Category.Breakdown.Item
country_order <- GFCF_shares_dom %>%
  distinct(Country.Code) %>%
  pull(Country.Code)
GFCF_shares_dom <- GFCF_shares_dom %>%
  select(-Category.Breakdown.Item) %>%
  group_by(Country.Code, Industry.Code, Industry.Name) %>%
  summarise(across(matches("^\\w{2}\\.i40\\."), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(Country.Code = factor(Country.Code, levels = country_order)) %>%
  arrange(Country.Code)

colSums(GFCF_shares_dom[,c(4:273)])


##########################################################################
# 2.7 overlay part allocated geographically with capex_geog, and part allocated domestically

colSums(GFCF_shares_geog[,c(4:273)]) + colSums(GFCF_shares_dom[,c(4:273)])

# make sure both dataframes are aligned
stopifnot(all(names(GFCF_shares_geog) == names(GFCF_shares_dom)))

# define which columns are numeric (i.e., not metadata)
meta_cols <- c("Country.Code", "Industry.Code", "Industry.Name")
numeric_cols <- setdiff(names(GFCF_shares_geog), meta_cols)

# sum the two dataframes
GFCF_shares <- GFCF_shares_geog
GFCF_shares[numeric_cols] <- GFCF_shares_geog[numeric_cols] + GFCF_shares_dom[numeric_cols]

colSums(GFCF_shares[,c(4:273)])

# rescale each column to sum to 1
# (to fix very minor differences that occurred during data wrangling)
GFCF_shares[numeric_cols] <- lapply(GFCF_shares[numeric_cols], function(col) {
  total <- sum(col, na.rm = TRUE)
  if (total == 0) rep(0, length(col)) else col / total
})

colSums(GFCF_shares[,c(4:273)])


##########################################################################
# 2.8 relalculate shares to "real" gfcf values from the scenario for 2020

# load "real" gfcf values from the scenario
GFCF_sums <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "5_scen_elh2")
GFCF_sums <- GFCF_sums[,c(1:3,23:27)]
GFCF_sums <- GFCF_sums %>%
  filter(Industry.Code %in% technologies) # temporary selection of only the modelled technologies

# loop over years and scenario versions
# map each year and scenario version to its corresponding GFCF_sums column
version_map <- list(
  `2020` = "gfcf.20",
  `2030_CentHt` = "gfcf.CentHt.30",
  `2030_CentMod` = "gfcf.CentMod.30",
  `2050_CentHt` = "gfcf.CentHt.50",
  `2050_CentMod` = "gfcf.CentMod.50"
)

# # map each year to its corresponding GFCF_sums column
# year_map <- list(
#   `2020` = "gfcf.20",
#   `2030` = "gfcf.CentHt.30",
#   `2050` = "gfcf.CentHt.50"
# )

# columns to preserve - identify all share columns (exclude metadata)
meta_cols <- c("Country.Code", "Industry.Code", "Industry.Name")
value_cols <- setdiff(names(GFCF_shares), meta_cols)

# initialize an empty list
GFCF_list <- list()

# loop over each year, scenario version and GFCF column to multiply shares with sums
for (version in names(version_map)) {
  gfcf_col <- version_map[[version]]
  
  # make a copy of shares
  GFCF <- GFCF_shares
  
  for (colname in value_cols) {
    country <- str_extract(colname, "^[A-Z]{2}")
    tech <- str_remove(colname, "^[A-Z]{2}\\.")
    
    gfcf_value <- GFCF_sums %>%
      filter(Country.Code == country, Industry.Code == tech) %>%
      summarise(value = sum(.data[[gfcf_col]], na.rm = TRUE)) %>%
      pull(value)
    
    if (length(gfcf_value) == 0 || is.na(gfcf_value)) gfcf_value <- 0
    
    GFCF[[colname]] <- GFCF[[colname]] * gfcf_value
  }
  
  # rownames(GFCF) <- paste0(IOsupdet.codes$Country.Code, "_", IOsupdet.codes$Industry.Code)
  GFCF_list[[version]] <- GFCF[, sapply(GFCF, is.numeric)]
  # GFCF_list[[version]] <- GFCF
}


##########################################################################
# 2.9 create GFCFsupdet.codes for the later footprint calculation

GFCFsupdet.codes <- data.frame(Country.Code = rep(countries_all, each = 10),
                               Category.Code = rep(paste0("y04.",technologies), times = 49))
GFCFsupdet.codes$Key <- paste0(GFCFsupdet.codes$Country.Code,".",GFCFsupdet.codes$Category.Code)

GFCFsupdet_EU.codes <- data.frame(Country.Code = rep(countries, each = 10),
                                  Category.Code = rep(paste0("y04.",technologies), times = 27))
GFCFsupdet_EU.codes$Key <- paste0(GFCFsupdet_EU.codes$Country.Code,".",GFCFsupdet_EU.codes$Category.Code)


##########################################################################
# 3. export
##########################################################################

saveRDS(GFCF_shares, file = paste0(exiopath,"GFCF_shares_2020.rds"))
for (version in names(version_map)) {
  saveRDS(GFCF_list[[version]], file = paste0(exiopath, "GFCF_", version, ".rds"))
}
# saveRDS(GFCF, file = paste0(exiopath,"GFCF_2020.rds"))

saveRDS(GFCFsupdet.codes, file = paste0(exiopath,"GFCFsupdet.codes.rds"))
saveRDS(GFCFsupdet_EU.codes, file = paste0(exiopath,"GFCFsupdet_EU.codes.rds"))


