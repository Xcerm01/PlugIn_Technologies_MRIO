##########################################################################
# 01_el_h2_eu_disaggtech

# disaggregate the relevant parts of MRIO by modelled technologies

# input data:
# exiobase 3.9.5 monetary ixi 2020: https://zenodo.org/records/14869924
# parsed data: set by user
# detailed tech data and scenarios: set by user (input file in the repository)

# libraries:
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(readxl)



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

# all detailed tech-spec sectors
wind_codes <- c(
  "i40.11.e.1", "i40.11.e.1.i", "i40.11.e.1.i.turb", "i40.11.e.1.i.air", "i40.11.e.1.i.airflex", "i40.11.e.1.i.airrig",
  "i40.11.e.1.ii", "i40.11.e.1.ii.turb", "i40.11.e.1.ii.air", "i40.11.e.1.ii.airflex", "i40.11.e.1.ii.airrig",
  "i40.11.e.2", "i40.11.e.2.i", "i40.11.e.2.i.turb", "i40.11.e.2.ii", "i40.11.e.2.ii.turb",
  "i40.11.e.2.ii.air", "i40.11.e.2.ii.airflex", "i40.11.e.2.ii.airrig"
)
wind_names <- c(
  "Production of electricity by wind onshore",
  "Production of electricity by wind onshore utility",
  "Production of electricity by wind onshore utility turbine",
  "Production of electricity by wind onshore utility airborne",
  "Production of electricity by wind onshore utility airborne flexible",
  "Production of electricity by wind onshore utility airborne rigid",
  "Production of electricity by wind onshore distributed",
  "Production of electricity by wind onshore distributed turbine",
  "Production of electricity by wind onshore distributed airborne",
  "Production of electricity by wind onshore distributed airborne flexible",
  "Production of electricity by wind onshore distributed airborne rigid",
  "Production of electricity by wind offshore",
  "Production of electricity by wind offshore fixed-bottom",
  "Production of electricity by wind offshore fixed-bottom turbine",
  "Production of electricity by wind offshore floating",
  "Production of electricity by wind offshore floating turbine",
  "Production of electricity by wind offshore floating airborne",
  "Production of electricity by wind offshore floating airborne flexible",
  "Production of electricity by wind offshore floating airborne rigid"
)
pv_codes <- c(
  "i40.11.h.1.i", "i40.11.h.1.i.sil", "i40.11.h.1.i.thf", "i40.11.h.1.i.per", "i40.11.h.1.i.tand",
  "i40.11.h.1.ii", "i40.11.h.1.ii.sil", "i40.11.h.1.ii.thf", "i40.11.h.1.ii.per", "i40.11.h.1.ii.tand",
  "i40.11.h.1.iii", "i40.11.h.1.iii.sil", "i40.11.h.1.iii.thf", "i40.11.h.1.iii.per", "i40.11.h.1.iii.tand",
  "i40.11.h.1.iii.org", "i40.11.h.2", "i40.11.h.2.sil", "i40.11.h.2.thf", "i40.11.h.2.per", "i40.11.h.2.tand", "i40.11.h.2.org"
)
pv_names <- c(
  "Production of electricity by solar photovoltaic utility",
  "Production of electricity by solar photovoltaic utility silicon-based panels",
  "Production of electricity by solar photovoltaic utility thin-film panels",
  "Production of electricity by solar photovoltaic utility perovskite solar cells",
  "Production of electricity by solar photovoltaic utility tandem photovoltaics",
  "Production of electricity by solar photovoltaic commercial",
  "Production of electricity by solar photovoltaic commercial silicon-based panels",
  "Production of electricity by solar photovoltaic commercial thin-film panels",
  "Production of electricity by solar photovoltaic commercial perovskite solar cells",
  "Production of electricity by solar photovoltaic commercial tandem photovoltaics",
  "Production of electricity by solar photovoltaic residential",
  "Production of electricity by solar photovoltaic residential silicon-based panels",
  "Production of electricity by solar photovoltaic residential thin-film panels",
  "Production of electricity by solar photovoltaic residential perovskite solar cells",
  "Production of electricity by solar photovoltaic residential tandem photovoltaics",
  "Production of electricity by solar photovoltaic residential organic photovoltaics",
  "Production of electricity by solar photovoltaic off-grid",
  "Production of electricity by solar photovoltaic off-grid silicon-based panels",
  "Production of electricity by solar photovoltaic off-grid thin-film panels",
  "Production of electricity by solar photovoltaic off-grid perovskite solar cells",
  "Production of electricity by solar photovoltaic off-grid tandem photovoltaics",
  "Production of electricity by solar photovoltaic off-grid organic photovoltaics"
)



##########################################################################
# 2. calculate technology shares for the disaggregations
##########################################################################

##########################################################################
# 2.1 load data from the master table

scen_elh2 <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "5_scen_elh2")
scen_el <- scen_elh2[!startsWith(scen_elh2$Industry.Code, "i40.41"),]

gen_2020 <- scen_el[,c("Country.Code", "Industry.Name", "Industry.Code", "gen.20")]
gen_2020 <- gen_2020 %>%
  mutate(Tech.Type = case_when(
    str_starts(Industry.Code, "i40.11.e") ~ "wind",
    str_starts(Industry.Code, "i40.11.h") ~ "pv",
    TRUE ~ NA_character_
  ))


##########################################################################
# 2.2 calculate shares of the detailed sectors to disaggregate the rows or columns of x, Y, Z and E

# construct artificial total wind row per country by summing i40.11.e.1 + i40.11.e.2
wind_totals <- gen_2020 %>%
  filter(Industry.Code %in% c("i40.11.e.1", "i40.11.e.2")) %>%
  group_by(Country.Code) %>%
  summarise(
    gen.20 = sum(gen.20, na.rm = TRUE),
    Industry.Name = "Production of electricity by wind",
    Industry.Code = "i40.11.e",
    Tech.Type = "wind",
    .groups = "drop"
  )

# add the artificial wind total row
gen_2020 <- bind_rows(gen_2020, wind_totals)
gen_2020 <- gen_2020 %>%
  arrange(
    Country.Code,
    case_when(
      Industry.Code == "i40.11.e" ~ 0,  # wind aggregate row first
      TRUE ~ 1                         # everything else in existing order
    )
  )

# calculate share.20 = tech / total within each Country and Tech.Type
gen_2020 <- gen_2020 %>%
  group_by(Country.Code, Tech.Type) %>%
  mutate(share.20 = gen.20 / gen.20[Industry.Code %in% c("i40.11.e", "i40.11.h")]) %>%
  ungroup()

# get lookup table for technology shares
share_lookup <- gen_2020 %>%
  filter(Tech.Type %in% c("wind", "pv")) %>%
  select(Industry.Code, Country.Code, Tech.Type, share.20)



##########################################################################
# 3. disaggregate the IO objects (IO.codes, x, Y, Z, E)
##########################################################################

##########################################################################
# 3.1 create disaggregated version of IO.codes (->IOdet.codes)

IOdet.codes <- readRDS(file = paste0(parsedpath, "IO.codes.rds"))
IO.codes_395 <- IOdet.codes # save the non-disaggregated version

insert_positions <- c()

# loop through original IOdet.codes to get insertion indices to add new rows or columns into x, Y, Z, and E
for (i in seq_len(nrow(IOdet.codes))) {
  row <- IOdet.codes[i, ]
  if (row$Industry.Code == "i40.11.e" && row$Industry.Name == "Production of electricity by wind") {
    insert_positions <- c(insert_positions, rep(i, 19))  # 19 NA rows after each
  }
  if (row$Industry.Code == "i40.11.h" && row$Industry.Name == "Production of electricity by solar photovoltaic") {
    insert_positions <- c(insert_positions, rep(i, 22))  # 22 NA rows after each
  }
}

# offset to account for growing vector length during insertion
offset <- 0

# define insertion metadata for wind and solar
insert_specs <- list(
  list(
    match_code = "i40.11.e",
    match_name = "Production of electricity by wind",
    new_codes = wind_codes,
    new_names = wind_names
  ),
  list(
    match_code = "i40.11.h",
    match_name = "Production of electricity by solar photovoltaic",
    new_codes = pv_codes,
    new_names = pv_names
  )
)

# helper function to insert rows after index
insert_rows_after <- function(df, idx, codes, names, region_code, country_code) {
  new_rows <- data.frame(
    Industry.Code = codes,
    Industry.Name = names,
    Region.Code = region_code,
    Country.Code = country_code,
    stringsAsFactors = FALSE
  )
  # fill in missing columns with NA
  new_rows[setdiff(names(df), names(new_rows))] <- NA
  new_rows <- new_rows[names(df)]
  df_top <- df[1:idx, ]
  df_bottom <- df[(idx + 1):nrow(df), ]
  bind_rows(df_top, new_rows, df_bottom)
}

# loop through the dataframe and apply both insertions
i <- 1
while (i <= nrow(IOdet.codes)) {
  row <- IOdet.codes[i, ]
  for (spec in insert_specs) {
    if (row$Industry.Code == spec$match_code && row$Industry.Name == spec$match_name) {
      IOdet.codes <- insert_rows_after(
        IOdet.codes, i,
        spec$new_codes, spec$new_names,
        row$Region.Code, row$Country.Code
      )
      i <- i + length(spec$new_codes)  # skip over inserted rows
      break  # avoid matching both rules for the same row
    }
  }
  i <- i + 1
}

# re-index
IOdet.codes$Index <- seq_len(nrow(IOdet.codes))


##########################################################################
# 3.2 create disaggregated version of x (->xdet)

xdet_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_x.rds"))

# insert NAs into xdet_2020 at the correct positions
for (pos in insert_positions) {
  xdet_2020 <- append(xdet_2020, NA, after = pos + offset)
  offset <- offset + 1
}

# loop through IOdet.codes and fill in new rows
for (i in seq_len(nrow(IOdet.codes))) {
  row <- IOdet.codes[i, ]
  
  if (row$Industry.Code %in% c("i40.11.e", "i40.11.h")) {
    agg_value <- xdet_2020[i]
    tech_type <- ifelse(row$Industry.Code == "i40.11.e", "wind", "pv")
    country <- row$Country.Code
    
    # Select correct code list
    tech_codes <- if (tech_type == "wind") wind_codes else pv_codes
    insert_start <- i + 1
    insert_end <- insert_start + length(tech_codes) - 1
    insert_indices <- insert_start:insert_end
    
    # Get share values in correct order
    shares <- share_lookup %>%
      filter(Country.Code == country, Industry.Code %in% tech_codes) %>%
      mutate(order = match(Industry.Code, tech_codes)) %>%
      arrange(order)
    
    if (country %in% countries && nrow(shares) == length(tech_codes)) {
      xdet_2020[insert_indices] <- agg_value * shares$share.20
    } else {
      xdet_2020[insert_indices] <- 0
    }
  }
}

# merge with IOdet.codes to check the result
xdet_2020_test <- cbind(IOdet.codes, xdet_2020)


##########################################################################
# 3.3 create disaggregated version of Y (->Ydet)

Ydet_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_Y.rds"))

# create empty list to store rows
Ydet_2020_list <- list()
row_idx <- 1  # row tracker for disaggregated matrix

for (i in seq_len(nrow(IO.codes_395))) {
  row <- IO.codes_395[i, ]
  country <- row$Country.Code
  ind_code <- row$Industry.Code
  orig_row_values <- Ydet_2020[i, , drop = FALSE]
  
  # always add the original row first
  Ydet_2020_list[[row_idx]] <- orig_row_values
  row_idx <- row_idx + 1
  
  # check if the row should be disaggregated (wind or pv)
  if (ind_code %in% c("i40.11.e", "i40.11.h")) {
    tech_type <- ifelse(ind_code == "i40.11.e", "wind", "pv")
    n_new_rows <- ifelse(tech_type == "wind", 19, 22)
    
    # get the corresponding disaggregated industry codes
    disagg_codes <- IOdet.codes[(row_idx):(row_idx + n_new_rows - 1), "Industry.Code"]
    
    # get matching tech shares in the same order as disaggregated codes
    tech_shares <- share_lookup %>%
      filter(Country.Code == country, Industry.Code %in% disagg_codes) %>%
      mutate(Industry.Code = factor(Industry.Code, levels = disagg_codes)) %>%
      arrange(Industry.Code)
    
    # fill new rows using the shares
    if (nrow(tech_shares) == n_new_rows) {
      for (j in seq_len(n_new_rows)) {
        share <- tech_shares$share.20[j]
        new_row <- orig_row_values * share
        Ydet_2020_list[[row_idx]] <- new_row
        row_idx <- row_idx + 1
      }
    } else {
      
      # insert zero rows if shares are missing (non-EU or mismatched)
      for (j in seq_len(n_new_rows)) {
        Ydet_2020_list[[row_idx]] <- matrix(0, nrow = 1, ncol = ncol(Ydet_2020))
        row_idx <- row_idx + 1
      }
    }
  }
}

# combine into final matrix
Ydet_2020 <- do.call(rbind, Ydet_2020_list)

# merge with IOdet.codes to check the result
Ydet_2020_test <- cbind(IOdet.codes, Ydet_2020)


##########################################################################
# 3.4 create disaggregated version of Z (->Zdet)

Zdet_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_Z.rds"))

# create empty list to store rows
Zdet_2020_list <- list()
row_idx <- 1  # row tracker for disaggregated matrix

for (i in seq_len(nrow(IO.codes_395))) {
  row <- IO.codes_395[i, ]
  country <- row$Country.Code
  ind_code <- row$Industry.Code
  orig_row_values <- Zdet_2020[i, , drop = FALSE]
  
  # always add the original row first
  Zdet_2020_list[[row_idx]] <- orig_row_values
  row_idx <- row_idx + 1
  
  # check if the row should be disaggregated (wind or pv)
  if (ind_code %in% c("i40.11.e", "i40.11.h")) {
    tech_type <- ifelse(ind_code == "i40.11.e", "wind", "pv")
    n_new_rows <- ifelse(tech_type == "wind", 19, 22)
    
    # get the corresponding disaggregated industry codes
    disagg_codes <- IOdet.codes[(row_idx):(row_idx + n_new_rows - 1), "Industry.Code"]
    
    # get matching tech shares in the same order as disaggregated codes
    tech_shares <- share_lookup %>%
      filter(Country.Code == country, Industry.Code %in% disagg_codes) %>%
      mutate(Industry.Code = factor(Industry.Code, levels = disagg_codes)) %>%
      arrange(Industry.Code)
    
    # fill new rows using the shares
    if (nrow(tech_shares) == n_new_rows) {
      for (j in seq_len(n_new_rows)) {
        share <- tech_shares$share.20[j]
        new_row <- orig_row_values * share
        Zdet_2020_list[[row_idx]] <- new_row
        row_idx <- row_idx + 1
      }
    } else {
      
      # insert zero rows if shares are missing (non-EU or mismatched)
      for (j in seq_len(n_new_rows)) {
        Zdet_2020_list[[row_idx]] <- matrix(0, nrow = 1, ncol = ncol(Zdet_2020))
        row_idx <- row_idx + 1
      }
    }
  }
}

# combine into final matrix
Zdet_2020 <- do.call(rbind, Zdet_2020_list)

# merge with IOdet.codes to check the result
Zdet_2020_test <- cbind(IOdet.codes, Zdet_2020)

# transpose to allow column-wise disaggregation
Zdet_2020 <- t(Zdet_2020)

# create empty list to store rows
Zdet_2020_list <- list()
row_idx <- 1  # row tracker for disaggregated matrix

for (i in seq_len(nrow(IO.codes_395))) {
  row <- IO.codes_395[i, ]
  country <- row$Country.Code
  ind_code <- row$Industry.Code
  orig_row_values <- Zdet_2020[i, , drop = FALSE]
  
  # always add the original row first
  Zdet_2020_list[[row_idx]] <- orig_row_values
  row_idx <- row_idx + 1
  
  # check if the row should be disaggregated (wind or pv)
  if (ind_code %in% c("i40.11.e", "i40.11.h")) {
    tech_type <- ifelse(ind_code == "i40.11.e", "wind", "pv")
    n_new_rows <- ifelse(tech_type == "wind", 19, 22)
    
    # get the corresponding disaggregated industry codes
    disagg_codes <- IOdet.codes[(row_idx):(row_idx + n_new_rows - 1), "Industry.Code"]
    
    # get matching tech shares in the same order as disaggregated codes
    tech_shares <- share_lookup %>%
      filter(Country.Code == country, Industry.Code %in% disagg_codes) %>%
      mutate(Industry.Code = factor(Industry.Code, levels = disagg_codes)) %>%
      arrange(Industry.Code)
    
    # fill new rows using the shares
    if (nrow(tech_shares) == n_new_rows) {
      for (j in seq_len(n_new_rows)) {
        share <- tech_shares$share.20[j]
        new_row <- orig_row_values * share
        Zdet_2020_list[[row_idx]] <- new_row
        row_idx <- row_idx + 1
      }
    } else {
      
      # insert zero rows if shares are missing (non-EU or mismatched)
      for (j in seq_len(n_new_rows)) {
        Zdet_2020_list[[row_idx]] <- matrix(0, nrow = 1, ncol = ncol(Zdet_2020))
        row_idx <- row_idx + 1
      }
    }
  }
}

# combine into final matrix
Zdet_2020 <- do.call(rbind, Zdet_2020_list)

# merge with IOdet.codes to check the result
Zdet_2020_test <- cbind(IOdet.codes, Zdet_2020)

# transpose again to get the original position
Zdet_2020 <- t(Zdet_2020)

# merge with IOdet.codes to make a final check of the result
Zdet_2020_test <- cbind(IOdet.codes, Zdet_2020)


##########################################################################
# 3.3 create disaggregated version of E
# note: no differences in the accounts by technology assumed!

Edet_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_F.rds"))

# transpose to allow column-wise disaggregation
Edet_2020 <- t(Edet_2020)

# create empty list to store rows
Edet_2020_list <- list()
row_idx <- 1  # row tracker for disaggregated matrix

for (i in seq_len(nrow(IO.codes_395))) {
  row <- IO.codes_395[i, ]
  country <- row$Country.Code
  ind_code <- row$Industry.Code
  orig_row_values <- Edet_2020[i, , drop = FALSE]
  
  # always add the original row first
  Edet_2020_list[[row_idx]] <- orig_row_values
  row_idx <- row_idx + 1
  
  # check if the row should be disaggregated (wind or pv)
  if (ind_code %in% c("i40.11.e", "i40.11.h")) {
    tech_type <- ifelse(ind_code == "i40.11.e", "wind", "pv")
    n_new_rows <- ifelse(tech_type == "wind", 19, 22)
    
    # get the corresponding disaggregated industry codes
    disagg_codes <- IOdet.codes[(row_idx):(row_idx + n_new_rows - 1), "Industry.Code"]
    
    # get matching tech shares in the same order as disaggregated codes
    tech_shares <- share_lookup %>%
      filter(Country.Code == country, Industry.Code %in% disagg_codes) %>%
      mutate(Industry.Code = factor(Industry.Code, levels = disagg_codes)) %>%
      arrange(Industry.Code)
    
    # fill new rows using the shares
    if (nrow(tech_shares) == n_new_rows) {
      for (j in seq_len(n_new_rows)) {
        share <- tech_shares$share.20[j]
        new_row <- orig_row_values * share
        Edet_2020_list[[row_idx]] <- new_row
        row_idx <- row_idx + 1
      }
    } else {
      
      # insert zero rows if shares are missing (non-EU or mismatched)
      for (j in seq_len(n_new_rows)) {
        Edet_2020_list[[row_idx]] <- matrix(0, nrow = 1, ncol = ncol(Edet_2020))
        row_idx <- row_idx + 1
      }
    }
  }
}

# combine into final matrix
Edet_2020 <- do.call(rbind, Edet_2020_list)

# merge with IOdet.codes to check the result
Edet_2020_test <- cbind(IOdet.codes, Edet_2020)

# transpose again to get the original position
Edet_2020 <- t(Edet_2020)



##########################################################################
# 4. export
##########################################################################

saveRDS(IOdet.codes, file = paste0(exiopath,"IOdet.codes.rds"))
saveRDS(xdet_2020, file = paste0(exiopath,"xdet_2020.rds"))
saveRDS(Ydet_2020, file = paste0(exiopath,"Ydet_2020.rds"))
saveRDS(Zdet_2020, file = paste0(exiopath,"Zdet_2020.rds"))
saveRDS(Edet_2020, file = paste0(exiopath,"Edet_2020.rds"))


