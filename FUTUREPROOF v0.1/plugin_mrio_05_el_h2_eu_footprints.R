##########################################################################
# 05_el_h2_eu_footprints

# implement scenarios into all modelled parts of the MRIO and calculate footprints

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
# respath set by user


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

# scenario by year versions
versions <- c("2020","2030_CentMod","2030_CentHt","2050_CentMod","2050_CentHt") # modelled versions

# # years considered in the scenarios
# years = c("2020", "2030", "2050")  # years considered in the analysis

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

# parent direct supplier (dirsup) sectors
parent <- c("i28", "i29", "i31", "i32", "i45")

# detailed direct supplier (dirsup) sector suffixes
dirsup <- c(
  "other", "eh", "e.1.i.turb", "e.1.i.airrig", "e.2.i.turb", "e.2.ii.turb", 
  "h.1.i.sil", "h.1.i.tand", "h.1.ii.sil", "h.1.ii.tand", "h.1.iii.sil", "h.1.iii.tand"
)

# detailed direct supplier (dirsup) sector suffixes - only those actually modelled
dirsup_modelled <- c(
  "e.1.i.turb", "e.1.i.airrig", "e.2.i.turb", "e.2.ii.turb", 
  "h.1.i.sil", "h.1.i.tand", "h.1.ii.sil", "h.1.ii.tand", "h.1.iii.sil", "h.1.iii.tand"
  )

# detailed direct supplier (dirsup) sector suffixes - only those actually modelled, by parent sector
dirsup_modelled_parent <- as.vector(outer(parent, dirsup_modelled, paste, sep = "."))

# identify modelled tech-spec (so far only electricity from wind and solar as "el") sectors in the EU27+UK countries
technologies_pattern <- paste(technologies, collapse = "|")
el_eu <- IOsupdet.codes$Index[grepl(technologies_pattern, IOsupdet.codes$Industry.Code) & IOsupdet.codes$Region.Code == "EU"]

# identify modelled dirsup sectors in the EU27+UK countries
dirsup_pattern <- paste(dirsup_modelled_parent, collapse = "|")
dirsup_eu <- IOsupdet.codes$Index[grepl(dirsup_pattern, IOsupdet.codes$Industry.Code) & IOsupdet.codes$Region.Code == "EU"]

# identify all other than modelled tech-spec sectors
other_nonel <- IOsupdet.codes$Index[!grepl(technologies_pattern, IOsupdet.codes$Industry.Code) & IOsupdet.codes$Region.Code == "EU"]

# footprint indicators
stressor_patterns <- c(
  "Metal Ores", "Non-Metallic Minerals - Chemical minerals n.e.c.",
  "Non-Metallic Minerals - Industrial minerals n.e.c",
  "Non-Metallic Minerals - Industrial sand and gravel",
  "Employment people", "Employment hours"
  )
compartment_patterns <- c("Value.added")

stressor_regex <- paste(stressor_patterns, collapse = "|")
compartment_regex <- paste(compartment_patterns, collapse = "|")

footprints <- Q.codes_395$Index[grepl(stressor_regex, Q.codes_395$Stressor, ignore.case = TRUE) |
                                  grepl(compartment_regex, Q.codes_395$Compartment, ignore.case = TRUE)]



##########################################################################
# 2. implement scenarios into xsupdet, calculate Asupdetnoel, and Lsupdetnoel
##########################################################################

##########################################################################
# 2.1 load xsupdet_2020 and define map of versions (incl. years)

# load xsupdet_2020
xsupdet_2020 <- readRDS(file = paste0(exiopath,"xsupdet_2020.rds"))

# map each year and scenario version to its corresponding x column
version_map <- list(
  `2020` = "x.20",
  `2030_CentHt` = "x.CentHt.30",
  `2030_CentMod` = "x.CentMod.30",
  `2050_CentHt` = "x.CentHt.50",
  `2050_CentMod` = "x.CentMod.50"
)

# # map each year to its corresponding x column
# year_map <- list(
#   `2020` = "x.20",
#   `2030` = "x.CentHt.30",
#   `2050` = "x.CentHt.50"
# )


##########################################################################
# 2.2 insert changes in total output (xsupdet) for each modelled year for the modelled tech-spec sectors

# load scenarios for the modelled tech-spec sectors
scen_elh2 <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "5_scen_elh2")
scen_el_modelled <- scen_elh2[grepl(technologies_pattern, scen_elh2$Industry.Code),]

# insert changes for each modelled year into xsupdet
xsupdet_el_list <- list()

for (version in names(version_map)) {
  xsupdet_el_list[[version]] <- xsupdet_2020
  
  # only apply changes if not 2020
  if (!grepl("2020$", version)) {
    xsupdet_el_list[[version]][el_eu] <- scen_el_modelled[[version_map[[version]]]] # replace values at the named indices
  }
}


##########################################################################
# 2.3 insert changes in total output (xsupdet) for each modelled year for the dirsup sectors

# note - this is likely not necessary, as x for dirsup sectors is modelled via x for tech-spec sectors and Lsupdet

# # load scenarios for the modelled tech-spec sectors
# scen_dir <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "5_scen_inpdir")
# scen_dir_modelled <- scen_dir[grepl(dirsup_pattern, scen_dir$Industry.Code),]
# 
# # insert changes for each modelled year into xsupdet
# xsupdet_eldirsup_list <- list()
# 
# for (version in names(version_map)) {
#   xsupdet_eldirsup_list[[version]] <- xsupdet_2020
#   
#   # only apply changes if not 2020
#   if (!grepl("2020$", version)) {
#     xsupdet_eldirsup_list[[version]][dirsup_eu] <- scen_dir_modelled[[version_map[[version]]]] # replace values at the named indices
#   }
# }


##########################################################################
# 2.4 merge changes in xsupdet for the modelled tech-spec sectors (2.1) and for the dirsup sectors (2.2)

# note - this is likely not necessary, as x for dirsup sectors is modelled via x for tech-spec sectors and Lsupdet

# # final combined list
# xsupdet_list <- list()
# 
# # ensure both lists have the same years (versions)
# versions_intersect <- intersect(names(xsupdet_el_list), names(xsupdet_eldirsup_list))
# 
# for (version in versions_intersect) {
#   xsupdet_combined <- xsupdet_el_list[[version]]
#   xsupdet_combined[dirsup_eu] <- xsupdet_eldirsup_list[[version]][dirsup_eu] # replace dirsup_eu rows with updated values
#   xsupdet_list[[version]] <- xsupdet_combined # save to final list
# }



##########################################################################
# 3. calculate Lsupdet
##########################################################################

##########################################################################
# 3.1 load Asupdet, check memory and calculate Leontief inverse (Lsupdet)

# load Asupdet and Zsupdet
Asupdet_alloc <- readRDS(file = paste0(exiopath,"Asupdet_2020_alloc.rds"))
Zsupdet_alloc <- readRDS(file = paste0(exiopath,"Zsupdet_2020_alloc.rds"))

# check if there are any negative values
which(Asupdet_alloc < 0, arr.ind = TRUE)
which(Zsupdet_alloc < 0, arr.ind = TRUE)

# check memory
gc()

# calculate Lsupdet
# note - takes about 55 min
Lsupdet <- solve(diag(nrow(Zsupdet_alloc))-Asupdet_alloc)

# check if there are any negative values and if they are truly negligible, threshold tiny negatives to zero
# note - it is normal to get small negative values (e.g., -1e-15, -3e-14, etc.) in the result.
# solve() uses floating-point arithmetic internally - small errors accumulate when solving large or nearly singular systems
# so even if A is non-negative, its inverse may involve small negative elements - especially if the matrix is nearly singular
# if this happens for instance for one country, that one country might have a numerically unstable technical coefficient structure
# (e.g., nearly linearly dependent sectors, very small flows, etc.)
which(Lsupdet < 0, arr.ind = TRUE)
Lsupdet[Lsupdet < 0 & Lsupdet > -1e-10] <- 0

# export
saveRDS(Lsupdet, file = paste0(exiopath,"Lsupdet_2020.rds"))



##########################################################################
# 4. calculate footprints
##########################################################################

# all results for the EU27 as a whole and for regions/countries in RoW
# by each modelled tech-spec sector
# by indicator groups and disaggregated indicators (perhaps even by industries/divisions? in the case of skill levels)

# domestic = "country" is Country.Origin (note that I can only calculate such domestic effects for the EU27 area, mostly because I don't calculate GFCF effects in the RoW)
# aggregated for EU27 is a "pure domestic" = where Country.Destination matches Country.Origin
# foreign (abroad, RoW) = where Country.Destination is "country" and Country.Origin is anything else but "country" :-)

##########################################################################
# 4.1 prepare Extsupdet_eue by extracting indicators we are interested in, prepare Extsupdet.names and other sets

# load Extsupdet
Extsupdet_eue <- readRDS(file = paste0(exiopath,"Extsupdet_2020_alloc.rds"))

# extract indicators we are interested in
Extsupdet_eue <- Extsupdet_eue[footprints,]
Extsupdet_eue <- t(Extsupdet_eue)

# check if there are any negative values (note - here some are actually ok in the value added part)
which(Extsupdet_eue < 0, arr.ind = TRUE)

# create Extsupdet.names for the selected indicators
Extsupdet.names <- Q.codes_395$Stressor[footprints]
Extsupdet.names <- gsub(": ", "_", Extsupdet.names)

# prepare clustering of value added
value_added_patterns <- c(
  "Taxes less subsidies", "Other net taxes",
  "Compensation of employees",
  "Operating surplus"
)
value_added_regex <- paste(value_added_patterns, collapse = "|")
value_added <- Extsupdet.names[grepl(value_added_regex, Extsupdet.names)]

# prepare clustering of domestic mineral extraction
minerals <- Extsupdet.names[grepl("Domestic Extraction Used", Extsupdet.names)]

# prepare separating results for employment only
employment <- Extsupdet.names[grepl("Employment", Extsupdet.names)]
employment_people <- Extsupdet.names[grepl("Employment people", Extsupdet.names)]
employment_hours <- Extsupdet.names[grepl("Employment hours", Extsupdet.names)]


##########################################################################
# 4.2 try a test run and prepare functions to calculate footprints (requirements)

# # test run - calculate a GFCF footprint (FP = MP * GFCF_list)
# # GFCF_list = GFCF of the modelled tech-spec sectors
# version <- "2020"
# indicator <- 5 # high-skilled employment compensation
# 
# MP <- Lsupdet * Extsupdet_eue[,indicator]
# FP <- MP %*% as.matrix(GFCF_list[[version]][,GFCFsupdet_EU.codes$Country.Code==country])
# FP <- cbind(IOsupdet.codes, data.frame(Version = version,
#                                        Country.Destination = country,
#                                        Indicator = Extsupdet.names[indicator],
#                                        Effect = "GFCF",
#                                        FP))
# FP <- data.table::melt(as.data.table(FP),
#                        id.vars = c(colnames(IOsupdet.codes),"Version","Country.Destination","Indicator","Effect"),
#                        variable.name = "Electricity",
#                        value.name = "Value")
# FP[, Electricity := substr(FP$Electricity, 4, 25)] # 25 is perfectly enough (depends on the length of the code)

# define O&M footprint function
footprint_om <- function(MP,Extsupdet_eue,indicator,results){
  
  # calculate footprint (FP = MP * x). x = total output
  # note - MP (Multiplier Matrix) is calculated later in the loop for the actual footprint calculation
  FP <- t(t(MP) * x_data)
  FP <- cbind(IOsupdet.codes, data.frame(Version = version,
                                         Indicator = Extsupdet.names[indicator],
                                         Effect = "O&M",
                                         FP))
  FP <- data.table::melt(as.data.table(FP),
                         id.vars = c(colnames(IOsupdet.codes),"Version","Indicator","Effect"),
                         variable.name = "Electricity",
                         value.name = "Value")
  FP[, Country.Destination := substr(FP$Electricity, 1, 2)]
  FP[, Electricity := substr(FP$Electricity, 4, 25)]
  FP <- FP[Electricity %in% technologies,]
  
  results <- rbindlist(list(results, FP), use.names = TRUE)
}

# define GFCF footprint function
footprint_gfcf <- function(MP,Extsupdet_eue,indicator,country,results){
  
  # calculate footprint (FP = MP * GFCF_list). GFCF_list = GFCF of the modelled tech-spec sectors
  # note - MP (Multiplier Matrix) is calculated later in the loop for the actual footprint calculation
  FP <- MP %*% as.matrix(Y_data[,GFCFsupdet_EU.codes$Country.Code==country])
  FP <- cbind(IOsupdet.codes, data.frame(Version = version,
                                         Country.Destination = country,
                                         Indicator = Extsupdet.names[indicator],
                                         Effect = "GFCF",
                                         FP))
  FP <- data.table::melt(as.data.table(FP),
                         id.vars = c(colnames(IOsupdet.codes),"Version","Country.Destination","Indicator","Effect"),
                         variable.name = "Electricity",
                         value.name = "Value")
  FP[, Electricity := substr(FP$Electricity, 4, 25)]
  
  results <- rbindlist(list(results, FP), use.names = TRUE)
}


##########################################################################
# 4.3 remove all big unnecessary objects that are not further used (except for original EXIOBASE objects)

remove(
  # A
  Asupdet_2020, Asupdet_2020_alloc, Asupdet_2020_parent,
  Asupdet_2020_geog, Asupdet_2020_geog_expanded, Asupdet_2020_geog_scaled,
  Asupdet_opex, Asupdet_opex_dirsup, Asupdet_opex_dirsup_scaled, Asupdet_opex_scaled,
  # capex
  capex, capex_geog, capex_geog_wide, capex_wide,
  # E
  Edet_2020, Edet_2020_list, Edet_2020_test, Esupdet_2020, Esupdet_2020_list, Esupdet_2020_test,
  Extsupdet_2020, Extsupdet_2020_alloc, Extsupdet_2020_dirsup_alloc,
  # GFCF
  GFCF, GFCF_capex, GFCF_capex_dom, GFCF_capex_geog, GFCF_geog, GFCF_shares, GFCF_shares_dom, GFCF_shares_geog,
  GFCF_sums,
  # IO.codes
  IO.codes_395, IO.codes_395_selected, IOdet.codes, IOsupdet.codes_global, IOY.codes_395,
  # opex
  opex, opex_dirsup, opex_dirsup_wide, opex_wide,
  # scen
  scen_dir, scen_dir_modelled, scen_el, scen_el_modelled, scen_elh2, share_lookup, share_lookup_eh, share_lookup_ZY,
  # VA
  VA_2020, VA_2020_inp,
  # x
  x_2020_inp, xdet_2020_test, xsupdet_2020_test, xsupdet_combined, xsupdet_eldirsup_list, xsupdet_list,
  # Y
  Y_2020, Y_2020_inp, Y.codes_395_renamed, Ydet_2020, Ydet_2020_list, Ydet_2020_test,
  Ysupdet_2020, Ysupdet_2020_list, Ysupdet_2020_test,
  # Z
  Z_2020, Z_2020_inp, Z_2020_inp_val, Zdet_2020, Zdet_2020_list, Zdet_2020_test,
  Zsupdet_2020, Zsupdet_2020_alloc, Zsupdet_2020_list, Zsupdet_2020_mod, Zsupdet_2020_test, Zsupdet_alloc,
  ZY_2020_inp, ZY_2020_inp_long, ZY_2020_inp_val,
  # rest
  col_info, expanded_cols, expanded_numeric, expanded_rows, gen_2020, mat_2020, mat_2020_eh, new_cols_list, spec
)

# check memory
gc()


##########################################################################
# 4.4 calculate footprints (requirements)

# calculate footprints (requirements)
# # yet another test run :-)
# versions_min <- versions[c(1,2)]
# countries_min <- countries[c(1,5)] # AT, CZ

# calculate footprints (requirements) - now really! :-)
# note - takes about 10 hours to run
results_eu27 <- list()
results_eu27_emp <- list()

gc()

for (version in versions) {
  print(paste0("version ", version))
  
  x_data <- xsupdet_el_list[[version]]
  Y_data <- GFCF_list[[version]]
  
  results_combined <- list()
  
  for (indicator in 1:ncol(Extsupdet_eue)) {
    print(paste0("extension ",indicator," from a total of ",ncol(Extsupdet_eue)))
    
    MP <- Lsupdet * Extsupdet_eue[,indicator]
    
    results <- data.table()
    results <- footprint_om(MP = MP, Ext = Extsupdet_eue, indicator = indicator, results = results)
    
    for (country in countries) {
      print(paste0("country ",country))
      results <- footprint_gfcf(MP = MP, Ext = Extsupdet_eue, indicator = indicator, country = country, results = results)
    }
    
    # clean and aggregate
    setDT(results)
    results[is.na(results)] <- 0
    results[, Version := as.character(Version)]
    
    setnames(results,"Country.Code","Country.Origin")
    
    results[Country.Destination %in% countries, Country.Destination := "EU27"]
    results[Country.Origin %in% countries, Country.Origin := "EU27"]
    results[Indicator %in% value_added, Indicator := "Value added"]
    
    aggregated <- results[
      , .(Value = sum(Value)), 
      by = .(Industry.Code, Industry.Name, Country.Origin, Region.Code, Version, Indicator, Electricity, Country.Destination)
    ]
    
    results_combined[[as.character(indicator)]] <- aggregated
  }
  
  # combine results across indicators for this version
  results_all <- rbindlist(results_combined, use.names = TRUE, fill = TRUE)
  
  # filter out sectors to avoid double-counting
  results_all <- results_all[!(Industry.Code %in% c(technologies_all, dirsup_all_parent))]
  
  # save employment separately
  results_eu27_emp[[version]] <- results_all[Indicator %in% employment_hours]
  fwrite(results_eu27_emp[[version]], file = paste0(respath, version, "_emp.csv"), sep = ";", dec = ".")
  
  # aggregate the rest over industries and electricity type
  results_all <- results_all[
    , .(Value = sum(Value)), 
    by = .(Country.Origin, Region.Code, Version, Indicator, Electricity, Country.Destination)
  ]
  results_eu27[[version]] <- results_all
  
  # split and export by electricity type
  for (el_type in technologies) {
    el_split <- results_all[Electricity == el_type]
    el_clean <- gsub("[ /]", "_", el_type)
    fwrite(el_split, file = paste0(respath,version,"_",el_clean,".csv"), sep = ";", dec = ".")
  }
}

# separate results by aggregated indicator groups
for (version in names(results_eu27)) {
  data_indicators <- results_eu27[[version]]
  
  # aggregate over indicator groups
  data_indicators$Indicator[data_indicators$Indicator %in% c(minerals)] <- "Domestic Extraction Used - Metal Ores and Selected Non-Metallic Minerals"
  data_indicators$Indicator[data_indicators$Indicator %in% c(employment_hours)] <- "Employment hours"
  data_indicators <- data_indicators %>%
    group_by(Country.Origin, Region.Code, Version, Indicator, Electricity, Country.Destination) %>%
    summarize(Value = sum(Value))
  
  # get all unique indicators
  indicators <- unique(data_indicators$Indicator)
  
  # loop over indicators and save each separately
  for (ind in indicators) {
    data_split <- data_indicators %>% filter(Indicator == ind)
    indicator_clean <- gsub("[ /]", "_", ind)
    
    # export
    fwrite(data_split, file = paste0(respath,version,"_",indicator_clean,".csv"), sep = ";", dec = ".")
  }
}


##########################################################################
# 4.5 calculate "nonel" footprints for the rest of the economy (requirements) to account for the EUE (2020 only)

# define nonel footprint function
footprint_nonel <- function(MP,Extsupdet_eue,indicator,results_rest){
  
  # calculate footprint (FP = MP * x). x = total output
  # note - MP (Multiplier Matrix) is calculated later in the loop for the actual footprint calculation
  FP <- t(t(MP) * x_data)
  FP <- cbind(IOsupdet.codes, data.frame(Version = version,
                                         Indicator = Extsupdet.names[indicator],
                                         FP))
  FP <- data.table::melt(as.data.table(FP),
                         id.vars = c(colnames(IOsupdet.codes),"Version","Indicator"),
                         variable.name = "Sector",
                         value.name = "Value")
  FP[, Country.Destination := substr(FP$Sector, 1, 2)]
  FP[, Sector := substr(FP$Sector, 4, 25)]
  FP <- FP[Sector %!in% technologies,]
  
  results_rest <- rbindlist(list(results_rest, FP), use.names = TRUE)
}


# calculate footprints (requirements)
# note - takes about 4 hours to run
results_rest <- list()

gc()

for (version in versions[1]) {
  print(paste0("version ", version))
  
  x_data <- xsupdet_el_list[["2020"]]
  results_base <- data.table()  # temp holder for this version
  
  for (indicator in 1:ncol(Extsupdet_eue)) {
    print(paste0("extension ",indicator," from a total of ",ncol(Extsupdet_eue)))
    
    MP <- Lsupdet * Extsupdet_eue[,indicator]
    
    tmp_result <- data.table()
    tmp_result <- footprint_nonel(MP = MP, Ext = Extsupdet_eue, indicator = indicator, results_rest = tmp_result)
    
    setDT(tmp_result)
    tmp_result[is.na(tmp_result)] <- 0
    tmp_result[, Version := as.character(Version)]
    
    setnames(tmp_result,"Country.Code","Country.Origin")
    
    tmp_result[Country.Destination %in% countries, Country.Destination := "EU27"]
    tmp_result[Country.Origin %in% countries, Country.Origin := "EU27"]
    tmp_result[Indicator %in% value_added, Indicator := "Value added"]
    
    tmp_result <- tmp_result[
      , .(Value = sum(Value)), 
      by = .(Industry.Code, Industry.Name, Country.Origin, Region.Code, Version, Indicator, Country.Destination)
    ]
    
    results_base <- rbindlist(list(results_base, tmp_result), use.names = TRUE, fill = TRUE)
  }
  
  # filter the modelled detailed sectors to avoid double-counting
  results_base <- results_base[!(Industry.Code %in% c(technologies_all, dirsup_all_parent))]
  
  # aggregate results over industries and sectors
  results_rest[[version]] <- results_base[
    , .(Value = sum(Value)), 
    by = .(Country.Origin, Region.Code, Version, Indicator, Country.Destination)
  ]
  
  # export
  fwrite(results_rest[[version]], file = paste0(respath, "rest_", version, ".csv"), sep = ";", dec = ".")
}

# separate results by aggregated indicator groups and export
for (version in names(results_rest)) {
  data_indicators <- results_rest[[version]]
  
  data_indicators$Indicator[data_indicators$Indicator %in% c(minerals)] <- "Domestic Extraction Used - Metal Ores and Selected Non-Metallic Minerals"
  data_indicators$Indicator[data_indicators$Indicator %in% c(employment_hours)] <- "Employment hours"
  data_indicators <- data_indicators %>%
    group_by(Country.Origin, Region.Code, Version, Indicator, Country.Destination) %>%
    summarize(Value = sum(Value), .groups = "drop")
  
  # get all unique indicators
  indicators <- unique(data_indicators$Indicator)
  
  # loop over indicators and save each separately
  for (ind in indicators) {
    data_split <- data_indicators %>% filter(Indicator == ind)
    indicator_clean <- gsub("[ /]", "_", ind)
    
    # export
    fwrite(data_split, file = paste0(respath,"rest_",version,"_",indicator_clean,".csv"), sep = ";", dec = ".")
  }
}


# remove objects that were only necessary in the footprints calculation
remove(MP, Y_data, x_data, aggregated, results, results_all, results_base, results_combined)


