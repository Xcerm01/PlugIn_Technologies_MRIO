##########################################################################
# 04_el_h2_eu_intermed

# create technical coefficients and extension (incl. value added) vectors for the modelled technologies
# including implementation of geographical origin (all domestically in the first version)

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

# modelled direct supplier (dirsup) sectors
sup_codes <- unique(read_xlsx(paste0(datapath,"el_h2_eu_inpdata.xlsx"), sheet = "5_scen_inpdir")[,3])
sup_codes <- sup_codes[!grepl("alk|pem", sup_codes$Industry.Code),]
sup_codes_vec <- sup_codes$Industry.Code

metal_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i28")]
machin_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i29")]
elec_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i31")]
comm_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i32")]
constr_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i45")]

sup_names <- unique(read_xlsx(paste0(datapath,"el_h2_eu_inpdata.xlsx"), sheet = "5_scen_inpdir")[,2])
sup_names <- sup_names[!grepl("green hydrogen", sup_names$Industry.Name),]
sup_names_vec <- sup_names$Industry.Name

metal_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of fabricated metal products, except machinery and equipment (28)")]
machin_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of machinery and equipment n.e.c. (29)")]
elec_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of electrical machinery and apparatus n.e.c. (31)")]
comm_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of radio, television and communication equipment and apparatus (32)")]
constr_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Construction (45)")]



##########################################################################
# 2. load super-disaggregated IO objects (IOsupdet.codes, xsupdet, Zsupdet, Esupdet), prepare Asupdet and Extsupdet
##########################################################################

##########################################################################
# 2.1 load IOsupdet.codes, xsupdet, Zsupdet, Esupdet, Q.codes_395

IOsupdet.codes <- readRDS(file = paste0(exiopath, "IOsupdet.codes.rds"))
xsupdet_2020 <- readRDS(file = paste0(exiopath,"xsupdet_2020.rds"))
Zsupdet_2020_alloc <- readRDS(file = paste0(exiopath,"Zsupdet_2020_alloc.rds"))
Esupdet_2020 <- readRDS(file = paste0(exiopath,"Esupdet_2020.rds"))
Q.codes_395 <- readRDS(file = paste0(parsedpath, "Q.codes.rds"))


##########################################################################
# 2.2 calculate Asupdet from Zsupdet and xsupdet

gc()

Asupdet_2020 <- t(t(Zsupdet_2020_alloc) / xsupdet_2020)
Asupdet_2020[!is.finite(Asupdet_2020)] <- 0


##########################################################################
# 2.3 calculate Extsupdet from Esupdet and xsupdet

gc()

Extsupdet_2020 <- t(t(Esupdet_2020) / xsupdet_2020)
Extsupdet_2020[!is.finite(Extsupdet_2020)] <- 0


##########################################################################
# 2.4 prepare adjusted IOsupdet.codes_global

IOsupdet.codes_global <- readRDS(file = paste0(exiopath, "IOsupdet.codes.rds"))
IOsupdet.codes_global <- IOsupdet.codes_global %>%
  group_by(Industry.Code, Industry.Name) %>%
  summarise()
IOsupdet.codes_global$Index <- seq_len(nrow(IOsupdet.codes_global))
IOsupdet.codes_global <- IOsupdet.codes_global[,c(3,2,1)]



##########################################################################
# 3. prepare O&M shares and perform disaggregations for the modelled tech-spec sectors
##########################################################################

##########################################################################
# 3.1 prepare replacement O&M columns with sectoral origin of inputs for the modelled tech-spec sectors

opex <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "3_inpdir_sec")
opex <- opex[grepl("Opex breakdown",opex$Category),c(1:4,9:14)]

# spread opex
industry_code_order <- opex %>%
  distinct(Industry.Code) %>%
  pull(Industry.Code)

opex_wide <- opex %>%
  group_by(Industry.Inp.Dir.Name, Industry.Inp.Dir.Code, Category.Breakdown.Item, Geo.Orig.Split, Industry.Code) %>%
  summarise(Alloc = sum(Alloc.Inp.Dir.20, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Industry.Code,
    values_from = Alloc,
    values_fill = 0
  )

opex_wide <- opex_wide %>%
  arrange(as.numeric(str_extract(Industry.Inp.Dir.Code, "(?<=i)\\d+")))
opex_wide <- opex_wide %>%
  relocate(Industry.Inp.Dir.Name, Industry.Inp.Dir.Code, Category.Breakdown.Item, Geo.Orig.Split, all_of(industry_code_order))

# join IOsupdet.codes_global with capex
Asupdet_opex <- IOsupdet.codes_global %>%
  left_join(opex_wide, by = c("Industry.Code" = "Industry.Inp.Dir.Code")) %>%
  select(-Industry.Inp.Dir.Name) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

colSums(Asupdet_opex[,c(6:15)])


##########################################################################
# 3.2 prepare and perform geographical and sectoral split for the O&M columns for the modelled tech-spec sectors
# (all with domestic allocation as for now)

# allocate the remaining (=not specified by geog origin in the source data - all as for now!) shares domestically
# columns to retain
meta_cols <- setdiff(names(Asupdet_opex), c(technologies, "Index", "Geo.Orig.Split"))

# expand rows for all countries
expanded_rows <- expand_grid(
  Country.Code = countries_all,
  Asupdet_opex %>% select(all_of(meta_cols), all_of(technologies))
)

# create new column names for EU27 countries
expanded_tech_colnames <- expand_grid(
  country = countries,
  tech = technologies
) %>%
  mutate(colname = paste0(country, ".", tech)) %>%
  pull(colname)

# initialize new columns with NA
Asupdet_opex <- expanded_rows %>%
  mutate(across(everything(), ~ .)) %>%
  bind_cols(
    as_tibble(matrix(NA, nrow = nrow(expanded_rows), ncol = length(expanded_tech_colnames),
                     dimnames = list(NULL, expanded_tech_colnames)))
  )

# fill new columns with original values where Country.Code matches
for (country in countries) {
  for (tech in technologies) {
    new_col <- paste0(country, ".", tech)
    Asupdet_opex[[new_col]] <- ifelse(
      Asupdet_opex$Country.Code == country,
      Asupdet_opex[[tech]],
      Asupdet_opex[[new_col]]  # keep as NA
    )
  }
}

# drop original i40.* columns
Asupdet_opex <- Asupdet_opex %>%
  select(-all_of(technologies)) %>%
  relocate(Country.Code)

# replace NAs in numeric columns with 0
Asupdet_opex <- Asupdet_opex %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# sum rows with disaggregated sectors that are split due to different Category.Breakdown.Item
country_order <- Asupdet_opex %>%
  distinct(Country.Code) %>%
  pull(Country.Code)
Asupdet_opex <- Asupdet_opex %>%
  select(-Category.Breakdown.Item) %>%
  group_by(Country.Code, Industry.Code, Industry.Name) %>%
  summarise(across(matches("^\\w{2}\\.i40\\."), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(Country.Code = factor(Country.Code, levels = country_order)) %>%
  arrange(Country.Code)

colSums(Asupdet_opex[,c(4:273)])


##########################################################################
# 3.3 integrate replaced columns to Asupdet for the modelled tech-spec sectors

# prepare columns to be replaced
Asupdet_2020_alloc <- Asupdet_2020 %>% as.data.frame()

cols_to_replace <- intersect(colnames(Asupdet_2020_alloc), colnames(Asupdet_opex))
colSums(Asupdet_opex[cols_to_replace])
colSums(Asupdet_2020_alloc[cols_to_replace])

Asupdet_opex <- Asupdet_opex %>% as.data.frame()
rownames(Asupdet_opex) <- paste0(Asupdet_opex$Country.Code, ".", Asupdet_opex$Industry.Code)
Asupdet_opex <- Asupdet_opex[,-c(1:3)]

# rescale columns in Asupdet_opex to match colsums in Asupdet_2020_alloc
# note - we do this because in the first version, we do not model the value added on a technology-specific basis
# identify column names to process
opex_cols <- colnames(Asupdet_opex)

# calculate actual column sums from Asupdet_2020_alloc
alloc_sums <- colSums(Asupdet_2020_alloc[opex_cols], na.rm = TRUE)

# extract country and tech info
col_info <- tibble(
  colname = opex_cols,
  country = str_extract(colname, "^[A-Z]{2}"),
  tech = str_remove(colname, "^[A-Z]{2}\\."),
  group = case_when(
    str_detect(colname, "i40\\.11\\.e") ~ "i40.11.e",
    str_detect(colname, "i40\\.11\\.h") ~ "i40.11.h",
    TRUE ~ NA_character_
  ),
  alloc_sum = alloc_sums
)

# fallback for zero alloc_sums: find non-zero alloc_sum in same country + group
col_info <- col_info %>%
  group_by(country, group) %>%
  mutate(
    fallback_sum = if_else(
      alloc_sum == 0,
      # Use mean of non-zero group sums as fallback
      mean(alloc_sum[alloc_sum > 0], na.rm = TRUE),
      alloc_sum
    )
  ) %>%
  ungroup()

# build named vector of final target sums
target_sums <- col_info %>%
  select(colname, fallback_sum) %>%
  deframe()

# rescale opex so that each column sums to the corresponding alloc sum (or fallback)
Asupdet_opex_scaled <- Asupdet_opex
for (col in opex_cols) {
  current_sum <- sum(Asupdet_opex[[col]], na.rm = TRUE)
  target_sum <- target_sums[[col]]
  
  if (!is.na(current_sum) && !is.na(target_sum)) {
    if (current_sum > 0 && target_sum > 0) {
      Asupdet_opex_scaled[[col]] <- Asupdet_opex[[col]] / current_sum * target_sum
    } else if (current_sum == 0 && target_sum > 0) {
      n <- nrow(Asupdet_opex)
      Asupdet_opex_scaled[[col]] <- rep(target_sum / n, n)
    } else {
      Asupdet_opex_scaled[[col]] <- 0
    }
  } else {
    Asupdet_opex_scaled[[col]] <- 0
  }
}

# compare colsums again
colSums(Asupdet_2020_alloc[cols_to_replace]) # old
colSums(Asupdet_opex_scaled[cols_to_replace]) # new

# replace columns in the "original" Asupdet (now as Asupdet_2020_alloc)
Asupdet_2020_alloc[cols_to_replace] <- Asupdet_opex_scaled[cols_to_replace]



##########################################################################
# 4. prepare O&M shares and perform disaggregations for the dirsup sectors
##########################################################################

##########################################################################
# 4.1 prepare replacement O&M columns with sectoral origin of inputs for the dirsup sectors

opex_dirsup <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "3_inpind_sec")
opex_dirsup <- opex_dirsup[grepl("Matex breakdown",opex_dirsup$Category),c(1:4,9:14)]

# spread opex_dirsup
industry_code_order <- opex_dirsup %>%
  distinct(Industry.Inp.Dir.Code) %>%
  pull(Industry.Inp.Dir.Code)

opex_dirsup_wide <- opex_dirsup %>%
  group_by(Industry.Inp.Ind1.Name, Industry.Inp.Ind1.Code, Category.Breakdown.Item, Geo.Orig.Split, Industry.Inp.Dir.Code) %>%
  summarise(Alloc = sum(Alloc.Inp.Ind1.20, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Industry.Inp.Dir.Code,
    values_from = Alloc,
    values_fill = 0
  )

opex_dirsup_wide <- opex_dirsup_wide %>%
  arrange(as.numeric(str_extract(Industry.Inp.Ind1.Code, "(?<=i)\\d+")))
opex_dirsup_wide <- opex_dirsup_wide %>%
  relocate(Industry.Inp.Ind1.Name, Industry.Inp.Ind1.Code, Category.Breakdown.Item, Geo.Orig.Split, all_of(industry_code_order))

# join IOsupdet.codes_global with capex
Asupdet_opex_dirsup <- IOsupdet.codes_global %>%
  left_join(opex_dirsup_wide, by = c("Industry.Code" = "Industry.Inp.Ind1.Code")) %>%
  select(-Industry.Inp.Ind1.Name) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

colSums(Asupdet_opex_dirsup[,c(6:15)])


##########################################################################
# 4.2 prepare and perform geographical and sectoral split for the O&M columns for the dirsup sectors
# (all with the same geographical allocation as their parent sectors)

# calculate input shares from the supplying sectors of concern ("Inp.Ind2") from the original parent dirsup sectors
# (note - takes about 30s to 1 min)
Asupdet_2020_parent <- cbind(IOsupdet.codes, Asupdet_2020)

# select relevant columns whose names end with any suffix in 'parent'
parent_cols <- grep(paste0("\\.", parent, "$", collapse = "|"), names(Asupdet_2020_parent), value = TRUE)

# identify which rows to preserve (i.e., intersecting Industry codes)
valid_rows <- Asupdet_2020_parent$Industry.Code %in% opex_dirsup_wide$Industry.Inp.Ind1.Code

# keep only Industry.Code + parent columns
Asupdet_2020_parent <- Asupdet_2020_parent %>%
  select(Industry.Name, Industry.Code, Country.Code, all_of(parent_cols))

# set values in non-matching rows to 0
Asupdet_2020_parent <- Asupdet_2020_parent %>%
  mutate(across(
    all_of(parent_cols),
    ~ ifelse(valid_rows, ., 0)
  ))

# rescale (normalise) column-wise sums so it sums to 1
Asupdet_2020_parent <- Asupdet_2020_parent %>%
  mutate(across(
    all_of(parent_cols),
    function(x) {
      s <- sum(x, na.rm = TRUE)
      if (s == 0) rep(0, length(x)) else x / s
    }
  ))

colSums(Asupdet_2020[,parent_cols]) # old
colSums(Asupdet_2020_parent[,c(4:248)]) # new


# aggregate inputs by valid_rows and country
Asupdet_2020_geog <- Asupdet_2020_parent

# add tech-spec dirsup columns into Asupdet_2020_geog
# define numeric columns, drop rows where all numeric columns are 0
numeric_cols <- setdiff(names(Asupdet_2020_geog), c("Country.Code", "Industry.Code", "Industry.Name"))

# generate new columns based on dirsup
new_cols_list <- list()
for (col in numeric_cols) {
  for (suffix in dirsup) {
    new_name <- paste0(col, ".", suffix)
    new_cols_list[[new_name]] <- Asupdet_2020_geog[[col]]
  }
}

# bind new columns
Asupdet_2020_geog_expanded <- bind_cols(Asupdet_2020_geog, as_tibble(new_cols_list))

# sort the columns by country prefix
id_cols <- c("Country.Code", "Industry.Name", "Industry.Code")

# extract only country-prefixed columns (e.g. AT.i28, BE.i28.eh, etc.)
data_cols <- setdiff(names(Asupdet_2020_geog_expanded), id_cols)

# deal with column order
expected_col_order <- c()
for (country in countries_all) {
  for (sector in parent) {
    # add parent sector column
    col_base <- paste0(country, ".", sector)
    if (col_base %in% data_cols) {
      expected_col_order <- c(expected_col_order, col_base)
    }
    
    # add disaggregated columns (e.g. AT.i28.other, AT.i28.eh, etc.)
    for (suffix in dirsup) {
      col_detailed <- paste0(country, ".", sector, ".", suffix)
      if (col_detailed %in% data_cols) {
        expected_col_order <- c(expected_col_order, col_detailed)
      }
    }
  }
}

# reorder columns
final_col_order <- c(id_cols, expected_col_order)
Asupdet_2020_geog <- Asupdet_2020_geog_expanded %>%
  select(all_of(final_col_order))

# checking colsums before proceeding
colSums(Asupdet_opex_dirsup[,c(6:55)])
colSums(Asupdet_2020_geog[,c(4:3188)])


# allocate the shares according to the geog origin of the concerned inputs in the parent sectors
# group Asupdet_opex_dirsup by industries only (and not Category.Breakdown.Item)
skip_cols <- c("Index", "Category.Breakdown.Item", "Geo.Orig.Split")
industry_order <- unique(Asupdet_opex_dirsup$Industry.Code)
Asupdet_opex_dirsup <- Asupdet_opex_dirsup %>%
  select(-any_of(skip_cols)) %>%
  group_by(Industry.Code, Industry.Name) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(Industry.Code = factor(Industry.Code, levels = industry_order)) %>%
  arrange(Industry.Code)

# create a copy of Asupdet_2020_geog to modify
Asupdet_2020_geog_scaled <- Asupdet_2020_geog

# apply scaling
# note - does not add up to colsum 1, as we are omitting many entries
# it is necessary to normalize to a sum of 1 in the next step

# identify numeric columns (skip ID columns)
id_cols <- c("Industry.Code", "Industry.Name", "Country.Code")
numeric_cols <- setdiff(names(Asupdet_2020_geog), id_cols)

# extract suffix from each numeric column (after first dot)
col_suffixes <- str_remove(numeric_cols, "^[A-Z]{2}\\.")  # e.g., "AT.i28.eh" → "i28.eh"
names(col_suffixes) <- numeric_cols  # keep reference to full names

for (col in numeric_cols) {
  suffix <- col_suffixes[[col]]

  # check if Asupdet_opex_dirsup has a column with this suffix
  if (suffix %in% names(Asupdet_opex_dirsup)) {
    
    # create a named vector of multipliers by Industry.Code
    multipliers <- Asupdet_opex_dirsup[[suffix]]
    names(multipliers) <- Asupdet_opex_dirsup$Industry.Code

    # get corresponding Industry.Code in Asupdet_2020_geog_scaled
    industry_vector <- Asupdet_2020_geog_scaled$Industry.Code

    # multiply only where the Industry.Code exists in the multipliers
    Asupdet_2020_geog_scaled[[col]] <- Asupdet_2020_geog_scaled[[col]] *
      ifelse(industry_vector %in% names(multipliers),
             multipliers[industry_vector],
             1)
    }
  }

# check colsums
# note - does not add up to colsum 1, as we are omitting many entries
# it is necessary to normalize to a sum of 1 in the next step
colSums(Asupdet_2020_geog_scaled[,c(4:3188)])

# rescale (normalise) column-wise sums so it sums to 1
Asupdet_2020_geog_scaled <- Asupdet_2020_geog_scaled %>%
  mutate(across(
    all_of(numeric_cols),
    function(x) {
      s <- sum(x, na.rm = TRUE)
      if (s == 0) rep(0, length(x)) else x / s
    }
  ))

colSums(Asupdet_2020_geog_scaled[,c(4:3188)])


# expand Asupdet_opex_dirsup to get columns by EU27 countries
# (for 5.2, to get a version with columns only for the actually modeled dirsup sectors, but by EU27 countries)
id_cols <- Asupdet_opex_dirsup %>%
  select(Industry.Name, Industry.Code)
numeric_cols <- Asupdet_opex_dirsup %>%
  select(where(is.numeric))
expanded_numeric <- purrr::map_dfc(countries, function(ctry) {
  temp <- numeric_cols
  colnames(temp) <- paste0(ctry, ".", colnames(temp))
  temp
})
Asupdet_opex_dirsup <- bind_cols(id_cols, expanded_numeric)


##########################################################################
# 4.3 integrate replaced columns to Asupdet for the dirsup sectors

# create a copy of Asupdet_2020_alloc to modify
# (Asupdet_2020_alloc is created from Asupdet_opex_scaled in 3.3 anyway)
Asupdet_opex_scaled <- Asupdet_2020_alloc

# prepare columns to be replaced
cols_to_replace <- intersect(colnames(Asupdet_opex_scaled), colnames(Asupdet_2020_geog_scaled))
colSums(Asupdet_2020_geog_scaled[cols_to_replace])
colSums(Asupdet_opex_scaled[cols_to_replace])

Asupdet_2020_geog_scaled <- Asupdet_2020_geog_scaled %>% as.data.frame()
rownames(Asupdet_2020_geog_scaled) <- paste0(Asupdet_2020_geog_scaled$Country.Code, ".", Asupdet_2020_geog_scaled$Industry.Code)
Asupdet_2020_geog_scaled <- Asupdet_2020_geog_scaled[,-c(1:3)]

# rescale columns in Asupdet_2020_geog_scaled to match colsums in Asupdet_opex_scaled
# note - we do this because in the first version, we do not model the value added on a technology-specific basis
# identify column names to process
opex_cols <- colnames(Asupdet_2020_geog_scaled)

# calculate actual column sums from Asupdet_opex_scaled
alloc_sums <- colSums(Asupdet_opex_scaled[opex_cols], na.rm = TRUE)

# extract country and tech info
col_info <- tibble(
  colname = opex_cols,
  country = str_extract(colname, "^[A-Z]{2}"),
  tech = str_remove(colname, "^[A-Z]{2}\\."),
  group = case_when(
    str_detect(colname, "i28") ~ "i28",
    str_detect(colname, "i29") ~ "i29",
    str_detect(colname, "i31") ~ "i31",
    str_detect(colname, "i32") ~ "i32",
    str_detect(colname, "i45") ~ "i45",
    TRUE ~ NA_character_
  ),
  alloc_sum = alloc_sums
)

# define fallback and overwrite fallback_sum in all (country, group) with alloc_sum from parent colname (e.g., AT.i28)
col_info <- col_info %>%
  mutate(parent_colname = paste0(country, ".", group))
parent_sums <- col_info %>%
  filter(colname == parent_colname) %>%
  select(country, group, parent_sum = alloc_sum)
col_info <- col_info %>%
  left_join(parent_sums, by = c("country", "group")) %>%
  mutate(fallback_sum = parent_sum)

# build named vector of final target sums
target_sums <- col_info %>%
  select(colname, fallback_sum) %>%
  deframe()

# rescale opex so that each column sums to the corresponding alloc sum (or fallback)
Asupdet_opex_dirsup_scaled <- Asupdet_2020_geog_scaled
for (col in opex_cols) {
  current_sum <- sum(Asupdet_2020_geog_scaled[[col]], na.rm = TRUE)
  target_sum <- target_sums[[col]]
  
  if (!is.na(current_sum) && !is.na(target_sum)) {
    if (current_sum > 0 && target_sum > 0) {
      Asupdet_opex_dirsup_scaled[[col]] <- Asupdet_2020_geog_scaled[[col]] / current_sum * target_sum
    } else if (current_sum == 0 && target_sum > 0) {
      n <- nrow(Asupdet_2020_geog_scaled)
      Asupdet_opex_dirsup_scaled[[col]] <- rep(target_sum / n, n)
    } else {
      Asupdet_opex_dirsup_scaled[[col]] <- 0
    }
  } else {
    Asupdet_opex_dirsup_scaled[[col]] <- 0
  }
}

# compare colsums again
colSums(Asupdet_opex_dirsup_scaled[cols_to_replace]) # new
colSums(Asupdet_2020_alloc[cols_to_replace]) # old

# replace columns in the "original" Asupdet (now as Asupdet_2020_alloc)
Asupdet_2020_alloc[cols_to_replace] <- Asupdet_opex_dirsup_scaled[cols_to_replace]



##########################################################################
# 5. perform disaggregations in the extensions (value added and footprints)
##########################################################################

# the extensions (intensity) matrix, incl. the value added, also copies the columns for the so far empty cols,
# from the "parent" (already existent, "operational" tech-spec modelled sectors), exactly as in Asupdet

##########################################################################
# 5.1 replace empty columns in Extsupdet for the modelled tech-spec sectors

# prepare columns to be replaced
Extsupdet_2020_alloc <- Extsupdet_2020 %>% as.data.frame()
colnames(Extsupdet_2020_alloc) <- paste0(IOsupdet.codes$Country.Code, ".", IOsupdet.codes$Industry.Code)

# replace empty disaggregated columns in Extsupdet_2020_alloc
# note - we do this because in the first version, we do not model the value added and extensions on a tech-spec basis
# fallback country map in case some columns are completely empty even in the original version of Extsupdet_2020_alloc
fallback_country_map <- c(
  AT = "DE",  # Austria → Germany
  BE = "NL",  # Belgium → Netherlands
  BG = "RO",  # Bulgaria → Romania
  CY = "EL",  # Cyprus → Greece
  CZ = "SK",  # Czechia → Slovakia
  DE = "FR",  # Germany → France
  DK = "SE",  # Denmark → Sweden
  EE = "FI",  # Estonia → Finland
  EL = "IT",  # Greece → Italy
  ES = "PT",  # Spain → Portugal
  FI = "SE",  # Finland → Sweden
  FR = "DE",  # France → Germany
  HR = "SI",  # Croatia → Slovenia
  HU = "PL",  # Hungary → Poland
  IE = "UK",  # Ireland → UK (still relevant data-wise)
  IT = "FR",  # Italy → France
  LT = "LV",  # Lithuania → Latvia
  LU = "BE",  # Luxembourg → Belgium
  LV = "LT",  # Latvia → Lithuania
  MT = "IT",  # Malta → Italy
  NL = "DE",  # Netherlands → Germany
  PL = "CZ",  # Poland → Czechia
  PT = "ES",  # Portugal → Spain
  RO = "BG",  # Romania → Bulgaria
  SE = "FI",  # Sweden → Finland
  SI = "HR",  # Slovenia → Croatia
  SK = "CZ"   # Slovakia → Czechia
)

# identify replacement columns (only those of the actually modelled tech-spec sectors)
shared_cols <- intersect(colnames(Extsupdet_2020_alloc), colnames(Asupdet_opex))

# extract country and tech info
col_info <- tibble(
  colname = shared_cols,
  country = str_extract(shared_cols, "^[A-Z]{2}"),
  tech = str_remove(shared_cols, "^[A-Z]{2}\\."),
  group = case_when(
    str_detect(shared_cols, "i40\\.11\\.e") ~ "i40.11.e",
    str_detect(shared_cols, "i40\\.11\\.h") ~ "i40.11.h",
    TRUE ~ NA_character_
  )
)

# loop over zero-valued columns
for (col in shared_cols) {
  if (all(Extsupdet_2020_alloc[[col]] == 0, na.rm = TRUE)) {
    
    this_country <- str_extract(col, "^[A-Z]{2}")
    this_group <- if (str_detect(col, "i40\\.11\\.e")) "i40.11.e"
    else if (str_detect(col, "i40\\.11\\.h")) "i40.11.h"
    else NA_character_
    
    # candidate fallback columns from same country
    candidate_cols <- col_info %>%
      filter(country == this_country, group == this_group, colname != col) %>%
      pull(colname)
    
    # find first non-zero fallback column in that country
    fallback_col <- NULL
    for (c in candidate_cols) {
      if (!all(Extsupdet_2020_alloc[[c]] == 0, na.rm = TRUE)) {
        fallback_col <- c
        break
      }
    }
    
    # if not found, use fallback country
    if (is.null(fallback_col) && this_country %in% names(fallback_country_map)) {
      alt_country <- fallback_country_map[[this_country]]
      
      alt_col <- col_info %>%
        filter(country == alt_country, group == this_group) %>%
        pull(colname)
      
      for (c in alt_col) {
        if (!all(Extsupdet_2020_alloc[[c]] == 0, na.rm = TRUE)) {
          fallback_col <- c
          break
        }
      }
    }
    
    # replace column if fallback was found
    if (!is.null(fallback_col)) {
      Extsupdet_2020_alloc[[col]] <- Extsupdet_2020_alloc[[fallback_col]]
    } else {
      message("No fallback found for: ", col)
    }
  }
}


##########################################################################
# 5.2 replace empty columns in Extsupdet for the dirsup sectors

# prepare columns to be replaced
# create a copy of Extsupdet_2020_alloc to modify (will be merged at the end)
# (Extsupdet_2020_alloc is created in 5.1 for the modelled tech-spec sectors only)
Extsupdet_2020_dirsup_alloc <- Extsupdet_2020_alloc %>% as.data.frame()
colnames(Extsupdet_2020_dirsup_alloc) <- paste0(IOsupdet.codes$Country.Code, ".", IOsupdet.codes$Industry.Code)

# replace empty disaggregated columns in Extsupdet_2020_dirsup_alloc
# note - we do this because in the first version, we do not model the value added and extensions on a tech-spec basis
# fallback country map in case some columns are completely empty even in the original version of Extsupdet_2020_alloc
fallback_country_map <- c(
  AT = "DE",  # Austria → Germany
  BE = "NL",  # Belgium → Netherlands
  BG = "RO",  # Bulgaria → Romania
  CY = "EL",  # Cyprus → Greece
  CZ = "SK",  # Czechia → Slovakia
  DE = "FR",  # Germany → France
  DK = "SE",  # Denmark → Sweden
  EE = "FI",  # Estonia → Finland
  EL = "IT",  # Greece → Italy
  ES = "PT",  # Spain → Portugal
  FI = "SE",  # Finland → Sweden
  FR = "DE",  # France → Germany
  HR = "SI",  # Croatia → Slovenia
  HU = "PL",  # Hungary → Poland
  IE = "UK",  # Ireland → UK (still relevant data-wise)
  IT = "FR",  # Italy → France
  LT = "LV",  # Lithuania → Latvia
  LU = "BE",  # Luxembourg → Belgium
  LV = "LT",  # Latvia → Lithuania
  MT = "IT",  # Malta → Italy
  NL = "DE",  # Netherlands → Germany
  PL = "CZ",  # Poland → Czechia
  PT = "ES",  # Portugal → Spain
  RO = "BG",  # Romania → Bulgaria
  SE = "FI",  # Sweden → Finland
  SI = "HR",  # Slovenia → Croatia
  SK = "CZ"   # Slovakia → Czechia
)

# identify replacement columns (only those of the actually modelled tech-spec sectors)
shared_cols <- intersect(colnames(Extsupdet_2020_dirsup_alloc), colnames(Asupdet_opex_dirsup))

# extract country and tech info
col_info <- tibble(
  colname = shared_cols,
  country = str_extract(shared_cols, "^[A-Z]{2}"),
  tech = str_remove(shared_cols, "^[A-Z]{2}\\."),
  group = case_when(
    str_detect(colname, "i28") ~ "i28",
    str_detect(colname, "i29") ~ "i29",
    str_detect(colname, "i31") ~ "i31",
    str_detect(colname, "i32") ~ "i32",
    str_detect(colname, "i45") ~ "i45",
    TRUE ~ NA_character_
  )
)

# find fallback column recursively in case values are missing in the fallback countries too (they are :-))
find_recursive_fallback <- function(origin_country, group, col_info, data, visited = character()) {
  # prevent infinite loop
  if (origin_country %in% visited) return(NULL)
  visited <- c(visited, origin_country)
  
  # candidate columns from this country + group
  candidate_cols <- col_info %>%
    filter(country == origin_country, group == group) %>%
    pull(colname)
  
  for (c in candidate_cols) {
    col_data <- data[[c]]
    if (!all(is.na(col_data) | col_data == 0)) {
      return(c)  # found usable fallback column
    }
  }
  
  # recurse to fallback country if one exists
  if (origin_country %in% names(fallback_country_map)) {
    next_country <- fallback_country_map[[origin_country]]
    return(find_recursive_fallback(next_country, group, col_info, data, visited))
  }
  
  return(NULL)  # no usable fallback found
}

# loop over zero-valued columns
for (col in shared_cols) {
  col_data <- Extsupdet_2020_dirsup_alloc[[col]]
  if (all(is.na(col_data) | col_data == 0)) {
    
    this_country <- str_extract(col, "^[A-Z]{2}")
    this_group <- if (str_detect(col, "i28")) "i28"
    else if (str_detect(col, "i29")) "i29"
    else if (str_detect(col, "i31")) "i31"
    else if (str_detect(col, "i32")) "i32"
    else if (str_detect(col, "i45")) "i45"
    else NA_character_
    
    if (is.na(this_group)) {
      message("Group not recognized for column: ", col)
      next
    }
    
    # first try local fallback in same country
    candidate_cols <- col_info %>%
      filter(country == this_country, group == this_group, colname != col) %>%
      pull(colname)
    
    fallback_col <- NULL
    for (c in candidate_cols) {
      col_c_data <- Extsupdet_2020_dirsup_alloc[[c]]
      if (!all(is.na(col_c_data) | col_c_data == 0)) {
        fallback_col <- c
        break
      }
    }
    
    # recursive fallback if needed
    if (is.null(fallback_col)) {
      fallback_col <- find_recursive_fallback(this_country, this_group, col_info, Extsupdet_2020_dirsup_alloc)
    }
    
    if (!is.null(fallback_col)) {
      Extsupdet_2020_dirsup_alloc[[col]] <- Extsupdet_2020_dirsup_alloc[[fallback_col]]
    } else {
      message("No fallback found for: ", col)
    }
  }
}

Extsupdet_2020_alloc <- Extsupdet_2020_dirsup_alloc



##########################################################################
# 6. export
##########################################################################

saveRDS(Asupdet_2020_alloc, file = paste0(exiopath,"Asupdet_2020_alloc.rds"))
saveRDS(Extsupdet_2020_alloc, file = paste0(exiopath,"Extsupdet_2020_alloc.rds"))


