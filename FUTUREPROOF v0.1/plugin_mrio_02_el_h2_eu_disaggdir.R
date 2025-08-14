##########################################################################
# 02_el_h2_eu_disaggdir

# disaggregate the relevant parts of the MRIO by sector-specific split-offs
# for direct supplier sectors of the modelled technologies

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

# modelled direct supplier (dirsup) sectors
sup_codes <- unique(read_xlsx(paste0(datapath,"el_h2_eu_inpdata.xlsx"), sheet = "5_scen_inpdir")[,3])
sup_codes <- sup_codes[!grepl("alk|pem", sup_codes$Industry.Code),]

metal_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i28")]
machin_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i29")]
elec_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i31")]
comm_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i32")]
constr_codes <- sup_codes$Industry.Code[startsWith(sup_codes$Industry.Code, "i45")]

sup_names <- unique(read_xlsx(paste0(datapath,"el_h2_eu_inpdata.xlsx"), sheet = "5_scen_inpdir")[,2])
sup_names <- sup_names[!grepl("green hydrogen", sup_names$Industry.Name),]

metal_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of fabricated metal products, except machinery and equipment (28)")]
machin_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of machinery and equipment n.e.c. (29)")]
elec_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of electrical machinery and apparatus n.e.c. (31)")]
comm_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Manufacture of radio, television and communication equipment and apparatus (32)")]
constr_names <- sup_names$Industry.Name[startsWith(sup_names$Industry.Name, "Construction (45)")]



##########################################################################
# 2. load direct supplier shares for the disaggregations
##########################################################################

# 1. direct suppliers by rows only supply "their" technology (appearing as a separate sector)
# 2. by columns, they only have inputs from sectors identified in the master table in sheet 3_indinp_sec
# 3. in the final demand categories (Ydet_2020), they supply the FD categories in equal shares as the parent sector,
# but in proportionally smaller volumes according to their share of supplies to the modelled sectors
# in intermediate consumption (Zdet) compared to the other sectors (see 5_exio_zrow_dir)


##########################################################################
# 2.1 load data from the master table

scen_dir <- readxl::read_excel(paste0(datapath, "el_h2_eu_inpdata.xlsx"), sheet = "5_scen_inpdir")

mat_2020 <- scen_dir[!grepl("alk|pem", scen_dir$Industry.Code),c("Country.Code", "Industry.Name", "Industry.Code", "xnorm.20")]
mat_2020 <- mat_2020 %>%
  mutate(Ind.Type = case_when(
    str_starts(Industry.Code, "i28") ~ "metal",
    str_starts(Industry.Code, "i29") ~ "machin",
    str_starts(Industry.Code, "i31") ~ "elec",
    str_starts(Industry.Code, "i32") ~ "comm",
    str_starts(Industry.Code, "i45") ~ "constr",
    TRUE ~ NA_character_
  ))


##########################################################################
# 2.2 calculate shares of the detailed sectors to disaggregate the rows or columns of x, Y, Z and E

# construct artificial total rows per country
# define the suffixes that are shared across all sectors
suffixes <- c("other" ,"eh", "e.1.i.turb", "e.1.airrig", "e.2.i.turb", "e.2.ii.turb",
              "h.1.i.sil", "h.1.i.tand", "h.1.ii.sil", "h.1.ii.tand", 
              "h.1.iii.sil", "h.1.iii.tand")

# define sector information with an explicit sort order
dirsup_sectors <- list(
  metal = list(prefix = "i28", agg_code = "i28", 
               name = "Manufacture of fabricated metal products, except machinery and equipment (28)", 
               type = "metal", order = 1),
  machinery = list(prefix = "i29", agg_code = "i29", 
                   name = "Manufacture of machinery and equipment n.e.c. (29)", 
                   type = "machin", order = 2),
  electrical = list(prefix = "i31", agg_code = "i31", 
                    name = "Manufacture of electrical machinery and apparatus n.e.c. (31)", 
                    type = "elec", order = 3),
  communication = list(prefix = "i32", agg_code = "i32", 
                       name = "Manufacture of radio, television and communication equipment and apparatus (32)", 
                       type = "comm", order = 4),
  construction = list(prefix = "i45", agg_code = "i45", 
                      name = "Construction (45)", 
                      type = "constr", order = 5)
)

# prepare a vector of aggregate codes and their order for sorting
agg_order <- sapply(dirsup_sectors, function(x) x$order)
names(agg_order) <- sapply(dirsup_sectors, function(x) x$agg_code)

# loop through each sector and add the artificial total row
for (sector in names(dirsup_sectors)) {
  info <- dirsup_sectors[[sector]]
  sector_codes <- paste0(info$prefix, ".", suffixes)
  
  # aggregate by country
  sector_totals <- mat_2020 %>%
    filter(Industry.Code %in% sector_codes) %>%
    group_by(Country.Code) %>%
    summarise(
      xnorm.20 = sum(xnorm.20, na.rm = TRUE),
      Industry.Name = info$name,
      Industry.Code = info$agg_code,
      Ind.Type = info$type,
      .groups = "drop"
    )
  
  # bind the new rows
  mat_2020 <- bind_rows(mat_2020, sector_totals)
}

# build full ordering vector: each sector's agg code followed by its suffix subcodes
sector_order_map <- unlist(lapply(dirsup_sectors, function(info) {
  c(info$agg_code, paste0(info$prefix, ".", suffixes))
}))

# create a named vector with ranks to apply
sector_rank <- seq_along(sector_order_map)
names(sector_rank) <- sector_order_map

# add ordering value to mat_2020 based on Industry.Code
mat_2020 <- mat_2020 %>%
  mutate(
    sector_rank = ifelse(Industry.Code %in% names(sector_rank),
                         sector_rank[Industry.Code], 
                         Inf)  # push non-matching codes to end
  ) %>%
  arrange(Country.Code, sector_rank) %>%
  select(-sector_rank)

# calculate share.20 = direct supplier by tech / total within each Country and Ind.Type
mat_2020 <- mat_2020 %>%
  group_by(Country.Code, Ind.Type) %>%
  mutate(share.20 = xnorm.20 / xnorm.20[Industry.Code %in% c("i28","i29","i31","i32","i45")]) %>%
  ungroup()

# get lookup table for direct supplier sector shares
share_lookup <- mat_2020 %>%
  filter(Ind.Type %in% c("metal","machin","elec","comm","constr")) %>%
  select(Industry.Code, Country.Code, Ind.Type, share.20)



##########################################################################
# 3. super-disaggregate the IO objects (IOdet.codes, xdet, Ydet, Zdet, Edet)
##########################################################################

##########################################################################
# 3.1 create super-disaggregated version of IOdet.codes (->IOsupdet.codes)

IOsupdet.codes <- readRDS(file = paste0(exiopath, "IOdet.codes.rds"))

insert_positions <- c()

# define the target industry codes and names
dirsup_target <- data.frame(
  Code = c("i28", "i29", "i31", "i32", "i45"),
  Name = c("Manufacture of fabricated metal products, except machinery and equipment (28)",
           "Manufacture of machinery and equipment n.e.c. (29)",
           "Manufacture of electrical machinery and apparatus n.e.c. (31)",
           "Manufacture of radio, television and communication equipment and apparatus (32)",
           "Construction (45)")
)

# loop through the rows of the dataframe and check for matches
for (i in seq_len(nrow(IOsupdet.codes))) {
  row <- IOsupdet.codes[i, ]
  
  # Check if the row's Industry.Code and Industry.Name match any target industry
  if (row$Industry.Code %in% dirsup_target$Code && 
      row$Industry.Name %in% dirsup_target$Name) {
    insert_positions <- c(insert_positions, rep(i, 12))  # 12 NA rows after each match
  }
}

# define insertion metadata
insert_specs <- list(
  list(
    match_code = "i28",
    match_name = "Manufacture of fabricated metal products, except machinery and equipment (28)",
    new_codes = metal_codes,
    new_names = metal_names
  ),
  list(
    match_code = "i29",
    match_name = "Manufacture of machinery and equipment n.e.c. (29)",
    new_codes = machin_codes,
    new_names = machin_names
  ),
  list(
    match_code = "i31",
    match_name = "Manufacture of electrical machinery and apparatus n.e.c. (31)",
    new_codes = elec_codes,
    new_names = elec_names
  ),
  list(
    match_code = "i32",
    match_name = "Manufacture of radio, television and communication equipment and apparatus (32)",
    new_codes = comm_codes,
    new_names = comm_names
  ),
  list(
    match_code = "i45",
    match_name = "Construction (45)",
    new_codes = constr_codes,
    new_names = constr_names
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
while (i <= nrow(IOsupdet.codes)) {
  row <- IOsupdet.codes[i, ]
  for (spec in insert_specs) {
    if (row$Industry.Code == spec$match_code && row$Industry.Name == spec$match_name) {
      IOsupdet.codes <- insert_rows_after(
        IOsupdet.codes, i,
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
IOsupdet.codes$Index <- seq_len(nrow(IOsupdet.codes))


##########################################################################
# 3.2 create super-disaggregated version of xdet (->xsupdet)

xsupdet_2020 <- readRDS(file = paste0(exiopath, "xdet_2020.rds"))

# offset to account for growing vector length during insertion
offset <- 0

# insert NAs into xsupdet_2020 at the correct positions
for (pos in insert_positions) {
  xsupdet_2020 <- append(xsupdet_2020, NA, after = pos + offset)
  offset <- offset + 1
}

# define mapping from industry codes to dirsup sectors and code lists
dirsup_map <- list(
  "i28" = list(type = "metal",  codes = metal_codes),
  "i29" = list(type = "machin", codes = machin_codes),
  "i31" = list(type = "elec",   codes = elec_codes),
  "i32" = list(type = "comm",   codes = comm_codes),
  "i45" = list(type = "constr", codes = constr_codes)
)

# loop through IOsupdet.codes and fill in new rows
for (i in seq_len(nrow(IOsupdet.codes))) {
  row <- IOsupdet.codes[i, ]
  code <- row$Industry.Code
  
  if (code %in% names(dirsup_map)) {
    agg_value <- xsupdet_2020[i]
    country <- row$Country.Code
    dirsup_info <- dirsup_map[[code]]
    dirsup_codes <- dirsup_info$codes
    
    insert_start <- i + 1
    insert_end <- insert_start + length(dirsup_codes) - 1
    insert_indices <- insert_start:insert_end
    
    # get share values in correct order
    shares <- share_lookup %>%
      filter(Country.Code == country, Industry.Code %in% dirsup_codes) %>%
      mutate(order = match(Industry.Code, dirsup_codes)) %>%
      arrange(order)
    
    if (country %in% countries && nrow(shares) == length(dirsup_codes)) {
      xsupdet_2020[insert_indices] <- agg_value * shares$share.20
    } else {
      xsupdet_2020[insert_indices] <- 0
    }
  }
}

# merge with IOsupdet.codes to check the result
xsupdet_2020_test <- cbind(IOsupdet.codes, xsupdet_2020)


##########################################################################
# 3.3 create disaggregated version of Ydet (->Ysupdet)

# there are zeros everywhere in the new dirsup sectors in Y, 
# except for the .other "subsector" that copies the values from the parent sector

Ysupdet_2020 <- readRDS(file = paste0(exiopath, "Ydet_2020.rds"))

# adapt share_lookup to share_lookup_ZY
share_lookup_ZY <- share_lookup %>%
  group_by(Country.Code) %>%
  mutate(
    # get the parent code for rows ending in .other
    parent_code = if_else(str_ends(Industry.Code, "\\.other"),
                          str_remove(Industry.Code, "\\.other$"),
                          NA_character_),
    # get the share.20 of the parent row for each .other row
    parent_share = if_else(!is.na(parent_code),
                           share.20[match(parent_code, Industry.Code)],
                           NA_real_),
    # new share.20 values based on rules
    share.20 = case_when(
      str_ends(Industry.Code, "\\.other") ~ parent_share,
      !str_detect(Industry.Code, "\\.") ~ share.20,  # keep parent value
      TRUE ~ 0  # everything else gets 0
    )
  ) %>%
  ungroup() %>%
  select(-parent_code, -parent_share)  # clean up

# define mapping from industry codes to dirsup sectors and code lists (already defined at xsupdet, just to make sure)
dirsup_map <- list(
  "i28" = list(type = "metal",  codes = metal_codes),
  "i29" = list(type = "machin", codes = machin_codes),
  "i31" = list(type = "elec",   codes = elec_codes),
  "i32" = list(type = "comm",   codes = comm_codes),
  "i45" = list(type = "constr", codes = constr_codes)
)

# flatten all disaggregated codes into a vector
all_disagg_codes <- unlist(lapply(dirsup_map, function(x) x$codes))

# initialize output list
Ysupdet_2020_list <- vector("list", length = nrow(IOsupdet.codes))

# track the current row in the original Ysupdet_2020
orig_row_index <- 1

for (i in seq_len(nrow(IOsupdet.codes))) {
  row <- IOsupdet.codes[i, ]
  country <- row$Country.Code
  ind_code <- as.character(row$Industry.Code)
  
  if (ind_code %in% names(dirsup_map)) {
    # original dirsup row (e.g., i28, i29, ...)
    Ysupdet_2020_list[[i]] <- Ysupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
    
  } else if (ind_code %in% all_disagg_codes) {
    # disaggregated sub-row — get share and apply to parent row
    parent_code <- substr(ind_code, 1, 3)
    parent_index <- orig_row_index - 1
    parent_row <- Ysupdet_2020[parent_index, , drop = FALSE]
    
    share <- share_lookup_ZY %>%
      filter(Country.Code == country, Industry.Code == ind_code) %>%
      pull(share.20)
    
    if (length(share) == 1) {
      Ysupdet_2020_list[[i]] <- parent_row * share
    } else {
      Ysupdet_2020_list[[i]] <- matrix(0, nrow = 1, ncol = ncol(Ysupdet_2020))
    }
    
  } else {
    # not disaggregated — preserve original row
    Ysupdet_2020_list[[i]] <- Ysupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
  }
}

# combine to final matrix
Ysupdet_2020 <- do.call(rbind, Ysupdet_2020_list)

# merge with IOdet.codes to check the result
Ysupdet_2020_test <- cbind(IOsupdet.codes, Ysupdet_2020)


##########################################################################
# 3.4 create disaggregated version of Zdet (->Zsupdet)

# in the final version (Zsupdet_alloc), there are zeros everywhere in the new dirsup sectors row-wise,
# except for the newly created modelled technology-specific sectors,
# where these newly created technology-specific sectors allocate the supply shares according to shares from xnorm.20

Zsupdet_2020 <- readRDS(file = paste0(exiopath, "Zdet_2020.rds"))

# define mapping from industry codes to dirsup sectors and code lists (already defined at xsupdet, just to make sure)
dirsup_map <- list(
  "i28" = list(type = "metal",  codes = metal_codes),
  "i29" = list(type = "machin", codes = machin_codes),
  "i31" = list(type = "elec",   codes = elec_codes),
  "i32" = list(type = "comm",   codes = comm_codes),
  "i45" = list(type = "constr", codes = constr_codes)
)

# flatten all disaggregated codes into a vector
all_disagg_codes <- unlist(lapply(dirsup_map, function(x) x$codes))

# initialize output list
Zsupdet_2020_list <- vector("list", length = nrow(IOsupdet.codes))

# track the current row in the original Ysupdet_2020
orig_row_index <- 1

for (i in seq_len(nrow(IOsupdet.codes))) {
  row <- IOsupdet.codes[i, ]
  country <- row$Country.Code
  ind_code <- as.character(row$Industry.Code)
  
  if (ind_code %in% names(dirsup_map)) {
    # original dirsup row (e.g., i28, i29, ...)
    Zsupdet_2020_list[[i]] <- Zsupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
    
  } else if (ind_code %in% all_disagg_codes) {
    # disaggregated sub-row — get share and apply to parent row
    parent_code <- substr(ind_code, 1, 3)
    parent_index <- orig_row_index - 1
    parent_row <- Zsupdet_2020[parent_index, , drop = FALSE]
    
    share <- share_lookup_ZY %>%
      filter(Country.Code == country, Industry.Code == ind_code) %>%
      pull(share.20)
    
    if (length(share) == 1) {
      Zsupdet_2020_list[[i]] <- parent_row * share
    } else {
      Zsupdet_2020_list[[i]] <- matrix(0, nrow = 1, ncol = ncol(Zsupdet_2020))
    }
    
  } else {
    # not disaggregated — preserve original row
    Zsupdet_2020_list[[i]] <- Zsupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
  }
}

# combine into final matrix
Zsupdet_2020 <- do.call(rbind, Zsupdet_2020_list)

# merge with IOdet.codes to check the result
Zsupdet_2020_test <- cbind(IOsupdet.codes, Zsupdet_2020)

# transpose to allow column-wise disaggregation
Zsupdet_2020 <- t(Zsupdet_2020)

# initialize output list
Zsupdet_2020_list <- vector("list", length = nrow(IOsupdet.codes))

# track the current row in the original Ysupdet_2020
orig_row_index <- 1

for (i in seq_len(nrow(IOsupdet.codes))) {
  row <- IOsupdet.codes[i, ]
  country <- row$Country.Code
  ind_code <- as.character(row$Industry.Code)
  
  if (ind_code %in% names(dirsup_map)) {
    # original dirsup row (e.g., i28, i29, ...)
    Zsupdet_2020_list[[i]] <- Zsupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
    
  } else if (ind_code %in% all_disagg_codes) {
    # disaggregated sub-row — get share and apply to parent row
    parent_code <- substr(ind_code, 1, 3)
    parent_index <- orig_row_index - 1
    parent_row <- Zsupdet_2020[parent_index, , drop = FALSE]
    
    share <- share_lookup_ZY %>%
      filter(Country.Code == country, Industry.Code == ind_code) %>%
      pull(share.20)
    
    if (length(share) == 1) {
      Zsupdet_2020_list[[i]] <- parent_row * share
    } else {
      Zsupdet_2020_list[[i]] <- matrix(0, nrow = 1, ncol = ncol(Zsupdet_2020))
    }
    
  } else {
    # not disaggregated — preserve original row
    Zsupdet_2020_list[[i]] <- Zsupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
  }
}

# combine into final matrix
Zsupdet_2020 <- do.call(rbind, Zsupdet_2020_list)

# merge with IOdet.codes to check the result
Zsupdet_2020_test <- cbind(IOsupdet.codes, Zsupdet_2020)

# transpose again to get the original position
Zsupdet_2020 <- t(Zsupdet_2020)

# merge with IOdet.codes to make a final check of the result
Zsupdet_2020_test <- cbind(IOsupdet.codes, Zsupdet_2020)

# export the preliminary version
saveRDS(Zsupdet_2020, file = paste0(exiopath,"Zsupdet_2020.rds"))

# remove values from the parent and .other sector rows and implement values from xnorm.20 to the modelled technologies
rownames_Zsupdet_2020 <- colnames_Zsupdet_2020 <- paste(Zsupdet_2020_test$Country.Code, Zsupdet_2020_test$Industry.Code, sep = ".")
Zsupdet_2020 <- as.data.frame(Zsupdet_2020)
colnames(Zsupdet_2020) <- colnames_Zsupdet_2020
rownames(Zsupdet_2020) <- rownames_Zsupdet_2020

# create mat_2020_eh for "eh" sectors only
dirsup_patterns <- c("eh", "e.1.i.turb", "e.1.airrig", "e.2.i.turb", "e.2.ii.turb",
                     "h.1.i.sil", "h.1.i.tand", "h.1.ii.sil", "h.1.ii.tand",
                     "h.1.iii.sil", "h.1.iii.tand") # vector of patterns to keep
regex_dirsup_pattern <- str_c(dirsup_patterns, collapse = "|")

mat_2020_eh <- mat_2020 %>%
  filter(str_detect(Industry.Code, regex_dirsup_pattern))
mat_2020_eh <- mat_2020_eh %>%
  group_by(Country.Code, Ind.Type) %>%
  mutate(share.20 = xnorm.20 / xnorm.20[Industry.Code %in% c("i28.eh","i29.eh","i31.eh","i32.eh","i45.eh")]) %>%
  ungroup() # recalculate share.20 = direct supplier by tech / total within each Country and Ind.Type

# get lookup table for direct supplier sector shares for the "eh" subsectors only
share_lookup_eh <- mat_2020_eh %>%
  filter(Ind.Type %in% c("metal","machin","elec","comm","constr")) %>%
  select(Industry.Code, Country.Code, Ind.Type, share.20)

# define columns and rows to filter
target_col_suffixes <- c(
  "i40.11.e.1", "i40.11.e.1.i", "i40.11.e.1.i.turb", "i40.11.e.1.i.air",
  "i40.11.e.1.i.airflex", "i40.11.e.1.i.airrig", "i40.11.e.1.ii", "i40.11.e.1.ii.turb",
  "i40.11.e.1.ii.air", "i40.11.e.1.ii.airflex", "i40.11.e.1.ii.airrig", "i40.11.e.2",
  "i40.11.e.2.i", "i40.11.e.2.i.turb", "i40.11.e.2.ii", "i40.11.e.2.ii.turb",
  "i40.11.e.2.ii.air", "i40.11.e.2.ii.airflex", "i40.11.e.2.ii.airrig"
)
target_sectors <- c("i28", "i29", "i31", "i32", "i45")

target_columns <- colnames(Zsupdet_2020)[
  str_ends(colnames(Zsupdet_2020), pattern = paste0(target_col_suffixes, collapse = "|"))
]
target_rows <- rownames(Zsupdet_2020)[
  str_detect(rownames(Zsupdet_2020), pattern = paste0("\\b(", paste0(target_sectors, collapse = "|"), ")\\b"))
]

Zsupdet_2020_mod <- Zsupdet_2020[target_rows, target_columns, drop = FALSE]
Zsupdet_2020_mod <- Zsupdet_2020_mod[!grepl("w",rownames(Zsupdet_2020_mod)),]

# define parameters
parent_sectors <- c("i28", "i29", "i31", "i32", "i45")
target_suffixes <- c("eh", "e.1.i.turb", "e.1.airrig", "e.2.i.turb", "e.2.ii.turb",
                     "h.1.i.sil", "h.1.i.tand", "h.1.ii.sil", "h.1.ii.tand",
                     "h.1.iii.sil", "h.1.iii.tand")
other_suffix <- "other"

# loop through countries and parent sectors
# note - takes about 3-5 min to run!
for (country in countries) {
  for (sector in parent_sectors) {
    
    row_other <- paste0(country, ".", sector, ".", other_suffix)
    if (!(row_other %in% rownames(Zsupdet_2020_mod))) next
    
    shares <- share_lookup_eh %>%
      filter(Country.Code == country,
             str_starts(Industry.Code, sector),
             str_remove(Industry.Code, paste0("^", sector, "\\.")) %in% target_suffixes) %>%
      mutate(suffix = str_remove(Industry.Code, paste0("^", sector, "\\."))) %>%
      select(suffix, share = share.20)
    
    if (nrow(shares) == 0) next
    
    row_vals <- Zsupdet_2020_mod[row_other, , drop = FALSE]
    
    for (i in seq_len(nrow(shares))) {
      target_suffix <- shares$suffix[i]
      share_val <- shares$share[i]
      
      target_row <- paste0(country, ".", sector, ".", target_suffix)
      if (!(target_row %in% rownames(Zsupdet_2020_mod))) next
      
      Zsupdet_2020_mod[target_row, ] <- Zsupdet_2020_mod[target_row, ] + 
        as.numeric(row_vals) * share_val
    }
    
    # zero out "other" rows (i.e., rows ending their industry codes with "other" :-))
    # note, zeroing out the links to the parent sectors will be solved later,
    # by integrating new production functions (column vectors) for the detailed modelled technologies
    Zsupdet_2020_mod[row_other, ] <- 0
  }
}

# integrate replaced rows and columns back to Zsupdet_2020
Zsupdet_2020_alloc <- Zsupdet_2020

rows_to_replace <- intersect(rownames(Zsupdet_2020_alloc), rownames(Zsupdet_2020_mod))
cols_to_replace <- intersect(colnames(Zsupdet_2020_alloc), colnames(Zsupdet_2020_mod))

Zsupdet_2020_alloc[rows_to_replace, cols_to_replace] <- Zsupdet_2020_mod[rows_to_replace, cols_to_replace]

# note - one should also recalculate the xsupdet of the "other" sectors,
# but since we are not interested in these in the analysis, we skip this step



##########################################################################
# 3.3 create disaggregated version of Edet (->Esupdet)
# note: no differences in the accounts by technology assumed!
# just a simple split according to the shares in production based on xnorm.20 (shares_lookup)
# NOTE - LATER, THE EXTENSIONS INTENSITY MATRIX SHOULD COPY THE COLUMNS,
# FROM THE "PARENT" (ALREADY EXISTENT, NON-DISAGGREGATED) SECTORS

Esupdet_2020 <- readRDS(file = paste0(exiopath, "Edet_2020.rds"))

# transpose to allow column-wise disaggregation
Esupdet_2020 <- t(Esupdet_2020)

# flatten all disaggregated codes into a vector
all_disagg_codes <- unlist(lapply(dirsup_map, function(x) x$codes))

# initialize output list
Esupdet_2020_list <- vector("list", length = nrow(IOsupdet.codes))

# track the current row in the original Ysupdet_2020
orig_row_index <- 1

for (i in seq_len(nrow(IOsupdet.codes))) {
  row <- IOsupdet.codes[i, ]
  country <- row$Country.Code
  ind_code <- as.character(row$Industry.Code)
  
  if (ind_code %in% names(dirsup_map)) {
    # original dirsup row (e.g., i28, i29, ...)
    Esupdet_2020_list[[i]] <- Esupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
    
  } else if (ind_code %in% all_disagg_codes) {
    # disaggregated sub-row — get share and apply to parent row
    parent_code <- substr(ind_code, 1, 3)
    parent_index <- orig_row_index - 1
    parent_row <- Esupdet_2020[parent_index, , drop = FALSE]
    
    share <- share_lookup %>%
      filter(Country.Code == country, Industry.Code == ind_code) %>%
      pull(share.20)
    
    if (length(share) == 1) {
      Esupdet_2020_list[[i]] <- parent_row * share
    } else {
      Esupdet_2020_list[[i]] <- matrix(0, nrow = 1, ncol = ncol(Esupdet_2020))
    }
    
  } else {
    # not disaggregated — preserve original row
    Esupdet_2020_list[[i]] <- Esupdet_2020[orig_row_index, , drop = FALSE]
    orig_row_index <- orig_row_index + 1
  }
}

# combine into final matrix
Esupdet_2020 <- do.call(rbind, Esupdet_2020_list)

# merge with IOdet.codes to check the result
Esupdet_2020_test <- cbind(IOsupdet.codes, Esupdet_2020)

# transpose again to get the original position
Esupdet_2020 <- t(Esupdet_2020)



##########################################################################
# 4. export
##########################################################################

saveRDS(IOsupdet.codes, file = paste0(exiopath,"IOsupdet.codes.rds"))
saveRDS(xsupdet_2020, file = paste0(exiopath,"xsupdet_2020.rds"))
saveRDS(Ysupdet_2020, file = paste0(exiopath,"Ysupdet_2020.rds"))
saveRDS(Zsupdet_2020_alloc, file = paste0(exiopath,"Zsupdet_2020_alloc.rds"))
saveRDS(Esupdet_2020, file = paste0(exiopath,"Esupdet_2020.rds"))


