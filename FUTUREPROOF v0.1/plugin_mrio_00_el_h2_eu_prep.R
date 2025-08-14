##########################################################################
# 00_el_h2_eu_prep

# prepare required exiobase data for the input data master table (el_h2_eu_inpdata.xlsx)

# input data:
# exiobase 3.9.5 monetary ixi 2020: https://zenodo.org/records/14869924
# parsed data: set by user

# libraries:
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2)
library(readxl)



##########################################################################
# 0. set path
##########################################################################

# parsedpath set by user
# exiopath set by user



##########################################################################
# 1. load data
##########################################################################

##########################################################################
# 1.1 load total output

x_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_x.rds"))
IO.codes_395 <- readRDS(file = paste0(parsedpath, "IO.codes.rds"))


##########################################################################
# 1.2 load value added

VA_2020 <- as.data.frame(head(readRDS(file = paste0(parsedpath, "ixi/2020_F.rds")), 9))
VA.codes_395 <- head(readRDS(file = paste0(parsedpath, "Q.codes.rds")), 9)


##########################################################################
# 1.3 load intermediate inputs and final demand

Z_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_Z.rds"))
Y_2020 <- readRDS(file = paste0(parsedpath, "ixi/2020_Y.rds"))
Y.codes_395 <- readRDS(file = paste0(parsedpath,"Y.codes.rds"))



##########################################################################
# 2. adjust the required parts for the input data master table
##########################################################################

##########################################################################
# 2.1 total output

x_2020_inp <- cbind(IO.codes_395, x_2020)


##########################################################################
# 2.2 value added

colnames(VA_2020) <- c(paste0(rep(unique(IO.codes_395$Country.Code), each = 163),"_",IO.codes_395$Industry.Code))
prefixes <- unique(sub("_.*", "", names(VA_2020))) # get unique country prefixes
extract_country_block <- function(prefix, data) { # function to extract columns by prefix and return long-form country block
  cols <- grep(paste0("^", prefix, "_"), names(data), value = TRUE)
  sub_df <- data[cols]
  names(sub_df) <- sub("^[^_]+_", "", names(sub_df)) # rename columns to remove prefix
  sub_df$Country.Code <- prefix # add country column
  return(sub_df)
}
VA_2020 <- do.call(rbind, lapply(prefixes, extract_country_block, data = VA_2020)) # apply function to all prefixes and bind
VA_2020 <- VA_2020 %>% select(Country.Code, everything()) # reorder columns

VA.codes_395 <- VA.codes_395 %>% slice(rep(row_number(), 49))

VA_2020_inp <- cbind(VA.codes_395, VA_2020)
VA_2020_inp <- VA_2020_inp[,c(5,2,3,68,105,108)] # select required categories and sectors (i24.d, i40.11.e, i40.11.h)


##########################################################################
# 2.3 intermediate outputs to get row-wise shares of "eh" (modelled) vs. other sectors

# row-wise shares are calculated for Z+Y together,
# due to the assumption that the dirsup sectors supply only to the modelled detailed technologies and nothing else,
# not even final demand

Z_2020_inp <- cbind(IO.codes_395, as.data.frame(Z_2020))
Y_2020_inp <- cbind(IO.codes_395, as.data.frame(Y_2020))
ZY_2020_inp <- cbind(Z_2020_inp, as.data.frame(Y_2020))
ZY_2020_inp <- ZY_2020_inp[ZY_2020_inp$Industry.Code %in% c("i28", "i29", "i31", "i32", "i45"), -c(1,2,5)]

# transpose Z rows into a column form to aggregate over industries
colnames_ZY_2020_inp <- paste(ZY_2020_inp$Country.Code, ZY_2020_inp$Industry.Code, sep = ".")
ZY_2020_inp_val <- as.matrix(ZY_2020_inp[, 3:ncol(ZY_2020_inp)])
ZY_2020_inp <- as.data.frame(t(ZY_2020_inp_val))
colnames(ZY_2020_inp) <- colnames_ZY_2020_inp

# aggregate to modelled and non-modelled sectors (wind, solar PV and the rest)
IO.codes_395_selected <- IO.codes_395 %>%
  select(Country.Code, Industry.Name, Industry.Code) %>%
  mutate(Source = "IO", OrigOrder = row_number())
Y.codes_395_renamed <- Y.codes_395 %>%
  rename(
    Industry.Name = Category.Name,
    Industry.Code = Category.Code
  ) %>%
  select(Country.Code, Industry.Name, Industry.Code) %>%
  mutate(Source = "Y", OrigOrder = row_number())
IOY.codes_395 <- bind_rows(IO.codes_395_selected, Y.codes_395_renamed) %>%
  mutate(Source = factor(Source, levels = c("IO", "Y"))) %>%
  arrange(Source, OrigOrder) %>%
  select(-Source, -OrigOrder)

ZY_2020_inp <- cbind(IOY.codes_395[,c(3,1)], ZY_2020_inp)
ZY_2020_inp$Industry.Code[!(ZY_2020_inp$Industry.Code %in% c("i40.11.e", "i40.11.h"))] <- "i.other"
ZY_2020_inp$Country.Code <- factor(ZY_2020_inp$Country.Code, levels = unique(ZY_2020_inp$Country.Code))
ZY_2020_inp <- ZY_2020_inp %>%
  group_by(Country.Code, Industry.Code) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))

# reverse transposition to get back to turn the Z rows back into row form :-)
colnames_ZY_2020_inp <- paste(ZY_2020_inp$Country.Code, ZY_2020_inp$Industry.Code, sep = ".")
Z_2020_inp_val <- as.matrix(ZY_2020_inp[, 3:ncol(ZY_2020_inp)])
ZY_2020_inp <- as.data.frame(t(Z_2020_inp_val))
colnames(ZY_2020_inp) <- colnames_ZY_2020_inp
ZY_2020_inp <- ZY_2020_inp %>%
  rownames_to_column("Combined.Code")
ZY_2020_inp <- ZY_2020_inp %>%
  separate(Combined.Code, into = c("Country.Code", "Industry.Code"), sep = "\\.")

# aggregate over input countries to get only global inputs
# (the precise inputs by country or region for each modelled technology are modelled later anyway)
ZY_2020_inp <- ZY_2020_inp %>%
  group_by(Industry.Code) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))

# list purchasing countries below each other
country_order <- names(ZY_2020_inp)[-1] |>   # exclude Industry.Code
  sub("\\..*", "", x = _) |>                # keep only the country part (before first dot)
  unique()
ZY_2020_inp_long <- ZY_2020_inp %>%
  pivot_longer(
    cols = -Industry.Code,
    names_to = "CountryIndustry",
    values_to = "Value"
  )
ZY_2020_inp_long <- ZY_2020_inp_long %>%
  mutate(
    Country.Code = str_extract(CountryIndustry, "^[^.]+"),
    Target.Industry = str_remove(CountryIndustry, "^[^.]+\\.")
  )
ZY_2020_inp <- ZY_2020_inp_long %>%
  select(Industry.Code, Country.Code, Target.Industry, Value) %>%
  pivot_wider(
    names_from = Target.Industry,
    values_from = Value
  ) %>%
  relocate(Country.Code, Industry.Code)
ZY_2020_inp <- ZY_2020_inp %>%
  mutate(
    Country.Code = factor(Country.Code, levels = country_order),
    Industry.Name = case_when(
      Industry.Code == "i28" ~ "Manufacture of fabricated metal products, except machinery and equipment (28)",
      Industry.Code == "i29" ~ "Manufacture of machinery and equipment n.e.c. (29)",
      Industry.Code == "i31" ~ "Manufacture of electrical machinery and apparatus n.e.c. (31)",
      Industry.Code == "i32" ~ "Manufacture of radio, television and communication equipment and apparatus (32)",
      Industry.Code == "i45" ~ "Construction (45)",
      TRUE ~ "Other"
    )
  ) %>%
  select(Country.Code, Industry.Name, Industry.Code, everything()) %>%
  arrange(Country.Code)



##########################################################################
# 3. export
##########################################################################

fwrite(x_2020_inp, file = paste0(exiopath,"x_2020_inp.csv"), sep=";", dec=".")
fwrite(VA_2020_inp, file = paste0(exiopath,"VA_2020_inp.csv"), sep=";", dec=".")
fwrite(ZY_2020_inp, file = paste0(exiopath,"ZY_2020_inp.csv"), sep=";", dec=".")
# fwrite(Z_2020_inp, file = paste0(exiopath,"Z_2020_inp.csv"), sep=";", dec=".")

saveRDS(IOY.codes_395, file = paste0(parsedpath,"IOY.codes.rds"))


