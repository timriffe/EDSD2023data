

library(tidyverse)
library(haven)
library(collapse)
library(tidyfast)
library(janitor)
library(lubridate)
library(labelled)


# https://hrsdata.isr.umich.edu/data-products/rand-hrs-longitudinal-file-2020
dat <- read_stata("Data/randhrs1992_2020v1_STATA/randhrs1992_2020v1.dta")
dim(dat) |> prod() # 721453278

wide_out <-
dat %>% 
  # cut down columns, don't keep spouse info, although we could, but then what weight??
  select(hhidpn,
         ragender,
         ends_with("wstat") & starts_with("r"),
         ends_with("wtcrnh") & starts_with("r"),
         ends_with("wmid") & starts_with("r"),
         rabdate,
         radyear,
         radmonth,
         ends_with("slfmem") & starts_with("r"),
         ends_with("adl5a") & starts_with("r"),
         ends_with("iadl5a") & starts_with("r"),
         -reiwmid) %>% 
  clean_names()|> 
  mutate(across(where(is.labelled), remove_labels)) |> 
  # this males a full stack, very long!
  pivot_longer(-c(hhidpn, rabdate, radyear,radmonth,ragender), names_to = "wave_var", values_to = "value") %>% 
  # separate() too tough to make regex work on all variables, so we do it manually
  fmutate(
    wave_var = substr(wave_var, 2, nchar(wave_var)),
    wave = parse_number(wave_var),
    var = gsub('[0-9]+', '', wave_var)) %>% 
  select(-wave_var) %>% 
  # bring variables back to columns
  pivot_wider(names_from = var, values_from = value) |> 
  mutate(rabdate = as_date(rabdate, origin = as_date("1960-01-01")),
         iwmid = as_date(iwmid, origin = as_date("1960-01-01"))) |> 
  filter(!is.na(iwmid),
         between(iwmid, as_date("2000-01-01"),as_date("2009-12-31"))) |> 
  pivot_wider(names_from = wave, values_from = c(iwmid,wtcrnh,iwmid,iwstat,slfmem,iadla, adla)) 

write_csv(wide_out, file = "Data/hrs_subset_wide.csv")
R.utils::gzip("Data/hrs_subset_wide.csv")

# slfmem: 1 excellent - 5 poor
# iadla 0-5 (count)
# adla


