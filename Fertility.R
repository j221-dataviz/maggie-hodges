#install World Bank package
install.packages('WDI')

# load required packages
library(WDI)
library(dplyr)
library(readr)
library(stringr)
library(readxl)

# get data
fert <- read_csv("data/original_fertility_data.csv")

# create list of indicators to be imported
indic_list <- c("NY.GDP.PCAP.PP.CD", "SH.DYN.MORT", "SP.DYN.TFRT.IN")

# import indicators into single data frame and rename fields
indicators <- WDI(indicator=indic_list, country="all", start=1990, end=2012, extra=T, cache=NULL) %>%
  rename(gdp_percap=NY.GDP.PCAP.PP.CD, child_mortal=SH.DYN.MORT, fertil_rate=SP.DYN.TFRT.IN) %>%
  filter(income != "Aggregates") %>%
  mutate(year = as.integer(year))

# data cleaning
indicators$region <- gsub("all income levels","", indicators$region)
indicators$region <- gsub("\\(|\\)","", indicators$region)
indicators$region <- str_trim(indicators$region)
indicators$income <- gsub(": nonOECD","", indicators$income)
indicators$income <- gsub(": OECD","", indicators$income)

# import primary infertility data
infert <- read_excel("original_infertility_data.xlsx", skip = 1, sheet=5, col_names = FALSE)
     names(infert) <- c("iso3c","country","year", "total population women 20-44","primary_infertility_rate")

# import secondary infertility data
second_infert <- read_excel("original_infertility_data.xlsx", skip = 1, sheet=6, col_names = FALSE)
     names(second_infert) <- c("iso3c","country","year", "total population women 20-44","secondary_infertility_rate")


# Convert year to an integer column
infert <- infert %>%
  mutate(year = as.integer(year))

second_infert <- second_infert %>%
  mutate(year = as.integer(year))

# filter fertility data so it only has the years available with infertility data
relevant_years_fert <- fert %>%
  filter(year == "1990" | year == "2010") %>%
  arrange(year)

# combine fertility and primary infertility data by countries
fertility <- left_join(relevant_years_fert,infert, by = c("country", "year"))

#combine fertility and secondary infertility data by countries
secondary_infertility <- left_join(relevant_years_fert,second_infert, by = c("country", "year"))

#combine 

# organize data so it appears by country and year
fertility <- fertility %>%
  group_by(country, year) 
  arrange(country, year)
  
secondary_infertility <- secondary_infertility %>%
  group_by(country, year) %>%
  arrange(country, year)
 
# filter combined fertility and infertility data so it does not include countries which have null values
# write to csv

write_csv(fertility, "fertility_prim_infert_by_country.csv", na="")

write_csv(secondary_infertility, "secondary_infertility_by_country.csv", na="")


# import excel file and filter to keep latest rows only, select columns to keep
age_first_birth <- read_excel("mean_age_firstbirth.xlsx") %>%
  filter(Period == "Latest") %>%
  select(1,4,5)

# replace symbols with NA
age_first_birth[age_first_birth == ".."] <- NA
names(age_first_birth) <- c("country","year","age_first_birth")

# change to numbers and round
age_first_birth <- age_first_birth %>%
  mutate(age_first_birth = round(as.numeric(age_first_birth),1))

# import fertility file which has primary infertility data
primary_infertility <- read_csv("fertility_prim_infert_by_country.csv") %>%
  filter(year == "2010") %>%
  select(2, 3, 8, 16)

age_first_birth <- age_first_birth %>%
  mutate(year = as.integer(year))

# combine primary infertility and age at first birth data
primary_infertility_age <- left_join(primary_infertility,age_first_birth, by = c("country"))

# filter combined primary infertility and age at first birth data so it does 
# not include countries which have null values, write to csv
write_csv(primary_infertility_age, "primary_infertility_age.csv", na="")

# import health service coverage file
health_service_coverage <- read_csv("health_service_coverage.csv") %>%
   select(1, 2, 3) %>%
   mutate(year = as.integer(year)) 

#combine primary infertility and health service coverage data
prim_infert_health_service_cov <- left_join(primary_infertility, health_service_coverage, by=c("country"))

write_csv(prim_infert_health_service_cov, "prim_infert_health_service_cov.csv", na="")