#install World Bank package
install.packages('WDI')

# load required packages
library(WDI)
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(ggplot2)
library(ggiraph)
library(htmlwidgets)
library(tidyr)

# get data
fert <- read_csv("data/original_fertility_data.csv")

# filter fertility data so it only has the years available with infertility data
relevant_years_fert <- fert %>%
  filter(year == "1990" | year == "2010") %>%
  arrange(year)

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
infert <- read_excel("data/original_infertility_data.xlsx", skip = 1, sheet=5, col_names = FALSE)
names(infert) <- c("iso3c","country","year", "total population women 20-44","primary_infertility_rate")

# import secondary infertility data
second_infert <- read_excel("data/original_infertility_data.xlsx", skip = 1, sheet=6, col_names = FALSE)
names(second_infert) <- c("iso3c","country","year", "total population women 20-44","secondary_infertility_rate")

# Convert year to an integer column
infert <- infert %>%
  mutate(year = as.integer(year))

second_infert <- second_infert %>%
  mutate(year = as.integer(year))

# combine fertility and primary infertility data by countries
fertility <- left_join(relevant_years_fert,infert, by = c("country", "year"))

#combine fertility and secondary infertility data by countries
secondary_infertility <- left_join(relevant_years_fert,second_infert, by = c("country", "year"))

# organize data so it appears by country and year
fertility <- fertility %>%
  # group_by(country, year) 
  arrange(country, year)
  
secondary_infertility <- secondary_infertility %>%
  # group_by(country, year) %>%
  arrange(country, year)
 
# filter combined fertility and infertility data so it does not include countries which have null values
# write to csv



write_csv(fertility, "data/fertility_prim_infert_by_country.csv", na="")
write_csv(secondary_infertility, "data/secondary_infertility_by_country.csv", na="")


# import excel file and filter to keep latest rows only, select columns to keep
age_first_birth <- read_excel("data/mean_age_firstbirth.xlsx") %>%
  filter(Period == "Latest") %>%
  select(1,4,5)

# replace symbols with NA
age_first_birth[age_first_birth == ".."] <- NA
names(age_first_birth) <- c("country","year","age_first_birth")

# change to numbers and round
age_first_birth <- age_first_birth %>%
  mutate(age_first_birth = round(as.numeric(age_first_birth),1))

# import fertility file which has primary infertility data
primary_infertility <- read_csv("data/fertility_prim_infert_by_country.csv") %>%
  filter(year == "2010") %>%
  select(2, 3, 8, 16)

age_first_birth <- age_first_birth %>%
  mutate(year = as.integer(year))

# combine primary infertility and age at first birth data
primary_infertility_age <- left_join(primary_infertility,age_first_birth, by = c("country"))

# filter combined primary infertility and age at first birth data so it writes null values,
# as blank, write to csv
write_csv(primary_infertility_age, "data/primary_infertility_age.csv", na="")

# import health service coverage file
health_service_coverage <- read_csv("data/health_service_coverage.csv") %>%
   select(1, 2, 3)

names(health_service_coverage) <- c("country","year","fp_met")

# filters data for most recent year after 2010
health_service_coverage <- health_service_coverage %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 2010 & !is.na(fp_met)) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

# not sure what the next bit is doing?

# # import health service coverage file
# health_service_coverage2 <- read_csv("health_service_coverage.csv") %>%
#   select(1, 2, 3)
# 
# names(health_service_coverage2) <- c("country","year","fp_met")
# 
# health_service_coverage2 <- health_service_coverage2 %>%
#   mutate(year = as.integer(year)) %>%
#   filter(year > 2010 & !is.na(fp_met)) %>%
#   mutate(diff = year - 2010) %>%
#   group_by(country) %>%
#   filter(diff == min(diff)) %>%
#   ungroup()

#combine primary infertility and health service coverage data
prim_infert_health_service_cov <- left_join(primary_infertility, health_service_coverage, by=c("country"))

write_csv(prim_infert_health_service_cov, "data/prim_infert_health_service_cov.csv", na="")

# import fertility file which has secondary infertility data 
secondary_infertility <- read_csv("data/secondary_infertility_by_country.csv") 

# import health_service_coverage
health_service_coverage <- read_csv("data/health_service_coverage.csv") %>%
  select(1, 2, 3) %>%
  mutate(year = as.integer(year)) 
  
#combine secondary infertility and health service coverage data
  sec_infert_health_service_cov <- left_join(subset(secondary_infertility, year == 2010), health_service_coverage, by=c("country"))
  
write_csv(sec_infert_health_service_cov, "data/sec_infert_health_service_cov.csv", na="")

# import health service coverage file
health_service_coverage2 <- read_csv("data/health_service_coverage.csv") %>%
  select(1, 2, 3)

names(health_service_coverage2) <- c("country","year","fp_met")

# import health service infrastructure file
health_infrastructure <- read_csv("data/health_infrastructure.csv") %>%
  select(1, 2, 3, 4, 5)
  
names(health_infrastructure) <- c("country","year","hospitals", "health_posts", "health_centres")

health_infrastructure <- health_infrastructure %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 2010 & !is.na(health_centres)) %>%
  mutate(diff = year - 2010) %>%
  group_by(country) %>%
  filter(diff == min(diff)) %>%
  ungroup()

#combine secondary infertility and health service coverage data
sec_infert_health_infrastructure <- left_join(subset(secondary_infertility, year == 2010), health_infrastructure, by=c("country"))

write_csv(sec_infert_health_infrastructure, "sec_infert_health_infrastructure.csv", na="")

#combine primary and secondary infertility data
prim_sec_infertility <- left_join(primary_infertility,secondary_infertility, by=c("country", "year")) %>%
  filter(year == "2010") %>%
  filter(!is.na(primary_infertility_rate)) %>%
  filter(!is.na(secondary_infertility_rate)) %>%
  rename(women20_44 = `total population women 20-44`) %>%
  mutate(total_infertility_rate = primary_infertility_rate+secondary_infertility_rate)
# 
# 
# write_csv(prim_sec_infertility, "prim_sec_infertility.csv", na="")
# 
# 
# #Used excel to add in a total_infertility column, then re-imported data
# prim_sec_infertility <- read_csv("prim_sec_infertility.csv") 

# names(prim_sec_infertility) <- c("iso2c", "country","year","gdp_percap", "child_mortality", "fertil_rate", "iso3c", "region", "capital", "longitude", "latitude", "income", "lending", "total population women aged 20-44", "primary_infertility_rate", "secondary_infertility_rate", "total_infertility")

# create scatter plot of relationship between fertility and infertility
prim_sec_infertility_chart <- ggplot(prim_sec_infertility, aes(x = fertil_rate, y = total_infertility_rate, color = region.x)) + 
  scale_color_brewer(palette = "Accent", name = "") +
  geom_point(size = 1.5, alpha =0.6) +
  theme_minimal(base_size = 12, base_family = "Georgia") +
  xlab("Birth Rate") +
  ylab("Infertility Rate") +
  theme(legend.position = "bottom") 
  
# add a trend line
prim_sec_infertility_chart +
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dotdash", size = 0.3) 

# making it interactive

prim_sec_infertility_chart <- ggplot(prim_sec_infertility, aes(x = fertil_rate, y = total_infertility_rate)) + 
  geom_smooth(method = lm, se = FALSE, color = "black", linetype = "dotdash", size = 0.3) +
  scale_fill_brewer(palette = "Accent", name = "") +
  geom_point_interactive(shape = 21, size = 2.5, alpha = 0.5, color= "black", aes(tooltip = country, fill=region.x)) +
  theme_minimal(base_size = 12, base_family = "Georgia") +
  xlab("Birth Rate") +
  ylab("Infertility Rate") +
  theme(legend.position = "bottom") 

prim_sec_infertility_interactive <- ggiraph(code = print(prim_sec_infertility_chart), height_svg=4)

# save chart as a web page
saveWidget(prim_sec_infertility_interactive, "fert_infert.html", selfcontained = TRUE, libdir = NULL, background = "white")


#stacked bar chart, primary and secondary infertility by region
# 
# # import regional infertility average file
# prim_sec_region <- read_csv("data/prim_sec_infertility_region.csv") %>%
#   select(18, 19, 20, 21)

# names(prim_sec_region) <- c("prim_avg","sec_avg","total_avg", "region")
# 
# write_csv(prim_sec_region, "prim_sec_infertility_region.csv", na="")

infert_regions <- prim_sec_infertility %>%
  mutate(total_primary = women20_44*primary_infertility_rate,
         total_secondary = women20_44*secondary_infertility_rate) %>%
  group_by(region.x) %>%
  summarize(women20_44 = sum(women20_44),
            total_primary = sum(total_primary),
            total_secondary = sum(total_secondary)) %>%
  mutate(Primary = total_primary/women20_44,
         Secondary = total_secondary/women20_44) %>%
  select(1,5,6) %>%
  gather(type,rate,-region.x)



#stacked bar chart, primary and secondary infertility by region

prim_sec_stackedbar <- ggplot(infert_regions, aes(x = region.x, y = rate, fill = type)) + 
  scale_fill_brewer(palette = "Set1", name = "") +
  geom_bar(stat = "identity", 
           color = "#888888", 
           alpha = 0.5) +
  theme_minimal(base_size = 12, base_family = "Georgia") +
  xlab("Region") +
  ylab("Infertility Rate") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank()) +
  ggtitle("Primary and Secondary Infertility by Region") +
  coord_flip()
  
