#The following code combines multiple data sets into a single data set 
#for the purpose of analyzing the impact of city gate prices on pipeline incident rates
#This was created as part of ECON 691 at UNC
#Note that this file should be run after thesis_summarystats.R, as that cleans some of the data

#downloading some packages
library(tidyverse)

#setting directory
setwd("C:/Users/benja/Downloads/Senior Thesis (Pipelines)")

#generating a list of all unique county-operator combinations ever to have an incident from gasdist_total
gasdist_incident_locs <- gasdist_total |>
  select(County, State, `Operator ID`) |>
  unique()

#generating a list of all unique county-operator combinations in 2024 [unfinished]
library(readxl)
all_operators <- read_excel("annual_gas_distribution_2023.xlsx", 
                            skip = 2)

#generating a list of all months in the citygate dataset
citygate_dates <- unique(citygate_filtered$Date)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
citygate_months <- substr(citygate_dates, 1, 7)

#combining the two to get the space of all possible incidents
library(reshape)
incident_samplespace <- expand.grid.df(gasdist_incident_locs, data.frame(citygate_months))

#figuring out whether there was an incident at that time
gasdist_total$Month <- substr(gasdist_total$Date, 1, 7)
colnames(incident_samplespace) <- c("County", "State", "Operator ID", "Month")
incidents_total <- merge(incident_samplespace, gasdist_total, all.x = TRUE, all.y = FALSE)
incidents_total$Incident <- if_else(is.na(incidents_total$`Report ID`), 0, 1)

#importing files for Heating Degree Days, precipitation, and Cooling Degree Days, renaming the columns to make sense
library(readr)
hdd <- read_table("climdiv-hddccy-v1.0.0-20241021", 
                  col_names = FALSE)
cdd <- read_table("climdiv-cddccy-v1.0.0-20241021", 
                  col_names = FALSE)
precip <- read_table("climdiv-pcpncy-v1.0.0-20241021", col_names = FALSE)
colnames(hdd) <- c("Description", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "?")
colnames(cdd) <- c("Description", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "?")
colnames(precip) <- c("Description", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "?")

#getting state, county, and year information
library(fipio)
hdd$FIPS <- substr(hdd$Description, 1, 5)
hdd$State <- fips_abbr(hdd$FIPS)
hdd$County <- fips_county(hdd$FIPS)
hdd$County <- str_to_upper(hdd$County)
hdd$Year <- substr(hdd$Description, 8, 12)
cdd$FIPS <- substr(cdd$Description, 1, 5)
cdd$State <- fips_abbr(cdd$FIPS)
cdd$County <- fips_county(cdd$FIPS)
cdd$County <- str_to_upper(cdd$County)
cdd$Year <- substr(cdd$Description, 8, 12)
precip$FIPS <- substr(precip$Description, 1, 5)
precip$State <- fips_abbr(precip$FIPS)
precip$County <- fips_county(precip$FIPS)
precip$County <- str_to_upper(precip$County)
precip$Year <- substr(precip$Description, 8, 12)

#making the data in a format where each month per county is a row
hdd_long <- pivot_longer(hdd, cols = c(`01`, `02`, `03`, `04`, `05`, `06`, `07`, `08`, `09`, `10`, `11`, `12`), 
                         names_to = "Month", values_to = "HDD")
cdd_long <- pivot_longer(cdd, cols = c(`01`, `02`, `03`, `04`, `05`, `06`, `07`, `08`, `09`, `10`, `11`, `12`), 
                         names_to = "Month", values_to = "CDD")
precip_long <- pivot_longer(precip, cols = c(`01`, `02`, `03`, `04`, `05`, `06`, `07`, `08`, `09`, `10`, `11`, `12`), 
                         names_to = "Month", values_to = "Precip")
hdd_long$Month <- paste(hdd_long$Year, hdd_long$Month, sep = "-")
cdd_long$Month <- paste(cdd_long$Year, cdd_long$Month, sep = "-")
precip_long$Month <- paste(precip_long$Year, precip_long$Month, sep = "-")

#merging in the HDD, CDD, and precip data
incidents_merge_hdd <- merge(incidents_total, hdd_long, by = c("County", "State", "Month"), all.x = TRUE, all.y = FALSE)
incidents_merge_cdd <- merge(incidents_merge_hdd, cdd_long, by = c("County", "State", "Month"), all.x = TRUE, all.y = FALSE)
incidents_merge_precip <- merge(incidents_merge_cdd, precip_long, by = c("County", "State", "Month"), all.x = TRUE, all.y = FALSE)

#importing the unemployment data and cleaning it up before merging it in
unemployment <- read_excel("ststdsadata.xlsx")
unemployment <- unemployment[which(unemployment$State != "Los Angeles County" & unemployment$State != "New York city"),]
unemployment$State <- fips_abbr(unemployment$`FIPS Code`)
unemployment$Month <- paste(unemployment$Year, unemployment$Month, sep = "-")
incidents_merge_unemployment <- merge(incidents_merge_precip, unemployment, by = c("State", "Month"), all.x = TRUE, all.y = FALSE)

#importing the consumption data and cleaning it up before merging it in
consumption_residential <- read_excel("NG_CONS_SUM_A_EPG0_VRS_MMCF_M.xls", 
                                      sheet = "Data 1", skip = 2)
consumption_commercial <- read_excel("NG_CONS_SUM_A_EPG0_VCS_MMCF_M.xls", 
                                      sheet = "Data 1", skip = 2)
consumption_res_pivot <- consumption_residential |>
  pivot_longer(cols = !Date, names_to = "State", values_to = "consumption")
consumption_res_pivot$State = str_remove(consumption_res_pivot$State,  "Natural Gas Residential Consumption \\(MMcf\\)")
consumption_com_pivot <- consumption_commercial |>
  pivot_longer(cols = !Date, names_to = "State", values_to = "consumption")
consumption_com_pivot$State = str_remove(consumption_com_pivot$State,  "Natural Gas Deliveries to Commercial Consumers \\(Including Vehicle Fuel through 1996\\) in")
consumption_com_pivot$State = str_remove(consumption_com_pivot$State, " \\(MMcf\\)")
consumption_com_pivot$State = str_remove(consumption_com_pivot$State, " the")
consumption_com_pivot$State = str_trim(consumption_com_pivot$State)
consumption_res_pivot$State = str_trim(consumption_res_pivot$State)
consumption_total <- merge(consumption_com_pivot, consumption_res_pivot, by = c("State", "Date"), all.x = TRUE, all.y = TRUE)
consumption_total$Month <- substr(consumption_total$Date, 1, 7)
consumption_total$Consumption <- consumption_total$consumption.x + consumption_total$consumption.y
consumption_total$State <- state.abb[match(consumption_total$State, state.name)]
incidents_merge_consumption <- merge(incidents_merge_unemployment, consumption_total, by = c("State", "Month"), all.x = TRUE, all.y = FALSE)

#lagging consumption
incidents_merge_consumption_2 <- incidents_merge_consumption |>
  select(-c(Year.x, Year.y)) |>
  group_by(State, County, `Operator ID`) |>
  mutate(`Lag Consumption` = lag(Consumption, n = 1, default = NA))

#merging in citygate
citygate_pivot <- citygate |>
  pivot_longer(cols = !c(Date, Year, NAs), names_to = "State", values_to = "Citygate")
citygate_pivot$State <- state.abb[match(citygate_pivot$State, state.name)]
citygate_pivot$Month <- substr(citygate_pivot$Date, 1, 7)
incidents_merge_citygate <- merge(incidents_merge_consumption_2, citygate_pivot, by = c("State", "Month"), all.x = TRUE, all.y = FALSE)

#making it per month with incident, not per incident, and adding an ID for fixed effects
incidents_being_reduced <- subset(incidents_merge_citygate, State != "AK" & State != "HI", select = c("County", "State", "Operator ID", "Month", "Incident", "HDD", "CDD", "Precip", "Unemployment", "Lag Consumption", "Consumption", "Citygate"))
incidents_reduced <- unique(incidents_being_reduced)
incidents_very_reduced <- incidents_reduced[complete.cases(incidents_reduced),]
incidents_very_reduced$ID <- paste(incidents_very_reduced$State, incidents_very_reduced$County, incidents_very_reduced$`Operator ID`, sep = "_")
incidents_very_reduced$Monthdate <- paste(incidents_very_reduced$Month, "01", sep ="-")
incidents_very_reduced$Monthdate <- as.Date(incidents_very_reduced$Monthdate, format = "%Y-%m-%d")
incidents_very_reduced$Monthdate <- as.integer(incidents_very_reduced$Monthdate)

#running my first batch of regression models, to look at fixed effect specifications
library(fixest)
no_fe <- feglm(Incident ~ Citygate + HDD + CDD + Precip + Unemployment + Monthdate + `Lag Consumption`, family = binomial(link="probit"), data = incidents_very_reduced)
id_month_fe <- feglm(Incident ~ Citygate + HDD + CDD + Precip + Unemployment + `Lag Consumption` | ID + Month, family = binomial(link = "probit"), data = incidents_very_reduced)
month_fe <- feglm(Incident ~ Citygate + HDD + CDD + Precip + Unemployment + `Lag Consumption` | Month, family = binomial(link = "probit"), data = incidents_very_reduced)
id_fe <- feglm(Incident ~ Citygate + HDD + CDD + Precip + Unemployment + Monthdate + `Lag Consumption` | ID, family = binomial(link = "probit"), data = incidents_very_reduced)
esttable(no_fe, id_month_fe, month_fe, id_fe)

#generating marginal effects
library(marginaleffects)
mfx_no_fe <- avg_slopes(no_fe)
mfx_month_fe <- avg_slopes(month_fe)
mfx_id_fe <- avg_slopes(id_fe)
mfx_id_month_fe <- avg_slopes(id_month_fe)
print(mfx_month_fe)

#doing the additional ones for consumption
consumption_nolag <- feglm(Incident ~ Citygate + HDD + CDD + Precip + Unemployment + Monthdate + Consumption, family = binomial(link="probit"), data = incidents_very_reduced)
no_consumption <- feglm(Incident ~ Citygate + HDD + CDD + Precip + Unemployment + Monthdate, family = binomial(link="probit"), data = incidents_very_reduced)
mfx_consumption_nolag <- avg_slopes(consumption_nolag)
mfx_no_consumption <- avg_slopes(no_consumption)