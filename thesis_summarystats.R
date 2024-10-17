#The following code generates summary statistics relating to pipeline incidents in the United States, from PHMSA Data
#It also generates summary statistics for certain independent variables for my project
#This was created as part of ECON 691 at UNC

#setting directory
setwd("C:/Users/benja/Downloads/Senior Thesis (Pipelines)")

#importing data (PHMSA pipeline incident files, the flagged data version)
library(readxl)
gasdist1 <- read_excel("gd1986tofeb2004.xlsx", sheet = "gd1986tofeb2004")
gasdist2 <- read_excel("gdmar2004to2009.xlsx", sheet = "gdmar2004to2009")
gasdist3 <- read_excel("gd2010toPresent.xlsx", sheet = "gd2010toPresent")

#combining all three gasdist files into a single file

#first, I need to filter it down to the parts of each data frame I care about
#I'm also giving the columns more natural names
gasdist1.1 <- subset(gasdist1, 
                     select = c(SIGNIFICANT, SERIOUS, RPTID, OPID, ACCNT, ACCST, ACZIP, CLASS, IFED, IDATE, FAT, INJ, TOTAL_COST_CURRENT, INPRS, MXPRS, MAP_CAUSE, MAP_SUBCAUSE, PRTLK, MLKD, NMDIA, PRTYR, LOCLK))
colnames(gasdist1.1) <- c("Significant", "Serious", "Report ID", "Operator ID", "County", "State", "ZIP", "Class", "Federal", "Date", "Fatalities", "Injuries", "Cost", "Est. Pressure", "Max Pressure", "Cause", "Subcause", "Part", "Material", "NPS", "Install Year", "Area")
gasdist2.1 <- subset(gasdist2, select = c(SIGNIFICANT, SERIOUS, RPTID, OPERATOR_ID, ACCOUNTY, ACSTATE, ACZIP, CLASS, IFED, IDATE, FATAL, INJURE, TOTAL_COST_CURRENT, INC_PRS, MAOP, MAP_CAUSE, MAP_SUBCAUSE, TYSYS_TEXT, MLKD_TEXT, NPS, PRTYR, LOCLK_TEXT))
colnames(gasdist2.1) <- c("Significant", "Serious", "Report ID", "Operator ID", "County", "State", "ZIP", "Class", "Federal", "Date", "Fatalities", "Injuries", "Cost", "Est. Pressure", "Max Pressure", "Cause", "Subcause", "Part", "Material", "NPS", "Install Year", "Area")
gasdist3.1 <- subset(gasdist3, select = c(SIGNIFICANT, SERIOUS, REPORT_NUMBER, OPERATOR_ID, LOCATION_COUNTY_NAME, LOCATION_STATE_ABBREVIATION, LOCATION_POSTAL_CODE, CLASS_LOCATION_TYPE, FEDERAL, LOCAL_DATETIME, FATAL, INJURE, TOTAL_COST_CURRENT, ACCIDENT_PSIG, MOP_PSIG, MAP_CAUSE, MAP_SUBCAUSE, SYSTEM_PART_INVOLVED, MATERIAL_INVOLVED, PIPE_DIAMETER, INSTALLATION_YEAR, INCIDENT_AREA_TYPE))
colnames(gasdist3.1) <- c("Significant", "Serious", "Report ID", "Operator ID", "County", "State", "ZIP", "Class", "Federal", "Date", "Fatalities", "Injuries", "Cost", "Est. Pressure", "Max Pressure", "Cause", "Subcause", "Part", "Material", "NPS", "Install Year", "Area")

#now I'm checking to make sure that all the variables are coded the same way across datasets
#and also fixing the ones that aren't
unique(gasdist1.1$Class)
unique(gasdist2.1$Class)
unique(gasdist3.1$Class)
#Dataset 3 has completely different class things. There are also NAs in 1 and 2 but that's not so big a deal
library(stringr)
gasdist3.1$Class <- str_sub(gasdist3.1$Class, start = 7, end = 7)

#Federal has some nulls also, but I'm not doing anything about it
#Date also has issues with the third one having extra time issue
gasdist3.1$Date <- str_sub(gasdist3.1$Date, start = 1, end = 10)

#some subcauses and parts are only in certain files, but that's not going to be a big deal for any of my analyses
#I also need to make my material data all more similar
unique(gasdist1.1$Material)
unique(gasdist2.1$Material)
unique(gasdist3.1$Material)
gasdist1.1$Material[gasdist1.1$Material == "POLYETHYLENE PLASTIC" | gasdist1.1$Material == "OTHER PLASTIC"] <- "PLASTIC"
gasdist2.1$Material[gasdist2.1$Material == "POLYETHYLENE PLASTIC" | gasdist2.1$Material == "OTHER PLASTIC" | gasdist2.1$Material == "POLYETHELENE PLASTIC"] <- "PLASTIC"
gasdist2.1$Material[gasdist2.1$Material == "CAST/WROUGHT IRON"] <- "CAST IRON"
gasdist2.1$Material[gasdist2.1$Material == "OTHER MATERIAL"] <- "OTHER"
gasdist2.1$Material[is.na(gasdist2.1$Material)] <- "NO DATA"
gasdist3.1$Material[gasdist3.1$Material == "CAST/WROUGHT IRON"] <- "CAST IRON"
gasdist3.1$Material[gasdist3.1$Material == "COPPER" | gasdist3.1$Material == "DUCTILE IRON"] <- "OTHER"
gasdist3.1$Material[gasdist3.1$Material == "UNKNOWN"] <- "NO DATA"

#It turns out area data is way too messed up to really work with but I'll keep it around since I can subset by year
unique(gasdist1.1$Area)
unique(gasdist2.1$Area)
unique(gasdist3.1$Area)

#time to actually merge all my data
gasdist_total <- rbind(gasdist1.1, gasdist2.1, gasdist3.1)

#okay, let's make a choropleth
#first step, clean up the county data, including making it fit neatly with FIPS.
#Maybe I'll bother to clean up all the weird typos at some point
library(dplyr)
gasdist_total$County <- toupper(gasdist_total$County)
gasdist_total$County <- str_replace(gasdist_total$County, "COUNTY", "")
gasdist_total$County <- str_replace(gasdist_total$County, "PARISH", "")
gasdist_total$County <- str_replace(gasdist_total$County, "BOROUGH", "")
gasdist_total$County <- str_squish(gasdist_total$County)
gasdist_total$County[gasdist_total$State == "PA" & gasdist_total$County == "ALLEGHANY"] <- "ALLEGHENY"
gasdist_total$County <- str_replace(gasdist_total$County, "COMMANCHE", "COMANCHE")
gasdist_total$County <- str_replace(gasdist_total$County, "ST LOUIS", "ST. LOUIS")
gasdist_total$County <- str_replace(gasdist_total$County, "SAINT LOUIS", "ST. LOUIS")

#Now I'm grouping by county
gasdist_by_county <- gasdist_total |>
  group_by(County, State) |>
  tally()

#Now I'm merging the county-grouped data with county fips codes
library(choroplethr)
library(choroplethrMaps)
data(county.regions)
colnames(county.regions) <- c("region", "county.fips.character", "County", "state.name", "state.fips.character", "State")
county.regions$County <- toupper(county.regions$County)
gasdist_by_county_fips <- merge(gasdist_by_county, county.regions, by = c("County", "State"), all.y = TRUE)
#Note that I have to eliminate some of the data because it doesn't match well with a county FIPS code, hopefully I'll have time to fix that later on

#here I'm getting it in the right setup for the choroplethr package
data(county.map)
gasdist_by_county_fips$county.fips.character <- str_remove(gasdist_by_county_fips$county.fips.character, "^0+")
gasdist_by_county_fips_small <- subset(gasdist_by_county_fips, select = c("county.fips.character", "n"))
colnames(gasdist_by_county_fips_small) <- c("region", "value")
gasdist_by_county_fips_small$value[is.na(gasdist_by_county_fips_small$value)] <- 0
gasdist_by_county_fips_small$region <- as.double(gasdist_by_county_fips_small$region)

#actually creating the choropleth
png("incident_choropleth_alltime.png")
county_choropleth(gasdist_by_county_fips_small, num_colors = 9)
dev.off()

#now making a table of the counties with the most incidents
gasdist_by_county_fips <- gasdist_by_county_fips[order(gasdist_by_county_fips$n, decreasing = TRUE), ]
gasdist_by_county_table <- subset(gasdist_by_county_fips, select = c("County", "State", "n"))
gasdist_by_county_table <- subset(gasdist_by_county_table, gasdist_by_county_table$n > 30)
gasdist_by_county_table <- distinct(gasdist_by_county_table)
write.csv(gasdist_by_county_table, "incidents_by_county_table.csv")
sum(gasdist_by_county_table$n)

#the next task is to produce a graph of county by year
gasdist_total$Year <- str_sub(gasdist_total$Date, 1, 4)
gasdist_by_year <- gasdist_total |>
  filter(Year != 2024) |>
  group_by(Year) |>
  tally()
gasdist_by_year$Year <- as.numeric(gasdist_by_year$Year)
year_by_n <- lm(gasdist_by_year$n ~ gasdist_by_year$Year)
png("incidents_by_year.png")
plot(gasdist_by_year$Year, gasdist_by_year$n, type ="l", ylim = c(0, 215),
     xlab = "Year", ylab = "Number of Incidents")
abline(year_by_n, col = "red")
dev.off()

#now breaking down significant and serious incidents
gasdist_significant <- gasdist_total |>
  filter(Significant == "YES" & Year != 2024) |>
  group_by(Year) |>
  tally()
gasdist_serious <- gasdist_total |>
  filter(Serious == "YES" & Year != 2024) |>
  group_by(Year) |>
  tally()
png("serious_and_significant_incidents.png")
plot(gasdist_by_year$Year, gasdist_by_year$n, type ="l", ylim = c(0, 215),
     xlab = "Year", ylab = "Number of Incidents")
lines(gasdist_significant$Year, gasdist_significant$n, type = "l", col = "blue")
lines(gasdist_serious$Year, gasdist_serious$n, type ="l", col = "red")
legend(x = "topright",
       legend = c("All", "Significant", "Serious"),
       col = c("black", "blue", "red"),
       lwd = 2)
dev.off()

#now importing city gate price data
library(readxl)
citygate <- read_excel("NG_PRI_SUM_A_EPG0_PG1_DMCF_M (2).xls", sheet = "Data 1", skip = 2)
citygate$Year <- str_sub(citygate$Date, 1, 4)
citygate <- subset(citygate, citygate$Year > 1985)
citygate$NAs <- rowSums(is.na(citygate))
colnames(citygate) <- str_replace(colnames(citygate), "Natural Gas Citygate Price", "")
colnames(citygate) <- str_replace(colnames(citygate), str_escape("(Dollars per Thousand Cubic Feet)"), "")
colnames(citygate) <- str_squish(colnames(citygate))
colnames(citygate) <- str_replace(colnames(citygate), "^[i][n]", "")
colnames(citygate) <- str_squish(colnames(citygate))
citygate$Date <- as.Date(citygate$Date)
citygate_filtered <- subset(citygate, NAs < 10)


#figuring out what states have had the most incidents
gasdist_by_state <- gasdist_total |>
  group_by(State) |>
  tally()
gasdist_by_state <- gasdist_by_state[order(gasdist_by_state$n, decreasing = TRUE), ]

#plotting citygate prices for the five states with the most incidents
library(ggplot2)
png("citygate_plot.png", width = 600, height = 300)
ggplot(citygate_filtered, aes(x = Date)) + 
  geom_line(aes(y = U.S., color = "black"))  + 
  geom_line(aes(y = Texas, color = "red")) + 
  geom_line(aes(y = California, color = "green")) +
  geom_line(aes(y = Pennsylvania, color = "blue")) +
  geom_line(aes(y = `New York`, color = "orange")) +
  geom_line(aes(y = Michigan, color = "yellow")) +
  ylim(0, 14) +
  scale_color_manual(labels = c("U.S.", "Texas", "California", "Pennsylvania", "New York", "Michigan"), values = c("black", "red", "green", "blue", "orange", "yellow"), name = "Region") +
  ylab("Citygate Price")
dev.off()

#now getting the median city gate price for every statecitygate_filtered <- as.data.frame(citygate_filtered)
citygate_more_filtered <- subset(citygate_filtered, select = -c(`U.S.`, `Date`, `Year`, `the District of Columbia`, `NAs`))
citygate_median <- sapply(citygate_more_filtered, median, na.rm = TRUE)
citygate_median <- data.frame(citygate_median)
statenames <- rownames(citygate_median)
rownames(citygate_median) <- NULL
citygate_median <- cbind(statenames, citygate_median)

#turning it into a choropleth
citygate_median$region <- tolower(citygate_median$statenames)
citygate_median$value <- citygate_median$citygate_median
citygate_median <- subset(citygate_median, select = c("value", "region"))
png("citygate_by_state.png")
state_choropleth(citygate_median)
dev.off()
median(citygate_filtered$U.S., na.rm = TRUE)

#exporting table
citygate_median$region <- toupper(citygate_median$region)
write.csv(citygate_median, "citygate_median_2.csv")
