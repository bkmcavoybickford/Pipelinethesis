#This was created as part of ECON 691 at UNC
#Note that this file should be run after 01_thesis_summarystats.R and 02_thesis_maindataset.R
#Currently 03 is a STATA file created by me and Dr. Klara Peter; my goal is to redo it in R
#This code imports and analyzes annual reports filed by gas distributors
#Loading packages
library(usdata)
library(viridis)
library(anytime)

#importing data from STATA
opstateyear <- read_csv("Data/opstateyear.csv") |>
  filter(year > 2003 & year < 2023) |>
  filter(is.na(commodity) | commodity == "Natural Gas" | commodity == "OTHER GAS: Natural Gas") |>
  mutate(leaks_mains = ifelse(is.na(leaks_mains), 0, leaks_mains)) |>
  mutate(leaks_srvs = ifelse(is.na(leaks_srvs), 0, leaks_srvs)) |>
  mutate(leaks_cor_mains = ifelse(is.na(leaks_cor_mains), 0, leaks_cor_mains)) |>
  mutate(leaks_of_mains = ifelse(is.na(leaks_of_mains), 0, leaks_of_mains)) |>
  mutate(leaks_nf_mains = ifelse(is.na(leaks_nf_mains), 0, leaks_nf_mains)) |>
  mutate(leaks_mat_mains = ifelse(is.na(leaks_mat_mains), 0, leaks_mat_mains)) |>
  mutate(leaks_ot_mains = ifelse(is.na(leaks_ot_mains), 0, leaks_ot_mains)) |>
  mutate(leaks_cor_srvs = ifelse(is.na(leaks_cor_srvs), 0, leaks_cor_srvs)) |>
  mutate(leaks_of_srvs = ifelse(is.na(leaks_of_srvs), 0, leaks_of_srvs)) |>
  mutate(leaks_nf_srvs = ifelse(is.na(leaks_nf_srvs), 0, leaks_nf_srvs)) |>
  mutate(leaks_mat_srvs = ifelse(is.na(leaks_mat_srvs), 0, leaks_mat_srvs)) |>
  mutate(leaks_ot_srvs = ifelse(is.na(leaks_ot_srvs), 0, leaks_ot_srvs)) |>
  filter(stateabbr != "AK" & stateabbr != "DC" & stateabbr != "HI" & stateabbr != "US")

#summary statistics: generating graphs for the number of leaks of each type for mains
opstateyear_main_grouped <- opstateyear |>
  select(c("leaks_cor_mains", "leaks_nf_mains", "leaks_ot_mains", "leaks_of_mains", "leaks_mat_mains", "year")) |>
  group_by(year) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(!year, names_to = "leaktype", values_to = "count")
png("Output/mainleaks.png")
ggplot(opstateyear_main_grouped, aes(fill = leaktype, y = count, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y ="Number of Leaks") +
  theme_classic() +
  scale_fill_discrete(name = "Type of Leak", labels = c("leaks_cor_mains" = "Corrosion", "leaks_mat_mains" = "Technical Failure", "leaks_nf_mains" = "Natural Force", "leaks_of_mains" = "Outside Force", "leaks_ot_mains" = "Other"))
dev.off()

#now doing the same, but for points of service
opstateyear_service_grouped <- opstateyear |>
  select(c("leaks_cor_srvs", "leaks_nf_srvs", "leaks_ot_srvs", "leaks_of_srvs", "leaks_mat_srvs", "year")) |>
  group_by(year) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(!year, names_to = "leaktype", values_to = "count")
png("Output/serviceleaks.png")
options(scipen=100000)
ggplot(opstateyear_service_grouped, aes(fill = leaktype, y = count, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y ="Number of Leaks") +
  theme_classic() +
  scale_fill_discrete(name = "Type of Leak", labels = c("leaks_cor_srvs" = "Corrosion", "leaks_mat_srvs" = "Technical Failure", "leaks_nf_srvs" = "Natural Force", "leaks_of_srvs" = "Outside Force", "leaks_ot_srvs" = "Other"))
dev.off()

#now doing the same, but for whether leaks are hazardous or not in mains
opstateyear_hazmain_grouped <- opstateyear |>
  filter(!is.na(hazleaks_mains)) |>
  mutate(nonhazleaks_mains = leaks_mains-hazleaks_mains) |>
  select(c("nonhazleaks_mains", "hazleaks_mains", "year")) |>
  group_by(year) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(!year, names_to = "leaktype", values_to = "count")
png("Output/hazmainleaks.png")
ggplot(opstateyear_hazmain_grouped, aes(fill = leaktype, y = count, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y ="Number of Leaks") +
  theme_classic() +
  scale_fill_discrete(name = "Type of Leak", labels = c("hazleaks_mains" = "Hazardous", "nonhazleaks_mains" = "Non-hazardous"))
dev.off()

#now doing the same, but for whether leaks are hazardous or not at points of service
opstateyear_hazsrvs_grouped <- opstateyear |>
  filter(!is.na(hazleaks_srvs)) |>
  mutate(nonhazleaks_srvs = leaks_srvs-hazleaks_srvs) |>
  select(c("nonhazleaks_srvs", "hazleaks_srvs", "year")) |>
  group_by(year) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(!year, names_to = "leaktype", values_to = "count")
png("Output/hazservicesleaks.png")
ggplot(opstateyear_hazsrvs_grouped, aes(fill = leaktype, y = count, x = year)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y ="Number of Leaks") +
  theme_classic() +
  scale_fill_discrete(name = "Type of Leak", labels = c("hazleaks_srvs" = "Hazardous", "nonhazleaks_srvs" = "Non-hazardous"))
dev.off()

#seeing the number of leaks by state
opstateyear_by_state <- opstateyear |>
  select(c("leaks_mains", "mmiles_total", "year", "stateabbr", "statefips")) |>
  group_by(year, stateabbr, statefips) |>
  summarise(leaks_mains = sum(leaks_mains, na.rm = TRUE), mmiles_total = sum(mmiles_total, na.rm = TRUE)) |>
  mutate(leaks_per_mile = leaks_mains/mmiles_total)

#generating two choropleths from that
opstateyear_by_state_2004 <- opstateyear_by_state |>
  filter(year == 2004) |>
  mutate(state = stateabbr)
png("Output/leaks_per_mile_2004.png", width = 600, height = 300)
plot_usmap(regions = "states", exclude = c("AK", "HI"), data = opstateyear_by_state_2004, values = "leaks_per_mile") + 
  scale_fill_continuous(low = "blue", high = "red", name = "Leaks per Mile in 2004", label = scales::comma) + 
  theme(legend.position = "right")
dev.off()
opstateyear_by_state_2022 <- opstateyear_by_state |>
  filter(year == 2022) |>
  mutate(state = stateabbr)
png("Output/leaks_per_mile_2022.png", width = 600, height = 300)
plot_usmap(regions = "states", exclude = c("AK", "HI"), data = opstateyear_by_state_2022, values = "leaks_per_mile") + 
  scale_fill_continuous(low = "blue", high = "red", name = "Leaks per Mile in 2022", label = scales::comma) + 
  theme(legend.position = "right")
dev.off()

#adding in histogram of leaks
hist <- ggplot(data=opstateyear, aes(x = leaks_mains)) +
  geom_histogram(breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000))

#regression: with lagged consumption, with and without the fixed effects
opstateyear$ID <- paste(opstateyear$stateabbr, opstateyear$operator_id, sep = "_")
opstateyear <- opstateyear |>
  group_by(ID) |>
  mutate(lagconsumption = lag(consumption, n = 1, default = NA))
leaks_fe <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_fe_sum <- summary(leaks_fe, vcov = "hetero")
leaks_no_fe <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption, data = opstateyear, vcov = "HC1")
leaks_no_fe_sum <- summary(leaks_no_fe, vcov = "hetero")
leaks_fe_id <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID, data = opstateyear, vcov = "HC1")
leaks_fe_id_sum <- summary(leaks_fe_id, vcov = "hetero")
leaks_fe_year <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|year, data = opstateyear, vcov = "HC1")
leaks_fe_year_sum <- summary(leaks_fe_year, vcov = "hetero")

#doing very similar regressions, but now with points of service leaks
services_fe <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_fe_sum <- summary(services_fe, vcov = "hetero")
services_no_fe <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption, data = opstateyear, vcov = "HC1")
services_no_fe_sum <- summary(services_no_fe, vcov = "hetero")
services_fe_id <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID, data = opstateyear, vcov = "HC1")
services_fe_id_sum <- summary(services_fe_id, vcov = "hetero")
services_fe_year <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                         weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                         severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                         nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|year, data = opstateyear, vcov = "HC1")
services_fe_year_sum <- summary(services_fe_year, vcov = "hetero")

#regressions on mains with different consumption specifications
leaks_consumption <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + consumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_consumption_sum <- summary(leaks_consumption, vcov = "hetero")
leaks_no_consumption <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt|ID + year, data = opstateyear, vcov = "HC1")
leaks_no_consumption_sum <- summary(leaks_no_consumption, vcov = "hetero")
leaks_both_consumptions <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption + consumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_both_consumptions_sum <- summary(leaks_both_consumptions, vcov = "hetero")

#regressions on points of service with different consumption specifications
services_consumption <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + consumption|ID + year, data = opstateyear, vcov = "HC1")
services_consumption_sum <- summary(services_consumption, vcov = "hetero")
services_no_consumption <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt|ID + year, data = opstateyear, vcov = "HC1")
services_no_consumption_sum <- summary(services_no_consumption, vcov = "hetero")
services_both_consumption <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + consumption + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_both_consumption_sum <- summary(services_both_consumption, vcov = "hetero")

#regressions on different kinds of leak for mains
leaks_cor_fe <- feols(leaks_cor_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_cor_fe_sum <- summary(leaks_cor_fe, vcov = "hetero")
leaks_nf_fe <- feols(leaks_nf_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_nf_fe_sum <- summary(leaks_nf_fe, vcov = "hetero")
leaks_of_fe <- feols(leaks_of_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_of_fe_sum <- summary(leaks_of_fe, vcov = "hetero")
leaks_mat_fe <- feols(leaks_mat_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_mat_fe_sum <- summary(leaks_mat_fe, vcov = "hetero")
leaks_ot_fe <- feols(leaks_ot_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_ot_fe_sum <- summary(leaks_ot_fe, vcov = "hetero")

#now for services
services_cor_fe <- feols(leaks_cor_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_cor_fe_sum <- summary(services_cor_fe, vcov = "hetero")
services_nf_fe <- feols(leaks_nf_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_nf_fe_sum <- summary(services_nf_fe, vcov = "hetero")
services_of_fe <- feols(leaks_of_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_of_fe_sum <- summary(services_of_fe, vcov = "hetero")
services_ot_fe <- feols(leaks_ot_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_ot_fe_sum <- summary(services_ot_fe, vcov = "hetero")
services_mat_fe <- feols(leaks_mat_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_mat_fe_sum <- summary(services_mat_fe, vcov = "hetero")

#now doing the same but for hazardous instead
services_haz_fe <- feols(hazleaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
services_haz_fe_sum <- summary(services_haz_fe, vcov = "hetero")
leaks_haz_fe <- feols(hazleaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_haz_fe_sum <- summary(leaks_haz_fe, vcov = "hetero")

#now taking a look at operator type as a possible mediator, for leaks on mains
leaks_coop <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Cooperative"), vcov = "HC1")
leaks_coop_sum <- summary(leaks_coop, vcov = "hetero")
leaks_invest <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Investor Owned"), vcov = "HC1")
leaks_invest_sum <- summary(leaks_invest, vcov = "hetero")
leaks_munic <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Municipal Owned"), vcov = "HC1")
leaks_munic_sum <- summary(leaks_munic, vcov = "hetero")
leaks_private <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Privately Owned"), vcov = "HC1")
leaks_private_sum <- summary(leaks_private, vcov = "hetero")

#now doing the same but for leaks at points of service
services_coop <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Cooperative"), vcov = "HC1")
services_coop_sum <- summary(services_coop, vcov = "hetero")
services_invest <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Investor Owned"), vcov = "HC1")
services_invest_sum <- summary(services_invest, vcov = "hetero")
services_munic <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Municipal Owned"), vcov = "HC1")
services_munic_sum <- summary(services_munic, vcov = "hetero")
services_private <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, operator_type == "Privately Owned"), vcov = "HC1")
services_private_sum <- summary(services_private, vcov = "hetero")

#printing a table to latex
etable(leaks_fe, services_fe, tex = TRUE)

     