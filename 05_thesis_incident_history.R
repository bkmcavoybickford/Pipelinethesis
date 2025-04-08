#This file looks at the incident history regression specifications
#It should be run after files 01, 02, and 04 in the GitHub

#importing any libraries necessary
library(jtools)

#generating a lagged year variable
opstateyear$lag_year <- opstateyear$year -  1

#grouping incidents in preparation for merging my two datasets
gasdist_grouped <- gasdist_total |>
  group_by(State, Year, `Operator ID`) |>
  tally() |>
  dplyr::rename(incident = n)
gasdist_sig_grouped <- gasdist_total |>
  subset(Significant == "YES") |>
  group_by(State, Year, `Operator ID`) |>
  tally() |>
  dplyr::rename(incident_significant = n)
gasdist_ser_grouped <- gasdist_total |>
  subset(Serious == "YES") |>
  group_by(State, Year, `Operator ID`) |>
  tally() |>
  dplyr::rename(incident_serious = n)

#matching whether there was an incident in the current year and past year, with NAs being equivalent to 0s
opstateyear_inc <- opstateyear |>
  mutate(Year = as.character(year)) |>
  mutate(lag_Year = as.character(lag_year)) |>
  left_join(gasdist_grouped, 
            join_by("operator_id" == "Operator ID", "stateabbr" == "State", "Year" == "Year"), multiple = "all") |>
  left_join(gasdist_grouped, 
            join_by("operator_id" == "Operator ID", "stateabbr" == "State", "lag_Year" == "Year" ), multiple = "all") |>
  dplyr::rename(lag_incident = incident.y, incident = incident.x) |>
  mutate(incident = ifelse(is.na(incident), 0, incident)) |>
  mutate(lag_incident = ifelse(is.na(lag_incident), 0, lag_incident)) |>
  left_join(gasdist_ser_grouped, 
            join_by("operator_id" == "Operator ID", "stateabbr" == "State", "Year" == "Year"), multiple = "all") |>
  left_join(gasdist_ser_grouped, 
            join_by("operator_id" == "Operator ID", "stateabbr" == "State", "lag_Year" == "Year" ), multiple = "all") |>
  dplyr::rename(lag_incident_serious = incident_serious.y, incident_serious = incident_serious.x) |>
  mutate(incident_serious = ifelse(is.na(incident_serious), 0, incident_serious)) |>
  mutate(lag_incident_serious = ifelse(is.na(lag_incident_serious), 0, lag_incident_serious))
  

#regression with incidents in that year on leaks
leaks_inc <- feols(leaks_mains ~ incident + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear_inc, vcov = "HC1")
leaks_inc_srvs <- feols(leaks_srvs ~ incident + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                     weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                     severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                     nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear_inc, vcov = "HC1")

#now with lagged incidents instead
leaks_inc_lag <- feols(leaks_mains ~ lag_incident + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                         weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                         severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                         nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear_inc, vcov = "HC1")
leaks_inc_lag_srvs <- feols(leaks_srvs ~ lag_incident + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                         weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                         severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                         nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear_inc, vcov = "HC1")

#trying those with serious instead
leaks_inc_ser <- feols(leaks_mains ~ incident_serious + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                        weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                        severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                        nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear_inc, vcov = "HC1")
leaks_inc_ser_lag <- feols(leaks_mains ~ lag_incident_serious + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear_inc, vcov = "HC1")

#adding in lagged leaks
opstateyear <- opstateyear |>
  group_by(operator_id, stateabbr) |>
  mutate(lagleaks_main = lag(leaks_mains, n = 1, default = NA)) |>
  mutate(lagleaks_points = lag(leaks_srvs, n = 1, default = NA))
leaks_fe_lag <- feols(leaks_mains ~ lagleaks_main + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_srvs_lag <- feols(leaks_srvs ~ lagleaks_points + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                         weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                         severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                         nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")

#now using the lagged version of the other measure of leaks
leaks_mains_cross <- feols(leaks_mains ~ lagleaks_points + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                             weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                             severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                             nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_srvs_cross <- feols(leaks_srvs ~ lagleaks_main + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                            weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                            severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                            nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")

#or without lags
leaks_mains_cross_nolag <- feols(leaks_mains ~ leaks_srvs + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                                   weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                                   severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                                   nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
leaks_srvs_cross_nolag <- feols(leaks_srvs ~ leaks_mains + price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                                  weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                                  severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                                  nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")

#now analyzing whether the effect is driven by specific states
state_level_results_leaks <- data.frame(region = NULL, coefficient = NULL)
region <- unique(opstateyear$region[!is.na(opstateyear$region)])
for (i in region) {
state_level_leaks <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                                         weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                                         severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                                         nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, region == i), vcov = "HC1")  
state_leaks_sum <- summary(state_level_leaks, vcov = "hetero")
citygate_coef <- state_leaks_sum$coeftable[1,1]
citygate_se <- state_leaks_sum$coeftable[1,2]
citygate_p <- state_leaks_sum$coeftable[1,4]
state_and_gate <- data.frame(region = i, coefficient = citygate_coef, se = citygate_se, p = citygate_p)
state_level_results_leaks <- rbind(state_level_results_leaks, state_and_gate)
}

#and doing the same but for services
state_level_results_services <- data.frame(region = NULL, coefficient = NULL)
region <- unique(opstateyear$region[!is.na(opstateyear$region)])
for (i in region) {
  state_level_services <- feols(leaks_srvs ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                               weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                               severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                               nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, region == i), vcov = "HC1")  
  state_leaks_sum <- summary(state_level_services, vcov = "hetero")
  citygate_coef <- state_leaks_sum$coeftable[1,1]
  citygate_se <- state_leaks_sum$coeftable[1,2]
  citygate_p <- state_leaks_sum$coeftable[1,4]
  state_and_gate <- data.frame(region = i, coefficient = citygate_coef, se = citygate_se, p = citygate_p)
  state_level_results_services <- rbind(state_level_results_services, state_and_gate)
}
state_level_results_leaks$p_bonferroni <- state_level_results_leaks$p*8
state_level_results_services$p_bonferroni <- state_level_results_services$p*8
kable(state_level_results_leaks, format = "latex")
kable(state_level_results_services, format = "latex")

leaks_wo_ne <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, region != "New England"), vcov = "HC1")  

holm_leaks <- p.adjust(state_level_results_leaks$p, method = "holm")
holm_services <- p.adjust(state_level_results_services$p, method = "holm")
b_leaks <- p.adjust(state_level_results_leaks$p, method = "bonferroni")

#Testing to see if logs do anything
loggate <- feols(leaks_mains ~ log(price) + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                   weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                   severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                   nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")
logservices <- feols(leaks_srvs ~ log(price) + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                       weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                       severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                       nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")

#Adding five-year chunks
opstateyear <- opstateyear |>
  mutate(year_group = case_when(
    year >= 2004 & year <= 2008~ "2004 to 2008",
    year >= 2009 & year <= 2013~ "2009 to 2013",
    year >= 2014 & year <= 2018~ "2014 to 2018",
    year >= 2019 & year <= 2022~ "2019 to 2022"
  ))
year_level_results_leaks <- data.frame(year = NULL, coefficient = NULL, se = NULL, p = NULL)
year_group <- unique(opstateyear$year_group)
for (i in year_group) {
  year_level_leaks <- feols(leaks_mains ~ price + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                               weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                               severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                               nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = filter(opstateyear, year_group == i), vcov = "HC1")  
  year_leaks_sum <- summary(year_level_leaks, vcov = "hetero")
  citygate_coef <- year_leaks_sum$coeftable[1,1]
  citygate_se <- year_leaks_sum$coeftable[1,2]
  citygate_p <- year_leaks_sum$coeftable[1,4]
  year_and_gate <- data.frame(year_group = i, coefficient = citygate_coef, se = citygate_se, p = citygate_p)
  year_level_results_leaks <- rbind(year_level_results_leaks, year_and_gate)
}

#Or trying a year interaction
leaks_mains_by_year <- feols(leaks_mains ~ i(year, price) + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                               weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                               severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                               nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")  
leaks_services_by_year <- feols(leaks_srvs ~ i(year, price) + mmiles_total + nsrvcs_total + avelength + weathdays1 +
                                  weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                                  severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                                  nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year, data = opstateyear, vcov = "HC1")  

#Now making charts of those
png("Output/leaks_mains_by_year.png")
plot_coefs(leaks_mains_by_year, coefs = c("2004" = "year::2004:price", "2005" = "year::2005:price", 
                                          "2006" = "year::2006:price", "2007" = "year::2007:price",
                                          "2008" = "year::2008:price", "2009" = "year::2009:price", 
                                          "2010" = "year::2010:price", "2011" = "year::2011:price",
                                          "2012" = "year::2012:price", "2013" = "year::2013:price", 
                                          "2014" = "year::2014:price", "2015" = "year::2015:price",
                                          "2016" = "year::2016:price", "2017" = "year::2017:price", 
                                          "2018" = "year::2018:price", "2019" = "year::2019:price",
                                          "2020" = "year::2020:price", "2021" = "year::2021:price", 
                                          "2022" = "year::2022:price"), legend.title = "Coefficient of Price")
dev.off()
png("Output/leaks_services_by_year.png")
plot_coefs(leaks_services_by_year, coefs = c("2004" = "year::2004:price", "2005" = "year::2005:price", 
                                          "2006" = "year::2006:price", "2007" = "year::2007:price",
                                          "2008" = "year::2008:price", "2009" = "year::2009:price", 
                                          "2010" = "year::2010:price", "2011" = "year::2011:price",
                                          "2012" = "year::2012:price", "2013" = "year::2013:price", 
                                          "2014" = "year::2014:price", "2015" = "year::2015:price",
                                          "2016" = "year::2016:price", "2017" = "year::2017:price", 
                                          "2018" = "year::2018:price", "2019" = "year::2019:price",
                                          "2020" = "year::2020:price", "2021" = "year::2021:price", 
                                          "2022" = "year::2022:price"), legend.title = "Coefficient of Price", colors = "red")
dev.off()
