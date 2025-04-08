#importing packages
library(Synth)
library(anytime)
library(SCtools)
library(synthdid)
library(did)

#importing consumer choice data for residential consumers
consumer_choice <- read_excel("Data/NG_CONS_ACCT_A_EPG0_VF9_PCT_A.xls", 
                              sheet = "Data 1", skip = 2) |>
  pivot_longer(cols = !Date, names_to = "State") |>
  mutate(State = str_split_i(State, " Natural", 1)) |>
  mutate(Date = str_sub(Date, 1, 4)) |>
  dplyr::rename(Choice = value)

#merging it with the opstateyear file
opstateyear_cc <- merge(opstateyear, consumer_choice, by.x = c("statename", "year"), by.y = c("State", "Date"), all.x = TRUE, all.y = FALSE) |>
  filter(year > 2006) |>
  mutate(Choice = ifelse(is.na(Choice), 0, Choice))

#running a regression with consumer choice as the new independent variable of interest
ccreg_mains_1 <- feols(leaks_mains ~ mmiles_total + nsrvcs_total + avelength + weathdays1 +
                         weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                         severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                         nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption | year + ID | price ~ Choice, data = opstateyear_cc)
ccreg_services_1 <- feols(leaks_srvs ~ mmiles_total + nsrvcs_total + avelength + weathdays1 +
                                        weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
                                        severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                                        nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year | price ~ Choice, data = opstateyear_cc, vcov = "HC1")

#now trying it with only some consumer choice recorded
ccreg_mains_2 <- feols(leaks_mains ~ mmiles_total + nsrvcs_total + avelength + weathdays1 +
    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year | price ~ Choice, data = opstateyear_cc, vcov = "HC1", subset = opstateyear_cc$Choice > 0)
ccreg_services_2 <- feols(leaks_srvs ~ mmiles_total + nsrvcs_total + avelength + weathdays1 +
    weathdays2 + weathdays3 + weathdays4 + weathdays5 + weathdays6 + weathdays7 + weathevents +
    severe + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption|ID + year | price ~ Choice, data = opstateyear_cc, vcov = "HC1", subset = opstateyear_cc$Choice > 0)

#seeing the relationship between consumer choice and price
price_cc <- feols(Choice ~ price + unrate + popubea + persinc + netearn + support + unemins + divrent + propinc + farminc +
                    nofarmi + gdpreal + gdpcap + gdppriv + gdpagri + gdputil + gdpmine + gdpmanf + gdptran + gdpgovt + lagconsumption |ID + year, data = opstateyear_cc, vcov = "HC1")
price_cc_simple <- feols(Choice ~ price, data = opstateyear_cc)

#now grouping leak data by state in order to do a synthetic control, and using expand to make sure there's one row for every state-year combination
gasdist_synth <- gasdist_total |>
  group_by(State, Year) |>
  summarise(n = n()) |>
  ungroup() |>
  tidyr::complete(State, Year)

#actually that's going to still have rare variable problems, let's do it with city gate price instead
fipscodes_state <- fipscodes |>
  select(STATE, STATEFP) |>
  unique()
citygate_pivot_small <- merge(citygate_pivot, fipscodes_state, by.x = "State", by.y = "STATE", all.x= TRUE, all.y = FALSE) |>
  filter(Year >= 1989 & !is.na(State)) |>
  tidyr::complete(State, Month) |>
  mutate(STATEFP = as.numeric(STATEFP)) |>
  mutate(Month = anydate(Month)) |>
  mutate(Citygate_interp = approx(1:n(), Citygate, 1:n(), method = "constant", f = 0.5)$y) |>
  group_by(State) |>
  mutate(lag_Citygate = lag(Citygate_interp)) |>
  mutate(numMonth = order(Month))
citygate_pivot_small <- as.data.frame(citygate_pivot_small)

#importing my spreadsheet of choice by state data
choice_by_state <- read_csv("Data/choice_over_time.csv") |>
  mutate(State_abbr = state2abbr(State))
choice_by_state_1997 <- merge(choice_by_state, fipscodes_state, by.x = "State_abbr", by.y = "STATE", all.x = TRUE, all.y = FALSE) |>
  filter(is.na(`Year of Implementation`) & State_abbr != "AK" & State_abbr != "HI")
choice_controls <- choice_by_state_1997$STATEFP

#now actually doing a synthetic control for Pennsylvania
dataprep_pa <- dataprep(foo = citygate_pivot_small, predictors = "Citygate_interp", dependent = "Citygate_interp",
                     unit.variable = "STATEFP", time.variable = "numMonth", treatment.identifier = 42, controls.identifier = c(01, 04:05, 09:10, 16, 20, 22, 23, 27:29, 33, 37:38, 40:41, 44:50, 53:55), time.predictors.prior = 2:97, time.optimize.ssr = 2:97, time.plot = 2:420, unit.names.variable = c("State"))
synth_pa <- synth(dataprep_pa)
path.plot(synth.res = synth_pa,
          dataprep.res = dataprep_pa,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "City Gate Price", Legend = c("Pennsylvania", "Synthetic Pennsylvania"))
gaps.plot(synth.res = synth_pa,
           dataprep.res = dataprep_pa,
           tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "Difference Between Treated and Synthetic City Gate Prices", Main = "Pennsylvania")
weights_pa <- as.data.frame(synth_pa$solution.w)
synth_tables_pa <- synth.tab(dataprep.res = dataprep_pa, synth.res = synth_pa)

#trying the same for Georgia (as another test)
dataprep_ga <- dataprep(foo = citygate_pivot_small, predictors = "lag_Citygate", dependent = "Citygate_interp",
                        unit.variable = "STATEFP", time.variable = "numMonth", treatment.identifier = 13, controls.identifier = c(1,4:6,8:10,12,16:42,44:51,53:56), time.predictors.prior = 2:97, time.optimize.ssr = 2:97, time.plot = 2:420)
synth_ga <- synth(dataprep_ga)
path.plot(synth.res = synth_ga,
          dataprep.res = dataprep_ga,
          tr.intake = 121)
gaps.plot(synth.res = synth_ga,
          dataprep.res = dataprep_ga,
          tr.intake = 121)

#trying the same for New York (as a placebo)
dataprep_ny <- dataprep(foo = citygate_pivot_small, predictors = "Citygate_interp", dependent = "Citygate_interp",
                        unit.variable = "STATEFP", time.variable = "numMonth", treatment.identifier = 36, controls.identifier = c(1,4:6,8:10,12:13,16:35,37:41,44:51,53:56), time.predictors.prior = 2:97, time.optimize.ssr = 2:97, time.plot = 2:420)
synth_ny <- synth(dataprep_ny)
path.plot(synth.res = synth_ny,
          dataprep.res = dataprep_ny,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "City Gate Price", Legend = c("New York", "Synthetic New York"))
gaps.plot(synth.res = synth_ny,
          dataprep.res = dataprep_ny,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "Difference Between Treated and Synthetic City Gate Prices", Main = "New York", Ylim = c(-6,6))
weights_ny <- as.data.frame(synth_ny$solution.w)
synth_tables_ny <- synth.tab(dataprep.res = dataprep_ny, synth.res = synth_ny)

#trying the same for Delaware (as a better placebo, b/c NY's program was also phased in around then)
dataprep_de <- dataprep(foo = citygate_pivot_small, predictors = "Citygate_interp", dependent = "Citygate_interp",
                        unit.variable = "STATEFP", time.variable = "numMonth", treatment.identifier = 10, controls.identifier = c(1,4:6,8:9,12:13,16:41,44:51,53:56), time.predictors.prior = 2:97, time.optimize.ssr = 2:97, time.plot = 2:420)
synth_de <- synth(dataprep_de)
path.plot(synth.res = synth_de,
          dataprep.res = dataprep_de,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "City Gate Price", Legend = c("Delaware", "Synthetic Delaware"))
gaps.plot(synth.res = synth_de,
          dataprep.res = dataprep_de,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "Difference Between Treated and Synthetic City Gate Prices", Main = "Delaware")
weights_de <- as.data.frame(synth_de$solution.w)
synth_tables_de <- synth.tab(dataprep.res = dataprep_de, synth.res = synth_de)

#and Rhode Island (to figure out why it weights Rhode Island so heavily)
dataprep_ri <- dataprep(foo = citygate_pivot_small, predictors = "Citygate_interp", dependent = "Citygate_interp",
                        unit.variable = "STATEFP", time.variable = "numMonth", treatment.identifier = 44, controls.identifier = c(1,4:6,8:10,12:13,16:41,45:51,53:56), time.predictors.prior = 2:97, time.optimize.ssr = 2:97, time.plot = 2:420)
synth_ri <- synth(dataprep_ri)
path.plot(synth.res = synth_ri,
          dataprep.res = dataprep_ri,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "City Gate Price", Legend = c("Rhode Island", "Synthetic Rhode Island"))
gaps.plot(synth.res = synth_ri,
          dataprep.res = dataprep_ri,
          tr.intake = 121, Xlab = "Months (starting in January 1989)", Ylab = "Difference Between Treated and Synthetic City Gate Prices", Main = "Rhode Island")
weights_ri <- as.data.frame(synth_ri$solution.w)
synth_tables_ri <- synth.tab(dataprep.res = dataprep_ri, synth.res = synth_ri)

#Montana
dataprep_mt <- dataprep(foo = citygate_pivot_small, predictors = "Citygate_interp", dependent = "Citygate_interp",
                        unit.variable = "STATEFP", unit.names.variable = "State", time.variable = "numMonth", treatment.identifier = 30, controls.identifier = c(01, 04:05, 09:10, 16, 20, 22, 23, 27:29, 33, 37:38, 40:41, 44:50, 53:55), time.predictors.prior = 2:73, time.optimize.ssr = 2:73, time.plot = 2:200)
synth_mt <- synth(data.prep.obj = dataprep_mt)
png("Output/synthetic_control_mt.png")
path.plot(synth.res = synth_mt,
          dataprep.res = dataprep_mt,
          tr.intake = 97, Xlab = "Months (starting in January 1989)", Ylab = "City Gate Price", Legend = c("Montana", "Synthetic Montana"))
dev.off()
gaps.plot(synth.res = synth_mt,
          dataprep.res = dataprep_mt,
          tr.intake = 97, Xlab = "Months (starting in January 1989)", Ylab = "Difference Between Treated and Synthetic City Gate Prices", Main = "Montana")
weights_mt <- as.data.frame(synth_mt$solution.w)
synth_tables_mt <- synth.tab(dataprep.res = dataprep_mt, synth.res = synth_mt)
placebo_mt <- generate.placebos(dataprep_mt, synth_mt)
png("Output/montana_placebos.png")
plot_placebos(placebo_mt)
dev.off()

#adding in residential price data and processing it like the citygate price data
resprice <- read_excel("Data/NG_PRI_SUM_A_EPG0_PRS_DMCF_M.xls", sheet = "Data 1", skip = 2) |>
  pivot_longer(cols = !Date, names_to = "State", values_to = "Price") |>
  filter(!is.na(Price)) |>
  mutate(Month = anytime(Date)) |>
  complete(State, Month)
resprice$State <- str_split_i(resprice$State, " Price", 1)
resprice$State_abbr <- state2abbr(resprice$State)
resprice <- merge(resprice, fipscodes_state, by.x = "State_abbr", by.y = "STATE", all.x = TRUE, all.y = FALSE) |>
  mutate(STATEFP = as.numeric(STATEFP)) |>
  filter(State_abbr != "AK" & !is.na(STATEFP) & State_abbr != "HI" & State_abbr != "DC")
resprice <- resprice |>
  group_by(State) |>
  arrange(Month) |>
  mutate(Price_i = approx(1:n(), Price, 1:n(), method = "constant", f = 0.5)$y) |>
  mutate(Year = str_sub(Month, 1, 4)) |>
  filter(Year > 1988) |>
  mutate(numMonth = order(Month))
resprice <- as.data.frame(resprice)

#Doing Montana but residential
dataprep_mt_res <- dataprep(foo = resprice, predictors = "Price_i", dependent = "Price_i",
                        unit.variable = "STATEFP", unit.names.variable = "State_abbr", time.variable = "numMonth", treatment.identifier = 30, controls.identifier = c(01, 04:05, 09:10, 16, 20, 22, 23, 27:29, 33, 37:38, 40:41, 44:50, 53:55), time.predictors.prior = 99:172, time.optimize.ssr = 99:172, time.plot = 99:400)
synth_mt_res <- synth(data.prep.obj = dataprep_mt_res, optimxmethod = "All")
path.plot(synth.res = synth_mt_res,
          dataprep.res = dataprep_mt_res,
          tr.intake = 194, Xlab = "Months (starting in January 1989)", Ylab = "Residential Price", Legend = c("Montana", "Synthetic Montana"))
gaps.plot(synth.res = synth_mt_res,
          dataprep.res = dataprep_mt_res,
          tr.intake = 194, Xlab = "Months (starting in January 1989)", Ylab = "Difference Between Treated and Synthetic Residential Prices", Main = "Montana")
weights_mt_res <- as.data.frame(synth_mt_res$solution.w)
synth_tables_mt_res <- synth.tab(dataprep.res = dataprep_mt_res, synth.res = synth_mt_res)
dataprep_mt_res$X0 <- as.matrix(dataprep_mt_res$X0)

#Doing the synthetic DiD for Montana residential
resprice_w_treat_mt <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Montana", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Montana" | is.na(`Year of Legality`))
setup_res_mt <- panel.matrices(resprice_w_treat_mt, time = 5)
tau.hat_res_mt = synthdid_estimate(setup_res_mt$Y, setup_res_mt$N0, setup_res_mt$T0)
tau.sc_res_mt   = sc_estimate(setup_res_mt$Y, setup_res_mt$N0, setup_res_mt$T0)
tau.did_res_mt  = did_estimate(setup_res_mt$Y, setup_res_mt$N0, setup_res_mt$T0)
std_synthdid_res_mt <- sqrt(unlist(vcov(tau.hat_res_mt, method = "placebo")))
std_synthcont_res_mt <- sqrt(unlist(vcov(tau.sc_res_mt, method = "placebo")))
std_did_res_mt <- sqrt(unlist(vcov(tau.did_res_mt, method = "placebo")))
est_synthdid_res_mt <- unlist(tau.hat_res_mt)
est_synthcont_res_mt <- unlist(tau.sc_res_mt)
est_did_res_mt <- unlist(tau.did_res_mt)
p_synthdid_res_mt <- pnorm(est_synthdid_res_mt, 0, std_synthdid_res_mt)
p_synthcont_res_mt <- pnorm(est_synthcont_res_mt, 0, std_synthcont_res_mt)
p_did_res_mt <- pnorm(est_did_res_mt, 0, std_did_res_mt)

#Now doing it again with city gate price instead, for Montana
gateprice_w_treat_mt <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "MT", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "MT" | is.na(`Year of Legality`))
setup_gate_mt <- panel.matrices(gateprice_w_treat_mt, time = 5)
tau.hat_gate_mt <- synthdid_estimate(setup_gate_mt$Y, setup_gate_mt$N0, setup_gate_mt$T0)
png("Output/synthdid_plot_Montana.png")
plot(tau.hat_gate_mt, se.method = 'placebo', overlay = 1)
dev.off()
std_synthdid_gate_mt <- sqrt(unlist(vcov(tau.hat_gate_mt, method = "placebo")))
est_synthdid_gate_mt <- unlist(tau.hat_gate_mt)
p_synthdid_gate_mt <- pnorm(est_synthdid_gate_mt, 0, std_synthdid_gate_mt)
tau.sc_gate_mt <- sc_estimate(setup_gate_mt$Y, setup_gate_mt$N0, setup_gate_mt$T0)
tau.did_gate_mt <- did_estimate(setup_gate_mt$Y, setup_gate_mt$N0, setup_gate_mt$T0)
std_sc_gate_mt <- sqrt(unlist(vcov(tau.sc_gate_mt, method = "placebo")))
std_did_gate_mt <- sqrt(unlist(vcov(tau.did_gate_mt, method = "placebo")))
est_sc_gate_mt <- unlist(tau.sc_gate_mt)
est_did_gate_mt <- unlist(tau.did_gate_mt)
p_sc_gate_mt <- pnorm(est_sc_gate_mt, 0, std_sc_gate_mt)
p_did_gate_mt <- pnorm(est_did_gate_mt, 0, std_did_gate_mt)

synthdid_placebo_plot(tau.hat_gate_mt, treated.fraction = .2)

#residential for Colorado (1999)
resprice_w_treat_co <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Colorado", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Colorado" | is.na(`Year of Legality`))
setup_res_co <- panel.matrices(resprice_w_treat_co, time = 5)
tau.hat_res_co = synthdid_estimate(setup_res_co$Y, setup_res_co$N0, setup_res_co$T0)
std_res_co <- unlist(sqrt(unlist(vcov(tau.hat_res_co, method = "placebo"))))
est_res_co <- unlist(tau.hat_res_co)
p_res_co <- pnorm(est_res_co, 0, std_res_co)

#residential for Florida (2002)
resprice_w_treat_fl <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Florida", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Florida" | is.na(`Year of Legality`))
setup_res_fl <- panel.matrices(resprice_w_treat_fl, time = 5)
tau.hat_res_fl = synthdid_estimate(setup_res_fl$Y, setup_res_fl$N0, setup_res_fl$T0)
std_res_fl <- unlist(sqrt(unlist(vcov(tau.hat_res_fl, method = "placebo"))))
est_res_fl <- unlist(tau.hat_res_fl)
p_res_fl <- pnorm(est_res_fl, 0, std_res_fl)

#residential for Georgia (1997)
resprice_w_treat_ga <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Georgia", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Georgia" | is.na(`Year of Legality`))
setup_res_ga <- panel.matrices(resprice_w_treat_ga, time = 5)
tau.hat_res_ga = synthdid_estimate(setup_res_ga$Y, setup_res_ga$N0, setup_res_ga$T0)
std_res_ga <- unlist(sqrt(unlist(vcov(tau.hat_res_ga, method = "placebo"))))
est_res_ga <- unlist(tau.hat_res_ga)
p_res_ga <- pnorm(est_res_ga, 0, std_res_ga)

#residential for Illinois (1996)
resprice_w_treat_il <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Illinois", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Illinois" | is.na(`Year of Legality`))
setup_res_il <- panel.matrices(resprice_w_treat_il, time = 5)
tau.hat_res_il = synthdid_estimate(setup_res_il$Y, setup_res_il$N0, setup_res_il$T0)
std_res_il <- unlist(sqrt(unlist(vcov(tau.hat_res_il, method = "placebo"))))
est_res_il <- unlist(tau.hat_res_il)
p_res_il <- pnorm(est_res_il, 0, std_res_il)

#residential for Indiana (1995)
resprice_w_treat_in <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Indiana", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Indiana" | is.na(`Year of Legality`))
setup_res_in <- panel.matrices(resprice_w_treat_in, time = 5)
tau.hat_res_in = synthdid_estimate(setup_res_in$Y, setup_res_in$N0, setup_res_in$T0)
std_res_in <- unlist(sqrt(unlist(vcov(tau.hat_res_in, method = "placebo"))))
est_res_in <- unlist(tau.hat_res_in)
p_res_in <- pnorm(est_res_in, 0, std_res_in)

#residential for Iowa (1999)
resprice_w_treat_ia <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Iowa", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Iowa" | is.na(`Year of Legality`))
setup_res_ia <- panel.matrices(resprice_w_treat_ia, time = 5)
tau.hat_res_ia = synthdid_estimate(setup_res_ia$Y, setup_res_ia$N0, setup_res_ia$T0)
std_res_ia <- unlist(sqrt(unlist(vcov(tau.hat_res_ia, method = "placebo"))))
est_res_ia <- unlist(tau.hat_res_ia)
p_res_ia <- pnorm(est_res_ia, 0, std_res_ia)

#residential for Kentucky (2000)
resprice_w_treat_ky <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Kentucky", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Kentucky" | is.na(`Year of Legality`))
setup_res_ky <- panel.matrices(resprice_w_treat_ky, time = 5)
tau.hat_res_ky = synthdid_estimate(setup_res_ky$Y, setup_res_ky$N0, setup_res_ky$T0)
std_res_ky <- unlist(sqrt(unlist(vcov(tau.hat_res_ky, method = "placebo"))))
est_res_ky <- unlist(tau.hat_res_ky)
p_res_ky <- pnorm(est_res_ky, 0, std_res_ky)

#residential for Maryland (1995)
resprice_w_treat_md <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Maryland", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Maryland" | is.na(`Year of Legality`))
setup_res_md <- panel.matrices(resprice_w_treat_md, time = 5)
tau.hat_res_md = synthdid_estimate(setup_res_md$Y, setup_res_md$N0, setup_res_md$T0)
std_res_md <- unlist(sqrt(unlist(vcov(tau.hat_res_md, method = "placebo"))))
est_res_md <- unlist(tau.hat_res_md)
p_res_md <- pnorm(est_res_md, 0, std_res_md)

#residential for Massachusetts (1999)
resprice_w_treat_ma <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Massachusetts", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Massachusetts" | is.na(`Year of Legality`))
setup_res_ma <- panel.matrices(resprice_w_treat_ma, time = 5)
tau.hat_res_ma = synthdid_estimate(setup_res_ma$Y, setup_res_ma$N0, setup_res_ma$T0)
std_res_ma <- unlist(sqrt(unlist(vcov(tau.hat_res_ma, method = "placebo"))))
est_res_ma <- unlist(tau.hat_res_ma)
p_res_ma <- pnorm(est_res_ma, 0, std_res_ma)

#residential for Michigan (2002)
resprice_w_treat_mi <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Michigan", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Michigan" | is.na(`Year of Legality`))
setup_res_mi <- panel.matrices(resprice_w_treat_mi, time = 5)
tau.hat_res_mi = synthdid_estimate(setup_res_mi$Y, setup_res_mi$N0, setup_res_mi$T0)
std_res_mi <- unlist(sqrt(unlist(vcov(tau.hat_res_mi, method = "placebo"))))
est_res_mi <- unlist(tau.hat_res_mi)
p_res_mi <- pnorm(est_res_mi, 0, std_res_mi)

#residential for Nebraska (1998)
resprice_w_treat_ne <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Nebraska", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Nebraska" | is.na(`Year of Legality`))
setup_res_ne <- panel.matrices(resprice_w_treat_ne, time = 5)
tau.hat_res_ne = synthdid_estimate(setup_res_ne$Y, setup_res_ne$N0, setup_res_ne$T0)
std_res_ne <- unlist(sqrt(unlist(vcov(tau.hat_res_ne, method = "placebo"))))
est_res_ne <- unlist(tau.hat_res_ne)
p_res_ne <- pnorm(est_res_ne, 0, std_res_ne)

#residential for New Jersey (1999)
resprice_w_treat_nj <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "New Jersey", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "New Jersey" | is.na(`Year of Legality`))
setup_res_nj <- panel.matrices(resprice_w_treat_nj, time = 5)
tau.hat_res_nj = synthdid_estimate(setup_res_nj$Y, setup_res_nj$N0, setup_res_nj$T0)
std_res_nj <- unlist(sqrt(unlist(vcov(tau.hat_res_nj, method = "placebo"))))
est_res_nj <- unlist(tau.hat_res_nj)
p_res_nj <- pnorm(est_res_nj, 0, std_res_nj)
plot(tau.hat_res_nj, se.method = 'placebo', overlay = 1)

#residential for New York (1996)
resprice_w_treat_ny <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "New York", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "New York" | is.na(`Year of Legality`))
setup_res_ny <- panel.matrices(resprice_w_treat_ny, time = 5)
tau.hat_res_ny = synthdid_estimate(setup_res_ny$Y, setup_res_ny$N0, setup_res_ny$T0)
std_res_ny <- unlist(sqrt(unlist(vcov(tau.hat_res_ny, method = "placebo"))))
est_res_ny <- unlist(tau.hat_res_ny)
p_res_ny <- pnorm(est_res_ny, 0, std_res_ny)

#residential for Ohio (1997)
resprice_w_treat_oh <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Ohio", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Ohio" | is.na(`Year of Legality`))
setup_res_oh <- panel.matrices(resprice_w_treat_oh, time = 5)
tau.hat_res_oh = synthdid_estimate(setup_res_oh$Y, setup_res_oh$N0, setup_res_oh$T0)
std_res_oh <- unlist(sqrt(unlist(vcov(tau.hat_res_oh, method = "placebo"))))
est_res_oh <- unlist(tau.hat_res_oh)
p_res_oh <- pnorm(est_res_oh, 0, std_res_oh)

#residential for Pennsylvania (1999)
resprice_w_treat_pa <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Pennsylvania", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Pennsylvania" | is.na(`Year of Legality`))
setup_res_pa <- panel.matrices(resprice_w_treat_pa, time = 5)
tau.hat_res_pa = synthdid_estimate(setup_res_pa$Y, setup_res_pa$N0, setup_res_pa$T0)
std_res_pa <- unlist(sqrt(unlist(vcov(tau.hat_res_pa, method = "placebo"))))
est_res_pa <- unlist(tau.hat_res_pa)
p_res_pa <- pnorm(est_res_pa, 0, std_res_pa)

#residential for Virginia (1999)
resprice_w_treat_va <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Virginia", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Virginia" | is.na(`Year of Legality`))
setup_res_va <- panel.matrices(resprice_w_treat_va, time = 5)
tau.hat_res_va = synthdid_estimate(setup_res_va$Y, setup_res_va$N0, setup_res_va$T0)
std_res_va <- unlist(sqrt(unlist(vcov(tau.hat_res_va, method = "placebo"))))
est_res_va <- unlist(tau.hat_res_va)
p_res_va <- pnorm(est_res_va, 0, std_res_va)

#residential for Wyoming (1996)
resprice_w_treat_wy <- merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "Wyoming", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Price_i", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "Wyoming" | is.na(`Year of Legality`))
setup_res_wy <- panel.matrices(resprice_w_treat_wy, time = 5)
tau.hat_res_wy = synthdid_estimate(setup_res_wy$Y, setup_res_wy$N0, setup_res_wy$T0)
std_res_wy <- unlist(sqrt(unlist(vcov(tau.hat_res_wy, method = "placebo"))))
est_res_wy <- unlist(tau.hat_res_wy)
p_res_wy <- pnorm(est_res_wy, 0, std_res_wy)

#putting all the residentials in a matrix
res_est <- matrix(data = c(est_res_co, std_res_co, p_res_co, est_res_fl, std_res_fl, p_res_fl, est_res_ga, std_res_ga, p_res_ga,
                  est_res_ia, std_res_ia, p_res_ia, est_res_il, std_res_il, p_res_il, est_res_in, std_res_in, p_res_in,
                  est_res_ky, std_res_ky, p_res_ky, est_res_ma, std_res_ma, p_res_ma, est_res_md, std_res_md, p_res_md,
                  est_res_mi, std_res_mi, p_res_mi, est_res_ne, std_res_ne, p_res_ne, est_res_nj, std_res_nj, p_res_nj,
                  est_res_ny, std_res_ny, p_res_ny, est_res_oh, std_res_oh, p_res_oh, est_res_pa, std_res_pa, p_res_pa,
                  est_res_va, std_res_va, p_res_va, est_res_wy, std_res_wy, p_res_wy, est_synthdid_res_mt, std_synthdid_res_mt, 
                  p_synthdid_res_mt), nrow = 18, ncol = 3, byrow = TRUE)
colnames(res_est) <- c("Estimate", "Std", "Pvalue")
res_est <- as.data.frame(res_est)
res_est_avg <- mean(res_est$Estimate)
res_est_se <- sqrt(sum((res_est$Std)^2))/nrow(res_est)
res_est_p <- pnorm(res_est_avg, 0, res_est_se)

#putting the dataset in the form necessary for a staggered DiD model
resprice_treatment <-  merge(resprice, choice_by_state, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(`Year of Legality` = paste0(`Year of Legality`, "-01-14")) |>
  mutate(Month_short = str_sub(Month, 1, 10)) |>
  mutate(Month_legal = ifelse(`Year of Legality` == Month_short, numMonth, 0)) |>
  select("State", "Month_legal") |>
  unique() |>
  mutate(Month_legal = ifelse(Month_legal > 60, Month_legal, 0)) |>
  filter(Month_legal > 0)
resprice_w_did <- merge(resprice, resprice_treatment, by = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(Month_legal = ifelse(is.na(Month_legal), 0, Month_legal)) |>
  filter(numMonth < 200) |>
  mutate(Month_of_year = as.character(str_sub(Month, 6, 7)))

#generating a staggered DiD model using the staggered package
did_all <- att_gt(yname = "Price_i", tname = "numMonth", idname = "STATEFP", gname = "Month_legal", data = resprice_w_did)
summary(did_all)
ggdid(did_all)

#doing a simple aggregation
simple_agg_did <- aggte(did_all, type = "simple")

#aggregating that model
aggregated_did <- aggte(did_all, type = "dynamic")
summary(aggregated_did)
png("Output/did_results.png", width = 800, height = 400)
ggdid(aggregated_did) + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
dev.off()

#aggregating, but now by group
grouped_did <- aggte(did_all)
summary(grouped_did)

#and now aggregating by time
alltime_did <-  aggte(did_all, type = "calendar")
summary(alltime_did)

#citygate model for Colorado (1999)
gateprice_w_treat_co <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "CO", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "CO" | is.na(`Year of Legality`))
setup_gate_co <- panel.matrices(gateprice_w_treat_co, time = 5)
tau.hat_gate_co <- synthdid_estimate(setup_gate_co$Y, setup_gate_co$N0, setup_gate_co$T0)
std_synthdid_gate_co <- sqrt(unlist(vcov(tau.hat_gate_co, method = "placebo")))
est_synthdid_gate_co <- unlist(tau.hat_gate_co)
p_synthdid_gate_co <- pnorm(est_synthdid_gate_co, 0, std_synthdid_gate_co)

#Citygate model for Florida (2002)
gateprice_w_treat_fl <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "FL", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "FL" | is.na(`Year of Legality`))
setup_gate_fl <- panel.matrices(gateprice_w_treat_fl, time = 5)
tau.hat_gate_fl <- synthdid_estimate(setup_gate_fl$Y, setup_gate_fl$N0, setup_gate_fl$T0)
std_synthdid_gate_fl <- sqrt(unlist(vcov(tau.hat_gate_fl, method = "placebo")))
est_synthdid_gate_fl <- unlist(tau.hat_gate_fl)
p_synthdid_gate_fl <- pnorm(est_synthdid_gate_fl, 0, std_synthdid_gate_fl)

#Citygate model for Georgia (1997)
gateprice_w_treat_ga <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "GA", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "GA" | is.na(`Year of Legality`))
setup_gate_ga <- panel.matrices(gateprice_w_treat_ga, time = 5)
tau.hat_gate_ga <- synthdid_estimate(setup_gate_ga$Y, setup_gate_ga$N0, setup_gate_ga$T0)
std_synthdid_gate_ga <- sqrt(unlist(vcov(tau.hat_gate_ga, method = "placebo")))
est_synthdid_gate_ga <- unlist(tau.hat_gate_ga)
p_synthdid_gate_ga <- pnorm(est_synthdid_gate_ga, 0, std_synthdid_gate_ga)

#Citygate model for Illinois (1996)
gateprice_w_treat_il <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "IL", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "IL" | is.na(`Year of Legality`))
setup_gate_il <- panel.matrices(gateprice_w_treat_il, time = 5)
tau.hat_gate_il <- synthdid_estimate(setup_gate_il$Y, setup_gate_il$N0, setup_gate_il$T0)
std_synthdid_gate_il <- sqrt(unlist(vcov(tau.hat_gate_il, method = "placebo")))
est_synthdid_gate_il <- unlist(tau.hat_gate_il)
p_synthdid_gate_il <- pnorm(est_synthdid_gate_il, 0, std_synthdid_gate_il)

#Citygate model for Indiana (1995)
gateprice_w_treat_in <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "IN", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "IN" | is.na(`Year of Legality`))
setup_gate_in <- panel.matrices(gateprice_w_treat_in, time = 5)
tau.hat_gate_in <- synthdid_estimate(setup_gate_in$Y, setup_gate_in$N0, setup_gate_in$T0)
std_synthdid_gate_in <- sqrt(unlist(vcov(tau.hat_gate_in, method = "placebo")))
est_synthdid_gate_in <- unlist(tau.hat_gate_in)
p_synthdid_gate_in <- pnorm(est_synthdid_gate_in, 0, std_synthdid_gate_in)

#Citygate model for Iowa (1999)
gateprice_w_treat_ia <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "IA", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "IA" | is.na(`Year of Legality`))
setup_gate_ia <- panel.matrices(gateprice_w_treat_ia, time = 5)
tau.hat_gate_ia <- synthdid_estimate(setup_gate_ia$Y, setup_gate_ia$N0, setup_gate_ia$T0)
std_synthdid_gate_ia <- sqrt(unlist(vcov(tau.hat_gate_ia, method = "placebo")))
est_synthdid_gate_ia <- unlist(tau.hat_gate_ia)
p_synthdid_gate_ia <- pnorm(est_synthdid_gate_ia, 0, std_synthdid_gate_ia)

#Citygate model for Kentucky (2000)
gateprice_w_treat_ky <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "KY", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "KY" | is.na(`Year of Legality`))
setup_gate_ky <- panel.matrices(gateprice_w_treat_ky, time = 5)
tau.hat_gate_ky <- synthdid_estimate(setup_gate_ky$Y, setup_gate_ky$N0, setup_gate_ky$T0)
std_synthdid_gate_ky <- sqrt(unlist(vcov(tau.hat_gate_ky, method = "placebo")))
est_synthdid_gate_ky <- unlist(tau.hat_gate_ky)
p_synthdid_gate_ky <- pnorm(est_synthdid_gate_ky, 0, std_synthdid_gate_ky)

#Citygate model for Maryland (1995)
gateprice_w_treat_md <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "MD", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "MD" | is.na(`Year of Legality`))
setup_gate_md <- panel.matrices(gateprice_w_treat_md, time = 5)
tau.hat_gate_md <- synthdid_estimate(setup_gate_md$Y, setup_gate_md$N0, setup_gate_md$T0)
std_synthdid_gate_md <- sqrt(unlist(vcov(tau.hat_gate_md, method = "placebo")))
est_synthdid_gate_md <- unlist(tau.hat_gate_md)
p_synthdid_gate_md <- pnorm(est_synthdid_gate_md, 0, std_synthdid_gate_md)

#Citygate model for Massachusetts (1999)
gateprice_w_treat_ma <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "MA", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "MA" | is.na(`Year of Legality`))
setup_gate_ma <- panel.matrices(gateprice_w_treat_ma, time = 5)
tau.hat_gate_ma <- synthdid_estimate(setup_gate_ma$Y, setup_gate_ma$N0, setup_gate_ma$T0)
std_synthdid_gate_ma <- sqrt(unlist(vcov(tau.hat_gate_ma, method = "placebo")))
est_synthdid_gate_ma <- unlist(tau.hat_gate_ma)
p_synthdid_gate_ma <- pnorm(est_synthdid_gate_ma, 0, std_synthdid_gate_ma)

#Citygate model for Michigan (2002)
gateprice_w_treat_mi <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "MI", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "MI" | is.na(`Year of Legality`))
setup_gate_mi <- panel.matrices(gateprice_w_treat_mi, time = 5)
tau.hat_gate_mi <- synthdid_estimate(setup_gate_mi$Y, setup_gate_mi$N0, setup_gate_mi$T0)
std_synthdid_gate_mi <- sqrt(unlist(vcov(tau.hat_gate_mi, method = "placebo")))
est_synthdid_gate_mi <- unlist(tau.hat_gate_mi)
p_synthdid_gate_mi <- pnorm(est_synthdid_gate_mi, 0, std_synthdid_gate_mi)

#Citygate model for Nebraska (1998)
gateprice_w_treat_ne <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "NE", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "NE" | is.na(`Year of Legality`))
setup_gate_ne <- panel.matrices(gateprice_w_treat_ne, time = 5)
tau.hat_gate_ne <- synthdid_estimate(setup_gate_ne$Y, setup_gate_ne$N0, setup_gate_ne$T0)
std_synthdid_gate_ne <- sqrt(unlist(vcov(tau.hat_gate_ne, method = "placebo")))
est_synthdid_gate_ne <- unlist(tau.hat_gate_ne)
p_synthdid_gate_ne <- pnorm(est_synthdid_gate_ne, 0, std_synthdid_gate_ne)

#Citygate model for New Jersey (1999)
gateprice_w_treat_nj <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "NJ", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "NJ" | is.na(`Year of Legality`))
setup_gate_nj <- panel.matrices(gateprice_w_treat_nj, time = 5)
tau.hat_gate_nj <- synthdid_estimate(setup_gate_nj$Y, setup_gate_nj$N0, setup_gate_nj$T0)
std_synthdid_gate_nj <- sqrt(unlist(vcov(tau.hat_gate_nj, method = "placebo")))
est_synthdid_gate_nj <- unlist(tau.hat_gate_nj)
p_synthdid_gate_nj <- pnorm(est_synthdid_gate_nj, 0, std_synthdid_gate_nj)

#Citygate model for New York (1996)
gateprice_w_treat_ny <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "NY", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "NY" | is.na(`Year of Legality`))
setup_gate_ny <- panel.matrices(gateprice_w_treat_ny, time = 5)
tau.hat_gate_ny <- synthdid_estimate(setup_gate_ny$Y, setup_gate_ny$N0, setup_gate_ny$T0)
std_synthdid_gate_ny <- sqrt(unlist(vcov(tau.hat_gate_ny, method = "placebo")))
est_synthdid_gate_ny <- unlist(tau.hat_gate_ny)
p_synthdid_gate_ny <- pnorm(est_synthdid_gate_ny, 0, std_synthdid_gate_ny)

#Citygate model for Ohio (1997)
gateprice_w_treat_oh <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "OH", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "OH" | is.na(`Year of Legality`))
setup_gate_oh <- panel.matrices(gateprice_w_treat_oh, time = 5)
tau.hat_gate_oh <- synthdid_estimate(setup_gate_oh$Y, setup_gate_oh$N0, setup_gate_oh$T0)
std_synthdid_gate_oh <- sqrt(unlist(vcov(tau.hat_gate_oh, method = "placebo")))
est_synthdid_gate_oh <- unlist(tau.hat_gate_oh)
p_synthdid_gate_oh <- pnorm(est_synthdid_gate_oh, 0, std_synthdid_gate_oh)

#Citygate model for Pennsylvania (1999)
gateprice_w_treat_pa <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "PA", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "PA" | is.na(`Year of Legality`))
setup_gate_pa <- panel.matrices(gateprice_w_treat_pa, time = 5)
tau.hat_gate_pa <- synthdid_estimate(setup_gate_pa$Y, setup_gate_pa$N0, setup_gate_pa$T0)
std_synthdid_gate_pa <- sqrt(unlist(vcov(tau.hat_gate_pa, method = "placebo")))
est_synthdid_gate_pa <- unlist(tau.hat_gate_pa)
p_synthdid_gate_pa <- pnorm(est_synthdid_gate_pa, 0, std_synthdid_gate_pa)

#Citygate model for Virginia (1999)
gateprice_w_treat_va <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "VA", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "VA" | is.na(`Year of Legality`))
setup_gate_va <- panel.matrices(gateprice_w_treat_va, time = 5)
tau.hat_gate_va <- synthdid_estimate(setup_gate_va$Y, setup_gate_va$N0, setup_gate_va$T0)
std_synthdid_gate_va <- sqrt(unlist(vcov(tau.hat_gate_va, method = "placebo")))
est_synthdid_gate_va <- unlist(tau.hat_gate_va)
p_synthdid_gate_va <- pnorm(est_synthdid_gate_va, 0, std_synthdid_gate_va)

#Citygate model for Wyoming (1996)
gateprice_w_treat_wy <- merge(citygate_pivot_small, choice_by_state, by.x = "State", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  mutate(year = str_sub(Month, 1, 4)) |>
  mutate(treated = ifelse(year >= `Year of Legality` & State == "WY", 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated)) |>
  mutate(treated = as.integer(treated)) |>
  select(c("State", "Month", "Citygate_interp", "treated", "numMonth", "Year of Legality")) |>
  filter(numMonth < 200) |>
  filter(State == "WY" | is.na(`Year of Legality`))
setup_gate_wy <- panel.matrices(gateprice_w_treat_wy, time = 5)
tau.hat_gate_wy <- synthdid_estimate(setup_gate_wy$Y, setup_gate_wy$N0, setup_gate_wy$T0)
std_synthdid_gate_wy <- sqrt(unlist(vcov(tau.hat_gate_wy, method = "placebo")))
est_synthdid_gate_wy <- unlist(tau.hat_gate_wy)
p_synthdid_gate_wy <- pnorm(est_synthdid_gate_wy, 0, std_synthdid_gate_wy)

#putting all the results into a matrix
gate_est <- matrix(data = c(est_synthdid_gate_co, std_synthdid_gate_co, p_synthdid_gate_co, est_synthdid_gate_fl, std_synthdid_gate_fl, 
                p_synthdid_gate_fl, est_synthdid_gate_ga, std_synthdid_gate_ga, p_synthdid_gate_ga, est_synthdid_gate_ia, 
                std_synthdid_gate_ia, p_synthdid_gate_ia, est_synthdid_gate_il, std_synthdid_gate_il, p_synthdid_gate_il, 
                est_synthdid_gate_in, std_synthdid_gate_in, p_synthdid_gate_in, est_synthdid_gate_ky, std_synthdid_gate_ky, 
                p_synthdid_gate_ky, est_synthdid_gate_ma, std_synthdid_gate_ma, p_synthdid_gate_ma, est_synthdid_gate_md, 
                std_synthdid_gate_md, p_synthdid_gate_md, est_synthdid_gate_mi, std_synthdid_gate_mi, p_synthdid_gate_mi, 
                est_synthdid_gate_ne, std_synthdid_gate_ne, p_synthdid_gate_ne, est_synthdid_gate_nj, std_synthdid_gate_nj, 
                p_synthdid_gate_nj, est_synthdid_gate_ny, std_synthdid_gate_ny, p_synthdid_gate_ny, est_synthdid_gate_oh, 
                std_synthdid_gate_oh, p_synthdid_gate_oh, est_synthdid_gate_pa, std_synthdid_gate_pa, p_synthdid_gate_pa,
                est_synthdid_gate_va, std_synthdid_gate_va, p_synthdid_gate_va, est_synthdid_gate_wy, std_synthdid_gate_wy, 
                p_synthdid_gate_wy, est_synthdid_gate_mt, std_synthdid_gate_mt, p_synthdid_gate_mt), nrow = 18, ncol = 3, 
       byrow = TRUE)
state_order <- c("CO", "FL", "GA", "IA", "IL", "IN", "KY", "MA", "MD", "MI", "NE", "NJ", "NY", "OH", "PA", "VA", "WY", "MT")
gate_est <- as.data.frame(cbind(gate_est, state_order)) |>
  select(c(Estimate = V1, `Standard Error` = V2,  State = state_order)) |>
  mutate(Estimate = as.numeric(Estimate), `Standard Error` = as.numeric(`Standard Error`))
res_est <- as.data.frame(cbind(res_est, state_order)) |>
  select(c(Estimate, `Standard Error` = Std, State = state_order))
gate_est_avg <- mean(gate_est$V1)
gate_est_se <- sqrt(sum((res_est$Std)^2))/nrow(res_est)
gate_est_p <- pnorm(gate_est_avg, 0, gate_est_se)
length(state_order)
#export the gate and res estimates
write.csv(res_est, "Output/res_est.csv")
write.csv(gate_est, "Output/gate_est.csv")
kable(res_est, format = "latex")
kable(gate_est, format = "latex")
