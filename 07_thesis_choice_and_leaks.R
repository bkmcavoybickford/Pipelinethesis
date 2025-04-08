#Adding in libraries
library(AER)
library(modelsummary)

#Importing the gate and res estimates so I don't have to run the giant bunches of code every time
res_est <- read_csv("Output/res_est.csv")
gate_est <- read_csv("Output/gate_est.csv")

#Extracting the leaks data from 1990 to 2003
for (i in 1990:2003) {
  name <- paste0("annual_", i)
  assign(name, read_excel(paste0("Data/Annual_Reports/annual_gas_distribution_", i, ".xlsx")))
}

#Putting together 1990-1992
annual_1990_1992 <- rbind(annual_1990, annual_1991, annual_1992) |>
  mutate(leaks_mains = CM + OM + TM + CDM + MDM + OTHM) |>
  mutate(leaks_services = CS + OS + TS + CDS + MDS + OTHS) |>
  select(STOP, leaks_mains, leaks_services, YR)

#Editing 1993
annual_1993 <- annual_1993 |>
  mutate(leaks_mains = CM1 + OM1 + TM1 + CDM1 + MDM1 + OTHM1) |>
  mutate(leaks_services = CS1 + OS1 + TS1 + CDS1 + MDS1 + OTHS1) |>
  select(STOP, leaks_mains, leaks_services, YR)

#Putting together 1994-1997
annual_1994_1997 <- rbind(annual_1994, annual_1995, annual_1996, annual_1997) |>
  mutate(leaks_mains = CM1 + OM1 + TM1 + CDM1 + MDM1 + OTHM1) |>
  mutate(leaks_services = CS1 + OS1 + TS1 + CDS1 + MDS1 + OTHS1) |>
  select(STOP, leaks_mains, leaks_services, YR)

#Editing 1998
annual_1998 <- annual_1998 |>
  mutate(leaks_mains = CM + OM + TM + CDM + MDM + OTHM) |>
  mutate(leaks_services = CS + OS + TS + CDS + MDS + OTHS) |>
  select(STOP, leaks_mains, leaks_services, YR)

#Editing 1999
annual_1999 <- annual_1999 |>
  mutate(leaks_mains = CM + OM + TM + CDM + MDM + OTHM) |>
  mutate(leaks_services = CS + OS + TS + CDS + MDS + OTHS) |>
  select(STOP, leaks_mains, leaks_services, YR)

#Putting together 2000-2003
annual_2000_2003 <- rbind(annual_2000, annual_2001, annual_2002, annual_2003) |>
  mutate(leaks_mains = CM + OM + TM + CDM + MDM + OTHM) |>
  mutate(leaks_services = CS + OS + TS + CDS + MDS + OTHS) |>
  select(STOP, leaks_mains, leaks_services, YR)

#Combining them all
annual_1990_2003 <- rbind(annual_1990_1992, annual_1993, annual_1994_1997, annual_1998, annual_1999, annual_2000_2003) |>
  mutate(leaks_mains = ifelse(is.na(leaks_mains), 0, leaks_mains)) |>
  mutate(leaks_services = ifelse(is.na(leaks_services), 0, leaks_services)) |>
  mutate(YR = ifelse(YR == 94, 1994, YR)) |>
  mutate(YR = ifelse(YR == 95, 1995, YR)) |>
  mutate(YR = ifelse(YR == 96, 1996, YR)) |>
  mutate(YR = ifelse(YR == 97, 1997, YR)) |>
  mutate(STOP = str_to_upper(STOP)) |>
  group_by(STOP, YR) |>
  summarize(leaks_mains = sum(leaks_mains), leaks_services = sum(leaks_services))

#Merging in the choice by year data and defining the treatment
annual_and_choice <- merge(annual_1990_2003, choice_by_state, by.x = "STOP", by.y = "State_abbr", all.x = TRUE, all.y = FALSE) |>
  filter(!is.na(State)) |>
  mutate(treated = ifelse(YR >= `Year of Legality` & `Year of Legality` > 1994, 1, 0)) |>
  mutate(treated = ifelse(is.na(treated), 0, treated))

#Merging in the residential estimate
annual_choice_res <- merge(annual_and_choice, res_est, by.x = "STOP", by.y = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(Estimate = ifelse(treated == 0, 0, Estimate)) |>
  filter(STOP != "AK" & STOP != "HI")

#Running an OLS model using the residential estimate as a variable [make this into a 2SLS]
choice_leaks_mains_res <- feols(leaks_mains ~ Estimate + treated|STOP + YR, data = annual_choice_res, vcov = "HC1")
summary(choice_leaks_mains_res)

#Merging in the citygate estimate
annual_choice_gate <- merge(annual_choice_res, gate_est, by.x = "STOP", by.y = "State", all.x = TRUE, all.y = FALSE) |>
  mutate(Estimate_gate = ifelse(treated == 0, 0, Estimate.y)) |>
  mutate(Std_res = ifelse(treated == 0, 0, Std)) |>
  mutate(Std_gate = ifelse(treated == 0, 0,`Standard Error`)) |>
  filter(STOP != "AK" & STOP != "HI")

#And adding in the citygate price also
citygate_annual_2 <- read_excel("NG_PRI_SUM_A_EPG0_PG1_DMCF_A.xls", 
                              sheet = "Data 1", skip = 2) |>
  pivot_longer(cols = !Date, names_to = "State", values_to = "citygate")
citygate_annual_2$State <- str_split_i(citygate_annual_2$State, " in ", 2)
citygate_annual_2$State <- str_split_i(citygate_annual_2$State, " \\(Doll", 1)
citygate_annual_2$Year <- str_sub(citygate_annual_2$Date, 1, 4)
citygate_annual_2 <- citygate_annual_2 |>
  filter(Year > 1988 & Year < 2005)
annual_priced <- merge(annual_choice_gate, citygate_annual_2, by.x = c("State", "YR"), by.y = c("State", "Year"), all.x = TRUE, all.y = FALSE)

#Running an OLS model using the citygate estimate as a variable
choice_leaks_mains_gate <- feols(leaks_mains ~ Estimate_gate|STOP + YR, data = annual_priced, vcov = "HC1")
summary(choice_leaks_mains_gate)
choice_mains_gate_direct <- feols(leaks_mains ~ citygate|STOP + YR, data = annual_priced, vcov = "HC1")
summary(choice_mains_gate_direct)

#Now turning that into a 2SLS instead
choice_mains_gate_iv <- ivreg(leaks_mains ~ citygate + STOP + as.factor(YR) | Estimate_gate + STOP + as.factor(YR), data = annual_priced)
summary(choice_mains_gate_iv, diagnostics = TRUE)

#And running it on services
choice_services_gate_iv <- ivreg(leaks_services ~ citygate + STOP + as.factor(YR) | Estimate_gate + STOP + as.factor(YR), data = annual_priced)
summary(choice_services_gate_iv, diagnostics = TRUE)
choice_services_gate_direct <- feols(leaks_services ~ citygate | STOP + YR, data = annual_priced, vcov = "HC1")
summary(choice_services_gate_direct)
choice_services_gate_est <- feols(leaks_services ~ Estimate_gate | STOP + YR, data = annual_priced, vcov = "HC1")
summary(choice_services_gate_est)

#Exporting the choice state-level data 
choice_by_state_trimmed <- choice_by_state[,1:5]
kable(choice_by_state_trimmed, format = "latex")
