## AS Data Assembly and Calculations
## Phil Hoxie
## July 6, 2021

# Packages ----------------------------------------------------------------------

library(tidyverse)

# Data -------------------------------------------------------------------------

cbo_raw <- readr::read_csv(file = paste0(data_folder, "Raw/CBO/cbo_forecasts.csv"))
civlf_raw <- read.csv(file = paste0(data_folder, "Raw/BLS/state_civlf.csv"), stringsAsFactors = FALSE)
unemp_raw <- read.csv(file = paste0(data_folder, "Raw/BLS/state_unemp.csv"), stringsAsFactors = FALSE)

fips_codes <- read.csv(file = paste0(data_folder, "fips_codes.csv"), stringsAsFactors = FALSE)
budget_raw <- read.csv(file = paste0(data_folder, "state_budget_2017.csv"), stringsAsFactors = FALSE) # from Clemens and Veuger (2020)

# Bartik Estimates -------------------------------------------------------------

# Base for 2019
unemp2019 <- (cbo_raw$unemp_jul21[1] + cbo_raw$unemp_jul21[2] + cbo_raw$unemp_jul21[3] + cbo_raw$unemp_jul21[4]) / 4

quarterly_multiplier <- -(67000000000/4) # bartik's change in aggregate state revenue w/ 1% change in UR (45 billion on annualized basis for states, and 22 billion for local governments)

names(cbo_raw)

estimates <- cbo_raw %>% # base difference for applying the multiplier
  mutate(urdiff_may20 = unemp_may20 - unemp2019,
         urdiff_jul20 = unemp_jul20 - unemp2019,
         urdiff_feb21 = unemp_feb21 - unemp2019,
         urdiff_jul21 = unemp_jul21 - unemp2019,
         urdiff_may22 = unemp_may22 - unemp2019)

estimates <- estimates %>%
  mutate(bartik_pmt_may20 = urdiff_may20 * quarterly_multiplier,
         bartik_pmt_jul20 = urdiff_jul20 * quarterly_multiplier,
         bartik_pmt_feb21 = urdiff_feb21 * quarterly_multiplier,
         bartik_pmt_jul21 = urdiff_jul21 * quarterly_multiplier,
         bartik_pmt_may22 = urdiff_may22 * quarterly_multiplier)

# Clemens and Veuger Estimates -------------------------------------------------

# assumed in Clemens and Veuger (2020)
salestax_elasticity <- 1.1
inctax_elasticity <- 1.6

estimates <- estimates %>% #Index PCE and Inc to Q42019
  mutate(indx_pce_jan20 = pce_jan20 / pce_jan20[4],
         indx_pce_may20 = pce_may20 / pce_may20[4],
         indx_pce_jul20 = pce_jul20 / pce_jul20[4],
         indx_pce_feb21 = pce_feb21 / pce_feb21[4],
         indx_pce_jul21 = pce_jul21 / pce_jul21[4],
         indx_pce_may22 = pce_may22 / pce_may22[4],
         indx_inc_jan20 = pinc_jan20 / pinc_jan20[4],
         indx_inc_may20 = pinc_may20 / pinc_may20[4],
         indx_inc_jul20 = pinc_jul20 / pinc_jul20[4],
         indx_inc_feb21 = pinc_feb21 / pinc_feb21[4],
         indx_inc_jul21 = pinc_jul21 / pinc_jul21[4],
         indx_inc_may22 = pinc_may22 / pinc_may22[4])

estimates <- estimates %>% # adjust for economic impact payments (EIPs)
  mutate(pinc_adj_may20 = ifelse(Period == "2020Q2", pinc_may20 - 1200, pinc_may20),
         pinc_adj_jul20 = ifelse(Period == "2020Q2", pinc_jul20 - 1200, pinc_jul20),
         pinc_adj_feb21 = ifelse(Period == "2020Q2", pinc_feb21 - 1200,
                                 ifelse(Period == "2021Q1", pinc_feb21 - 696, pinc_feb21)),
         pinc_adj_jul21 = ifelse(Period == "2020Q2", pinc_jul21 - 1200,
                                 ifelse(Period == "2021Q1", pinc_jul21 - 696,
                                        ifelse(Period == "2021Q2", pinc_jul21 - 1564, pinc_jul21))),
         pinc_adj_may22 = ifelse(Period == "2020Q2", pinc_may22 - 1200,
                                 ifelse(Period == "2021Q1", pinc_may22 - 696,
                                        ifelse(Period == "2021Q2", pinc_may22 - 1564, pinc_may22))))

# check
estimates$pinc_jul21[6]
estimates$pinc_jul21[9]
estimates$pinc_jul21[10]
estimates$pinc_jul21[11]
estimates$pinc_adj_jul21[6] # EIP 1
estimates$pinc_adj_jul21[9] # EIP 2
estimates$pinc_adj_jul21[10] # EIP 3
estimates$pinc_adj_jul21[11] # no diff as a check

estimates$pinc_may22[6]
estimates$pinc_may22[9]
estimates$pinc_may22[10]
estimates$pinc_may22[11]
estimates$pinc_adj_may22[6] # EIP 1
estimates$pinc_adj_may22[9] # EIP 2
estimates$pinc_adj_may22[10] # EIP 3
estimates$pinc_adj_may22[11] # no diff as a check


estimates <- estimates %>% # index adjusted series w/out EIPs
  mutate(indx_incadj_may20 = pinc_adj_may20 / pinc_adj_may20[4],
         indx_incadj_jul20 = pinc_adj_jul20 / pinc_adj_jul20[4],
         indx_incadj_feb21 = pinc_adj_feb21 / pinc_adj_feb21[4],
         indx_incadj_jul21 = pinc_adj_jul21 / pinc_adj_jul21[4],
         indx_incadj_may22 = pinc_adj_may22 / pinc_adj_may22[4])

estimates <- estimates %>% # get diff from Jan20
  mutate(pce_short_may20 = indx_pce_may20 - indx_pce_jan20,
         pce_short_jul20 = indx_pce_jul20 - indx_pce_jan20,
         pce_short_feb21 = indx_pce_feb21 - indx_pce_jan20,
         pce_short_jul21 = indx_pce_jul21 - indx_pce_jan20,
         pce_short_may22 = indx_pce_may22 - indx_pce_jan20,
         inc_short_may20 = indx_incadj_may20 - indx_inc_jan20,
         inc_short_jul20 = indx_incadj_jul20 - indx_inc_jan20,
         inc_short_feb21 = indx_incadj_feb21 - indx_inc_jan20,
         inc_short_jul21 = indx_incadj_jul21 - indx_inc_jan20,
         inc_short_may22 = indx_incadj_may22 - indx_inc_jan20)

# state data
names(budget_raw)

state_data <- budget_raw %>%
  select(state_name, state_abbrv, pop17, percap_inctax, percap_sales, nominalgrowthq42016toq42019)

# loop for calcs over each state

shortfalls <- estimates %>%
  select(Period, pce_short_may20, pce_short_jul20, pce_short_feb21, pce_short_jul21, pce_short_may22, inc_short_may20, inc_short_jul20, inc_short_feb21, inc_short_jul21, inc_short_may22)

cv_state_calcs <- NULL

for(i in 1:50){ #loop

  growth <- state_data$nominalgrowthq42016toq42019[i]
  pop <- state_data$pop17[i]
  salestax_basepercap <- state_data$percap_sales[i]
  inctax_basepercap <- state_data$percap_inctax[i]

  state <- state_data$state_abbrv[i]

  cv_est <- shortfalls %>% # percap ests
    mutate(inctax_shortfall_percap_may20 = .25*inctax_elasticity*inctax_basepercap*growth*inc_short_may20,
           inctax_shortfall_percap_jul20 = .25*inctax_elasticity*inctax_basepercap*growth*inc_short_jul20,
           inctax_shortfall_percap_feb21 = .25*inctax_elasticity*inctax_basepercap*growth*inc_short_feb21,
           inctax_shortfall_percap_jul21 = .25*inctax_elasticity*inctax_basepercap*growth*inc_short_jul21,
           inctax_shortfall_percap_may22 = .25*inctax_elasticity*inctax_basepercap*growth*inc_short_may22,
           salestax_shortfall_percap_may20 = .25*salestax_elasticity*salestax_basepercap*growth*pce_short_may20,
           salestax_shortfall_percap_jul20 = .25*salestax_elasticity*salestax_basepercap*growth*pce_short_jul20,
           salestax_shortfall_percap_feb21 = .25*salestax_elasticity*salestax_basepercap*growth*pce_short_feb21,
           salestax_shortfall_percap_jul21 = .25*salestax_elasticity*salestax_basepercap*growth*pce_short_jul21,
           salestax_shortfall_percap_may22 = .25*salestax_elasticity*salestax_basepercap*growth*pce_short_may22)

  cv_est <- cv_est %>% # absolute estimates
    mutate(inctax_shortfall_may20 = inctax_shortfall_percap_may20 * pop,
           inctax_shortfall_jul20 = inctax_shortfall_percap_jul20 * pop,
           inctax_shortfall_feb21 = inctax_shortfall_percap_feb21 * pop,
           inctax_shortfall_jul21 = inctax_shortfall_percap_jul21 * pop,
           inctax_shortfall_may22 = inctax_shortfall_percap_may22 * pop,
           salestax_shortfall_may20 = salestax_shortfall_percap_may20 * pop,
           salestax_shortfall_jul20 = salestax_shortfall_percap_jul20 * pop,
           salestax_shortfall_feb21 = salestax_shortfall_percap_feb21 * pop,
           salestax_shortfall_jul21 = salestax_shortfall_percap_jul21 * pop,
           salestax_shortfall_may22 = salestax_shortfall_percap_may22 * pop)

  cv_est <- cv_est %>% # ID info
    mutate(state_abbrv = state,
           pop17 = pop)

  cv_state_calcs <- bind_rows(cv_state_calcs, cv_est)

  print(i / 50)
}

# sum by period

cv_tot_est <- cv_state_calcs %>%
  group_by(Period) %>%
  summarise(inctax_shortfall_may20 = sum(inctax_shortfall_may20),
            inctax_shortfall_jul20 = sum(inctax_shortfall_jul20),
            inctax_shortfall_feb21 = sum(inctax_shortfall_feb21),
            inctax_shortfall_jul21 = sum(inctax_shortfall_jul21),
            inctax_shortfall_may22 = sum(inctax_shortfall_may22),
            salestax_shortfall_may20 = sum(salestax_shortfall_may20),
            salestax_shortfall_jul20 = sum(salestax_shortfall_jul20),
            salestax_shortfall_feb21 = sum(salestax_shortfall_feb21),
            salestax_shortfall_jul21 = sum(salestax_shortfall_jul21),
            salestax_shortfall_may22 = sum(salestax_shortfall_may22))

cv_tot_est <- cv_tot_est %>%
  mutate(cvtax_shortfall_may20 = (inctax_shortfall_may20 + salestax_shortfall_may20)/0.614,
         cvtax_shortfall_jul20 = (inctax_shortfall_jul20 + salestax_shortfall_jul20)/0.614,
         cvtax_shortfall_feb21 = (inctax_shortfall_feb21 + salestax_shortfall_feb21)/0.614,
         cvtax_shortfall_jul21 = (inctax_shortfall_jul21 + salestax_shortfall_jul21)/0.614,
         cvtax_shortfall_may22 = (inctax_shortfall_may22 + salestax_shortfall_may22)/0.614)

# now scale up for state and local together (local component is just under 40% of state shortfall)
cv_tot_est <- cv_tot_est %>%
  mutate(cvtax_shortfall_may20 = (cvtax_shortfall_may20)*1.3964,
         cvtax_shortfall_jul20 = (cvtax_shortfall_jul20)*1.3964,
         cvtax_shortfall_feb21 = (cvtax_shortfall_feb21)*1.3964,
         cvtax_shortfall_jul21 = (cvtax_shortfall_jul21)*1.3964,
         cvtax_shortfall_may22 = (cvtax_shortfall_may22)*1.3964)

# Merge in CV estimates ---------------------------------------------------------

estimates <- left_join(estimates, cv_tot_est, by = "Period")
names(estimates)

# Graph differences -------------------------------------------------------------

graph <- estimates %>%
  select(Period, bartik_pmt_may20, bartik_pmt_jul20, bartik_pmt_feb21, bartik_pmt_jul21, cvtax_shortfall_may20, cvtax_shortfall_jul20, cvtax_shortfall_feb21, cvtax_shortfall_jul21)

order <- seq(1, 14, 1) %>% tbl_df() %>% rename(order = value)

graph <- bind_cols(order, graph) %>%
  filter(order > 4)

graph <- graph %>%
  gather(key = "series", "shortfall", 3:10)

graph <- graph %>% # categories
  mutate(paper = ifelse(str_detect(series, "bartik"), "Bartik", "Clemens and Veuger"),
         forecast = ifelse(str_detect(series, "may20"), "May 2020",
                                      ifelse(str_detect(series, "jul20"), "July 2020",
                                                        ifelse(str_detect(series, "feb21"), "February 2021", "July 2021"))))

graph <- graph %>% # units
  mutate(shrtfl_billions = shortfall / 1000000000,
         forecast_num = ifelse(forecast == "May 2020", 1,
                               ifelse(forecast == "July 2020", 2,
                                      ifelse(forecast == "February 2021", 3,
                                             ifelse(forecast == "July 2021", 4, NA)))))

comp_graph <- ggplot(data = graph, aes(x = order, y = shrtfl_billions, col = paper, linetype = as.factor(forecast_num)))+
  geom_line()+
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted"), labels = c("May 2020", "July 2020", "Feb. 2021", "July 2021"), name = "CBO Forecast")+
  scale_color_manual(values = c("#FB0023", "#008CCC"), labels = c("Bartik (2020)", "Clemens and Veuger (2020b)"), name = "Estimate")+
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14), labels = c("2020Q2", "2020Q4", "2021Q2", "2021Q4", "2022Q2"))+
  geom_hline(yintercept = 0)+
  xlab("")+
  ylab("Estimated Shortfall (Billions USD)")+
  theme_classic()+
  guides(color = guide_legend(order = 1),linetype = guide_legend(order = 2))+
  scale_y_continuous(breaks = c(0, -35, -70, -105, -140,-175,-210), labels = c("$0", "-$35", "-$70", "-$105", "-$140","-$175","-$210"))
comp_graph

# Save -----------------------------------------

ggsave(file = paste0(folder_output, "state quarterly comparison.png"), comp_graph, device = "png")



# Graph differences, May 2022 Update  -------------------------------------------------------------

graph <- estimates %>%
  select(Period, bartik_pmt_may20, bartik_pmt_feb21, bartik_pmt_may22, cvtax_shortfall_may20, cvtax_shortfall_feb21, cvtax_shortfall_may22)

order <- seq(1, 14, 1) %>% tbl_df() %>% rename(order = value)

graph <- bind_cols(order, graph) %>%
  filter(order > 4)

graph <- graph %>%
  gather(key = "series", "shortfall", 3:8)

graph <- graph %>% # categories
  mutate(paper = ifelse(str_detect(series, "bartik"), "Bartik", "Clemens and Veuger"),
         forecast = ifelse(str_detect(series, "may20"), "May 2020",
                           ifelse(str_detect(series, "jul20"), "July 2020",
                                  ifelse(str_detect(series, "feb21"), "February 2021", "May 2022 Actual"))))

graph <- graph %>% # units
  mutate(shrtfl_billions = shortfall / 1000000000,
         forecast_num = ifelse(forecast == "May 2020", 2,
                               ifelse(forecast == "July 2020", 4,
                                      ifelse(forecast == "February 2021", 3,
                                             ifelse(forecast == "July 2021", 5,
                                                    ifelse(forecast=="May 2022 Actual",1,NA))))))

comp_graph <- ggplot(data = graph, aes(x = order, y = shrtfl_billions, col = paper, linetype = as.factor(forecast_num)))+
  geom_line()+
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "dotted"), labels = c("May 2022 Actual", "May 2020", "Feb. 2021", "July 2021"), name = "CBO Forecast")+
  scale_color_manual(values = c("#FB0023", "#008CCC"), labels = c("Bartik (2020)", "Clemens and Veuger (2020b)"), name = "Estimate")+
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14), labels = c("2020Q2", "2020Q4", "2021Q2", "2021Q4", "2022Q2"))+
  geom_hline(yintercept = 0)+
  xlab("")+
  ylab("Estimated Shortfall (Billions USD)")+
  theme_classic()+
  guides(color = guide_legend(order = 1),linetype = guide_legend(order = 2))+
  scale_y_continuous(breaks = c(0, -35, -70, -105, -140,-175,-210), labels = c("$0", "-$35", "-$70", "-$105", "-$140","-$175","-$210"))
comp_graph

# Save -----------------------------------------

ggsave(file = paste0(folder_output, "state quarterly comparison_20220630.png"), comp_graph, device = "png",width=6,height=4,units="in")

