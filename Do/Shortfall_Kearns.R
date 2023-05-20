## AS - Kearns Extension
## John Kearns
## 12/22/2022

library(dynlm)

# Clemens and Veuger Estimates -------------------------------------------------

# assumed in Clemens and Veuger (2020)
salestax_elasticity <- 1.1
inctax_elasticity <- 1.6

# forecast growth in state gdp growth given the predicted series using ARIMA
armax_precovid = read_csv(paste0(data_folder,"Final/armax_precovid.csv"))
armax_postcovid = read_csv(paste0(data_folder,"Final/armax_postcovid.csv"))

armax_preds = bind_rows(armax_precovid,armax_postcovid)

dynlm((armax_preds %>% filter(state=="NJ"&date<"2020-01-01"))$rgsp_yoy_pchange~L((armax_preds %>% filter(state=="NJ"&date<"2020-01-01"))$predicted_mean,1:2))

d.arima <- stats::arima((armax_preds %>% filter(state=="AL"&date<="2020-04-01"))$predicted_mean,order=c(1,0,0))
autoplot(forecast::forecast(d.arima, h = 8))

forecasts = data.frame()
for(stat in state.abb){
  
  d.arima <- stats::arima((armax_preds %>% filter(state==stat&date<="2020-04-01"))$predicted_mean,order=c(1,0,0))
  df1 = data.frame(forecast=as.data.frame(forecast::forecast(d.arima, h = 6))[,1],state=stat,date=seq.Date(as.Date("2020-07-01"),by="3 months",length.out=6))
  forecasts = bind_rows(forecasts,df1)
  
}

forecasts = bind_rows(forecasts,armax_preds %>% filter(date=="2020-01-01") %>% select(date,state,rgsp_yoy_pchange) %>% rename(forecast=rgsp_yoy_pchange))
forecasts = bind_rows(forecasts,armax_preds %>% filter(date=="2020-04-01") %>% select(date,state,predicted_mean) %>% rename(forecast=predicted_mean)) %>% 
  arrange(state,date)

# get state personal income at Q4 2019
pinc = read_csv(paste0(data_folder,"Raw/pinc_2019.csv"))
pinc$GeoName[3] = "Alaska"
pinc$GeoName[13] = "Hawaii"
pinc = pinc %>% 
  inner_join(data.frame(state_name=state.name,state=state.abb),by=c("GeoName"="state_name")) %>% 
  select(-c(GeoFips,GeoName)) %>% 
  pivot_longer(cols=`2018:Q1`:`2020:Q4`) %>% 
  mutate(date=as.Date(as.yearqtr(name,format="%Y:Q%q"))) %>% 
  full_join(forecasts) %>% 
  mutate(pinc_forecast=value,
         pinc_shortfall=value) %>% 
  arrange(state,date) %>% 
  group_by(state) %>% 
  mutate(growth=(value[date=="2019-10-01"]/value[date=="2018-01-01"])^(1/8)) %>% 
  ungroup()

for(i in 1:nrow(pinc)){
  
  pinc$pinc_forecast[i] = ifelse(pinc$date[i]>="2020-01-01",pinc$pinc_forecast[i-4]*(1+pinc$forecast[i]/100),pinc$pinc_forecast[i])
  pinc$pinc_shortfall[i] = ifelse(pinc$date[i]>="2020-01-01",pinc$pinc_shortfall[i-1]*pinc$growth[i],pinc$pinc_shortfall[i])
  
}

pinc = pinc %>% 
  group_by(state) %>% 
  mutate(pinc_forecast_indx=pinc_forecast/pinc_forecast[date=="2019-10-01"],
         pinc_shortfall_indx=pinc_shortfall/pinc_shortfall[date=='2019-10-01']) %>% 
  filter(date>="2020-01-01") 

shortfalls <- pinc %>% 
  mutate(Period = gsub(" ","",as.character(as.yearqtr(date)))) %>% 
  select(state,Period,pinc_forecast_indx,pinc_shortfall_indx) %>% 
  mutate(pinc_shortfall=pinc_forecast_indx-pinc_shortfall_indx)

cv_state_calcs <- NULL

for(stat in state.abb){ #loop
  
  growth <- state_data$nominalgrowthq42016toq42019[state_data$state_abbrv==stat]
  pop <- state_data$pop17[state_data$state_abbrv==stat]
  salestax_basepercap <- state_data$percap_sales[state_data$state_abbrv==stat]
  inctax_basepercap <- state_data$percap_inctax[state_data$state_abbrv==stat]
  
  state <- state_data$state_abbrv[state_data$state_abbrv==stat]
  
  cv_est <- shortfalls %>% # percap ests
    filter(state==stat) %>% 
    mutate(inctax_shortfall_percap = .25*inctax_elasticity*inctax_basepercap*growth*pinc_shortfall,
           salestax_shortfall_percap = .25*salestax_elasticity*salestax_basepercap*growth*pinc_shortfall)
  
  cv_est <- cv_est %>% # absolute estimates
    mutate(inctax_shortfall = inctax_shortfall_percap * pop,
           salestax_shortfall = salestax_shortfall_percap * pop)
  
  cv_est <- cv_est %>% # ID info
    mutate(state_abbrv = stat,
           pop17 = pop)
  
  cv_state_calcs <- bind_rows(cv_state_calcs, cv_est)
  
}

# sum by period

cv_tot_est <- cv_state_calcs %>%
  group_by(Period) %>%
  summarise(inctax_shortfall = sum(inctax_shortfall),
            salestax_shortfall = sum(salestax_shortfall))

cv_tot_est <- cv_tot_est %>%
  mutate(cvtax_shortfall = (inctax_shortfall + salestax_shortfall)/0.614)

# now scale up for state and local together (local component is just under 40% of state shortfall)
cv_tot_est <- cv_tot_est %>%
  mutate(cvtax_shortfall = (cvtax_shortfall)*1.3964)

graph_add = cv_tot_est %>% 
  select(Period,cvtax_shortfall) %>% 
  rename(shortfall=cvtax_shortfall) %>% 
  mutate(order=5:12,
         series="kearns_shortfall_jul20",
         paper="Kearns (2022)",
         forecst="July 2020",
         shrtfl_billions=shortfall/1000000000,
         forecast_num=4)

graph1 = bind_rows(graph,graph_add)

actual_shortfall = readxl::read_excel(paste0(data_folder,"Processing/sl_tax_rev.xlsx")) %>% 
  filter(year%in%c(2020:2021)) %>% 
  mutate(paper="actual_shortfall",
         series="actual_shortfall",
         shrtfl_billions=shortfall/1000,
         order=5:12)

graph2 = bind_rows(graph1,actual_shortfall)

comp_graph <- ggplot(data = graph2 %>% filter(series%in%c("kearns_shortfall_jul20","bartik_pmt_may20","cvtax_shortfall_may20","actual_shortfall")), aes(x = order, y = shrtfl_billions, col = paper))+
  geom_line()+
  scale_color_manual(values = c("black","#FB0023", "#008CCC","green"), labels = c("Actual Shortfall","Bartik (2020)", "Clemens and Veuger (2020b)","Kearns (2022)"), name = "Estimate")+
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14), labels = c("2020Q2", "2020Q4", "2021Q2", "2021Q4", "2022Q2"))+
  geom_hline(yintercept = 0)+
  xlab("")+
  ylab("Estimated Shortfall (Billions USD)")+
  theme_classic()+
  guides(color = guide_legend(order = 1),linetype = guide_legend(order = 2))+
  scale_y_continuous(breaks = c(0, -35, -70, -105, -140,-175,-210), labels = c("$0", "-$35", "-$70", "-$105", "-$140","-$175","-$210"))
comp_graph

comp_graph2 = ggplot(data = graph2 %>% 
         filter(series%in%c("kearns_shortfall_jul20","bartik_pmt_may20","cvtax_shortfall_may20","actual_shortfall")) %>% 
         group_by(series) %>% 
         mutate(shrtfl_cum = cumsum(shrtfl_billions)) %>% 
         ungroup(), aes(x = order, y = shrtfl_cum, col = paper))+
  geom_hline(yintercept = 0,color="black")+
  geom_line(size=2)+
  xlim(c(5,12)) +
  scale_color_manual(values = c("grey","green", "#008CCC","#FB0023"), labels = c("Actual Shortfall","Google Forecast", "National Income Forecast","Unemployment Forecast"), name = "Estimate")+
  scale_x_continuous(breaks = c(6, 8, 10, 12), labels = c("2020Q2", "2020Q4", "2021Q2", "2021Q4"))+
  xlab("")+
  ylab("Estimated Cumulative Shortfall (Billions USD)")+
  theme_classic()+
  guides(color = guide_legend(order = 1),linetype = guide_legend(order = 2))+
  scale_y_continuous(breaks = seq(300,-900,by=-100), labels = seq(300,-900,by=-100))

# Save -----------------------------------------

ggsave(file = paste0(charts_folder, "kearns_comparison.png"), comp_graph, device = "png",width=6,height=4,units="in")
ggsave(file = paste0(charts_folder, "kearns_comparison_cumulative.png"), comp_graph2, device = "png",width=6,height=4,units="in")

graph1 %>% filter(order<13) %>% group_by(paper) %>% summarize(shortfall=sum(shrtfl_billions))

#####

# do estimates imply a more inequitable division of funds?

crfb_data = haven::read_dta(paste0(data_folder,"Processing/muni_emp_panel_df.dta"))

aid_data = crfb_data %>% 
  distinct(total_muni_aid_per_resident,state_abbrev) %>% 
  mutate(total_muni_aid_per_resident=as.numeric(total_muni_aid_per_resident))

rep_data = crfb_data %>% 
  distinct(reps_per_million,state_abbrev) %>% 
  mutate(reps_per_million=as.numeric(reps_per_million))

covid_data = crfb_data %>% 
  filter(year==2020&month==4) %>% 
  distinct(total_deaths_current_month,state_abbrev) %>% 
  mutate(total_deaths_current_month=as.numeric(total_deaths_current_month))

cv_tot_est_state <- cv_state_calcs %>%
  group_by(state) %>%
  summarise(inctax_shortfall = sum(inctax_shortfall),
            salestax_shortfall = sum(salestax_shortfall))

cv_tot_est_state <- cv_tot_est_state %>%
  mutate(cvtax_shortfall = (inctax_shortfall + salestax_shortfall)/0.614)

cv_tot_est_state <- cv_tot_est_state %>%
  mutate(cvtax_shortfall = (cvtax_shortfall)*1.3964) %>% 
  left_join(crfb_data %>% distinct(state_abbrev,pop),by=c("state"="state_abbrev")) %>% 
  mutate(cvtax_shortfall_pc = cvtax_shortfall/pop) %>% 
  left_join(aid_data,by=c("state"="state_abbrev")) %>% 
  mutate(aid = total_muni_aid_per_resident*pop,
         aid_share= aid/sum(aid),
         shortfall_share = cvtax_shortfall/sum(cvtax_shortfall),
         share_diff=aid_share-shortfall_share,
         equitable_aid = (sum(aid)-sum(pop)*1804)*shortfall_share+1804*pop,
         equitable_aid_pc = equitable_aid/pop,
         ch_aid = equitable_aid_pc-total_muni_aid_per_resident,
         equal_aid_pc = (pop/sum(pop))*sum(aid)/pop) %>% 
  left_join(armax_preds %>% filter(date=="2020-04-01") %>% select(state,rgsp_yoy_pchange,predicted_mean)) %>% 
  left_join(covid_data,by=c("state"="state_abbrev")) %>% 
  left_join(rep_data,by=c("state"="state_abbrev"))


