# run_MIDAS_regression.R
# John Kearns
# Goal: Write script to run MIDAS regression and spit results back into Python

master_dir = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Fintech/Project/Fintech-Trends-and-State-and-Local-Finance/"
do_folder = paste0(master_dir,"Do/")
data_folder = paste0(master_dir,"Data/")
results_folder = paste0(master_dir,"Results/")
charts_folder = paste0(master_dir,"Charts/")

# load packages
library(estimatr)
library(gtrendsR)
library(tidyverse)
library(fuzzyjoin)
library(lubridate)
library(KFAS)
library(xts)
library(parallelly)
library(parallel)
library(mFilter)
library(fredr)
library(forecast)
library(glmnet)
library(caret)
library(tseries)
library(midasr)
library(choroplethr)
library(vtable)

# load data
monthly_data = read_csv(paste0(data_folder,"Processing/monthly_data.csv"))
weekly_data = read_csv(paste0(data_folder,"Processing/weekly_data.csv"))
quarterly_data = read_csv(paste0(data_folder,"Processing/quarterly_data.csv"))
  
rmse_df = data.frame()
for(dat in as.Date((quarterly_data %>% filter(date>="2017-04-01"&date<"2020-01-01") %>% distinct(date))$date,format="%Y-%m-%d")){

  reg_df = data.frame()
  for(stat in state.abb){
    
    X_data_mon = monthly_data %>% filter(state==stat&date>="2016-01-01"&date< as.Date(dat) %m+% months(3)) %>% 
      select(date,lfpr_yoy_change,unemp_rate_yoy_change)
    X_data_week = (weekly_data %>% filter(state==stat&date>="2016-01-01"&date< as.Date(dat) %m+% months(3)))[,grepl("hits",colnames(weekly_data))]
    
    
    y_data = quarterly_data %>% filter(state_abbrev==stat&date< as.Date(dat) %m+% months(3)) %>% select(date,rgsp_yoy_pchange,lag1_rgsp_yoy_pchange)
    
    X_data_mon_df = mls(c(unlist(X_data_mon[,2])),0:2,3)
    colnames(X_data_mon_df)[1:3] = paste0(colnames(X_data_mon_df)[1:3],colnames(X_data_mon)[2])
    for(i in 3:ncol(X_data_mon)){
      
      X_data_mon_df = cbind(X_data_mon_df,mls(c(unlist(X_data_mon[,i])),0:2,3))
      colnames(X_data_mon_df)[(2*i-2):(2*i)] = paste0(colnames(X_data_mon_df)[(2*i-2):(2*i)],colnames(X_data_mon)[i])
      
    }
    
    week_vec = unlist(X_data_week[,1])
    X_data_week_df = mls(unlist(c(week_vec,rep(NA,length(week_vec) + (13 - length(week_vec) %% 13)-length(week_vec)))),0:12,13)
    colnames(X_data_week_df)[1:13] = paste0(colnames(X_data_week_df)[1:13],colnames(X_data_week)[1])
    for(i in 2:ncol(X_data_week)){
      
      week_vec = unlist(X_data_week[,i])
      X_data_week_df = cbind(X_data_week_df,mls(c(week_vec,rep(NA,length(week_vec) + (13 - length(week_vec) %% 13)-length(week_vec))),0:12,13))
      colnames(X_data_week_df)[(13*i-12):(13*i)] = paste0(colnames(X_data_week_df)[(13*i-12):(13*i)],colnames(X_data_week)[i])
      
    }
    
    reg_df = bind_rows(reg_df,cbind(y_data,X_data_mon_df[1:nrow(y_data),],X_data_week_df[1:nrow(y_data),]) %>% mutate(state=stat))
    
  }

  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          search = "random",
                          verboseIter = TRUE)
  
  # Training ELastic Net Regression model
    mod <- train(rgsp_yoy_pchange~.-date-state+factor(state),data=reg_df %>% filter(date<dat),
               method = "glmnet",
               preProcess = c("center", "scale"),
               tuneLength = 25,
               trControl = control)
    
    df = cbind(date=as.Date(as.numeric(dat)),state=quarterly_data$state_abbrev[quarterly_data$date==dat],actual_gdp=quarterly_data$rgsp_yoy_pchange[quarterly_data$date==dat],
               pred_gdp=predict(mod,reg_df %>% filter(date==dat)))
    
    rmse_df = dplyr::bind_rows(rmse_df,data.frame(df) %>% mutate(date=as.Date(as.numeric(date)))) %>% 
      mutate(date=as.Date(as.numeric(date)))
    
}
  
write.csv(rmse_df,file=paste0(data_folder,"Processing/MIDAS_RMSE_train.csv"))

rmse_df_train = rmse_df


rmse_df = data.frame()

  reg_df = data.frame()
  for(stat in state.abb){
    
    X_data_mon = monthly_data %>% filter(state==stat&date>="2016-01-01") %>% 
      select(date,lfpr_yoy_change,unemp_rate_yoy_change)
    X_data_week = (weekly_data %>% filter(state==stat&date>="2016-01-01"))[,grepl("hits",colnames(weekly_data))]
    
    
    y_data = quarterly_data %>% filter(state_abbrev==stat) %>% select(date,rgsp_yoy_pchange,lag1_rgsp_yoy_pchange)
    
    month_vec = unlist(X_data_mon[,2])
    X_data_mon_df = mls(c(month_vec,rep(NA,length(month_vec) + (3 - length(month_vec) %% 3)-length(month_vec))),0:2,3)
    colnames(X_data_mon_df)[1:3] = paste0(colnames(X_data_mon_df)[1:3],colnames(X_data_mon)[2])
    for(i in 3:ncol(X_data_mon)){
      
      month_vec = unlist(X_data_mon[,i])
      X_data_mon_df = cbind(X_data_mon_df,mls(c(month_vec,rep(NA,length(month_vec) + (3 - length(month_vec) %% 3)-length(month_vec))),0:2,3))
      colnames(X_data_mon_df)[(2*i-2):(2*i)] = paste0(colnames(X_data_mon_df)[(2*i-2):(2*i)],colnames(X_data_mon)[i])
      
    }
    
    week_vec = unlist(X_data_week[,1])
    X_data_week_df = mls(c(week_vec,rep(NA,length(week_vec) + (13 - length(week_vec) %% 13)-length(week_vec))),0:12,13)
    colnames(X_data_week_df)[1:13] = paste0(colnames(X_data_week_df)[1:13],colnames(X_data_week)[1])
    for(i in 2:ncol(X_data_week)){
      
      week_vec = unlist(X_data_week[,i])
      X_data_week_df = cbind(X_data_week_df,mls(c(week_vec,rep(NA,length(week_vec) + (13 - length(week_vec) %% 13)-length(week_vec))),0:12,13))
      colnames(X_data_week_df)[(13*i-12):(13*i)] = paste0(colnames(X_data_week_df)[(13*i-12):(13*i)],colnames(X_data_week)[i])
      
    }
    
    reg_df = bind_rows(reg_df,cbind(y_data,X_data_mon_df[1:nrow(y_data),],X_data_week_df[1:nrow(y_data),]) %>% mutate(state=stat))
    
  }
  
  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          search = "random",
                          verboseIter = TRUE)
  
  # Training ELastic Net Regression model
  mod <- train(rgsp_yoy_pchange~.-date-state+factor(state),data=reg_df %>% filter(date<"2020-01-01"),
               method = "glmnet",
               preProcess = c("center", "scale"),
               tuneLength = 25,
               trControl = control)
  
  df = cbind(reg_df %>% filter(date>="2020-01-01") %>% select(date,state,rgsp_yoy_pchange),
             pred_gdp=predict(mod,reg_df %>% filter(date>="2020-01-01")))
  
  rmse_df = dplyr::bind_rows(rmse_df,data.frame(df)) %>% 
    mutate(date=as.Date(as.numeric(date)))
  
colnames(rmse_df)[3] = "actual_gdp"
  
write.csv(rmse_df,file=paste0(data_folder,"Processing/MIDAS_RMSE_test.csv"))

# make monthly MIDAS dataset
reg_df_monthly_midas = data.frame()
for(stat in state.abb){
  
  X_data_mon = monthly_data %>% filter(state==stat&date>="2016-01-01") %>% 
    select(date,lfpr_yoy_change,unemp_rate_yoy_change)
  X_data_week = (weekly_data %>% filter(state==stat&date>="2016-01-01") %>% mutate(month=lubridate::month(date)) %>% group_by(year,month)  %>% summarize_at(vars(hits_sa_yoy_pchange_674:hits_sa_yoy_pchange_340),mean,na.rm=TRUE) %>% ungroup() %>% select(hits_sa_yoy_pchange_674:hits_sa_yoy_pchange_340))
    
  y_data = quarterly_data %>% filter(state_abbrev==stat) %>% select(date,rgsp_yoy_pchange,lag1_rgsp_yoy_pchange)
  
  month_vec = unlist(X_data_mon[,2])
  X_data_mon_df = mls(c(month_vec,rep(NA,length(month_vec) + (3 - length(month_vec) %% 3)-length(month_vec))),0:2,3)
  colnames(X_data_mon_df)[1:3] = paste0(colnames(X_data_mon_df)[1:3],colnames(X_data_mon)[2])
  for(i in 3:ncol(X_data_mon)){
    
    month_vec = unlist(X_data_mon[,i])
    X_data_mon_df = cbind(X_data_mon_df,mls(c(month_vec,rep(NA,length(month_vec) + (3 - length(month_vec) %% 3)-length(month_vec))),0:2,3))
    colnames(X_data_mon_df)[(2*i-2):(2*i)] = paste0(colnames(X_data_mon_df)[(2*i-2):(2*i)],colnames(X_data_mon)[i])
    
  }
  
  week_vec = unlist(X_data_week[,2])
  X_data_week_df = mls(c(week_vec,rep(NA,length(week_vec) + (3 - length(week_vec) %% 3)-length(week_vec))),0:2,3)
  colnames(X_data_week_df)[1:3] = paste0(colnames(X_data_week_df)[1:3],colnames(X_data_week)[1])
  for(i in 2:ncol(X_data_week)){
    
    week_vec = unlist(X_data_week[,i])
    X_data_week_df = cbind(X_data_week_df,mls(c(week_vec,rep(NA,length(week_vec) + (3 - length(week_vec) %% 3)-length(week_vec))),0:2,3))
    colnames(X_data_week_df)[(3*i-2):(3*i)] = paste0(colnames(X_data_week_df)[(3*i-2):(3*i)],colnames(X_data_week)[i])
    
  }
  
  reg_df_monthly_midas = bind_rows(reg_df_monthly_midas,data.frame(cbind.fill(cbind.fill(y_data,X_data_mon_df),X_data_week_df)) %>% mutate(state=stat))
  
}
reg_df_monthly_midas = reg_df_monthly_midas %>% 
  mutate_at(vars(rgsp_yoy_pchange:X.2.mhits_sa_yoy_pchange_340),as.numeric)
reg_df_monthly_midas$date[27] = "2022-07-01"
reg_df_monthly_midas$date[28] = "2022-11-01"

save(reg_df_monthly_midas,file=paste0(data_folder,"Processing/reg_df_monthly_midas.RData"))

# create DF structure that is needed for LSTM
weekly_data_lstm = weekly_data %>% 
  group_by(state,year,quarter) %>% 
  mutate(quarter=ifelse(date==max(date),quarter,NA)) %>% 
  ungroup() %>% 
  mutate(month=lubridate::month(date)) %>% 
  group_by(state,year,month) %>% 
  mutate(month=ifelse(date==max(date),month,NA)) %>% 
  ungroup()

monthly_data_lstm = monthly_data %>% 
  mutate(month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  select(state,lfpr_yoy_change,unemp_rate_yoy_change,year,month)

quarterly_data_lstm = quarterly_data %>% 
  select(state_abbrev,year,quarter,rgsp_yoy_pchange,lag1_rgsp_yoy_pchange:lag4_rgsp_yoy_pchange) %>% 
  rename(state=state_abbrev)

weekly_data_lstm = left_join(weekly_data_lstm,quarterly_data_lstm,by=c("state","year","quarter")) %>% 
  left_join(monthly_data_lstm,by=c("state","year","month")) %>% 
  filter(year>2015)

write.csv(weekly_data_lstm,file=paste0(data_folder,"Processing/weekly_data_lstm.csv"))


# make US heat map of GDP
state_gdp_map_data = quarterly_data %>% 
  filter(year==2020&quarter==2) %>% 
  select(state_abbrev,rgsp_yoy_pchange) %>% 
  left_join(data.frame(state_name=tolower(state.name),state_abbrev=state.abb)) %>% 
  rename(region=state_name,
         abb=state_abbrev,
         value=rgsp_yoy_pchange) %>% 
  mutate(value=as.numeric(value)) %>% 
  bind_rows(data.frame(abb="DC",region="district of columbia",value=-8.49))

state_choropleth(state_gdp_map_data,
                 title      = "State GDP during COVID-19",
                 legend     = "%Change in GDP from Q2 2019 to Q2 2020",
                 num_colors = 1) +
  scale_fill_gradient(name="%Change in GDP from\nQ2 2019 to Q2 2020",high=scales::muted("green"),low="red")
  
choro = StateChoropleth$new(state_gdp_map_data)
choro$title = "State GDP during COVID-19"
choro$ggplot_scale = scale_fill_brewer(name="%Change in GDP from\nQ2 2019 to Q2 2020",palette="YlOrRd", drop=FALSE, na.value="grey",direction=-1)
state_map = choro$render() + theme(legend.position = "bottom")

ggsave(paste0(charts_folder,"state_gdp_heatmap.png"),state_map,scale=.5,width=15,height=10)


nj_gdp = ggplot() +
  geom_line(data=monthly_data %>% filter(state=="NJ"&date>="2016-01-01"),aes(x=date,y=lfpr_yoy_change,color="Labor Force\nParticipation Rate (Change)")) +
  geom_line(data=monthly_data%>% filter(state=="NJ"&date>="2016-01-01"),aes(x=date,y=unemp_rate_yoy_change,color="Unemployment Rate (Change)")) +
  geom_line(data=quarterly_data%>% filter(state_abbrev=="NJ"&date>="2016-01-01"),aes(x=date,y=rgsp_yoy_pchange,color="Real GDP (%Change)")) +
  ggthemes::theme_fivethirtyeight()

ggsave(paste0(charts_folder,"NJ_gdp_timeplot.png"),nj_gdp,scale=.5,width=15,height=10)

google_index_ex_data = trends_sa3 %>%
  ungroup() %>% 
  filter(state%in%c("NJ","AL")&category%in%c(718,918)) %>% 
  select(date,state,category,hits) %>% 
  rowwise() %>% 
  mutate(cat_name=which_category(category)) %>% 
  ungroup() %>% 
  group_by(state,cat_name) %>% 
  mutate(hits1=hits/hits[date=="2020-03-15"]*100) %>% 
  ungroup()

index_unadj = ggplot(google_index_ex_data,aes(x=date,y=hits,color=as.character(category))) +
  geom_line() +
  geom_vline(xintercept=as.Date("2020-03-12"),color="red") +
  facet_wrap(~state+cat_name,scales="free_y") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position="none") +
  scale_color_manual(values=c(scales::muted("orange"),scales::muted("green")))

ggsave(paste0(charts_folder,"index_unadjustd.png"),index_unadj,scale=.5,width=15,height=10)

google_index_ex_data = trends_sa3 %>%
  ungroup() %>% 
  filter(state%in%c("NJ","AL")&category%in%c(918)) %>% 
  select(date,state,category,hits_sa) %>% 
  rowwise() %>% 
  mutate(cat_name=which_category(category)) %>% 
  ungroup()

index_adj = ggplot(google_index_ex_data,aes(x=date,y=hits_sa,color=as.character(state))) +
  geom_line() +
  facet_wrap(~state+cat_name) +
  geom_vline(xintercept=as.Date("2020-03-12"),color='red') +
  theme_bw() +
  labs(y="Google Search Intensity Index (max = 100)",x="") +
  theme(legend.position="none",axis.title.y=element_text()) +
  scale_color_manual(values=c(scales::muted("orange"),scales::muted("green")))

ggsave(paste0(charts_folder,"index_adjustd.png"),index_adj,scale=.5,width=20,height=10)


armax_coeffs = read_csv(paste0(data_folder,"Raw/coefs.csv")) %>% 
  mutate(id=gsub("hits_sa_yoy_pchange_","",id)) %>% 
  rowwise() %>% 
  mutate(name = which_category(id)) %>% 
  ungroup() %>% 
  filter(pval<=.1)

coef_bar_plot = ggplot(armax_coeffs,aes(x=reorder(name,coeff),y=coeff,fill=coeff)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="red",high="green") +
  labs(y="Increase in GDP growth (%) for\n1% increase in search intensity") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position="none",axis.title.x=element_text()) 

ggsave(paste0(charts_folder,"coef_bar_plot.png"),coef_bar_plot,scale=.5,width=15,height=10)

armax_coeffs_pc = read_csv(paste0(data_folder,"Raw/coefs_pc.csv")) %>% 
  mutate(id=gsub("hits_sa_yoy_pchange_","",id)) %>% 
  rowwise() %>% 
  mutate(name = which_category(id)) %>% 
  ungroup() %>% 
  filter(pval<=.1)

coef_bar_plot_pc = ggplot(armax_coeffs_pc,aes(x=reorder(name,coeff),y=coeff,fill=coeff)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="red",high="green") +
  labs(y="Increase in GDP growth (%) for\n1% increase in search intensity") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position="none",axis.title.x=element_text()) 

ggsave(paste0(charts_folder,"coef_bar_plot_pc.png"),coef_bar_plot_pc,scale=.5,width=15,height=10)

armax_coeffs_diff = armax_coeffs_pc %>% filter((id%in%armax_coeffs$id))
coef_bar_plot_diff = ggplot(armax_coeffs_diff,aes(x=reorder(name,coeff),y=coeff,fill=coeff)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="red",high="green") +
  labs(y="Increase in GDP growth (%) for\n1% increase in search intensity") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position="none",axis.title.x=element_text()) 

ggsave(paste0(charts_folder,"coef_bar_plot_diff.png"),coef_bar_plot_diff,scale=.5,width=15,height=10)

linplot1 = ggplot() +
  geom_line(data=quarterly_data %>% filter(state_abbrev=="RI"&date>="2016-01-01"),aes(x=date,y=rgsp_yoy_pchange,color="GDP Growth")) +
  geom_line(data=trends_sa3%>% filter(state=="RI"&date>="2016-01-01"&category==438) %>% mutate(hits_sa=(hits_sa-50)/3),aes(x=date,y=hits_sa,color="Vehicle Wheels Index")) +
  ggthemes::theme_fivethirtyeight()

ggsave(paste0(charts_folder,"linplot1.png"),linplot1,scale=.5,width=15,height=10)


reg_data_final = read_csv(paste0(data_folder,"Final/reg_data_final.csv"))

# make summary statistics table
st(reg_data_final %>% 
     filter(year>=2016) %>% 
     select(rgsp_yoy_pchange,unemp_rate_y,unemp_rate_yoy_change,lfpr,lfpr_yoy_change,hits_sa_yoy_pchange_674,hits_sa_yoy_pchange_468,
                       hits_sa_yoy_pchange_463,hits_sa_yoy_pchange_158) %>% 
     rename("Real GDP Growth (%)"=rgsp_yoy_pchange,
            "Labor Force Participation Rate (%)"=lfpr,
            "Change in LFPR (%)"=lfpr_yoy_change,
            "Unemployment Rate (%)"=unemp_rate_y,
            "Change in Unemployment Rate (%)"=unemp_rate_yoy_change,
            "Change in Plastics GT Index (%)"=hits_sa_yoy_pchange_674,
            "Change in Auto Financing GT Index (%)"=hits_sa_yoy_pchange_468,
            "Change in Property Inspections GT Index (%)"=hits_sa_yoy_pchange_463,
            "Change in Home Improvement GT Index (%)"=hits_sa_yoy_pchange_158),
   digits=2)

# look at government need



