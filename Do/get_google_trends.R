# get_google_trends.R
# John Kearns
# Goal: Write script to pull categories to replicate Nakazawa 2022

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
library(vtable)

data(categories) # categories from Google Trends
fred_key = "156b9cd1b9a52db3b9fc0bab8aca2b39"

# initialize FRED link
fredr_set_key(fred_key)

print('hello')

# read in categories to pull
ci_str_detect <- function(x, y) {
  str_detect(y, pattern = sub('(?<=.{3})', '.', x, perl = TRUE))
}

which_category = function(num){
  
  return(nk_categories$variables[nk_categories$id==num])
  
}

nk_categories = read_csv(paste0(data_folder,"Raw/Nakazawa_Categories.csv")) %>% 
  left_join(categories %>% mutate(name=gsub(" ","",name)),by=c("variables"="name")) %>% 
  filter(!is.na(id)) %>% 
  distinct(id,.keep_all=TRUE)
# do we want to add anymore categories?

get_weekly_daily = function(state,start_date,cat){
  
  state_trends_df_daily = data.frame()
  
  start_date = ymd(start_date)
  end_date = start_date
  
  #myData = fromJSON(file="https://gimmeproxy.com/api/getProxy")
  
  while(end_date<Sys.Date()){
    
    end_date = min(start_date + years(5) - days(1),ymd(Sys.Date()))
    
    print(paste0(state," ",cat," ",start_date," ",end_date))
    
    testdf <- NULL
    attempt <-0
    while( is.null(testdf) && attempt <= 10 ) {
      attempt <- attempt + 1
      if(attempt>1){
        print(paste0("attempt ",attempt))
        Sys.sleep(5+runif(1,5,10))
      }
      try({
        testdf = gtrends(geo = paste0("US-",state),time=paste0(start_date," ",end_date),category=cat,low_search_volume=TRUE,onlyInterest = TRUE)
      })
      
      Sys.sleep(5)
    }
    if(is.null(testdf)){ break }
    
    state_trends_df_daily = bind_rows(state_trends_df_daily,testdf$interest_over_time %>% mutate(stat=state,hits=as.numeric(gsub("<|>","",hits))))
    
    Sys.sleep(5)
    
    start_date = start_date + years(5)
  }
  
  return(state_trends_df_daily)
}

#### code below is to pull the data. Because of Google limitations, you have to keep running in parts ####
# current_df = data.frame()
# for(cat1 in nk_categories$id){
# 
#   df = lapply(state.abb,FUN=get_weekly_daily,start_date="2015-01-01",cat=cat1)
#   # compromise: pull weekly data (less zeros) since 2015
#   # takes about 7 days to collect all of the data
# 
#   current_df = bind_rows(current_df,data.table::rbindlist(df))
# }
####

# read in data from the three computers used to pull the data
# for(file in list.files(paste0(data_folder,"Processing/"))[grep("current_df",list.files(paste0(data_folder,"Processing/")))]){
#   
#   load(paste0(data_folder,"Processing/",file))
#   
# }
# 
# trends_data = data.table::rbindlist(list(current_df_laptop,current_df_aei,current_df_server,current_df_terminal_extra,current_df_terminal_extra2,current_df_extra_laptop,current_df_extra_laptop2,current_df_extra_laptop3))

# are there any missing categories?
setdiff(nk_categories$id,trends_data$category)
# we are good

# for categories that were captured, are all filled?
completeness_check = trends_data %>% 
  group_by(category,substr(time,1,10)) %>% 
  distinct(stat) %>% 
  summarize(num=n()) %>% 
  ungroup() %>% 
  filter(num<50)
nrow(completeness_check)
# we are good

check2 = trends_data %>% 
  group_by(stat,substr(time,1,10),category) %>% 
  summarize(num=n())

# trends_data1 = trends_data %>% 
#   mutate(date_group = substr(time,1,10)) %>% 
#   distinct(date,stat,category,.keep_all=TRUE) %>% 
#   ungroup()

check2 = trends_data1 %>% 
  group_by(stat,substr(time,1,10),category) %>% 
  summarize(num=n())
# we are good

# seasonally adjust by category, by state

print('hello')

seasonally_adjust_data = function(stat1){
  
  trends_sa = data.frame()
  
  for(cat in unique(trends_data1$category)){
    
    print(paste0(stat1," ",cat))
    test_cat = trends_data[trends_data$category==cat&trends_data$stat==stat1,] %>% 
      select(date,hits) %>% 
      mutate(date=as.Date(date))
    
    test_cat = test_cat %>% 
      mutate(hits_adj = ifelse(date<"2016-01-01",hits/(hits[date<="2015-12-27"&date>="2015-12-01"]/hits[date=="2016-01-03"]),hits),
             hits_adj = ifelse(date<"2016-01-01"&max(hits_adj[date<"2016-01-01"],na.rm=TRUE)>100,hits_adj/max(hits_adj[date<"2016-01-01"],na.rm=TRUE)*100,hits_adj)) %>% 
      rowwise() %>% 
      #mutate(hits_adj = max(0,hits_adj)) %>% 
      ungroup()
    
    hits <- test_cat$hits_adj
    #--------------------------------------------------------------
    
    #do some other convenience operations---------------------------
    dates <- test_cat$date
    hits <- xts(hits,order.by=dates)
    #-----------------------------------------------------------------
    
    #-------------------------------------------------------------------
    # a few quick illustative plots
    
    #--------------------------------
    tripsmodel<-SSModel(hits ~ SSMtrend(degree = 1, Q=list(matrix(NA))) + 
                          SSMseasonal(period=52, sea.type="dummy", Q = NA), H = NA)
    tripsFit<-fitSSM(tripsmodel,inits=c(0.8,0.05, 0.001),method='BFGS')$model
    tripsSmooth <- KFS(tripsFit,smooth= c('state', 'mean','disturbance'))
    
    #Get smoothed estimates for the level
    tripsLevel <-signal(tripsSmooth, states = 'level')
    tripsLevel$signal <- xts(tripsLevel$signal, order.by = dates)
    
    #plot level
    print(autoplot(cbind(hits, tripsLevel$signal),facets = FALSE) + labs(caption = paste0(stat1," ",cat)))
    
    test_cat = cbind(test_cat,tripsLevel$signal)
    colnames(test_cat)[ncol(test_cat)] = "hits_sa"
    
    trends_sa = bind_rows(trends_sa,test_cat %>% mutate(stat=stat1,category=cat))
    
  }
  
  return(trends_sa)
  
}

print(supportsMulticore())

# trends_sa1 = mcmapply(seasonally_adjust_data,state.abb,mc.cores=50)
# 
# trends_sa2 = data.frame()
# pdf(file=paste0(charts_folder,"seasonal_adjust_charts.pdf"))
# for(i in 1:50){
#   
#   df = bind_cols(trends_sa1[((i-1)*6+1):(i*6)])
#   
#   colnames(df) = c("date","hits","hits_adj","hits_sa","state","category")
#   
#   for(j in unique(df$category)){
#     
#     print(ggplot(df %>% filter(category==j),aes(x=date)) +
#             geom_line(aes(y=hits_adj),color="blue") +
#             geom_line(aes(y=hits_sa),color="red") +
#             labs(caption = paste0(df$state[1]," ",j)))
#     
#   }
#   
#   trends_sa2 = bind_rows(trends_sa2,df)
#   
# }
# dev.off()


# fix some of the smoothed data that did not get smoothed
trends_sa3 = trends_sa2 %>% 
  mutate(hits_sa = ifelse(hits_sa<0,0,hits_sa)) %>% 
  distinct(date,state,category,.keep_all=TRUE)

avg_var = trends_sa3 %>% 
  mutate(month = lubridate::month(date),
         year= lubridate::year(date)) %>% 
  group_by(state,category,year,month) %>% 
  mutate(growth = (hits_sa/dplyr::lag(hits_sa)-1)*100) %>% 
  summarize(avg_growth = mean(growth[!is.infinite(growth)],na.rm=TRUE)) %>% 
  group_by(state,category) %>% 
  summarize(avg_growth = abs(median(avg_growth,na.rm=TRUE))) %>% 
  ungroup() %>% 
  filter(avg_growth>.16) # 0.16 is the 3rd quartile. Fix the 25% that are too erratic
  

# calculate trend
# pdf(file=paste0(charts_folder,"seasonal_adjust_smoothed.pdf"))
# for(i in 1:nrow(avg_var)){
# 
#   print(i/nrow(avg_var))
# 
#   test1 = ggplot() +
#     geom_line(data=trends_sa3 %>% filter(state==avg_var$state[i]&category==avg_var$category[i]),aes(x=date,y=hits_adj,colour="Adjusted Hits")) +
#     stat_smooth(data=trends_sa3 %>% filter(state==avg_var$state[i]&category==avg_var$category[i]),aes(x=date,y=hits_adj),method="loess",span=.1) +
#     labs(caption=paste0(avg_var$state[i]," ",avg_var$category[i]))
# 
#   testvec = ggplot_build(test1)$data[[2]]$y
#   hits_sa = zoo::na.approx(c(testvec[1],sapply(testvec[-c(1,74:80)], function(x) c(rep(NA,4),x)),sapply(testvec[c(74:80)], function(x) c(rep(NA,6),x))))
# 
#   trends_sa3$hits_sa[trends_sa3$state==avg_var$state[i]&trends_sa3$category==avg_var$category[i]] = hits_sa
# 
#   print(ggplot(trends_sa3 %>% filter(state==avg_var$state[i]&category==avg_var$category[i])) +
#                       geom_line(aes(x=date,y=hits_adj),color="blue") +
#                        geom_line(aes(x=date,y=hits_sa),color="red") +
#           labs(caption=paste0(avg_var$state[i]," ",avg_var$category[i])))
# 
# }
# dev.off()

trends_sa3 = trends_sa3 %>% 
  mutate(hits_sa = ifelse(hits_sa<0,0,hits_sa))

avg_var = trends_sa3 %>% 
  mutate(month = lubridate::month(date),
         year= lubridate::year(date)) %>% 
  group_by(state,category,year,month) %>% 
  mutate(growth = (hits_sa/dplyr::lag(hits_sa)-1)*100) %>% 
  summarize(avg_growth = mean(growth[!is.infinite(growth)],na.rm=TRUE)) %>% 
  group_by(state,category) %>% 
  summarize(avg_growth = abs(median(avg_growth,na.rm=TRUE))) 


# turn into yoy and wow (Do not really want to throw away information)
trends_sa3 = trends_sa3 %>% 
  group_by(state,category) %>% 
  mutate(hits_sa = ifelse(hits_sa<=0,1,hits_sa), # make correction so I can use percentage changes
         hits_sa_yoy_pchange = (hits_sa/dplyr::lag(hits_sa,52)-1)*100,
         hits_sa_yoy_change = hits_sa - dplyr::lag(hits_sa,52),
         hits_sa_wow_pchange = (hits_sa/dplyr::lag(hits_sa,1)-1)*100,
         hits_sa_wow_change = hits_sa - dplyr::lag(hits_sa,1),
         hits_sa_hpfilt = mFilter::hpfilter(hits_sa, type = "lambda", freq = 14400)$trend,
         hits_sa_dev_rate = (hits_sa/hits_sa_hpfilt-1)*100) %>% 
  ungroup()

# reshape, using the yoy pchange variable
trends_sa_wide_pchange_yoy = trends_sa3 %>% 
  select(date,state,category,hits_sa_yoy_pchange) %>% 
  pivot_wider(names_from=category,values_from=hits_sa_yoy_pchange,names_prefix="hits_sa_yoy_pchange_")

save(trends_sa_wide_pchange_yoy,file=paste0(data_folder,"Processing/GT_wide_pchange.RData"))

trends_sa_wide_trend_dev = trends_sa3 %>% 
  select(date,state,category,hits_sa_dev_rate) %>% 
  pivot_wider(names_from=category,values_from=hits_sa_dev_rate,names_prefix="hits_sa_dev_rate_")

save(trends_sa_wide_trend_dev,file=paste0(data_folder,"Processing/GT_wide_dev.RData"))

num_id = sample(nk_categories$id,1)
ggplot(trends_sa_wide_pchange_yoy %>% filter(state=="NE"),aes_string(x="date",y=paste0("hits_sa_yoy_pchange_",num_id))) +
  geom_line(size=2) +
  labs(y="YoY Percentage Growth in Search Activity",caption=paste0(num_id," ",which_category(as.numeric(num_id))))

#ggplot(trends_sa_wide_trend_dev %>% filter(state=="AL"),aes_string(x="date",y=paste0("hits_sa_dev_rate_",num_id))) +
#  geom_line(size=2) +
#  labs(caption=paste0(num_id," ",which_category(as.numeric(num_id))))

# get state GDP
# state_gdp = data.frame()
# for(stat in state.abb){
#   
#   df = fredr(paste0(stat,"RQGSP"))
#   state_gdp = bind_rows(state_gdp,df)
#   
# }
# 
# state_gdp = state_gdp %>% 
#   filter(date>"2014-12-31") %>% 
#   select(date,series_id,value) %>% 
#   mutate(state=substr(series_id,1,2),
#          rgdp = value) %>% 
#   group_by(state) %>% 
#   mutate(rgdp_yoy_pchange = (rgdp/dplyr::lag(rgdp,4)-1)*100,
#          lag1_rgdp_yoy_pchange = dplyr::lag(rgdp_yoy_pchange,1),
#          lag2_rgdp_yoy_pchange = dplyr::lag(rgdp_yoy_pchange,2),
#          lag3_rgdp_yoy_pchange = dplyr::lag(rgdp_yoy_pchange,3),
#          lag4_rgdp_yoy_pchange = dplyr::lag(rgdp_yoy_pchange,4)) %>% 
#   ungroup() %>% 
#   select(date,state,rgdp,rgdp_yoy_pchange,lag1_rgdp_yoy_pchange,lag2_rgdp_yoy_pchange,lag3_rgdp_yoy_pchange,lag4_gdp_yoy_pchange)
# 
# # clip the Google Trends data so outliers arent screwing up the data
# clip=TRUE
# if(clip){
# for(i in 3:ncol(trends_sa_wide_pchange_yoy)){
#   
#   perc_up = 1.0
#   upper = quantile(trends_sa_wide_pchange_yoy[,i],perc_up,na.rm=TRUE)
#   while(upper>100){
#     
#     perc_up = perc_up-.01
#     upper = as.numeric(quantile(trends_sa_wide_pchange_yoy[,i],perc_up,na.rm=TRUE))
#     
#   }
#   
#   perc_low = 0
#   lower = as.numeric(quantile(trends_sa_wide_pchange_yoy[,i],perc_low,na.rm=TRUE))
#   while(lower<(-50)){
#     
#     perc_low = perc_low+.01
#     lower = as.numeric(quantile(trends_sa_wide_pchange_yoy[,i],perc_low,na.rm=TRUE))
#     
#   }
#   
#   trends_sa_wide_pchange_yoy[,i] = ifelse(unlist(trends_sa_wide_pchange_yoy[,i])>upper,upper,
#                                           ifelse(unlist(trends_sa_wide_pchange_yoy[,i])<lower,lower,unlist(trends_sa_wide_pchange_yoy[,i])))
#   
#   print(i/ncol(trends_sa_wide_pchange_yoy))
# }
# }
# 
reg_data = state_gdp %>% #add onto reg_data
  mutate(year=lubridate::year(date),quarter=lubridate::quarter(date)) %>%
  left_join(trends_sa_wide_pchange_yoy %>%
              ungroup() %>%
              mutate(year=lubridate::year(date),quarter=lubridate::quarter(date)) %>%
              group_by(state,year,quarter) %>%
              summarize_at(vars(hits_sa_yoy_pchange_674:hits_sa_yoy_pchange_340),mean,na.rm=TRUE) %>%
              ungroup(),by=c("state","year","quarter"))

# # write code to make pseudo-out-of-sample RMSE measurement and test set
# AR_model_function = function(data,extra_vars=c(),gdp_var="rgdp_yoy_pchange") {
#   
#   mod = lm_robust(as.formula(paste0(gdp_var,"~lag1_",gdp_var,paste(c("",extra_vars),collapse="+"),"+factor(state)")),data=data)
#   
#   return(mod)
#   
# }
# 
# ELNET_model_function = function(X,Y,extra_vars=c(),gdp_var="rgdp_yoy_pchange") {
#   
#   control <- trainControl(method = "repeatedcv",
#                           number = 5,
#                           repeats = 5,
#                           search = "random",
#                           verboseIter = TRUE)
#   
#   # Training ELastic Net Regression model
#   mod <- train(as.formula(paste0(gdp_var,"~lag1_",gdp_var,paste(c("",extra_vars),collapse="+"),paste(c("",state.abb),collapse="+"))),
#                data = cbind(X, Y),
#                method = "glmnet",
#                preProcess = c("center", "scale"),
#                tuneLength = 25,
#                penalty.factor=ifelse(nchar(colnames(X))==2,0,1),
#                trControl = control)
#   
#   return(mod)
#   
# }
# 
# pseudo_OOS_RMSE = function(data_type=c("test","train"),model_type=c("AR"),extra_vars=c(),gdp_var="rgdp_yoy_pchange"){
#   
#   if(data_type=="train"){
#     
#     data = reg_data %>% filter(date<"2020-01-01")
#     
#     if(model_type=="ELNET"){
#       
#       data = data %>% 
#         mutate(dummy=1) %>%
#         spread(key=state,value=dummy, fill=0)
#       
#     }
#     
#     rmse_df = data.frame()
#     for(dat in as.Date((data %>% filter(date>="2017-04-01") %>% distinct(date))$date,format="%Y-%m-%d")){
#       
#       if(model_type=="AR"){
#         mod = AR_model_function(data %>% filter(date<dat),extra_vars,gdp_var)
#       
#         rmse_df = bind_rows(rmse_df,cbind(data %>% filter(date==dat),
#                                         pred_gdp=predict(mod,data %>% filter(date==dat))))
#       
#         }
#     
#     if(model_type=="ELNET"){
#       
#       X <- data %>%
#         filter(date>"2016-03-31"&date<dat) %>% 
#         select(c(paste0("lag1_",gdp_var),extra_vars,state.abb)) %>% 
#         mutate_at(vars(-state.abb), funs(c(scale(.,center=TRUE,scale=FALSE))))
#       Y <- data %>% 
#         filter(date>"2016-03-31"&date<dat) %>% 
#         select(gdp_var)
#       
#       mod = ELNET_model_function(X,Y,extra_vars,gdp_var)
#       
#       rmse_df = bind_rows(rmse_df,cbind(data %>% filter(date==dat),
#                                         pred_gdp=predict(mod,data %>% filter(date==dat))))
#       
#       
#     }
#     }
#     
#     if(model_type=="ELNET"){
#       RMSE = rmse_df %>%
#         mutate_at(vars(AK:WY), ~ ifelse(. == 0, NA, .)) %>%
#         gather("state", "present", AK:WY, na.rm = TRUE) %>% 
#         select(-present) %>% 
#         group_by(state) %>% 
#         summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#         ungroup() %>% 
#         summarize(rmse=mean(rmse))
#       
#       RMSE = RMSE$rmse[1]
#     }
#     
#     if(model_type=="AR"){
#       
#       RMSE = rmse_df %>% 
#         group_by(state) %>% 
#         summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#         ungroup() %>% 
#         summarize(rmse=mean(rmse))
#       
#       RMSE = RMSE$rmse[1]
#       
#     }
#     
#     
#     
#   }
#   
#   if(data_type=="test"){
#     
#     
#     if(model_type=="AR"){
#       mod = AR_model_function(reg_data %>% filter(date<"2020-01-01"),extra_vars,gdp_var)
#     
#       data = reg_data %>% filter(date>="2020-01-01")
#     
#     rmse_df = cbind(data,
#                     pred_gdp=predict(mod,data))
#     
#     }
#     
#     if(model_type=="ELNET"){
#       
#         data = reg_data %>% 
#           mutate(dummy=1) %>%
#           spread(key=state,value=dummy, fill=0)
#       
#       X <- data %>%
#         filter(date<"2020-01-01"&date>"2016-03-31") %>% 
#         select(c(paste0("lag1_",gdp_var),extra_vars,state.abb)) %>% 
#         mutate_at(vars(-state.abb), funs(c(scale(.,center=TRUE,scale=FALSE))))
#       Y <- data %>% 
#         filter(date<"2020-01-01"&date>"2016-03-31") %>% 
#         select(gdp_var)
#       
#       mod = ELNET_model_function(X,Y,extra_vars,gdp_var)
#       
#       data = data %>% filter(date>="2020-01-01")
#       
#       rmse_df = cbind(data,
#                       pred_gdp=predict(mod,data))
#       
#     }
#     
#     
#     if(model_type=="AR"){
#     RMSE = rmse_df %>% 
#       group_by(state) %>% 
#       summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#       ungroup() %>% 
#       summarize(rmse=mean(rmse))
#     
#     RMSE = RMSE$rmse[1]
#     }
#     
#     if(model_type=="ELNET"){
#       RMSE = rmse_df %>%
#         mutate_at(vars(AK:WY), ~ ifelse(. == 0, NA, .)) %>%
#         gather("state", "present", AK:WY, na.rm = TRUE) %>% 
#         select(-present) %>% 
#         group_by(state) %>% 
#         summarize(rmse = sqrt(sum((pred_gdp-rgdp_yoy_pchange)^2)/n())) %>% 
#         ungroup() %>% 
#         summarize(rmse=mean(rmse))
#       
#       RMSE = RMSE$rmse[1]
#     }
#     
#   }
#   
#   return(list(RMSE=RMSE,model=mod,pred_df=rmse_df %>% select(-c(hits_sa_yoy_pchange_674:hits_sa_yoy_pchange_340))))
# }
# 
# plot_forecast = function(model,data){
#   
#   predictions = cbind(data,pred_gdp=predict(model,data))
#   
#   if(!("state"%in%colnames(predictions))){
#     
#     predictions = predictions %>% 
#       mutate_at(vars(AK:WY), ~ ifelse(. == 0, NA, .)) %>%
#       gather("state", "present", AK:WY, na.rm = TRUE) %>% 
#       select(-present)
#       
#   }
#   
#   predictions = predictions %>% 
#     select(date,state,rgdp_yoy_pchange,pred_gdp)
#   
#   plot1 = ggplot(predictions,aes(x=date)) +
#     geom_line(aes(y=rgdp_yoy_pchange,color="Actual RGSP"),size=1) +
#     geom_line(aes(y=pred_gdp,color="Predicted RGSP"),size=1) +
#     facet_wrap(~state) +
#     labs(x="Quarter",y="Annual Real GSP Growth (%)",caption="Source: BEA, Google")
#   
#   return(plot1)
# }
# 
# #### AR without Google Trends or External Info ####
# train_AR_eval = pseudo_OOS_RMSE(data_type="train",model_type="AR")
# print(train_AR_eval$RMSE)
# # 0.97
# 
# test_AR_eval = pseudo_OOS_RMSE(data_type="test",model_type="AR")
# print(test_AR_eval$RMSE)
# # 5.16
# 
# model_AR_forecast = plot_forecast(test_AR_eval$model,reg_data %>% filter(date>"2016-03-31"))
# ggsave(paste0(charts_folder,"AR_model_plots.png"),model_AR_forecast,width=20,height=10,units="in")
# 
# #### AR with Google Trends, No External Info, No Dimensionality Reduction ####
# train_AR_GT_eval = pseudo_OOS_RMSE(data_type="train",model_type="AR",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(train_AR_GT_eval$RMSE)
# # 4.18 without clipping, 3.80 with clipping
# # note that we do need dimension reduction here
# # and there is a problem with huge outliers (I think clipping is okay)
# 
# test_AR_GT_eval = pseudo_OOS_RMSE(data_type="test",model_type="AR",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(test_AR_GT_eval$RMSE)
# # 5.49 without clipping, 5.11 with clipping
# 
# model_AR_GT_forecast = plot_forecast(test_AR_GT_eval$model,reg_data %>% filter(date>"2016-03-31"))
# ggsave(paste0(charts_folder,"AR_GT_model_plots.png"),model_AR_GT_forecast,width=20,height=10,units="in")
# 
# #### ELNET without Google Trends ####
# train_ELNET_eval = pseudo_OOS_RMSE(data_type="train",model_type="ELNET")
# print(train_ELNET_eval$RMSE)
# # 1.34 without clipping, 1.28 with clipping
# 
# test_ELNET_eval = pseudo_OOS_RMSE(data_type="test",model_type="ELNET")
# print(test_ELNET_eval$RMSE)
# # XXX without clipping, 5.05 with clipping
# 
# model_ELNET_forecast = plot_forecast(test_ELNET_eval$model,reg_data %>% filter(date>"2016-03-31") %>%  mutate(dummy=1) %>%
#                                        spread(key=state,value=dummy, fill=0))
# ggsave(paste0(charts_folder,"ELNET_model_plots.png"),model_ELNET_forecast,width=20,height=10,units="in")
# 
# 
# #### ELNET with Google Trends ####
# train_ELNET_GT_eval = pseudo_OOS_RMSE(data_type="train",model_type="ELNET",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(train_ELNET_GT_eval$RMSE)
# # 1.59 without clipping, 1.24 with clipping
# 
# test_ELNET_GT_eval = pseudo_OOS_RMSE(data_type="test",model_type="ELNET",extra_vars=colnames(reg_data)[grep("hits_sa",colnames(reg_data))])
# print(test_ELNET_GT_eval$RMSE)
# # XXX without clipping, 5.47 with clipping
# 
# model_ELNET_GT_forecast = plot_forecast(test_ELNET_GT_eval$model,reg_data %>% filter(date>"2016-03-31") %>%  mutate(dummy=1) %>%
#                                        spread(key=state,value=dummy, fill=0))
# ggsave(paste0(charts_folder,"ELNET_GT_model_plots.png"),model_ELNET_GT_forecast,width=20,height=10,units="in")
# 

# make summary statistics table



save.image(paste0(data_folder,"Processing/get_google_trends_20221128.RData"))


