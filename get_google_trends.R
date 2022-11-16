# get_google_trends.R
# John Kearns
# Goal: Write script to pull categories to replicate Nakazawa 2022

master_dir = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Fintech/Project/Fintech-Trends-and-State-and-Local-Finance/"
do_folder = paste0(master_dir,"Do/")
data_folder = paste0(master_dir,"Data/")
results_folder = paste0(master_dir,"Results/")
charts_folder = paste0(master_dir,"Charts/")

# load packages
library(gtrendsR)
library(tidyverse)
library(fuzzyjoin)
library(lubridate)
library(KFAS)
library(xts)
library(parallelly)
library(parallel)
library(mFilter)

data(categories) # categories from Google Trends

print('hello')

# read in categories to pull
ci_str_detect <- function(x, y) {
  str_detect(y, pattern = sub('(?<=.{3})', '.', x, perl = TRUE))
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
  mutate(hits_sa = ifelse(hits_sa<=0,1,hits_sa), # make correction so I can use log differences like the OECD does
         hits_sa_yoy_logchange = log(hits_sa)-log(dplyr::lag(hits_sa,52)),
         hits_sa_yoy_change = hits_sa - dplyr::lag(hits_sa,52),
         hits_sa_wow_logchange = log(hits_sa)-log(dplyr::lag(hits_sa,1)),
         hits_sa_wow_change = hits_sa - dplyr::lag(hits_sa,1),
         hits_sa_hpfilt = mFilter::hpfilter(hits_sa, type = "lambda", freq = 14400)$trend,
         hits_sa_dev_rate = (hits_sa/hits_sa_hpfilt-1)*100)

save.image(paste0(data_folder,"Processing/get_google_trends_20221115.RData"))

# state_trend_df = pd.DataFrame()
# for state in states:
#   print(state)
# 
# testdf = pytrend.get_historical_interest(['money'], year_start=2004, month_start=1, day_start=1, hour_start=0, year_end=2022, month_end=10, day_end=1, hour_end=0, cat=0, geo='US-'+state, gprop='', sleep=0)
# testdf = testdf.reset_index()
# testdf = testdf.groupby([testdf['date'].dt.date]).agg({ 
#   "money" : ["mean"]
# })
# testdf.columns = ["_".join(x) for x in testdf.columns.ravel()]
# testdf = testdf.reset_index()
# testdf = testdf.set_index('date')
# testdf = testdf.asfreq('D')
# 
# classical_res = sm.tsa.seasonal_decompose(testdf)
# # Extract the trend and seasonal components
# classical_trend = classical_res.trend
# classical_seasonal = classical_res.seasonal
# 
# # Construct the seasonally adjusted series
# testdf['adjusted'] = (testdf['money_mean'] - classical_res.seasonal)
# 
# testdf['state'] = state
# state_trend_df = state_trend_df.append(testdf)
# 
# 
# print(state)
# # requesting data
# 
