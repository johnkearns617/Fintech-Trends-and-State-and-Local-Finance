# get_google_trends.R
# John Kearns
# Goal: Write script to pull categories to replicate Nakazawa 2022

master_dir = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Fintech/Project/"
do_folder = paste0(master_dir,"Do/")
data_folder = paste0(master_dir,"Data/")
results_folder = paste0(master_dir,"Results/")
charts_folder = paste0(master_dir,"Charts/")

# load packages
library(gtrendsR)
library(tidyverse)
library(fuzzyjoin)
library(KeyboardSimulator)

data(categories) # categories from Google Trends

# read in categories to pull
ci_str_detect <- function(x, y) {
  str_detect(y, pattern = sub('(?<=.{3})', '.', x, perl = TRUE))
}

nk_categories = read_csv(paste0(data_folder,"Raw/Nakazawa_Categories.csv")) %>% 
  left_join(categories %>% mutate(name=gsub(" ","",name)),by=c("variables"="name")) %>% 
  filter(!is.na(id)) %>% 
  distinct(id,.keep_all=TRUE)
# do we want to add anymore categories?

state_trends_df = data.frame()
for(cat in nk_categories$id){
  print(which(nk_categories$id==cat)/length(nk_categories$id))
  for(state in state.abb){
    
    keybd.press('Alt')
    
    Sys.sleep(3)
    
    print(state)
    
    testdf <- NULL
    attempt <-0
    while( is.null(testdf) && attempt <= 10 ) {
      attempt <- attempt + 1
      if(attempt>1){ 
        print(paste0("attempt ",attempt))
        Sys.sleep(2)
        }
      try({
        testdf = gtrends(geo = paste0("US-",state),time="all",category=cat,low_search_volume=TRUE,onlyInterest = TRUE)
      })
    } 
    if(is.null(testdf)){ break }
    
    state_trends_df = bind_rows(state_trends_df,testdf$interest_over_time %>% mutate(stat=state,hits=as.numeric(gsub("<|>","",hits))))
    
    print(state)
    
    Sys.sleep(3)
  }
}

save.image(paste0(data_folder,"Processing/get_google_trends.RData"))

state_trend_df = pd.DataFrame()
for state in states:
  print(state)

testdf = pytrend.get_historical_interest(['money'], year_start=2004, month_start=1, day_start=1, hour_start=0, year_end=2022, month_end=10, day_end=1, hour_end=0, cat=0, geo='US-'+state, gprop='', sleep=0)
testdf = testdf.reset_index()
testdf = testdf.groupby([testdf['date'].dt.date]).agg({ 
  "money" : ["mean"]
})
testdf.columns = ["_".join(x) for x in testdf.columns.ravel()]
testdf = testdf.reset_index()
testdf = testdf.set_index('date')
testdf = testdf.asfreq('D')

classical_res = sm.tsa.seasonal_decompose(testdf)
# Extract the trend and seasonal components
classical_trend = classical_res.trend
classical_seasonal = classical_res.seasonal

# Construct the seasonally adjusted series
testdf['adjusted'] = (testdf['money_mean'] - classical_res.seasonal)

testdf['state'] = state
state_trend_df = state_trend_df.append(testdf)


print(state)
# requesting data

