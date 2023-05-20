## AS Figure 4 code
## Phil Hoxie
## July 6, 2021

# Folder -----------------------------------------------------------------------

master_dir = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Fintech/Project/Fintech-Trends-and-State-and-Local-Finance/"
do_folder = paste0(master_dir,"Do/")
data_folder = paste0(master_dir,"Data/")
results_folder = paste0(master_dir,"Results/")
charts_folder = paste0(master_dir,"Charts/")
folder_output = charts_folder

# Packages ----------------------------------------------------------------------

library(tidyverse)

source(paste0(master_dir,"Do/AS 1 state pulls.R"))
source(paste0(master_dir,"Do/AS 2 clean and estimate.R"))


# Graph differences, May 2022 Update  -------------------------------------------------------------

graph <- estimates %>%
  select(Period, bartik_pmt_may20, bartik_pmt_feb21, bartik_pmt_may22, cvtax_shortfall_may20, cvtax_shortfall_feb21, cvtax_shortfall_may22)

order <- seq(1, 14, 1) %>% tbl_df() %>% rename(order = value)

graph <- bind_cols(order, graph) %>%
  filter(order > 4)

graph <- graph %>%
  gather(key = "series", "shortfall", 3:8)

graph <- graph %>% # categories
  mutate(paper = ifelse(str_detect(series, "bartik"), "Bartik (2020)\nUses unemployment data", "Clemens and Veuger (2020b)\nUses national income data"),
         forecast = ifelse(str_detect(series, "may20"), "May 2020\nForecast",
                           ifelse(str_detect(series, "jul20"), "July 2020\nForecast",
                                  ifelse(str_detect(series, "feb21"), "February 2021\nForecast", "May 2022\nActual"))))

graph <- graph %>% # units
  mutate(shrtfl_billions = shortfall / 1000000000,
         forecast_num = ifelse(forecast == "May 2020\nForecast", 2,
                               ifelse(forecast == "July 2020\nForecast", 4,
                                      ifelse(forecast == "February 2021\nForecast", 3,
                                             ifelse(forecast == "July 2021\nForecast", 5,
                                                    ifelse(forecast=="May 2022\nActual",6,NA))))))

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

comp_graph = ggplot(data = graph %>% group_by(series) %>% summarize(shrtfl_billions=sum(shrtfl_billions,na.rm=TRUE),paper=paper[1],forecast=forecast[1],forecast_num=forecast_num[1]) %>% arrange(forecast_num), aes(x = fct_inorder(forecast), y = shrtfl_billions, fill = paper))+
  geom_col() +
  facet_wrap(~paper) +
  geom_hline(yintercept = 0)+
  xlab("Data Used")+
  ylab("Estimated Total Shortfall (Billions USD)") +
  theme_classic()+
  theme(legend.position="none")
comp_graph

# Save -----------------------------------------

ggsave(file = paste0(folder_output, "state quarterly comparison_20220630.png"), comp_graph, device = "png",width=6,height=4,units="in")

write.csv(graph,paste0(folder_output,"shortfall_graph_data_20220630.csv"))

