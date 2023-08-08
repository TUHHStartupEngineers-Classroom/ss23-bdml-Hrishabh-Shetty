library(tidyverse)
library(lubridate)
library(glue)
install.packages("ggrepel")
library(ggrepel)
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
countries <- c("Germany","United Kingdom","France","Spain","United States","Europe")
last_date <- max(covid_data_tbl$date)
#wrangling
cumulative_cases_tbl <- covid_data_tbl %>% 
  select(location,date,total_cases) %>%
  filter(location %in% countries) %>% filter(!is.na(total_cases)) #%>% 
  # as_factor(location) %>% fct_reorder(total_cases) #%>% 
  #mutate(new_date=floor_date(date(date),unit = "months")) %>% 
  # group_by(location) %>% 
  # summarise(cum_cases=sum(new_cases)) %>% 
  # mutate(month_year=glue('{format(new_date,"%B %Y")}'))
  #ungroup()
#visualization
cumulative_cases_tbl$label <- NA
cumulative_cases_tbl$label[which((cumulative_cases_tbl$date == 
                                   max(cumulative_cases_tbl$date)))] <- 
  cumulative_cases_tbl$total_cases[which(cumulative_cases_tbl$date==
                                           max(cumulative_cases_tbl$date))]
cumulative_cases_tbl %>% 
  ggplot(aes(date,total_cases,color=location)) +
  geom_line(size =1)+
  scale_y_continuous(labels = scales::label_number(scale=1e-6,
                                                    suffix="M"))+
  geom_label_repel(aes(label = label))+
  scale_color_discrete(type = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = glue("As of {last_date}"), #vielleicht richtiges Datum eintragen
    x = "",
    y = "Cumulative Cases",
    fill = "location",
    caption = "Europe Bypassed US in number of cumulative cases in the second half of 2020"
  ) 
 
# Case 2
#Lat and Longitude Data for world
world <- map_data("world")

#Calculate moratality rate
mortality_rate_tbl <- covid_data_tbl %>% 
  select(location, date,new_deaths,population) %>%filter(!is.na(new_deaths)) %>%
  filter(date<=as.Date("2021-04-16")) %>% 
  mutate(mort_rate=new_deaths/population) %>% 
  group_by(location) %>% 
  summarise(mortality_rate=sum(mort_rate)) %>% arrange(desc(mortality_rate))%>% 
  mutate(location = case_when(
    
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
    
  ))
mapped_data <- world %>% left_join(mortality_rate_tbl,by=c("region"="location"))
mapped_data %>% 
  ggplot(aes(long,lat, mortality_rate))+
  geom_map(aes(map_id=region, fill = mortality_rate ),map = mapped_data) +
  scale_fill_continuous(low="red", high="black",
                        labels = scales::label_number(scale  = 1e2,
                                                      prefix = "",
                                                      suffix = "%")) +
labs(
  title = "Confirmed COVID-19 deaths relative to the size of population",
  fill = "mortality_rate",
  caption = max(covid_data_tbl$date)
) 

