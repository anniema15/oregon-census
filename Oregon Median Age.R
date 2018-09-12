library(tidyr)
library(maps)
library(dplyr)
library(ggplot2)
library(tools)

#clear workspace
rm(list = ls())

#read in data, trim and recode proper variables 
df <- read.csv("cc-est2017-alldata-41.csv") 

df_clean <- df %>% 
  filter(YEAR > 2) %>% 
  filter(YEAR < 10) %>% 
  mutate(YEAR = ifelse(YEAR == 3, 2010, YEAR)) %>% 
  mutate(YEAR = ifelse(YEAR == 4, 2011, YEAR)) %>% 
  mutate(YEAR = ifelse(YEAR == 5, 2012, YEAR)) %>% 
  mutate(YEAR = ifelse(YEAR == 6, 2013, YEAR)) %>% 
  mutate(YEAR = ifelse(YEAR == 7, 2014, YEAR)) %>% 
  mutate(YEAR = ifelse(YEAR == 8, 2015, YEAR)) %>% 
  mutate(YEAR = ifelse(YEAR == 9, 2016, YEAR)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 0, "Total", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 1, "0 to 4", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 2, "5 to 9", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 3, "10 to 14", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 4, "15 to 19", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 5, "20 to 24", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 6, "25 to 29", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 7, "30 to 34", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 8, "35 to 39", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 9, "40 to 44", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 10, "45 to 49", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 11, "50 to 54", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 12, "55 to 59", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 13, "60 to 64", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 14, "65 to 69", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 15, "70 to 74", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 16, "75 to 79", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 17, "80 to 84", AGEGRP)) %>% 
  mutate(AGEGRP = ifelse(AGEGRP == 18, "85 or older", AGEGRP))


#Calculate the median age by county and year 
#using a cumulative sum to find when the 
#running total tips past the midpoint, thus 
#marking the median age bucket 
oregon_median_age <- df_clean %>% 
  filter(AGEGRP != "Total") %>% 
  group_by(CTYNAME, YEAR) %>% 
  mutate(MEDIAN_COUNT = sum(TOT_POP)/2) %>% 
  group_by(CTYNAME, YEAR) %>% 
  mutate(RUNNING_TOTAL = cumsum(TOT_POP)) %>% 
  mutate(MEDIAN_GROUP = ifelse(RUNNING_TOTAL >= MEDIAN_COUNT, TRUE, FALSE)) %>% 
  group_by(CTYNAME, YEAR) %>% 
  mutate(MEDIAN = ifelse(MEDIAN_GROUP != lag(MEDIAN_GROUP), TRUE, FALSE)) %>% 
  filter(MEDIAN == TRUE) %>% 
  select(CTYNAME, AGEGRP, YEAR)

#join age data to spatial mapping data 
oregon_map_data <- map_data("county") %>% 
  filter(region == "oregon") %>% 
  mutate(CTYNAME = paste(toTitleCase(subregion), "County"))

or_median_map <- left_join(oregon_map_data, oregon_median_age) 

#map the data for 2010 and 2016
map2010 <- ggplot() + 
  geom_polygon(data = filter(or_median_map, YEAR ==2010), 
               mapping = aes(x = long, y = lat, group = group, fill = AGEGRP),
               color = "white", 
               alpha = I(.9)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank()) + 
  scale_fill_brewer("Age Group") 


map2016 <- ggplot() + 
  geom_polygon(data = filter(or_median_map, YEAR ==2016), 
               mapping = aes(x = long, y = lat, group = group, fill = AGEGRP),
               color = "white", 
               alpha = I(.9)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank()) + 
  scale_fill_brewer("Age Group") 


#spread and reshape the data frame so it 
#is suited for mapping in Carto
oregon_median_age_for_carto <- oregon_median_age %>% 
  spread(YEAR, AGEGRP)

#code to write CSV
write.csv(oregon_median_age_for_carto, "Oregon_median_age_carto.csv")
