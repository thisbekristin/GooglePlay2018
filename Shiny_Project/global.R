library(tidyverse)
library(dplyr)

# Cleaned the data in separate file
df = readr::read_csv("./google_store_apps_clean.csv")
df = data.frame(df) 

# Adjusted columns needed for analysis
df = df %>% select(., -Last.Updated, -Android.Ver, -size_mult, -size_base, -Current.Ver)
df = df %>% 
  group_by(., Category) %>% 
  mutate(., Weighted_Rating = round(((Rating * Reviews) + 1)/(Reviews + 1),2)) %>% 
  select(., App, Category, Rating, Weighted_Rating, Reviews, Size, everything())

# top/bottom 10 categories
 top_cat_df = df %>% group_by(.,Category) %>%
   summarise(., n = n()) %>% arrange(desc(n)) %>% top_n(10)
 top_cat = top_cat_df[[1]]

 bot_cat_df = df %>% group_by(.,Category) %>%
   summarise(., n = n()) %>% arrange(desc(n)) %>% top_n(-10)
 bottom_cat = bot_cat_df[[1]]

top_ins_df = df %>% group_by(.,Category) %>% 
  summarise(., Installs = sum(Installs)) %>% arrange(desc(Installs)) %>% top_n(10)
top_ins = top_ins_df[[1]]

bot_ins_df = df %>% group_by(.,Category) %>% 
  summarise(., Installs = sum(Installs)) %>% arrange(desc(Installs)) %>% top_n(-10)
bot_ins = bot_ins_df[[1]]



library(rsconnect)
rsconnect::setAccountInfo(name='kteves', token='758EB59B1046A2D2D6F041068D54436E', secret='kfhBTbjQcIfa9SuFX45l6gUSnbFlUDL9HBtRjWaZ')
