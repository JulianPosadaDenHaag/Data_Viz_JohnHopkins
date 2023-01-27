library("ProjectTemplate")
load.project()

library(readr)
library(visdat)
library(skimr)
library(here)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(scales)
library(stringr)


data_fastfood_sales <- read_csv(here::here("data","raw_data", "data_fastfood_sales.csv"))
vis_dat(data_fastfood_sales)
skim(data_fastfood_sales)

data_fastfood_calories <- read_csv(here::here("data","raw_data", "data_fastfood_calories.csv"))
vis_dat(data_fastfood_calories)
skim(data_fastfood_calories)


head(data_fastfood_sales)

data_fastfood_sales %>% mutate(proportion = num_franchised_stores / unit_count) %>% 
  ggplot(aes(x = us_sales, y = unit_count, label = restaurant, color = proportion)) + 
  geom_point()+
  scale_x_continuous(trans = 'log10')+ 
  scale_y_continuous(trans = 'log10')+
  xlab("U.S. sales in milllions (log10 scale)")+
  ylab("Total number of stores (log10 scale)")+
  labs(color = "Proportion of stores\nfranchised")+
  geom_text_repel() +
  geom_point(color = 'dodgerblue') +
  theme_light()


data_fastfood_sales %>% 
  ggplot(aes(label = label_dollar()(ceiling(average_sales)),  
             x = average_sales, 
             y = reorder (restaurant, average_sales))) +  
  geom_bar(stat = "identity")+
  xlab("Average sales per unit store (in thousand)")+
  ylab("Restaurant")+
  geom_text(hjust = 0)+
  scale_x_continuous(labels=scales::dollar_format())+
  theme_classic()
  

head(data_fastfood_calories)


data_fastfood_calories%>% 
  ggplot(aes(x = calories, y = sodium,
             label = item))+ 
  geom_jitter()+facet_wrap(~restaurant)+
  geom_hline(yintercept = 2300)+
  xlab("Calories")+
  ylab("Sodium (mg)")+
  geom_text_repel(data=filter(data_fastfood_calories, 
                              sodium>2300), 
                              size = 2, 
                              max.overlaps = Inf,
                              hjust = 0,
                              vjust = 0)+
  theme_classic() + theme_light()
  
  
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data_fastfood_calories%>% 
  mutate(salad = str_detect(item, "Salad")) %>% 
  mutate(salad = case_when(salad ~  "Salad",
                    !salad  ~ "Not a salad")) %>% 
  group_by(restaurant)%>% 
  summarize(median = median(calories), salad, calories, .groups = "drop") %>% 
  arrange(-median)%>% 
  ggplot(aes(x = calories, y = reorder (restaurant, median)))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(color = salad))+
  scale_x_continuous(trans = 'log10')+ 
  coord_cartesian(xlim= c(30,3000))+
  labs(color = "Is the entree a\nsalad?")+
  ylab("Restaurants")+
  xlab("Calories (log10())")+
  theme_classic()+
  theme_light()
  
getwd()
install.packages("imager")

df_sugar_filtered <- data_fastfood_calories%>% 
  filter(restaurant != "Taco Bell") %>% 
  select(restaurant,item,sugar) %>% 
  group_by(restaurant) %>% 
  summarise(median_sugar = median(sugar),.groups = "drop") %>% 
  arrange(-median_sugar)

df_us_sales <- data_fastfood_sales %>%  
  select(restaurant,us_sales) #%>% mutate(us_sales = dollar(us_sales))

df_us_sales_vs_sugar <- left_join(df_sugar_filtered, df_us_sales, by = "restaurant") %>% 
  drop_na() %>% 
  arrange(us_sales)
  
df_us_sales_vs_sugar %>% 
  ggplot(aes(x = reorder(restaurant, us_sales), 
             y = us_sales, 
             fill = median_sugar))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(legend.position = "right")+ ## to chanage the position of the legend.
  scale_fill_gradient2(midpoint = 7,  
                       low = "darkblue", 
                       mid = "darkgreen", 
                       high = "yellow", 
                       na.value = NA)+
  #to change the default gradient.. 2 includes mid.  you can use color as well
  ylab("US sales (in millions)")+
  xlab("Restaurant")+
  labs(fill = "Median sugar (grams)\nin fast food entrees?")

 


