#Load Libraries 
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(rlang)
library(cowplot)
library(scales)


#Reading in data
test <- read_csv("Data/Test.csv")
train <- read_csv("Data/Train.csv")

#Understanding the structure of the datasets
glimpse(test)
glimpse(train) 

all_equal(test, train, ignore_col_order = F) #train has one additional column

names(test)
names(train) #Item_Outlet_Sales is the additional column, this is what we will be predicting later on

#Checking for continuous variables that might need to be imputed
summary(train)

#Replacing the 0's in Item Visibility with the average
#The dplyr method would be more readable
#Just practicing base R right now. In the next imputation I will use dplyr
train$Item_Visibility <- ifelse(train$Item_Visibility == 0, 
                                mean(train$Item_Visibility, na.rm = TRUE), 
                                train$Item_Visibility) 

#Item weight has 1463 NA values. Will replace these values with the average
train <- train %>% 
          mutate(Item_Weight = case_when(is.na(Item_Weight) ~ mean(Item_Weight, na.rm = TRUE),
                                         TRUE ~ Item_Weight))

#Check the summary of the train DS agagin
summary(train)


#EDA - Plotting target variable from the train dataset - Right skewed
train %>% 
  ggplot(aes(Item_Outlet_Sales)) + 
    geom_histogram(binwidth = 150, fill = "light green") +
    theme_minimal()


#EDA - Analyzing the actual stores to understand if any of its attributes affect sales

#What kind of stores are in the dataset?
#Supermarket Type 1 is the most prevalent store in the dataset
#It makes up 65% of stores!
train %>% 
  group_by(Outlet_Type) %>%
  tally() %>%
  mutate(percentage_of_type = n/sum(n))



#Since there are so many, are Supermarket Type 1 stores older?
#Creating a decade column for easier visualization
train <- train %>% 
  mutate(Outlet_Establishment_Decade = case_when(Outlet_Establishment_Year >= 1980 & Outlet_Establishment_Year <= 1989 ~ "80's",
                                                 Outlet_Establishment_Year >= 1990 & Outlet_Establishment_Year <= 1999 ~ "90's",
                                                 Outlet_Establishment_Year >= 2000 ~ "00's"))

train %>%
  ggplot(aes(Outlet_Type)) + 
  geom_bar(stat = "count", aes(fill = Outlet_Establishment_Decade )) +
  theme_minimal()

#Looks like this assumption was incorrect about Supermarket Type 1
#About 50% of stores were built in the 2000's which indicates that most Supermarket Type 1 stores are newer
train %>% 
  filter(Outlet_Type == "Supermarket Type1") %>% 
  group_by(Outlet_Establishment_Decade) %>%
  tally() %>%
  mutate(percentage_of_type = n/sum(n))



#Where are the stores located
train %>% 
  group_by(Outlet_Type, Outlet_Location_Type) %>%
  tally() %>%
  select(Outlet_Location_Type, Outlet_Type, n) %>%
  arrange(Outlet_Location_Type)

#Tier 2 has the highest proportion of Supermarket Type 1 stores
#Tier 3 has the most outlet types and the highest store count (which can mean there are more people/a more diverse population with more diverse needs)
train %>% 
  ggplot(aes(Outlet_Location_Type)) + 
  geom_bar(stat = "count", aes(fill = Outlet_Type)) + theme_minimal()



#Which location had the highest sales? And what type of stores
loc_typ_sales <- train %>% 
                  group_by(Outlet_Location_Type, Outlet_Type) %>% 
                  summarise(sum_of_sales = sum(Item_Outlet_Sales), 
                            avg_sales = mean(Item_Outlet_Sales))

#In Tier 3, Supermarket Type 3 dominates the market
#Overall Supermarket Type 1 encompasses most of the sales
loc_typ_sales %>% 
  ggplot(aes(sum_of_sales,Outlet_Location_Type)) + 
  geom_col(aes(fill = Outlet_Type)) +
  scale_x_continuous(labels = comma) +
  theme_minimal()


#Future Stores EDA: Explore store size

#EDA - Analyzing item fields to understand if any of its attributes affect sales

#Function to plot histogram
hist_plot <- function(ds, col, bw) {
  ggplot(data = ds, aes(x = {{col}})) + 
    geom_histogram(binwidth = bw, fill = "light blue" ) +
    theme_minimal()
}

weight_plot <- hist_plot(train, Item_Weight, 0.5)
vis_plot <- hist_plot(train, Item_Visibility, 0.005)
item_mrp_plot <- hist_plot(train, Item_MRP, 0.5)

#Visualizing item related continous variables
#It looks like there is no clear patter for Item_Weight
#Item Visibility is skewed to the right
#It looks like Item MRP has 4 ranges. Should explore this later
plot_grid(weight_plot, vis_plot, item_mrp_plot)



#Trying to understand the categorical item variables
train %>%
  distinct(Item_Fat_Content)

#There are multiple low fat and regular options, they all represent the same thing but they are just spelled differently
train <- train %>% 
          mutate(Item_Fat_Content = case_when(Item_Fat_Content == "low fat" | Item_Fat_Content == "LF" ~ "Low Fat",
                                              Item_Fat_Content == "reg" ~ "Regular",
                                              TRUE ~ Item_Fat_Content))

#The Item Type field does not need to be adjusted, there are no duplicates/typos
train %>%
  distinct(Item_Type)

#Creating an item_category field
train <- train %>%
          mutate(item_category = substr(Item_Identifier, 1, 2), .before = 1)

#Visualize count of sales by category
train %>% ggplot(aes(Item_Type, fill = Item_Type)) + 
            geom_bar() + 
            coord_flip() +
            facet_wrap(~item_category, ncol = 1) +
            theme_minimal()
            

#Sales distribution for the top 10 Item Types
top10_item_types <- train %>%
                    group_by(Item_Type) %>% 
                    summarise(total_sales = sum(Item_Outlet_Sales)) %>%
                    arrange(desc(total_sales)) %>%
                    slice(1:10)

#Took the square root of Item_Outlet_Sales to normalize the sales data

train %>%
  select(Item_Type,Item_Outlet_Sales) %>%
  filter(Item_Type %in% top10_item_types$Item_Type) %>% 
  ggplot(aes(sqrt(Item_Outlet_Sales))) +
  aes(fill = as.factor(Item_Type)) +
  geom_histogram(binwidth = 15, alpha = 0.5) +
  facet_wrap(~Item_Type, ncol = 5) +
  theme_minimal() 
  
train %>% 
  select(Item_Type,Item_Outlet_Sales) %>%
  filter(Item_Type %in% top10_item_types$Item_Type) %>% 
  ggplot(aes(Item_Type, sqrt(Item_Outlet_Sales))) +
  geom_boxplot() +
  theme_minimal()
  
    



 








