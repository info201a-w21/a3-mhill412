library(ggplot2)
library(dplyr)
library(tidyr)
library(gcookbook)
library(leaflet)
library(tigris)

##Calling the csv file to a main variable 'incarc_main'

incarc_main <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

incarcdata <- incarc_main[c(2,4,6,10:14,21,28:33)]

##This first section will calculate the values that I am citing in my
## introduction

#Average percent white jail population in most recent year

percentwhitejail <- incarcdata[c(1,9,14)] %>% 
  filter(year == '2018')
percentwhitejail2018 <- sum(percentwhitejail[c(3)], na.rm = TRUE)/
  sum(percentwhitejail[c(2)], na.rm = TRUE) * 100

#Average percent black jail population in most recent year

percentblackjail <- incarcdata[c(1,9,11)] %>% 
  filter(year =='2018')
percentblackjail2018 <- sum(percentblackjail[c(3)], na.rm = TRUE)/
  sum(percentblackjail[c(2)], na.rm = TRUE) * 100

#This creates a vector that holds two values, percentage of whites incarcerated 
# versus white population and percentage of blacks incarcerated versus black 
# population ages 15-64.

percent_incarcerated_dataframe <- incarcdata[c(1,5,8,11,14)] %>%
  filter(year == '2018')
percent_incarcerated <- c(sum(percent_incarcerated_dataframe$black_jail_pop, na.rm = TRUE)/
                           sum(percent_incarcerated_dataframe$black_pop_15to64, na.rm = TRUE) * 100,
                         sum(percent_incarcerated_dataframe$white_jail_pop, na.rm = TRUE)/
                           sum(percent_incarcerated_dataframe$white_pop_15to64, na.rm = TRUE) * 100)

#This calculates the difference in jail population from 1978 to 2018, stored in 
# variable jail_pop_diff.

jail_pop_diff_1978 <- incarcdata %>% filter(year == '1978') 
jail_pop_diff_1978 <- sum(jail_pop_diff_1978$total_pop, na.rm = TRUE)
jail_pop_diff_2018 <- incarcdata %>% filter(year == '2018')
jail_pop_diff <- sum(jail_pop_diff_2018$total_pop) - jail_pop_diff_1978

##This marks the end of section 1: Introduction

##This next section will deal with the trends over time chart

# For my trends of time graph, I decided to compare the percentage of the population 
# ages 15 to 64 that end up in jail of black men and white men from 1990 to 2018.

trend_graph_data <- select(incarcdata, 1:3, 5, 8, 11, 14) %>% 
  filter(year >= 1988, black_pop_15to64 >= 3000, black_jail_pop > 300) %>%
  mutate(jailrate_black = black_jail_pop/black_pop_15to64 *100) %>% 
  mutate(jailrate_white = white_jail_pop/white_pop_15to64 *100) %>%
  group_by(year) %>% summarise(BlackMean = mean(jailrate_black),
                               WhiteMean = mean(jailrate_white))

#Visualization
time_trend_plot <- ggplot(trend_graph_data, aes(x = year)) + ggtitle("Rate of Jailing") +
  geom_line(aes(y = BlackMean), color = "darkred") +
  geom_line(aes(y = WhiteMean), color = "steelblue") +
  labs(x = "Year", y = "Percent of Population in Jail", color = "Legend") +
  scale_color_manual(name = "Legend", breaks = c("African American", "White"), 
                     values = c("darkred", "steelblue"))

#This marks the end of section 2: Trends over time plot

##This section is related to the plot of the comparison of two continuous variables

#For my comparison of two continuous variables, I chose to compare the rate of 
# jailing for African Americans and the rate of jailing for whites over time.

#Visualization

ggplot(trend_graph_data, aes(x = BlackMean)) + ggtitle("% Blacks going to Jail vs White") +
  geom_line(aes(y = WhiteMean), color = "cyan", size = 1.5) +
  labs(x = "% of African Americans Going to Jail", y = "% of Whites Going to Jail")
                             
##This marks the end of section 2: Comparing two continuous variables.

##This next section will create a map displaying where differences in jailing
#rates are the most severe in the US

#This code sets up my data to use leaflet to make it into a map.

leaflet_data <- select(incarcdata, 1:3, 5, 8, 11, 14) %>% group_by(state) %>%
  summarise(state_black_pop = sum(black_pop_15to64, na.rm = TRUE), 
            state_white_pop = sum(white_pop_15to64, na.rm = TRUE),
            state_black_jail = sum(black_jail_pop, na.rm = TRUE),
            state_white_jail = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(white_state_rate = state_white_jail/state_white_pop *100)%>%
  mutate(black_state_rate = state_black_jail/state_black_pop *100)%>%
  mutate(state_rate_diff = black_state_rate-white_state_rate)%>%
  select(1, 8)

#Creating the map

states <- states(cb=T)
states_merged <- geo_join(states, leaflet_data, "STUSPS", "state")

pal <- colorNumeric("Blues", domain = states_merged$state_rate_diff)
popup <- paste0("Rate Difference: ", as.character(states_merged$state_rate_diff))

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  setView(-98.48333, 38.712046, zoom = 4) %>%
  addPolygons(data = states_merged, fillColor = ~pal(states_merged$state_rate_diff),
              fillOpacity = 0.8, weight = 0.3, smoothFactor = 0.5, popup = ~popup) %>%
  addLegend(pal = pal, values = states_merged$state_rate_diff, position = "bottomleft",
            title = "Rate of Jailing Difference: Black vs White (%)")

#This marks the end of section 3: map of US states based of their difference
#between white jailing and black jailing 





