library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")


incarceration_df <- get_data()

## Section 2  ----
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# Which state has the highest average number of LatinX population incarcerated as of 2018?
highest_avg_latinx_pop <- incarceration_df %>%
  select(state, latinx_jail_pop, year) %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarise(avg_latinx_pop = sum(latinx_jail_pop, na.rm = TRUE) / n()) %>%
  filter(avg_latinx_pop == max(avg_latinx_pop)) %>%
  pull(avg_latinx_pop)
# California, pulls average latinx population number


#Finds the average number of white people incarcerated in California. Supporting 
#data for question 1. 
highest_avg_white_pop <- incarceration_df %>%
  select(state, white_jail_pop, year) %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarise(avg_white_pop = sum(white_jail_pop, na.rm = TRUE) / n()) %>%
  filter(state == "CA") %>%
  filter(avg_white_pop == max(avg_white_pop)) %>%
  pull(avg_white_pop)

# Which state has the highest average number of AAPI population incarcerated in 2018?
highest_avg_aapi_pop <- incarceration_df %>%
  select(state, aapi_jail_pop, year) %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarise(avg_aapi_pop = sum(aapi_jail_pop, na.rm = TRUE) / n()) %>%
  filter(avg_aapi_pop == max(avg_aapi_pop)) %>%
  pull(avg_aapi_pop)
#Louisiana, pulls highest average number of AAPI population. 

# Which state has the highest rate of Black population incarcerated in 2018?
highest_rate_black_pop <- incarceration_df %>%
  select(state, black_jail_pop, year, total_pop) %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarise(rate_black_pop = (sum(black_jail_pop, na.rm = TRUE) / sum(total_pop, na.rm = TRUE)) * 100) %>%
  filter(rate_black_pop == max(rate_black_pop)) %>%
  pull(rate_black_pop)


# Supporting data for question 3
highest_rate_white_pop <- incarceration_df %>%
  select(state, white_jail_pop, year, total_pop) %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarise(rate_white_pop = (sum(white_jail_pop, na.rm = TRUE) / sum(total_pop, na.rm = TRUE)) * 100) %>%
  filter(state == "LA") %>%
  pull(rate_white_pop)


## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function gets the jail population for all years 1970-2018.
get_year_jail_pop <- function() {
  jail_data <- incarceration_df %>%
    select(year, total_jail_pop) %>%
    group_by(year) %>%
    drop_na(total_jail_pop) %>%
    summarise(total_population_jailed = sum(total_jail_pop))
  return(jail_data)
}

# This function plots a bar graph for the jail population for the of 1970-2018.
plot_jail_pop_for_us <- function() {
  total_jailed_plot <- ggplot(data = get_year_jail_pop(), aes(x = year, y = total_population_jailed)) +
    geom_col() +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(total_jailed_plot)
}



## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
#----------------------------------------------------------------------------#

#This function gets the total jail population for each state from 1970-2018 to display growth.
get_jail_pop_by_states <- function(states) {
  jail_data <- incarceration_df %>%
    select(year, state, total_jail_pop) %>%
    drop_na() %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(total_states_jailed = sum(total_jail_pop))
  return(jail_data)
}

#This function plots a line chart that shows the growth of jail population for 
#the desired states in the parameter. 
plot_jail_pop_by_states <- function(states) {
  total_states_jailed <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_states_jailed, color = state)) +
    labs(
      title = "Increase of Jail Population in U.S. by State (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(total_states_jailed)
}


## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#
# What county has the highest average Black jail population vs their White Population?

#Finds the number for the highest average Black jail population in a county.
highest_black_pop_county <- incarceration_df %>%
  select(county_name, black_jail_pop, total_pop) %>%
  group_by(county_name) %>%
  summarise(black_pop_county = sum(black_jail_pop, na.rm = TRUE) / n()) %>%
  filter(black_pop_county == max(black_pop_county)) %>%
  rename("pop" = "black_pop_county") %>%
  mutate(named_county = paste0(county_name, " (Black Incarcerated Population)"))
# New York County

#Gets the white jailed population in New York County to compare to Black population.
white_pop_county <- incarceration_df %>%
  select(county_name, white_jail_pop, total_pop) %>%
  group_by(county_name) %>%
  summarise(white_pop_county = sum(white_jail_pop, na.rm = TRUE) / n()) %>%
  filter(county_name == "New York County") %>%
  rename("pop" = "white_pop_county") %>%
  mutate(named_county = paste0(county_name, " (White Incarcerated Population)"))

#Plots a bar chart showing the comparison of Black incarcerated population vs
#White incarcerated population in New York County. 
plotted <- ggplot(NULL, aes(named_county, pop)) +
  geom_col(data = highest_black_pop_county, width = 0.4) +
  geom_col(data = white_pop_county, width = 0.4) +
  scale_y_continuous(limits = c(0, 5400), breaks = seq(0, 5400, 1000)) +
  labs(
    title = "Population of Black vs White Incarcerated",
    x = "New York County, NY",
    y = "Jail Population"
  )


## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
#----------------------------------------------------------------------------#
# Ratio of Black/White Jailed Population

#Get the ratio of Black to White jailed population for each state.
get_jail_pop_black_white <- incarceration_df %>%
  select(state, black_jail_pop, white_jail_pop) %>%
  group_by(state) %>%
  summarise(ratio = sum(black_jail_pop, na.rm = TRUE) / sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(state = tolower(state.name[match(state, state.abb)]))

#Omit na values. 
get_jail_pop_black_white <- na.omit(get_jail_pop_black_white)

state_shape <- map_data("state") %>%
  rename("state" = "region") %>%
  left_join(get_jail_pop_black_white, by = "state")

#Plots the Black to White jailed population ratio values for each state in the map. 
plot_ratio <- ggplot(data = state_shape) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ratio),
    color = "white"
  ) +
  labs(
    title = "Ratio of Black to White Population Jailed Per State",
  )
