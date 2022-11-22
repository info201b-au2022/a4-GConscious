library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

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

# Which state has the highest average number of AAPI population incarcerated in 2018?
highest_avg_aapi_pop <- incarceration_df %>% 
  select(state, aapi_jail_pop, year) %>% 
  group_by(state) %>% 
  filter(year == 2018) %>% 
  summarise(avg_aapi_pop = sum(aapi_jail_pop, na.rm = TRUE) / n()) %>% 
  filter(avg_aapi_pop == max(avg_aapi_pop)) %>% 
  pull(avg_aapi_pop)

# Which state has the highest rate of Black population incarcerated in 2018?
highest_rate_black_pop <- incarceration_df %>% 
  select(state, black_jail_pop, year, total_pop) %>% 
  group_by(state) %>% 
  filter(year == 2018) %>% 
  summarise(rate_black_pop = (sum(black_jail_pop, na.rm = TRUE) / sum(total_pop, na.rm = TRUE)) * 100) %>% 
  pull(rate_black_pop)

#Supporting data
highest_rate_white_pop <- incarceration_df %>% 
  select(state,white_jail_pop, year, total_pop) %>% 
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
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  jail_data <-incarceration_df %>% 
  select(year, total_jail_pop) %>% 
  group_by(year) %>%
  drop_na(total_jail_pop) %>% 
  summarise(total_population_jailed = sum(total_jail_pop))
return(jail_data)   
}

View(get_year_jail_pop())
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  total_jailed_plot <- ggplot(data = get_year_jail_pop(), aes(x = year, y = total_population_jailed)) +
    geom_col() +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)", 
      x = "Year",
      y = "Total Jail Population"
    )
  return(total_jailed_plot)   
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states){
  jail_data <- incarceration_df %>% 
    select(year, state, total_jail_pop) %>% 
    drop_na() %>% 
    filter(state %in% states) %>% 
    group_by(year, state) %>% 
    summarise(total_states_jailed = sum(total_jail_pop))
  return(jail_data)
}


plot_jail_pop_by_states <- function(states){
  total_states_jailed <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_states_jailed, color = state)) + 
    labs(
      title = "Increase of Jail Population in U.S. by State (1970-2018)",
      x = "Year",
      y = "Total Jail Population"
    )
  return(total_states_jailed)
}

plot_jail_pop_by_states(c("WA", "NJ", "FL"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#County with the highest average Black jail population vs their White Population


highest_black_pop_county <- incarceration_df %>% 
  select(county_name, black_jail_pop, total_pop) %>% 
  group_by(county_name) %>% 
  summarise(black_pop_county = sum(black_jail_pop, na.rm = TRUE) / n()) %>% 
  filter(black_pop_county == max(black_pop_county)) %>% 
  rename("pop" = "black_pop_county") %>% 
  mutate(named_county = paste0(county_name, " (Black Incarcerated Population)"))

  #New York County

white_pop_county <- incarceration_df %>% 
  select(county_name, white_jail_pop, total_pop) %>% 
  group_by(county_name) %>% 
  summarise(white_pop_county = sum(white_jail_pop, na.rm = TRUE) / n()) %>% 
  filter(county_name == "New York County") %>% 
  rename("pop" = "white_pop_county") %>% 
  mutate(named_county = paste0(county_name, " (White Incarcerated Population)"))


plotted <- ggplot(NULL, aes(named_county, pop)) + 
  geom_col(data = highest_black_pop_county) +
  geom_col(data = white_pop_county) +
  scale_y_continuous()
  labs()


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


