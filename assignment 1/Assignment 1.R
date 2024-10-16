#Loading libraries and data
library(ggplot2)
library(dplyr)
library(maps)
library(readr)
library(tidyverse)
library(gganimate)
library(gifski)

#DATA PREPERATION


url <- "https://www.dropbox.com/scl/fi/9d6ctufur3g72nfdtneea/countypres_2000-2020.csv?rlkey=vm0mtz12wgh6qxsgf7ops6pjx&dl=1"
election_data <- read_csv(url)

#Checking column names for coding
colnames(election_data)

#Focusing on 2020 election data
election_data_2020 <- election_data %>%
  filter(year >= 2000 & year <= 2020)

#Grouping data by state, county_name and party and calculate the total votes for each party
county_party_totals <- election_data_2020 %>%
  group_by(state, county_name, party) %>%
  summarise(total_votes = sum(candidatevotes), .groups = 'drop')

#the winning party and candidate in each county
county_winners <- county_party_totals %>%
  group_by(state, county_name) %>%
  slice_max(order_by = total_votes, n = 50) %>%
  ungroup()

view(county_winners)




#STATE LEVEL VISUALISATION 2020


#State Winners Map


url <- "https://www.dropbox.com/scl/fi/9d6ctufur3g72nfdtneea/countypres_2000-2020.csv?rlkey=vm0mtz12wgh6qxsgf7ops6pjx&dl=1"
election_data <- read_csv(url)

#Focusing on 2020 election data
# lowercase state names
election_data_2020 <- election_data %>%
  filter(year == 2020) %>%
  group_by(state, party) %>%
  summarise(total_votes = sum(candidatevotes)) %>%
  slice(which.max(total_votes)) %>%
  ungroup() %>%
  mutate(state = tolower(state))  # Convert state names to lowercase

# state boundaries
us_states <- map_data("state")

# merge election results with state boundaries
state_results <- us_states %>%
  left_join(election_data_2020, by = c("region" = "state"))

# centroids for each state to use for labels
state_centroids <- state_results %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)))

# state-level map, coloring by the winning party and adding state labels
ggplot(state_results, aes(map_id = region, fill = party)) +
  geom_map(map = us_states, aes(fill = party), color = "white") +
  expand_limits(x = us_states$long, y = us_states$lat) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red")) +
  labs(title = "2020 U.S. Presidential Election Results", fill = "Party") +
  theme_minimal() +
  theme_void() +
  coord_fixed(1.3) +
  # Add state labels, ensuring geom_text() does not inherit party aesthetics -- asked gpt - state labels were not working initially
  geom_text(data = state_centroids, aes(x = long, y = lat, label = toupper(region)), inherit.aes = FALSE, size = 1, color = "black")






#Republican Vote Share Map

election_data_2020 <- election_data %>%
  filter(year == 2020)

# total votes and the Republican vote share by state
rep_vote_share <- election_data_2020 %>%
  group_by(state, party) %>%
  summarise(total_votes = sum(candidatevotes), .groups = 'drop') %>%
  pivot_wider(names_from = party, values_from = total_votes, values_fill = 0) %>%
  mutate(rep_vote_share = REPUBLICAN / (DEMOCRAT + REPUBLICAN + OTHER)) %>%  # Calculate Republican vote share
  select(state, rep_vote_share)

# lowercase state names
rep_vote_share <- rep_vote_share %>%
  mutate(state = tolower(state))

# us state boundaries
us_states <- map_data("state")

# merge the republican vote share data with the state boundaries data
state_results_rep <- us_states %>%
  left_join(rep_vote_share, by = c("region" = "state"))

# centroids for each state for labels
state_centroids <- state_results %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat)))

# Map
ggplot(state_results_rep, aes(map_id = region, fill = rep_vote_share)) +
  geom_map(map = us_states, aes(fill = rep_vote_share), color = "white") +
  expand_limits(x = us_states$long, y = us_states$lat) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray") +  # Color gradient from white to red
  labs(title = "2020 U.S. Election: Republican Votes by State", fill = "Vote Share") +
  theme_void() +
  coord_fixed(1.3) +
  # Add state labels
  geom_text(data = state_centroids, aes(x = long, y = lat, label = toupper(region)), inherit.aes = FALSE, size = 1, color = "black")





#COUNTY LEVEL VISUALISATION 2000-2020. --- no access to copilot to help (still not gone through), had gpt help me for most of this 



# 2000-2020 county level results
election_data <- election_data %>%
  select(year, county_name, party, candidatevotes, totalvotes) %>%
  mutate(county_vote_shares = candidatevotes / totalvotes * 100) 

# winners in each county
county_results <- election_data %>%
  group_by(year, county_name) %>%
  filter(candidatevotes == max(candidatevotes)) %>%
  select(year, county_name, party) %>%
  mutate(county_name = tolower(county_name))


# Merge map data with county results data frame
county_map_data <- map_data("county") %>%
  inner_join(county_results, relationship = "many-to-many", by = c("subregion" = "county_name")) %>%.    #many-to-many to stop errors
  filter(!is.na(party))  # Remove rows with missing party information

# Plot map
ggplot(county_map_data, aes(x = long, y = lat, group = group, fill = party)) +
  geom_polygon(color = "black", size = 0.05) +
  scale_fill_manual(values = c("DEMOCRAT" = "cornflowerblue", "REPUBLICAN" = "red"), na.value = "grey50") +
  theme_void() +
  labs(title = "US Election Results by County",
       fill = "Party") +
  theme(
    legend.title = element_text(size = 8),  # legend title size
    legend.text = element_text(size = 6),   # legend text size
    plot.title = element_text(hjust = 0.5, size = 12)  # Center and  title size
  ) +
  facet_wrap(~year, ncol = 3) +  # Spread over multiple rows to adjust the number of columns
  coord_fixed(1.3) +  
  theme(panel.spacing = unit(0.5, "lines"))  # space between panels




#Animated U.S. Election Map (2000-2020)

animated_map <- ggplot(county_map_data, aes(x = long, y = lat, group = group, fill = party)) +
  geom_polygon(color = "black", size = 0.05) +
  scale_fill_manual(values = c("DEMOCRAT" = "cornflowerblue", "REPUBLICAN" = "red"), na.value = "grey50") +
  theme_void() +
  labs(title = "US Election Results by County (2000-2020)",
       fill = "Party") +
  theme(
    legend.title = element_text(size = 8),  
    legend.text = element_text(size = 6),   
    plot.title = element_text(hjust = 0.5, size = 12)  
  ) +
  transition_states(year, transition_length = 5, state_length = 5) +
  enter_fade() +
  exit_fade() +
  coord_fixed(1.3) +  
  theme(panel.spacing = unit(0.5, "lines")) +  
  ggtitle('US Election Results by County: {closest_state}')  

# Animate and save the gif
animate(animated_map, nframes = 100, fps = 20, duration = 5, renderer = gifski_renderer("Animated U.S. Election Map (2000-2020).gif"))



#all images (and code) saved onto github
