library(tidyverse)
library(gganimate)
library(gifski)

boty_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/BOTY-votes-2019.csv")


# functions -------------------------------------

count_votes <- function(df) {
  #summarises raw data into totals for the 1st choice votes
  
  df %>% 
    group_by(bird_breed, vote_rank) %>%
    summarise(votes = n()) %>%
    ungroup() %>%
    filter(vote_rank == 'vote_1') %>%
    arrange(desc(votes)) %>%
    select(-vote_rank)
}


is_winning_bird <- function(df) {
  #returns TRUE if one bird has over 50% of the vote
  
  num_votes_winner <- df %>%
    top_n(1) %>% .$votes
  
  total_votes <- df %>% 
    .$votes %>% sum()
  
  if (num_votes_winner > (total_votes/2)) {
    return(TRUE)
  } else {return(FALSE)}
}


losing_bird <- function(df, round) {
  #idendifies the bird with least votes
  
  df %>%
    arrange(votes) %>%
    filter(row_number()==1)
}


count_next_round_votes <- function(df, losing_bird) {
  #counts the second choice voices for the bird who came last
  
  losing_ids <- df %>%
    filter(bird_breed == losing_bird, vote_rank == 'vote_1') %>%
    .$voter_ID
           
  df %>%
    filter(voter_ID %in% losing_ids, vote_rank == 'vote_2') %>%
    group_by(bird_breed) %>%
    summarise(second_votes = n())
}
  

redistribute_loser_votes <- function(df) {
  # reallocates 2nd choice votes from the losing bird to the other birds
  
  loser <- df %>% losing_bird()
  
  votes_to_add <- boty_raw_long %>%
    count_next_round_votes(loser$bird_breed)
  
  new_vote_count <- df %>%
    left_join(votes_to_add) %>%
    mutate(votes = rowSums(.[,c("votes","second_votes")], na.rm = TRUE)) %>%
    select(-second_votes) %>%
    filter(!bird_breed == loser$bird_breed)
  
  return(new_vote_count)
  
}



# Analysis -------------------------------------

# make raw data long, and record the voter ID
boty_raw_long <- boty_raw %>%
  select(-country) %>%
  mutate(voter_ID = row_number()) %>%
  pivot_longer(cols = c(vote_1:vote_5), names_to = "vote_rank", values_to = "bird_breed") 

# tally up the fist choice votes
vote_count <- boty_raw_long %>%
  count_votes()

# define an index for iteration - used in the loop below
i <- 1

# create a df to save all vote iterations. The original 1st choices form the first iteration
vote_counts_all <- vote_count %>% mutate(vote_iteration = i)

# reallocate votes from the losing bird, until theres a majority
while (!vote_count %>% is_winning_bird) {
  
  i <- i+1
  
  vote_count <- vote_count %>%
    redistribute_loser_votes()
  
  #add the new vote count to the big dataframe (and specifiy the vote iteration)  
  vote_counts_all <- bind_rows(vote_counts_all, vote_count %>% mutate(vote_iteration = i))
}


# visualisation -------------------------------------------------

# rank the birds within each vote iteration 
vote_counts_all <- vote_counts_all %>%
  group_by(vote_iteration) %>%
  mutate(rank_in_iteration = as.integer(rank(-votes))) %>%
  mutate(votes_label = paste0(" ", votes)) %>%
  ungroup()



# ggplot/gganimate template from:
# https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da
  
#first define a static plot with all vote_iterations
staticplot <- ggplot(vote_counts_all_annimate, aes(rank_in_iteration, group = bird_breed, 
                                       fill = as.factor(bird_breed), color = as.factor(bird_breed))) +
  geom_tile(aes(y = votes/2,
                height = votes,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(bird_breed, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=votes,label = votes_label, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=27, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=20, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))



# produce an animation from the static plot with gganimate 
animation <- staticplot + transition_states(vote_iteration, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'NZ Bird of The Year - Vote iteration: {closest_state}',  
       subtitle  =  "Yellow-eyed penguin wins a majority after second choices are reallocated for the 84 lowest scoring birds",
       caption  = "TidyTuesday week 47")

#save plot
animate(animation, 200, fps = 7,  width = 1200, height = 1350, detail = 20, 
        renderer = gifski_renderer("tidy_tuesday_week47.gif"))



