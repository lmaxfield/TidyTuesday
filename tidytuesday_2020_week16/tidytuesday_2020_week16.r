
library(tidyverse)
library(extrafont)
#library(here)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

my_theme <- function() {
  fun <- theme(text = element_text(family = "Roboto Condensed"),
               rect = element_blank(),
               legend.position = 'none',
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "grey"), 
               panel.grid.minor.y = element_blank(),
               axis.ticks.x = element_line(colour = "grey"),
               axis.ticks.y = element_blank(),
               axis.title = element_text(size = 12),
               plot.title = element_text(size = 14),
               axis.title.y = element_blank()
  )
}



top_artists <- rankings %>%
  group_by(artist) %>%
  summarise(total_points = sum(points), num_songs = n()) %>%
  mutate(points_per_song = total_points/num_songs) %>%
  arrange(desc(total_points)) %>%
  head(10)





rankings %>%
  select(ID, title, artist, year, points) %>%
  right_join(top_artists, by = c("artist")) %>%
  arrange(artist, desc(points)) %>%
  group_by(artist) %>%
  mutate(artist_song_number = row_number()) %>%
  ggplot(aes(x=reorder(artist, (.$total_points)), y=points)) +
  geom_bar(aes(fill=reorder(artist_song_number, (points))), stat = "identity", colour = 'grey') +
  scale_fill_viridis_d(direction = -1) +
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, 'foo', ' '), width = 20)) +
  labs(title = "Hip Hop Artists with Top Ranking Songs",
       subtitle = "Spot the one-hit-wonder") +
  ylab('Points awarded by critics') +
  annotate('text', "JAY-Z", 120, label = "16 JAY-Z songs \n were voted for!", size = 2.5, family = "Roboto Condensed") +
  my_theme() +
  coord_flip() +
  theme(axis.title.y = element_blank())

#ggsave(filename = here("plots/tidy_tuesday_2020_week16.png"), width = 14, height = 11, units = "cm")



