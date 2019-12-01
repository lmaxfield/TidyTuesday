library(tidyverse)
library(gganimate)
library(scales)
library(gifski)


loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")


loans.summary <- loans %>% 
  mutate(year = as.numeric(paste("20", year, sep = ""))) %>%
  mutate(year.quarter = (year+(quarter/4)-0.0001)) %>%
  group_by(year.quarter) %>%
  summarise(starting = sum(starting, na.rm = TRUE)) %>%
  mutate(row_id = row_number())

ylab <- c(0, 30, 60, 90, 120)

a <- ggplot(loans.summary, aes(x = year.quarter, y = starting)) + 
  geom_line() +
  scale_y_continuous(labels = paste0("$", ylab, " M"), breaks = (10^9 * ylab), limits = c(0,1.2E+11)) +
  expand_limits(y = 0) +
  theme_minimal() + 
  theme(axis.title.x = element_blank()) +
  ylab("Total Debt") +
  transition_reveal(along = year.quarter) +
  view_static() +
  labs(title = 'The Rise of Student Debt in the USA',
       caption  = "#TidyTuesday week 48")



animate(a, nframes = 50, renderer = gifski_renderer("tidy_tuesday_week48.gif"))
