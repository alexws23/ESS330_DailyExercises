#Alex Smilor
#ESS330
#Daily Exercise 7

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)

#Question 1
covid %>% 
  filter(date == max(date)) %>%
  group_by(state) %>% 
  summarise(cases=sum(cases)) %>% 
  arrange(desc(cases))

covidstate <- covid %>% 
  aggregate(cases ~ state + date, FUN=sum)

ggplot1 <- covidstate %>% 
  filter(state %in% c("California","Texas","Florida","New York","Illinois","Pennsylvania")) %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = state)) +
  labs(title = "Cumulative Covid-19 Cases From Feb. 22, 2023 to Mar. 23, 2023",
       x = "Date",
       y = "Cumulative Covid-19 Cases",
       subtitle = 'Data Source: NY Times',
       color = "") +
  facet_wrap(~state, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(colour = "white")
  )

ggsave(ggplot1, file = "state-covid-cases.png")

#Question 2
ggplot2 <- covid %>% 
  group_by(date) %>% 
  summarise(cases = sum(cases)) %>% 
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  labs(title = "Cumulative Covid-19 Cases From Feb. 22, 2023 to Mar. 23, 2023",
       x = "Date",
       y = "Cumulative Covid-19 Cases",
       subtitle = 'Data Source: NY Times',
       color = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(colour = "white")
  )

ggsave(ggplot2, file = "country-covid-cases.png")

