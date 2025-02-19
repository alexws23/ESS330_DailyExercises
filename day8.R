library(tidyverse)
library(usethis)
library(lubridate)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
url2 = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covidtotal = read_csv(url2)

stateregion <- data.frame(region = state.region,
                          state = state.name)
print(stateregion)


covidregion <- inner_join(covidtotal,stateregion,by = "state") #join that only includes elements in both tables. In this case filters out US territories since they don't have an assigned region
head(covidregion)

regiondate <- aggregate(covidregion[,5:6],by=list(covidregion$region, covidregion$date), sum) %>% 
  rename(
    region = Group.1,
    date = Group.2
  ) %>% 
  pivot_longer(cols = c("cases","deaths"))
head(regiondate)

exercise8plot <- regiondate %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(colour = region)) +
  facet_grid(name~region, scales = "free_y") +
  labs(title = "Cumulative Covid-19 Cases by Region",
       x = "Date",
       y = "Cumulative Covid-19 Cases",
       subtitle = 'Data Source: NY Times',
       color = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(colour = "white")
  ) + 
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+ 
  scale_y_continuous(labels = scales::comma)

ggsave(exercise8plot, file = "imgs/region-covid-cases.png")


