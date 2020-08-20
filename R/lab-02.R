#Geog 176A Lab 2

library(tidyverse)
library(readr)


library(zoo)
library(readxl)

Pop = read_excel("data/lab-02-materials 2/PopulationEstimates.xls", skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt, Area_Name)

housing_prices = read_csv("data/lab-02-materials 2/landdata-states.csv")

covid_data = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")



#Question 1
library(tidyverse)
library(knitr)
library(readxl)
library(zoo)
library(ggthemes)
library(scales)


q1subset = inner_join(Pop, covid_data,by = 'fips') %>%
  filter(state == "California") %>%
  group_by(county) %>%
  mutate(daily_new_cases = cases - lag(cases)) %>%
  ungroup()


mostcases = q1subset %>%
  filter(date == max(date)) %>%
  slice_max(cases, n = 5)

knitr::kable(mostcases, caption = paste("California", 'counties with the most cumulative cases'), col.names = c('County', 'Cases'))


mostcases_new = q1subset %>%
  filter(date == max(date)) %>%
  slice_max(daily_new_cases, n = 5)

knitr::kable(mostcases_new, caption = paste("California", 'counties with the most new cases'), col.names = c('County', 'New Cases'))



last14days = q1subset %>%
  filter(date == max(date) - 13) %>%
  group_by(county, pop2019) %>%
  summarise(daily_new_cases = sum(daily_new_cases)) %>%
  ungroup() %>%
  mutate(casePer100000 = daily_new_cases / (pop2019 / 100000)) %>%
  filter(casePer100000 <= 100)




last14days = q1subset %>%
  filter(date == max(date) - 13) %>%
  group_by(county, pop2019) %>%
  summarise(daily_new_cases = sum(daily_new_cases, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(casePer100000 = daily_new_cases / (pop2019 / 100000)) %>%
  filter(casePer100000 <= 100)

knitr::kable(last14days, caption = c("Pop Lowest Cases in CA Counties"), col.names = c('County', 'NLowest New Cases with Pop'))



last14days = q1subset %>%
  group_by(county) %>%
  summarise(totNewcases = sum(daily_new_cases, na.rm = TRUE) / (max(pop2019) / 100000)) %>%
  filter(totNewcases <= 100)

knitr::kable(last14days, caption = c("Pop Lowest Cases in CA Counties"), col.names = c('County', 'Lowest New Cases with Pop'))




#Question 2

fourStates = covid_data %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  group_by(state, date) %>%
  summarise(totalcases = sum(cases)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newCases = totalcases - lag(totalcases)) %>%
  mutate(roll7 = zoo::rollmean(newCases, 7, fill = NA, allign = "right")) %>%
  ungroup()

ggplot(data = fourStates, aes(x = date)) +
  geom_col(aes(y = newCases), col = NA, fill = "pink") +
  geom_line(aes(y = roll7)) +
  ggthemes::theme_economist() +
  labs(title = paste("Daily New Cases in NY, CA, LA, FL")) +
  facet_wrap(~state) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10)) +
  theme(aspect.ratio  = .5)

library(tidyverse)


fourStates_pc = covid_data %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  left_join(Pop, by = c("state" = "Area_Name")) %>%
  group_by(state, date) %>%
  summarise(totalcases = sum(cases), pop2019 = pop2019[1]) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newCases = (totalcases - lag(totalcases)) / pop2019) %>%
  mutate(roll7 = (zoo::rollmean(newCases, 7, fill = NA, allign = "right"))) %>%
  ungroup()


ggplot(data = fourStates_pc, aes(x = date)) +
  geom_col(aes(y = newCases), col = NA, fill = "pink") +
  geom_line(aes(y = roll7)) +
  ggthemes::theme_economist() +
  labs(title = paste("Daily New Cases in NY, CA, LA, FL")) +
  facet_wrap(~state) +
  theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 10)) +
  theme(aspect.ratio  = .5)







































#Question 3


































p2 = Pop %>%
  select(fips = "FIPStxt", state = "State", pop2019 = "POP_ESTIMATE_2019") %>%
  group_by(state) %>%
  slice_max(pop2019, n = 1)


dim(Pop)
names(Pop)


names(home)
fivenum(home$Land.Value)

home %>%
  filter(State %in% c("HI", "TX", "AL")) %>%
  group_by(State) %>%
  summarize(minLV = min(Land.Value),
            meanLV = mean(Land.Value),
            maxLV = max(Land.Value))

home %>%
  filter(State %in% c("HI", "TX", "AL")) %>%
  ggplot(aes(x = Date, y = Land.Value)) +
  geom_line(aes(color = State))



