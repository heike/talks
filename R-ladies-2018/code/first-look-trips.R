library(readr)
q1 <- read_csv("~/Documents/Data/Bikesharing/2017/2017-Q1-Trips-History-Data.csv")
q2 <- read_csv("~/Documents/Data/Bikesharing/2017/2017-Q2-Trips-History-Data.csv")
q3 <- read_csv("~/Documents/Data/Bikesharing/2017/2017-Q3-Trips-History-Data.csv")
q4 <- read_csv("~/Documents/Data/Bikesharing/2017/2017-Q4-Trips-History-Data.csv")

trips <- rbind(q1,q2,q3,q4)
library(lubridate)
trips$Day <- wday(trips$`Start date`, label=TRUE, abbr=FALSE)
trips$hour <- hour(trips$`Start date`)
trips <- trips %>% mutate(
  Mdate = mday(`Start date`), 
  Month=month(`Start date`, label=TRUE, abbr=FALSE),
  Year=2017) 

saveRDS(trips, "data/trips-2017.rds")
trips %>% ggplot(aes(x = `Duration (ms)`/1000/60)) + geom_histogram(binwidth=15) + 
  xlim(c(0, 500))


trips %>% ggplot(aes(y = `Start station number`, x = hour)) + facet_wrap(~startday) +
  geom_point(alpha=0.1)

stations.summary <- trips %>% group_by(`Start station number`) %>% tally() %>% arrange(desc(n))


hourlies <- trips %>% 
  group_by(Year, Month, Mdate, Day, hour, `Member type`) %>%
  summarise(
    Hourly_counts = n(),
    Date = as.Date(`Start date`)[1]
  )
library(sugrrants)
calendar_df <- hourlies %>% filter(`Member type`=="Member") %>%
  ungroup() %>%
  mutate(
    Weekend = if_else(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  ) %>%
  frame_calendar(
    x = hour, y = Hourly_counts, date = Date, calendar = "monthly"
  ) 

p <- calendar_df %>% 
  ggplot(aes(x = .hour, y = .Hourly_counts, group = Date, colour = Weekend)) +
  geom_line() + 
  theme_bw() +
  theme(legend.position = "bottom")
prettify(p, label.padding = unit(0.08, "lines"))


hourlies <- trips %>% 
  group_by(`Start station number`, `Start station`, Year, Month, Mdate, Day, hour) %>%
  summarise(
    Hourly_counts = n(),
    Date = as.Date(`Start date`)[1]
  )
library(sugrrants)
calendar_df <- hourlies %>% filter(`Start station number` %in% c(31623, 31258, 31247)) %>%
  ungroup() %>%
  mutate(
    Weekend = if_else(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  ) %>%
  frame_calendar(
    x = hour, y = Hourly_counts, date = Date, calendar = "monthly"
  ) 

p <- calendar_df %>% 
  ggplot(aes(x = .hour, y = .Hourly_counts, 
             group = interaction(Date, `Start station number`), 
             colour = `Start station`)) +
  geom_line() + 
  theme_bw() +
  theme(legend.position = "bottom")
prettify(p, label.padding = unit(0.08, "lines"))



hourlies %>% ggplot(aes(x = hour, y = Hourly_counts)) +
  geom_line(aes(group=Date)) +
  facet_wrap(~Day)



calendar_df %>%
  ggplot(aes(x = .hour, y = .Hourly_counts, group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
