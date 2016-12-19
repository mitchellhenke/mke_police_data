library(tidyverse)
calls <- read_csv("file.csv", col_types = cols(time = col_datetime(), location = col_character()))

# nature and status as factors
calls$nature <- as.factor(calls$nature)
calls$status <- as.factor(calls$status)

# create column for hour and weekday
# time is UTC, but the calls were made in Central time, so convert back
calls$time <- with_tz(calls$time, "America/Chicago")
calls$hour <- hour(calls$time)
calls$day <- day(calls$time)
calls$month <- month(calls$time, label=TRUE)
calls$wday <- wday(calls$time, label=TRUE)
calls <- calls %>% filter(time >= as.Date("2016-11-13") & time <= as.Date("2016-12-10"))

# plot call volume by weekday and time
ggplot(data = calls) + geom_bar(mapping = aes(x = wday))
ggplot(data = calls) + geom_bar(mapping = aes(x = hour))

# get the top 10 most common call types
top_calls_natures <- group_by(calls, nature) %>% summarize(count = n()) %>% arrange(desc(count)) %>% select(nature) %>% slice(1:10) %>% .$nature

calls %>%
  group_by(hour, wday) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = wday, y = hour,
      fill = count)) +
  geom_tile() +
  labs(y = "Hour of Call",
     x = "Day of Call",
     title = "Number of Calls During Hour of Day")

calls %>%
    filter(nature %in% top_calls_natures) %>%
    group_by(nature, hour) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = hour, y = reorder(nature, count),
        fill = count)) +
    geom_tile() +
    labs(y = "Nature of Call",
       x = "Hour of Call",
       title = "Number of Calls during the Day by Type")
