library(tidyverse)
library(lubridate)

#Day4 Part 1

times <- read_csv("day4/input.txt", col_names = FALSE)

times_parsed <- times %>%
  mutate(dttime = ymd_hm(str_extract(X1, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}")),
         date = date(dttime),
         minute = minute(dttime)) %>%
  arrange(dttime) %>%
  mutate(action = str_extract(X1, "[:alpha:]+ [#,a-zA-Z0-9 ]*"),
         guard_id = parse_number(action),
         action = str_extract(action, "[A-z]+ [A-z]+")) %>%
  fill(guard_id) %>%
  select(guard_id, minute, action) %>%
  filter(action %in% c("falls asleep", "wakes up"))

times_parsed %>%
  group_by(guard_id, action) %>%
  summarize(sum(minute)) %>%
  spread(action, `sum(minute)`) %>%
  mutate(time_asleep = `wakes up` - `falls asleep`) %>%
  arrange(desc(time_asleep)) #id2663 has most time asleep

time_slept <- 
  bind_cols(
    
    times_parsed %>%
      filter(guard_id == 2663, action == 'falls asleep') %>%
      mutate(minute_sleep = minute),
    
    times_parsed %>%
      filter(guard_id == 2663, action == 'wakes up') %>%
      mutate(minute_woke = minute)
    
  ) %>%
  select(guard_id, minute_sleep, minute_woke)

time_slept %>% #visualization of time slept
  arrange(minute_woke) %>%
  ggplot(aes(x = 1:nrow(.), ymin = minute_sleep, ymax = minute_woke)) +
  geom_errorbar() +
  coord_flip() + 
  labs(x = "nap id#",
       y = "minute")

time_slept %>%
  mutate(min_list = seq(minute_sleep, minute_woke))

minutes <- list()
for (i in 1:nrow(time_slept)) {
  minutes[[i]] <- seq(time_slept$minute_sleep[i], time_slept$minute_woke[i])
}

df <- tbl_df(unlist(minutes))
count(df, value, sort = TRUE)


#Day4 part 2
times_tidy <- 
  
  bind_cols(
    
    times_parsed %>%
      filter(action == 'falls asleep'),
    
    times_parsed %>%
      filter(action == 'wakes up')
    
  )

times_tidy %>%
  select(-guard_id1, -action, -action1) 


minutes <- list()

for (i in 1:nrow(times_tidy)) {
  minutes[[i]] <- seq(times_tidy$minute[i], times_tidy$minute1[i])
}

minutes

times_tidy %>%
  mutate(list = minutes) %>%
  unnest(list) %>%
  group_by(guard_id) %>%
  count(list, sort = TRUE)
