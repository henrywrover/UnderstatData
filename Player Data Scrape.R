### loading libraries

library(tidyverse)
library(rvest)
library(understatr)

### read the latest file so we can use that and add onto the bottom

files <- file.info(list.files("~/R/Understat Data/Player Data"))

old_data <- read_csv(row.names(files)[which.max(files[["ctime"]])])

### match list, update each time

match_list <- as.list(c(max(75):18000))

### getting the stats

stats <- NULL;
for (i in match_list) {
  tryCatch({
    tmp <- get_match_stats(i)
    stats <- rbind(stats, tmp)
  }, error = function(e){})
}

rm(tmp)

### getting date and league of match

dist_matches <- stats %>%
  distinct(match_id)

temp <- NULL;
for (i in dist_matches$match_id){
  
  tryCatch({
    
    url <- paste0("https://understat.com/match/", i)
    
    temp2 <- url %>%
      read_html() %>%
      html_nodes("li") %>%
      html_text() %>%
      as.data.frame %>%
      filter(row_number() %in% c(5, 6)) %>%
      rename("value" = 1) %>%
      mutate(match_id = i,
             t = row_number()) %>%
      pivot_wider(names_from = t,
                  values_from = value) %>%
      rename("league" = 2,
             "date_of_match" = 3) %>%
      mutate(date_of_match = mdy(date_of_match))
    
    temp <- rbind(temp, temp2)
    rm(temp2)
    
  }, error = function(e){})
  
}

### create dataframe and save file

df <- stats %>%
  left_join(temp, by = "match_id") %>%
  select(player, goals, xG, shots, assists, xA, key_passes, time, position,
         h_a, yellow_card, red_card, key_passes, match_id, league,
         date_of_match) %>%
  rbind(old_data) %>%
  arrange(-match_id)

write_csv(file = paste0(Sys.Date(), ".csv"), df)
