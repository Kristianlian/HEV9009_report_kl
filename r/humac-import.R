## Import of strength measurements ##

library(tidyverse); library(readxl); library(readr)


# Function to read and organize humac data from a specific sheet,
# making it possible to loop over files and sheets within files


# file
# returns a tidy data frame
read_humac <- function(file) {
  
  
  # Get sheets
  sheets <- excel_sheets(file)
  
  results <-list()
  
  for(i in 1:length(sheets)) {
    
    # Meta data
    meta <- read_excel(file, sheet = sheets[i], range = "A1:I7", .name_repair = "unique_quiet")
    
    # Participant id and date recorde in file
    id1 <- data.frame(meta)[2,7]
    id2 <- data.frame(meta)[2,5]
    date <- strsplit(data.frame(meta)[2,9], " +")[[1]][1]
    
    
    
    ## Speeds, read and organize data
    complete_sheet <- read_excel(file, sheet = sheets[i],
                                 range = "A11:E166",
                                 .name_repair = "unique_quiet") %>%
      data.frame()
    
    speed60 <- complete_sheet[1:22,]
    speed120 <- complete_sheet[48:69,]
    speed240 <- complete_sheet[95:116,]
    speed0 <- complete_sheet[139:152,]
    
    
    
    
    
    ## Sanity checks
    
    
    
    dat60 <-   data.frame(
      speed = "60",
      sheet = c(sheets[i], sheets[i]),
      date = c(date, date),
      participant = c(id1, id1),
      participant2 = c(id2, id2),
      leg = c("R", "L"),
      pt =         c(as.numeric(speed60[1,5]), as.numeric(speed60[2,5])),
      rep_work =   c(as.numeric(speed60[5,5]), as.numeric(speed60[6,5])),
      rep_power =  c(as.numeric(speed60[9,5]), as.numeric(speed60[10,5])),
      pt_angle =   c(as.numeric(speed60[14,5]), as.numeric(speed60[15,5])),
      pt_tt =      c(as.numeric(speed60[21,5]), as.numeric(speed60[22,5])))
    
    dat120 <-   data.frame(
      speed = "120",
      sheet = c(sheets[i], sheets[i]),
      date = c(date, date),
      participant = c(id1, id1),
      participant2 = c(id2, id2),
      leg = c("R", "L"),
      pt =         c(as.numeric(speed120[1,5]), as.numeric(speed120[2,5])),
      rep_work =   c(as.numeric(speed120[5,5]), as.numeric(speed120[6,5])),
      rep_power =  c(as.numeric(speed120[9,5]), as.numeric(speed120[10,5])),
      pt_angle =   c(as.numeric(speed120[14,5]), as.numeric(speed120[15,5])),
      pt_tt =      c(as.numeric(speed120[21,5]), as.numeric(speed120[22,5])))
    
    dat240 <-   data.frame(
      speed = "240",
      sheet = c(sheets[i], sheets[i]),
      date = c(date, date),
      participant = c(id1, id1),
      participant2 = c(id2, id2),
      leg = c("R", "L"),
      pt =         c(as.numeric(speed240[1,5]), as.numeric(speed240[2,5])),
      rep_work =   c(as.numeric(speed240[5,5]), as.numeric(speed240[6,5])),
      rep_power =  c(as.numeric(speed240[9,5]), as.numeric(speed240[10,5])),
      pt_angle =   c(as.numeric(speed240[14,5]), as.numeric(speed240[15,5])),
      pt_tt =      c(as.numeric(speed240[21,5]), as.numeric(speed240[22,5])))
    
    
    dat0 <-   data.frame(
      speed = "0",
      sheet = c(sheets[i], sheets[i]),
      date = c(date, date),
      participant = c(id1, id1),
      participant2 = c(id2, id2),
      leg = c("R", "L"),
      pt =         c(as.numeric(speed0[1,2]), as.numeric(speed0[2,2])),
      rep_work =   c(NA, NA),
      rep_power =  c(NA, NA),
      pt_angle =   c(60, 60),
      pt_tt =      c(as.numeric(speed0[13,2]), as.numeric(speed0[14,2])))
    
    
    results[[i]] <-  bind_rows(dat60, dat120, dat240, dat0) %>%
      select(participant, participant2, sheet, date, leg, speed, pt, rep_work, rep_power, pt_angle, pt_tt)
    
    
    
  }
  
  
  bind_rows(results)
  
  
  
}


# Run function on all files in directory

import <- list.files("data/humac", full.names = TRUE) %>%
  map(read_humac) %>%
  bind_rows()



relief_humac <- import %>%
  mutate(participant2 = gsub("TR030", "", participant2),
         participant2 = gsub(",", "", participant2),
         participant2 = gsub("FP", "", participant2),
         participant2 = gsub("_", "", participant2)) %>%
  mutate(participant2 = if_else(participant2 == "", participant, participant2),
         time = sheet,
         
         # Remove letters from time
         timerep = if_else(time %in% c("pre1", "mid1", "post1"), 1, 2),
         time = gsub('[0-9]+', "", time)) %>%
  select(participant = participant2, time, timerep, date, leg, speed, pt, rep_work, rep_power, pt_angle, pt_tt)

saveRDS(relief_humac, "./data/data-gen/relief_humac.RDS")
