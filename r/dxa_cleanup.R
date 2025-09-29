## Relief Bodycomp data cleanup

library(tidyverse); library(lubridate)



dxa.dat <- read.delim("data/tr030_dxa_notepad.txt", sep = ";") %>%
  
  mutate(across(everything(), ~ gsub(",", ".", ., fixed = TRUE)),
         across(Alder:TotalvenstreVev.fett, ~ as.numeric(.))) %>%
  
  as_tibble() %>%
  select(study = Etternavn,
         participant = Fornavn,
         date = Malingsdato,
         age = Alder,
         weight.arms = "ArmerTotalmasse",
         weight.right_arm = "ArmhoyreTotalmasse",
         weight.left_arm = "ArmvenstreTotalmasse",
         weight.legs = "BeinTotalmasse",
         weight.right_leg = "BeinhoyreTotalmasse",
         weight.left_leg = "BeinvenstreTotalmasse",
         weight.body = "KroppTotalmasse",
         weight.right_upperbody = "OverkropphoyreTotalmasse",
         weight.left_upperbody = "OverkroppvenstreTotalmasse",
         weight.android = "AndroidTotalmasse",
         weight.gynoid = "GynoidTotalmasse",
         weight.whole = "HeleTotalmasse",
         weight.right_whole = "TotalhoyreTotalmasse",
         weight.left_whole = "TotalvenstreTotalmasse",
         fat.arms = "ArmerFettmasse",
         fat.right_arm = "ArmhoyreFettmasse",
         fat.left_arm = "ArmvenstreFettmasse",
         fat.legs = "BeinFettmasse",
         fat.right_leg = "BeinhoyreFettmasse",
         fat.left_leg = "BeinvenstreFettmasse",
         fat.body = "KroppFettmasse",
         fat.right_upperbody = "OverkropphoyreFettmasse",
         fat.left_upperbody = "OverkroppvenstreFettmasse",
         fat.android = "AndroidFettmasse",
         fat.gynoid = "GynoidFettmasse",
         fat.whole = "HeleFettmasse",
         fat.right_whole = "TotalhoyreFettmasse",
         fat.left_whole = "TotalvenstreFettmasse",
         lean.arms = "ArmerMagermasse",
         lean.right_arm = "ArmhoyreMagermasse",
         lean.left_arm = "ArmvenstreMagermasse",
         lean.legs = "BeinMagermasse",
         lean.right_leg = "BeinhoyreMagermasse",
         lean.left_leg = "BeinvenstreMagermasse",
         lean.body = "KroppMagermasse",
         lean.right_upperbody = "OverkropphoyreMagermasse",
         lean.left_upperbody = "OverkroppvenstreMagermasse",
         lean.android = "AndroidMagermasse",
         lean.gynoid = "GynoidMagermasse",
         lean.whole = "HeleMagermasse",
         lean.right_whole = "TotalhoyreMagermasse",
         lean.left_whole = "TotalvenstreMagermasse",
         BMC.arms = "ArmerBeinmasse",
         BMC.right_arm = "ArmhoyreBeinmasse",
         BMC.left_arm = "ArmvenstreBeinmasse",
         BMC.legs = "BeinBeinmasse",
         BMC.right_leg = "BeinhoyreBeinmasse",
         BMC.left_leg = "BeinvenstreBeinmasse",
         BMC.body = "KroppBeinmasse",
         BMC.right_upperbody = "OverkropphoyreBeinmasse",
         BMC.left_upperbody = "OverkroppvenstreBeinmasse",
         BMC.android = "AndroidBeinmasse",
         BMC.gynoid = "GynoidBeinmasse",
         BMC.whole = "HeleBeinmasse",
         BMC.right_whole = "TotalhoyreBeinmasse",
         BMC.left_whole = "TotalvenstreBeinmasse") %>%
  mutate(date = dmy(date)) %>%
  filter(!study %in% c("test1", "test2", "test3", "test4")) %>%
  mutate(participant = gsub("TR030_FP", "", participant),
         age = as.character(age),
         group = if_else(age > "65",
                         "old",
                         if_else(age < "65",
                                 "yng", age))) %>%
  filter(!participant %in% c("3092", "3093"))




## Categorize date into time points (pre, mid and post measurements), then age groups (youn/old)

dxa.dat$day <- day(dxa.dat$date)

dxa.dat$month <- month(dxa.dat$date, label = TRUE)

dxa.dat$time <- case_when(
  dxa.dat$month == "Sep" ~ "pre",
  dxa.dat$month == "Oct" & dxa.dat$day < 5 ~ "pre",
  dxa.dat$month == "Oct" & dxa.dat$day > 5 ~ "mid",
  dxa.dat$month == "Nov" & dxa.dat$day < 25 ~ "mid",
  dxa.dat$month == "Nov" & dxa.dat$day > 25 ~ "post",
  dxa.dat$month == "Dec" ~ "post")



## Join height data
# Height is not included in the export, but is registered in the prodigy program, thus has to be added in after export

library(readxl)

height.dat <- read_excel("./data/relief_age&height.xlsx") %>%
  mutate(participant = as.character(participant)) %>%
  filter(!participant %in% c("3092", "3093")) %>%
  select(participant, height)

dxa.dat2 <- dxa.dat %>%
  full_join(height.dat)



## Sorting out the double measurements

doubleR.dat <- dxa.dat2 %>%
  filter(study %in% c("TR030R")) %>%
  mutate(study = gsub("TR030R", "TR030", study)) %>%
  select(study, participant, date, age, time, group, height,
         weight.right_arm, weight.right_leg, weight.right_upperbody, weight.right_whole,
         fat.right_arm, fat.right_leg, fat.right_upperbody, fat.right_whole,
         lean.right_arm, lean.right_leg, lean.right_upperbody, lean.right_whole,
         BMC.right_leg, BMC.right_arm, BMC.right_upperbody, BMC.right_whole) %>%
  print()


doubleL.dat <- dxa.dat2 %>%
  filter(study %in% c("TR030L")) %>%
  mutate(study = gsub("TR030L", "TR030", study)) %>%
  select(study, participant, date, age, time, group, height,
         weight.left_arm, weight.left_leg, weight.left_upperbody, weight.left_whole,
         fat.left_arm, fat.left_leg, fat.left_upperbody, fat.left_whole,
         lean.left_arm, lean.left_leg, lean.left_upperbody, lean.left_whole,
         BMC.left_leg, BMC.left_arm, BMC.left_upperbody, BMC.left_whole) %>%
  print()

double.joined <- doubleR.dat %>%
  right_join(doubleL.dat) %>%
  group_by(participant, time) %>%
  mutate(weight.arms = weight.left_arm + weight.right_arm,
         weight.legs = weight.left_leg + weight.right_leg,
         weight.whole = weight.left_whole + weight.right_whole,
         fat.arms = fat.left_arm + fat.right_arm,
         fat.legs = fat.left_leg + fat.right_leg,
         fat.whole = fat.left_whole + fat.right_whole,
         lean.arms = lean.left_arm + lean.right_arm,
         lean.legs = lean.left_leg + lean.right_leg,
         lean.whole = lean.left_whole + lean.right_whole,
         BMC.arms = BMC.left_arm + BMC.right_arm,
         BMC.legs = BMC.left_leg + BMC.right_leg,
         BMC.whole = BMC.left_whole + BMC.right_whole) %>%
  print()



## Joining double measures with the rest

dxa.dat3 <- dxa.dat2 %>%
  full_join(double.joined) %>%
  filter(!study %in% c("TR030R", "TR030L"))



relief_bodycomp <- dxa.dat3 %>%
  mutate(weight.whole = weight.whole/1000)




