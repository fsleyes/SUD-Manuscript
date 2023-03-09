
library(tidyverse)
library(haven)

demographics <- read_sav("demographics both sites all cohorts.sav")
sud <- read_sav("Substance Abuse_All Cohorts_Both Sites_T1.sav")
lsiRaw <- read_sav("LSIT1.sav")
SCIDs <- read_sav("SCID Consensus All Times-Both sites_ All cohorts.sav")


#loading all the files
files <- c("SUDT1.sav", "SUDT3.sav", "SUDT5.sav", "SUDT7.sav",
           "SUDT9.sav", "SUDT11.sav", "SUDT13.sav", "SUDT15.sav",
           "SUDT17.sav", "SUDT19.sav")
for (f in files){
  df <- read_sav(f)
  assign(gsub(".sav", "", f), df)
}

#loading all chronic LSI files
LSIfiles <- c("LSIT1.sav", "LSIT3.sav", "LSIT5.sav", 
              "LSIT7.sav", "LSIT9.sav", "LSIT11.sav", 
              "LSIT13.sav", "LSIT15.sav", "LSIT17.sav", 
              "LSIT19.sav")

for (i in LSIfiles){
  LSIdf <- read_sav(i)
  assign(gsub(".sav", "", i), LSIdf)
}

#rename id into ID
dumbList <- list(LSIT1, LSIT3, LSIT5, LSIT7, LSIT9)
dumbList <- lapply(dumbList, function(df) {
  df <- df %>% rename(ID = id)
})

LSIT1 <- LSIT1 %>% rename(ID = id)
LSIT3 <- LSIT3 %>% rename(ID = id)
LSIT5 <- LSIT5 %>% rename(ID = id)
LSIT7 <- LSIT7 %>% rename(ID = id)
LSIT9 <- LSIT9 %>% rename(ID = id)




#lsi is broken into subscales
#allison's paper used three models: total, interpersonal, and non-interpersonal
#total was created by averaging across all relevant domains

#clean the lsi first
#create a total by averaging across all items
lsi <- lsiRaw %>% 
  select(!(contains("specifier"))) %>%
  rowwise() %>%
  mutate(tot_avg = mean(c_across(lsi_friendship_t1:lsi_family_health_t1))) %>%
  mutate(int_avg = mean(c_across(c(lsi_friendship_t1, lsi_romantic_t1,
                                   lsi_social_t1, lsi_family_t1)))) %>%
  mutate(nonint_avg = mean(c_across(c(lsi_neighborhood_t1, lsi_school_t1,
                                      lsi_work_t1, lsi_finances_t1, 
                                      lsi_self_health_t1, lsi_family_health_t1))))
#numbers of averages line up with allison's paper

#alcohol abuse CSR is alab_cur_csr_t1
#can filter for everyone who had CSR of greater than 4
test <- SCIDs %>% select(contains("alab_cur_csr")) %>%
  summarize( Min = min(alab_cur_csr_t1),
             Mean = mean(alab_cur_csr_t1),
             Max = max(alab_cur_csr_t1))
#for t1, the max CSR for alcohol abuse is 3--what to do here?

#what about for alcohol dependence, which is scored separately?
#aldep_cur_csr_t1
test2 <- SCIDs %>% select(contains("aldep_cur_csr")) %>%
  summarize( Min = min(aldep_cur_csr_t1),
             Mean = mean(aldep_cur_csr_t1),
             Max = max(aldep_cur_csr_t1))
#even with alcohol dependence the max is 5
#how many people have CSR of 5?
SCIDs %>% select(contains("aldep_cur_csr")) %>% count(aldep_cur_csr_t1 == 5)
#only TWO people have a CSR of 5 in the sample for current alc dep

test <- SCIDs %>% select(contains("alab_cur_"))
#doesn't seem like there is t3 SCID data--did they not do SCID at t3?


test <- SCIDs %>% select(contains("alab_cur_csr")) %>%
  summarize( Min = min(alab_cur_csr_t5, na.rm = TRUE),
             Mean = mean(alab_cur_csr_t5, na.rm = TRUE),
             Max = max(alab_cur_csr_t5, na.rm = TRUE),
             )

test <- SCIDs %>% select(contains("alab_cur_csr"))

test <- SCIDs %>% select(contains("alab_cur_csr")) %>%
  count(alab_cur_csr_t5)


#filtering to anyone who has a CSR at ANY point for alc abuse
#going to want to use this for the dataset
anyCSRalab <- SCIDs %>% filter(alab_cur_csr_t1 != 0 &
                           !is.na(alab_cur_csr_t1) |
                           alab_cur_csr_t5 != 0 & 
                           !is.na(alab_cur_csr_t5) |
                           alab_cur_csr_t7 != 0 &
                           !is.na(alab_cur_csr_t7) |
                           alab_cur_csr_t9 != 0 &
                           !is.na(alab_cur_csr_t9) |
                           alab_cur_csr_T11 != 0 &
                           !is.na(alab_cur_csr_T11) |
                           alab_cur_csr_t13 != 0 &
                           !is.na(alab_cur_csr_t13) |
                           alab_cur_csr_t15 != 0 &
                           !is.na(alab_cur_csr_t15) |
                           alab_cur_csr_t17 != 0 &
                           !is.na(alab_cur_csr_t17) |
                           alab_cur_csr_t19 != 0 &
                           !is.na(alab_cur_csr_t19)) %>%
  select(alab_cur_csr_t1, alab_cur_csr_t5, alab_cur_csr_t7,
         alab_cur_csr_t9, alab_cur_csr_T11, alab_cur_csr_t13, 
         alab_cur_csr_t15, alab_cur_csr_t17, alab_cur_csr_t19)



#filtering to all who have CSRs for alc dependence
anyCSRaldep <- SCIDs %>% filter(aldep_cur_csr_t1 != 0 &
                           !is.na(aldep_cur_csr_t1) |
                           aldep_cur_csr_t5 != 0 & 
                           !is.na(aldep_cur_csr_t5) |
                           aldep_cur_csr_t7 != 0 &
                           !is.na(aldep_cur_csr_t7) |
                           aldep_cur_csr_t9 != 0 &
                           !is.na(aldep_cur_csr_t9) |
                           aldep_cur_csr_T11 != 0 &
                           !is.na(aldep_cur_csr_T11) |
                           aldep_cur_csr_t13 != 0 &
                           !is.na(aldep_cur_csr_t13) |
                           aldep_cur_csr_t15 != 0 &
                           !is.na(aldep_cur_csr_t15) |
                           aldep_cur_csr_t17 != 0 &
                           !is.na(aldep_cur_csr_t17) |
                           aldep_cur_csr_t19 != 0 &
                           !is.na(aldep_cur_csr_t19)) %>%
  select(aldep_cur_csr_t1, aldep_cur_csr_t5, aldep_cur_csr_t7,
         aldep_cur_csr_t9, aldep_cur_csr_T11, aldep_cur_csr_t13, 
         aldep_cur_csr_t15, aldep_cur_csr_t17, aldep_cur_csr_t19)

#putting together a chart of all the CSR counts per timepoint


alabt1 <- SCIDs %>% count(alab_cur_csr_t1) %>%
  rename(CSR = alab_cur_csr_t1,
         alab_cur_csr_t1 = n)
alabt5 <- SCIDs %>% count(alab_cur_csr_t5) %>%
  rename(CSR = alab_cur_csr_t5,
         alab_cur_csr_t5 = n)
alabt7 <- SCIDs %>% count(alab_cur_csr_t7) %>%
  rename(CSR = alab_cur_csr_t7,
         alab_cur_csr_t7 = n)
alabt9 <- SCIDs %>% count(alab_cur_csr_t9) %>%
  rename(CSR = alab_cur_csr_t9,
         alab_cur_csr_t9 = n)
alabT11 <- SCIDs %>% count(alab_cur_csr_T11) %>%
  rename(CSR = alab_cur_csr_T11,
         alab_cur_csr_T11 = n)
alabt13 <- SCIDs %>% count(alab_cur_csr_t13) %>%
  rename(CSR = alab_cur_csr_t13,
         alab_cur_csr_t13 = n)
alabt15 <- SCIDs %>% count(alab_cur_csr_t15) %>%
  rename(CSR = alab_cur_csr_t15,
         alab_cur_csr_t15 = n)
alabt17 <- SCIDs %>% count(alab_cur_csr_t17) %>%
  rename(CSR = alab_cur_csr_t17,
         alab_cur_csr_t17 = n)
alabt19 <- SCIDs %>% count(alab_cur_csr_t19) %>% 
  rename(CSR = alab_cur_csr_t19,
         alab_cur_csr_t19 = n)


testList <- list(alabt1, alabt5, alabt7, alabt9, alabT11, alabt13,
                 alabt15, alabt17, alabt19)
testList <- testList %>% reduce(full_join, by = "CSR")



#actually cleaning the data

SCIDfiltered <- SCIDs %>%
  filter(alab_cur_csr_t1 != 0 &
           !is.na(alab_cur_csr_t1) |
           alab_cur_csr_t5 != 0 & 
           !is.na(alab_cur_csr_t5) |
           alab_cur_csr_t7 != 0 &
           !is.na(alab_cur_csr_t7) |
           alab_cur_csr_t9 != 0 &
           !is.na(alab_cur_csr_t9) |
           alab_cur_csr_T11 != 0 &
           !is.na(alab_cur_csr_T11) |
           alab_cur_csr_t13 != 0 &
           !is.na(alab_cur_csr_t13) |
           alab_cur_csr_t15 != 0 &
           !is.na(alab_cur_csr_t15) |
           alab_cur_csr_t17 != 0 &
           !is.na(alab_cur_csr_t17) |
           alab_cur_csr_t19 != 0 &
           !is.na(alab_cur_csr_t19)) %>%
  select(id,
         alab_cur_csr_t1, alab_cur_csr_t5, alab_cur_csr_t7,
         alab_cur_csr_t9, alab_cur_csr_T11, alab_cur_csr_t13, 
         alab_cur_csr_t15, alab_cur_csr_t17, alab_cur_csr_t19) %>%
  rename(ID = id)
#do we want the CSRs?

#joining all of the Ts together
SUDdfs <- list(SUDT1, SUDT3, SUDT5, SUDT7,
               SUDT9, SUDT11, SUDT13, SUDT15,
               SUDT17, SUDT19)

allTs <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE),
                SUDdfs)

#merging all the Ts with the IDs from SCIDs
#selecting columns that refer to alcohol abuse
df <- merge(x = allTs, y = SCIDfiltered, by = "ID") %>%
  select(ID, contains("alc_drinks_per_day"), 
         contains("alc_days_per_month"), contains("current"))


nums <- c(1, 3, 5, 7, 9, 11, 13 ,15 ,17, 19)


# THIS FUNCTION CREATES A NEW COLUMN FOR EACH TIMEPOINT WITH THEIR
# CURRENTLY ASSESSED PATTERN OF DRINKING, SPECIFICALLY
# DRINKS PER DAY (alc_use_T) AND THEN DAYS PER MONTH (alc_use_days_T)
for (num in nums) {
  colname <- paste0("alc_use_T", num)
  colpattern1 <- paste0("alc_pattern1_current_t", num)
  colpattern2 <- paste0("alc_pattern2_current_t", num)
  colpattern3 <- paste0("alc_pattern3_current_t", num)
  coldrinks1 <- paste0("alc_drinks_per_day_pattern1_t", num)
  coldrinks2 <- paste0("alc_drinks_per_day_pattern2_t", num)
  coldrinks3 <- paste0("alc_drinks_per_day_pattern3_t", num)
  df <- df %>%
    mutate(!!colname := case_when(
      !!sym(colpattern1) == 1 ~ !!sym(coldrinks1),
      !!sym(colpattern2) == 1 ~ !!sym(coldrinks2),
      !!sym(colpattern3) == 1 ~ !!sym(coldrinks3),
      TRUE ~ NA_real_
    ))
  
  colname_days <- paste0("alc_use_days_T", num)
  coldays1 <- paste0("alc_days_per_month_pattern1_t", num)
  coldays2 <- paste0("alc_days_per_month_pattern2_t", num)
  coldays3 <- paste0("alc_days_per_month_pattern3_t", num)
  
  df <- df %>%
    mutate(!!colname_days := case_when(
      !!sym(colpattern1) == 1 ~ !!sym(coldays1),
      !!sym(colpattern2) == 1 ~ !!sym(coldays2),
      !!sym(colpattern3) == 1 ~ !!sym(coldays3),
      TRUE ~ NA_real_
      )
    ) 
  
  colname_totDrinks <- paste0("alc_drinks_per_month_T", num)
  
  df <- df %>%
    mutate(!!colname_totDrinks := !!sym(coldrinks1) * !!sym(coldays1))
}

#alcData is the df with the alcohol use data
alcData <- df %>% select(ID, alc_use_T1:alc_drinks_per_month_T19)
#now need to join with the stress data in the lsi df

#joining all LSI timepoints
LSIlist <- list(LSIT1, LSIT3, LSIT5, LSIT7, 
                LSIT9, LSIT11, LSIT13, LSIT15, 
                LSIT17, LSIT19)
allLSIts <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE),
                   LSIlist)
#merge with the SCID dataset for the IDs
df <- merge(x = allLSIts, y = SCIDfiltered, by = "ID")
df <- df[, !duplicated(names(df))]
df <- df %>%
  select(!(contains("specifier")))


#calculating subscales for each timepoint
lsiNums <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
for (num in lsiNums){
  avgname <- paste0("LSI_avg_t", num)
  intname <- paste0("LSI_int_tot_t", num)
  nonintname <- paste0("LSI_nonint_tot_t", num)
  
  friendship <- paste0("lsi_friendship_t", num)
  famhealth <- paste0("lsi_family_health_t", num)
  romantic <- paste0("lsi_romantic_t", num)
  social <- paste0("lsi_social_t", num)
  family <- paste0("lsi_family_t", num)
  
  df <- df %>%
    rowwise() %>%
    mutate(!!avgname := mean(c_across(!!sym(friendship):!!sym(famhealth))))
  
}
#can't figure out how to do the subscales in this loop but won't look at that now


lsiData <- df %>% select(ID, LSI_avg_t1:LSI_avg_t19)

#getting gender
gender <- demographics %>% 
  select(id, gender) %>%
  rename(ID = id)



dataList <- list(lsiData, alcData, gender)
#don't know where neuroticism data is??
test <- read_sav("Qs.sav") %>% select(contains("bg5"))
#looks like big 5 Qs here but no subscale scoring? only 39 items?

fullData <- Reduce(function(x, y) merge(x, y, by = "ID"), dataList)
fullData <- fullData %>% select(ID, gender, LSI_avg_t1:ncol(.))


write.csv(fullData, file = "fullData.csv")
