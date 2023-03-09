
library(tidyverse)
library(haven)

demographics <- read_sav("demographics both sites all cohorts.sav")
sud <- read_sav("Substance Abuse_All Cohorts_Both Sites_T1.sav")
lsi <- read_sav("Chronic LSI Time 1_All cohorts_ both sites.sav")
SCIDs <- read_sav("SCID Consensus All Times-Both sites_ All cohorts.sav")


#loading all the files
files <- c("SUDT1.sav", "SUDT3.sav", "SUDT5.sav", "SUDT7.sav",
           "SUDT9.sav", "SUDT11.sav", "SUDT13.sav", "SUDT15.sav",
           "SUDT17.sav", "SUDT19.sav")
for (f in files){
  df <- read_sav(f)
  assign(gsub(".sav", "", f), df)
}

#lsi is broken into subscales
#allison's paper used three models: total, interpersonal, and non-interpersonal
#total was created by averaging across all relevant domains

#clean the lsi first
#create a total by averaging across all items
lsi <- lsi %>% 
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
  select(ID, contains("alc"))
