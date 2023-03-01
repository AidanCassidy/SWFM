# Estimate missing data 

setwd("E:/dd/EAS7/Homelessness/Scottish Welfare Fund Monitoring/SWF - IT System/R projects/SWFM/Data")

SWFM <- haven::read_sas("//s0177a/sasdata2/housing/homelessness/SWFM.sas7bdat") %>% 
  mutate(MY = as.yearmon(paste(MONTH, YEAR), "%m %Y"), 
         Date = as.Date(as.yearmon(MY)))

saveRDS(SWFM, "SWFM.rds")
LANAMES <- read_csv("LANAMES.csv")




# ESTIMATING MISSING CCG_APPS VALUES:

SWFM_NA <- SWFM %>%
  mutate(CCG_APPS = na_if(CCG_APPS, 0)) %>%
  group_by(Date) %>% 
  summarise(sumNA = sum(is.na(CCG_APPS))) %>%
  filter(sumNA != 0)
incomplete_dates <- SWFM_NA$Date

# copy CCG_APPS column for estimates to be added to while keeping the original column intact
SWFM_EST <- SWFM %>%
  mutate(CCG_APPS = na_if(CCG_APPS, 0))  %>%
  mutate(CCG_APPS_est = CCG_APPS)

#for each LA with missing value in m1 estimate value by multiplying value in M0 by delta

for(i in incomplete_dates){
  M1 <- as.Date(i)
  M0 <- as.Date(i) - months(1)
  
  #identify LAs with values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & !is.na(CCG_APPS_est))
  LA_m1 <- df$LACODE
  
  #identify LAs with missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & is.na(CCG_APPS_est))
  LA_NA_m1 <- df$LACODE
  
  #sum values in M1 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & LACODE %in% LA_m1)
  sum_m1 = sum(df$CCG_APPS_est)
  
  #sum values in M0 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M0 & LACODE %in% LA_m1)
  sum_m0 = sum(df$CCG_APPS_est)
  
  #calculate change in complete LAs between M0 and M1
  delta = sum_m1 / sum_m0
  
  SWFM_EST <- SWFM_EST %>%
    group_by(LACODE) %>%
    mutate(
      CCG_APPS_est = case_when(
        LACODE %in% LA_NA_m1 & Date == M1 ~ delta * CCG_APPS_est[Date == M0],
        TRUE ~ CCG_APPS_est
      ))}

# ESTIMATING MISSING CCG_PAIDOUT VALUES:

SWFM_NA <- SWFM %>%
  mutate(CCG_PAIDOUT = na_if(CCG_PAIDOUT, 0)) %>%
  group_by(Date) %>% 
  summarise(sumNA = sum(is.na(CCG_PAIDOUT))) %>%
  filter(sumNA != 0 & Date >= "2013-10-01")
incomplete_dates <- SWFM_NA$Date

# copy CCG_PAIDOUT column for estimates to be added to while keeping the original column intact
SWFM_EST <- SWFM_EST %>%
  mutate(CCG_PAIDOUT = na_if(CCG_PAIDOUT, 0))  %>%
  mutate(CCG_PAIDOUT_est = CCG_PAIDOUT)

#for each LA with missing value in m1 estimate value by multiplying value in M0 by delta

for(i in incomplete_dates){
  M1 <- as.Date(i)
  M0 <- as.Date(i) - months(1)
  
  #identify LAs with values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & !is.na(CCG_PAIDOUT_est))
  LA_m1 <- df$LACODE
  
  #identify LAs with missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & is.na(CCG_PAIDOUT_est))
  LA_NA_m1 <- df$LACODE
  
  #sum values in M1 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & LACODE %in% LA_m1)
  sum_m1 = sum(df$CCG_PAIDOUT_est)
  
  #sum values in M0 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M0 & LACODE %in% LA_m1)
  sum_m0 = sum(df$CCG_PAIDOUT_est)
  
  #calculate change in complete LAs between M0 and M1
  delta = sum_m1 / sum_m0
  
  SWFM_EST <- SWFM_EST %>%
    group_by(LACODE) %>%
    mutate(
      CCG_PAIDOUT_est = case_when(
        LACODE %in% LA_NA_m1 & Date == M1 ~ delta * CCG_PAIDOUT_est[Date == M0],
        TRUE ~ CCG_PAIDOUT_est
      ))}

# ESTIMATING MISSING CCG_SPEND VALUES:

SWFM_NA <- SWFM %>%
  mutate(CCG_SPEND = na_if(CCG_SPEND, 0)) %>%
  group_by(Date) %>% 
  summarise(sumNA = sum(is.na(CCG_SPEND))) %>%
  filter(sumNA != 0 & Date >= "2013-10-01")
incomplete_dates <- SWFM_NA$Date

# copy CCG_SPEND column for estimates to be added to while keeping the original column intact
SWFM_EST <- SWFM_EST %>%
  mutate(CCG_SPEND = na_if(CCG_SPEND, 0))  %>%
  mutate(CCG_SPEND_est = CCG_SPEND)

#for each LA with missing value in m1 estimate value by multiplying value in M0 by delta

for(i in incomplete_dates){
  M1 <- as.Date(i)
  M0 <- as.Date(i) - months(1)
  
  #identify LAs with values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & !is.na(CCG_SPEND_est))
  LA_m1 <- df$LACODE
  
  #identify LAs with missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & is.na(CCG_SPEND_est))
  LA_NA_m1 <- df$LACODE
  
  #sum values in M1 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & LACODE %in% LA_m1)
  sum_m1 = sum(df$CCG_SPEND_est)
  
  #sum values in M0 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M0 & LACODE %in% LA_m1)
  sum_m0 = sum(df$CCG_SPEND_est)
  
  #calculate change in complete LAs between M0 and M1
  delta = sum_m1 / sum_m0
  
  SWFM_EST <- SWFM_EST %>%
    group_by(LACODE) %>%
    mutate(
      CCG_SPEND_est = case_when(
        LACODE %in% LA_NA_m1 & Date == M1 ~ delta * CCG_SPEND_est[Date == M0],
        TRUE ~ CCG_SPEND_est
      ))}


# ESTIMATING MISSING CRISIS_APPS VALUES:

SWFM_NA <- SWFM %>%
  mutate(CRISIS_APPS = na_if(CRISIS_APPS, 0)) %>%
  group_by(Date) %>% 
  summarise(sumNA = sum(is.na(CRISIS_APPS))) %>%
  filter(sumNA != 0)
incomplete_dates <- SWFM_NA$Date

# copy CRISIS_APPS column for estimates to be added to while keeping the original column intact
SWFM_EST <- SWFM_EST %>%
  mutate(CRISIS_APPS = na_if(CRISIS_APPS, 0))  %>%
  mutate(CRISIS_APPS_est = CRISIS_APPS)

#for each LA with missing value in m1 estimate value by multiplying value in M0 by delta

for(i in incomplete_dates){
  M1 <- as.Date(i)
  M0 <- as.Date(i) - months(1)
  
  #identify LAs with values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & !is.na(CRISIS_APPS_est))
  LA_m1 <- df$LACODE
  
  #identify LAs with missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & is.na(CRISIS_APPS_est))
  LA_NA_m1 <- df$LACODE
  
  #sum values in M1 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & LACODE %in% LA_m1)
  sum_m1 = sum(df$CRISIS_APPS_est)
  
  #sum values in M0 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M0 & LACODE %in% LA_m1)
  sum_m0 = sum(df$CRISIS_APPS_est)
  
  #calculate change in complete LAs between M0 and M1
  delta = sum_m1 / sum_m0
  
  SWFM_EST <- SWFM_EST %>%
    group_by(LACODE) %>%
    mutate(
      CRISIS_APPS_est = case_when(
        LACODE %in% LA_NA_m1 & Date == M1 ~ delta * CRISIS_APPS_est[Date == M0],
        TRUE ~ CRISIS_APPS_est
      ))}

# ESTIMATING MISSING CRISIS_PAIDOUT VALUES:

SWFM_NA <- SWFM %>%
  mutate(CRISIS_PAIDOUT = na_if(CRISIS_PAIDOUT, 0)) %>%
  group_by(Date) %>% 
  summarise(sumNA = sum(is.na(CRISIS_PAIDOUT))) %>%
  filter(sumNA != 0 & Date >= "2013-10-01")
incomplete_dates <- SWFM_NA$Date

# copy CRISIS_PAIDOUT column for estimates to be added to while keeping the original column intact
SWFM_EST <- SWFM_EST %>%
  mutate(CRISIS_PAIDOUT = na_if(CRISIS_PAIDOUT, 0))  %>%
  mutate(CRISIS_PAIDOUT_est = CRISIS_PAIDOUT)

#for each LA with missing value in m1 estimate value by multiplying value in M0 by delta

for(i in incomplete_dates){
  M1 <- as.Date(i)
  M0 <- as.Date(i) - months(1)
  
  #identify LAs with values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & !is.na(CRISIS_PAIDOUT_est))
  LA_m1 <- df$LACODE
  
  #identify LAs with missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & is.na(CRISIS_PAIDOUT_est))
  LA_NA_m1 <- df$LACODE
  
  #sum values in M1 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & LACODE %in% LA_m1)
  sum_m1 = sum(df$CRISIS_PAIDOUT_est)
  
  #sum values in M0 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M0 & LACODE %in% LA_m1)
  sum_m0 = sum(df$CRISIS_PAIDOUT_est)
  
  #calculate change in complete LAs between M0 and M1
  delta = sum_m1 / sum_m0
  
  SWFM_EST <- SWFM_EST %>%
    group_by(LACODE) %>%
    mutate(
      CRISIS_PAIDOUT_est = case_when(
        LACODE %in% LA_NA_m1 & Date == M1 ~ delta * CRISIS_PAIDOUT_est[Date == M0],
        TRUE ~ CRISIS_PAIDOUT_est
      ))}

# ESTIMATING MISSING CRISIS_SPEND VALUES:

SWFM_NA <- SWFM %>%
  mutate(CRISIS_SPEND = na_if(CRISIS_SPEND, 0)) %>%
  group_by(Date) %>% 
  summarise(sumNA = sum(is.na(CRISIS_SPEND))) %>%
  filter(sumNA != 0 & Date >= "2013-10-01")
incomplete_dates <- SWFM_NA$Date

# copy CRISIS_SPEND column for estimates to be added to while keeping the original column intact
SWFM_EST <- SWFM_EST %>%
  mutate(CRISIS_SPEND = na_if(CRISIS_SPEND, 0))  %>%
  mutate(CRISIS_SPEND_est = CRISIS_SPEND)

#for each LA with missing value in m1 estimate value by multiplying value in M0 by delta

for(i in incomplete_dates){
  M1 <- as.Date(i)
  M0 <- as.Date(i) - months(1)
  
  #identify LAs with values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & !is.na(CRISIS_SPEND_est))
  LA_m1 <- df$LACODE
  
  #identify LAs with missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & is.na(CRISIS_SPEND_est))
  LA_NA_m1 <- df$LACODE
  
  #sum values in M1 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M1 & LACODE %in% LA_m1)
  sum_m1 = sum(df$CRISIS_SPEND_est)
  
  #sum values in M0 for LAs without missing values in M1
  df <- SWFM_EST %>%
    filter(Date == M0 & LACODE %in% LA_m1)
  sum_m0 = sum(df$CRISIS_SPEND_est)
  
  #calculate change in complete LAs between M0 and M1
  delta = sum_m1 / sum_m0
  
  SWFM_EST <- SWFM_EST %>%
    group_by(LACODE) %>%
    mutate(
      CRISIS_SPEND_est = case_when(
        LACODE %in% LA_NA_m1 & Date == M1 ~ delta * CRISIS_SPEND_est[Date == M0],
        TRUE ~ CRISIS_SPEND_est
      ))}



# Create data frame containing the Scotland level totals for apps, awards and spend and for each estimated:


estimates <- c("CRISIS_APPS_est", "CRISIS_PAIDOUT_est", "CRISIS_SPEND_est", "CCG_APPS_est", "CCG_PAIDOUT_est", "CCG_SPEND_est")
  
SWFM_TOTALS_LA <- SWFM_EST %>%
  ungroup() %>%
  left_join(LANAMES, by = "LACODE") %>%
  select(LANAME, YEAR, MONTH, MY, Date, 
         CCG_APPS, CCG_PAIDOUT, CCG_SPEND, 
         CCG_APPS_est, CCG_PAIDOUT_est, CCG_SPEND_est,
         CRISIS_APPS, CRISIS_PAIDOUT, CRISIS_SPEND, 
         CRISIS_APPS_est, CRISIS_PAIDOUT_est, CRISIS_SPEND_est) %>%
  mutate_at(estimates, round, digits = 2) %>% 
  mutate(SWF_APPS = CCG_APPS + CRISIS_APPS,
         SWF_PAIDOUT = CCG_PAIDOUT + CRISIS_PAIDOUT,
         SWF_SPEND = CCG_SPEND + CRISIS_SPEND, 
         SWF_APPS_est = CCG_APPS_est + CRISIS_APPS_est,
         SWF_PAIDOUT_est = CCG_PAIDOUT_est + CRISIS_PAIDOUT_est,
         SWF_SPEND_est = CCG_SPEND_est + CRISIS_SPEND_est)

SWFM_TOTALS <- SWFM_TOTALS_LA %>%
  group_by(Date) %>%
  summarise(CCG_APPS = sum(CCG_APPS, na.rm = TRUE),
            CCG_APPS_est = sum(CCG_APPS_est), 
            CCG_PAIDOUT = sum(CCG_PAIDOUT, na.rm = TRUE),
            CCG_PAIDOUT_est = sum(CCG_PAIDOUT_est, na.rm = TRUE),
            CCG_SPEND = sum(CCG_SPEND, na.rm = TRUE),
            CCG_SPEND_est = sum(CCG_SPEND_est, na.rm = TRUE),
            CRISIS_APPS = sum(CRISIS_APPS, na.rm = TRUE),
            CRISIS_APPS_est = sum(CRISIS_APPS_est), 
            CRISIS_PAIDOUT = sum(CRISIS_PAIDOUT, na.rm = TRUE),
            CRISIS_PAIDOUT_est = sum(CRISIS_PAIDOUT_est, na.rm = TRUE),
            CRISIS_SPEND = sum(CRISIS_SPEND, na.rm = TRUE),
            CRISIS_SPEND_est = sum(CRISIS_SPEND_est, na.rm = TRUE),
            SWF_APPS = sum(SWF_APPS, na.rm = TRUE),
            SWF_APPS_est = sum(SWF_APPS_est),
            SWF_PAIDOUT = sum(SWF_PAIDOUT, na.rm = TRUE),
            SWF_PAIDOUT_est = sum(SWF_PAIDOUT_est, na.rm = TRUE),
            SWF_SPEND = sum(SWF_SPEND, na.rm = TRUE),
            SWF_SPEND_est = sum(SWF_SPEND_est, na.rm = TRUE))
  

saveRDS(SWFM_TOTALS_LA, "SWFM_TOTALS_LA.rds")
saveRDS(SWFM_TOTALS, "SWFM_TOTALS.rds")

