# PROJECT GOALS:
# The goal of this project is to provide a snapshot of ADHD among Americans, as surveyed by MEPS.
#
#Measurements:
# 1. Total number of persons with office-based visit for ADHD
# 2. Total number of office-based visits for ADHD
# 3. Total expenditures of office-based visits per person for ADHD
# 4. Total expenditures of office-based visits for ADHD
# 5. Percent of people with office-based visit for ADHD
# 6. Average per-person expense for ADHD office-based visits



# Install and load needed packages, including MEPS package from GitHub----------------
devtools::install_github("e-mitchell/meps_r_pkg/MEPS")

library(tidyr)
library(survey)
library(foreign)
library(haven)
library(dplyr)
library(MEPS)
library(ggplot2)

options(survey.lonely.psu = 'adjust')

# Load data with MEPS package---------------------------------------------------------

# OB   = Office-based medical visits file (record = medical visit)
# COND = Medical conditions file (record = medical condition)
# CLNK = Conditions-event link file (crosswalk between conditions and
#            events, including PMED events)
# FYC  = Full-year-consolidated file (record = MEPS sample person)

office_med_visits_2022 <- read_MEPS(year = 2022, type = "OB")
medical_conditions_2022 <- read_MEPS(year = 2022, type = "COND")
conditions_event_link_2022 <- read_MEPS(year = 2022, type = "CLNK")
full_year_consolidated_2022 <- read_MEPS(year = 2022, type = "FYC")

# Select variables needed for analysis----------------------------------------------

office_med_visits_2022_clean   = office_med_visits_2022 %>%
  select(PANEL, DUPERSID, EVNTIDX, EVENTRN, OBDATEYR, OBDATEMM, OBXP22X)

medical_conditions_2022_clean = medical_conditions_2022 %>%
  select(PANEL, DUPERSID, CONDIDX, ICD10CDX, CCSR1X:CCSR4X, RXCOND)

full_year_consolidated_2022_clean  = full_year_consolidated_2022 %>%
  select(PANEL, DUPERSID, ADHDAGED, AGELAST, PERWT22F, VARSTR, VARPSU)

# View medical conditions for 2022----------------------------------------------------

view_conditions = medical_conditions_2022_clean %>%
  count(ICD10CDX)

View(view_conditions)

# Filter to households with ADHD diagnosis--------------------------------------------

adhd <- medical_conditions_2022_clean %>%
  filter(ICD10CDX == "F90")

# Merge with office medical visits dataset
adhd_merged <- adhd %>%
  inner_join(conditions_event_link_2022, by = c("PANEL", "DUPERSID", "CONDIDX"), multiple = "all") %>%
  inner_join(office_med_visits_2022_clean, by = c("PANEL", "DUPERSID", "EVNTIDX"), multiple = "all") %>%
  mutate(ob_visit = 1)

# Count number of medical office visits
office_med_visits_2022_clean %>% count(EVENTYPE)
adhd_merged %>% count(EVENTYPE)

# >> Check events for example person:
adhd_merged %>% filter(DUPERSID == '2460010101')



# De-duplicate on EVNTIDX so we don't count the same event twice


# >> Example of same event (EVNTIDX) for treating multiple cancer
adhd_merged %>% filter(DUPERSID == '2460010101')


adhd_unique = adhd_merged %>%
  distinct(PANEL, DUPERSID, EVNTIDX, OBXP22X, ob_visit)


# >> Check example person:
adhd_unique %>% filter(DUPERSID == '2460010101')

# Aggregate to person-level --------------------------------------------------
pers = adhd_unique %>%
  group_by(DUPERSID) %>%
  summarize(
    pers_XP      = sum(OBXP22X),   # total person exp. for adhd office visits
    pers_nvisits = sum(ob_visit))  # total number of cancer adhd visits

# Add indicator variable
pers = pers %>%
  mutate(any_OB = 1)


# Merge onto FYC file ---------------------------------------------------------
#  >> Need to capture all Strata (VARSTR) and PSUs (VARPSU) for all MEPS sample
#     persons for correct variance estimation

fyc_adhd <- full_year_consolidated_2022_clean %>%
  full_join(pers, by = "DUPERSID")  %>%

  # replace NA with 0
  replace_na(list(pers_nvisits = 0, any_OB = 0))


# QC: should have same number of rows as FYC file
nrow(full_year_consolidated_2022_clean) == nrow(fyc_adhd)



# Define the survey design ----------------------------------------------------

meps_dsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT22F,
  data = fyc_adhd,
  nest = TRUE)

adhd_dsgn = subset(meps_dsgn, any_OB == 1)


# Calculate estimates ---------------------------------------------------------

# National Totals:
svytotal(~ any_OB +       # Total people w/ office visit for adhd
           pers_nvisits + # Total number of office visits for adhd
           pers_XP,       # Total expenditures for office visits for adhd
         design = adhd_dsgn)


# Percent of people with office visit for adhd
svymean( ~any_OB,  design = meps_dsgn)

# Average per-person expense for office visits for adhd
svymean( ~pers_XP, design = adhd_dsgn)

# Visualization ---------------------------------------------------------------
#Visualize number of ADHD-related office-based visits per person

#Visualize total expenditure per person of ADHD-related office-based visits
