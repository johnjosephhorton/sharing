###############################################################################
#
# Sharing / John Horton
# oDesk Contract #13552832
#
# Code for import & basic analysis
#
# Author: Alexander Gedranovich  
# Created: 2014-09-25
#
# Code style follows 'Google R Style Guide'
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
#
###############################################################################

# Clears the workspace
rm(list = ls(all = T))
gc(reset = T)
set.seed(12345)

# Libraries
require(tidyr)
require(dplyr)
require(ggplot2)
require(stringr)

ConvertFactor <- function(x) {
  # Converts factors with "na" levels to factor with missing values
  # 
  # Args:
  #   x: factor vector
  #
  # Returns:
  #   Factor with missing values
  
  lev <- levels(x)
  lev <- lev[!lev %in% c("", "na")]
  
  x <- as.character(x) %>% tolower()
  x[!x %in% lev] <- NA
    
  factor(x)
}

# Reads data
.data.file <- "data/sharing_survey_results_pilot.csv"
df <- read.table(.data.file, header = T, sep = ",", fill = NA) %>%
  select(worker.id = WorkerId,
         assignment.id = AssignmentId,         
         assignment.status = AssignmentStatus,
         work.time = WorkTimeInSeconds,
         input.good = Input.good,
         answer.own = Answer.own,
         answer.lent = Answer.lent,
         answer.borrowed = Answer.borrowed,
         answer.rent = Answer.rent,
         answer.usage = Answer.usage,
         answer.granularity = Answer.granularity,
         answer.predictability = Answer.predictability,
         answer.no_own_reason = Answer.no_own_reason,
         answer.own_income = Answer.own_income) %>%
  tbl_df()

# Converts factors
df <- df %>% mutate_each(funs(ConvertFactor), answer.own, answer.lent,
                         answer.borrowed, answer.rent, answer.no_own_reason)

# Basic analysis

# Number of owners of 'input.good'
df %>% 
  select(input.good, answer.own) %>% 
  group_by(input.good, answer.own) %>% 
  summarise(count = n()) %>%
  spread(answer.own, count, fill = 0) %>%
  mutate(total = no + yes + `NA`)

# Reasons for not owning 'input.good'
df %>% 
  select(input.good, answer.no_own_reason) %>% 
  group_by(input.good, answer.no_own_reason) %>% 
  summarise(count = n()) %>%
  spread(answer.no_own_reason, count, fill = 0) %>%
  mutate(total = expensive + little_use + space + `NA`)

# Are owners of 'input.good' ever borrowed or rent it?
df %>%
  select(input.good, answer.own, answer.borrowed, answer.rent) %>% 
  unite(answer.rent_or_borrowed, answer.borrowed, answer.rent, remove = T) %>%
  mutate(answer.rent_or_borrowed = str_detect(answer.rent_or_borrowed, "yes")) %>%
  group_by(input.good, answer.own, answer.rent_or_borrowed) %>%
  summarise(count = n()) %>%
  spread(answer.rent_or_borrowed, count, fill = 0) %>%
  mutate(total = `TRUE` + `FALSE`)

