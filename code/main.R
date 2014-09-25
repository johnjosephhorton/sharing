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

require(tidyr)
require(dplyr)
require(ggplot2)

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



