#!/usr/bin/Rscript
###############################################################################
#
# Sharing / John Horton
# oDesk Contract #13552832
#
# Code for import & basic analysis
#
# Authors:
# Date Clean-up/ETL/Summary Stats: Alexander Gedranovich
# Statistical Analysis: John Horton 
# Created: 2014-09-25
#
# Code style follows 'Google R Style Guide'
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
###############################################################################

# Clears the workspace
rm(list = ls(all = T))
gc(reset = T)
set.seed(12345)

# Libraries
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
library(lme4)
library(stargazer)
library(scales)
library(Hmisc)
library(lfe)
library(Hmisc)

library(JJHmisc) # non-cran resource 

# Settings----------------------------------------------------------------------

SHOW.PLOTS <- FALSE 
PLOTS.DIR <- "../writeup/plots/" 

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

#######################################
# Read and clean data from MTurk Survey
#######################################

.data.file <- "../data/sharing_survey_results_pilot.csv"

df.raw <- read.table(.data.file, header = T, sep = ",", fill = NA) %>%
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
         answer.own_income = Answer.own_income) %>% tbl_df()

# Converts factors-------------------------------------------------------------- 
df <- df.raw %>% mutate_each(funs(ConvertFactor), answer.own, answer.lent,
                         answer.borrowed, answer.rent, answer.no_own_reason)


# ecode usage levels------------------------------------------------------------- 
usage.levels <- 0:11
usage.labels <- c(
    "We would not use this at all",
    "1 minute a week (about 1 hour a year)",
    "5 minutes a week (about 4 hours a year)",
    "1/2 an hour a week",
    "1 hour a week",
    "1/2 an hour a day",
    "1 hour a day",
    "2 hours a day",
    "4 hours a day",
    "8 hours a day",
    "16 hours a day",
    "24 hours a day (I would continuously be using this good")
minute.per.week <- (60/7)/(24*60*60*7)
hour.per.day <- 1.0/24.0
day.frac <- c(0,
              minute.per.week, 
              5 * minute.per.week,
              30 * minute.per.week, 
              60 * minute.per.week,
              0.5 * hour.per.day,
              hour.per.day,
              2 * hour.per.day,
              4 * hour.per.day,
              8 * hour.per.day,
              15 * hour.per.day,
              24 * hour.per.day)

df <- within(df, {
    usage = factor(answer.usage, levels = usage.levels, labels = usage.labels)
    x = as.numeric(as.character(factor(answer.usage, levels = usage.levels, labels = day.frac)))
    own = answer.own == "yes"
    borrowed = answer.borrowed == "yes"
    lent = answer.lent == "yes"
    usage.index = scale(as.numeric(answer.usage))
    income.index = scale(as.numeric(answer.own_income))
    predict.index = scale(as.numeric(answer.predictability))
    granular.index = scale(as.numeric(answer.granularity))
    rent = answer.rent == "yes"
})

# fix typo
levels(df$input.good)[16] <- "kid's bouncy castle"

df <- data.table(df)

df$input.good.short <- revalue(df$input.good, c("high-end digitial camera"="high-end\ndigitial camera",
                                          "kitchen timer (or egg timer)" = "kitchen timer\n(or egg timer)",
                                          "portable air conditioner" = "portable\nair conditioner",
                                          "cat carrier (for transporting cats)" = "cat carrier\n(for transporting cats)",
                                          "back-up electric generator" = "back-up\nelectric generator",
                                          "high-end audio headphones" = "high-end\naudio headphones")
                               )

##################################################
# Fractions owning, renting, lending and borrowing
##################################################

# Fraction of respondents owning the various goods------------------------------ 

df.by.good <- subset(df, !is.na(answer.own))[,
                             list(lower = Hmisc::binconf(sum(own), .N)[2],
                                  upper = Hmisc::binconf(sum(own), .N)[3],
                                  frac.own = mean(own)),
                             by = input.good]
df.by.good$input.good <- with(df.by.good, reorder(input.good, frac.own, mean))



g.own <- ggplot(data = df.by.good, aes(x = input.good, y = frac.own)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))

if (interactive() && SHOW.PLOTS){
    print(g.own)
}
writeImage(g.own, "ownership_fractions", width = 6, height = 4, path = PLOTS.DIR)

# Fraction of respondents renting various goods--------------------------------- 

df.by.good.rent <- subset(df, !is.na(answer.rent))[,
                                  list(
                                      lower = Hmisc::binconf(
                                          sum(rent), .N)[2],
                                      upper = Hmisc::binconf( 
                                          sum(rent), .N)[3],
                                      frac.rent = mean(rent)),
                                  by = input.good]
df.by.good.rent$input.good <- with(df.by.good.rent, reorder(input.good, frac.rent, mean))


g.rent <- ggplot(data = df.by.good.rent, aes(x = input.good, y = frac.rent)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))

if (interactive() && SHOW.PLOTS){
    print(g.rent)
}
writeImage(g.rent, "rental_fractions", width = 6, height = 4, path = PLOTS.DIR)

# Fraction of respondents lending various goods----------------------------------

df.by.good.lent <- subset(df, !is.na(answer.lent))[,
                                  list(
                                      lower = Hmisc::binconf(
                                          sum(lent), .N)[2],
                                      upper = Hmisc::binconf( 
                                          sum(lent), .N)[3],
                                      frac.lent = mean(lent)),
                                  by = input.good]
df.by.good.lent$input.good <- with(df.by.good.lent, reorder(input.good, frac.lent, mean))

g.lent <- ggplot(data = df.by.good.lent, aes(x = input.good, y = frac.lent)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))

if (interactive() && SHOW.PLOTS){
    print(g.lent)
}
writeImage(g.lent, "lent_fractions", width = 6, height = 4, path = PLOTS.DIR)


# Relationship between renting and ownership------------------------------------ 
df.cs <- data.table(df)[, list(own.frac =  mean(own, na.rm = TRUE),
                               rent.frac = mean(rent, na.rm = TRUE),
                               lent.frac = mean(rent, na.rm = TRUE),
                               borrow.frac = mean(answer.borrowed == "yes", na.rm = TRUE)
                               ),
                        by = input.good]
m.1 <- lm(rent.frac ~ own.frac, data = df.cs)
m.2 <- lm(rent.frac ~ own.frac, data = subset(df.cs, input.good != "car"))
out.file <- "../writeup/tables/own_vs_rent.tex"
s <- stargazer(m.1, m.2,
               title = "Fraction of respondents owning a good versus fraction having rented a good",
               label = "tab:own_vs_rent",
               column.labels = c("All goods", "Cars Excluded"), 
               dep.var.labels = c("Rental Fraction"),
               covariate.labels = c("Ownership Fraction")
               )
AddTableNote(s, out.file, note = "\\\\{\\footnotesize \\begin{minipage}{0.75 \\linewidth} \\emph{Notes:}
The unit of observation for the regressions in this table is the individual good.
The dependent variable is the fraction of respondents reporting having rented that good, while the indendent variable is the fraction reporting owning that good. 
Column~(1) includes all goods surveyed, while Column~(2) excludes cars.
For the full list of goods and the survey langugage, see Appendix~\\ref{sec:survey}. 
\\starlanguage \\end{minipage} }")

# Scatter plot showing rental versus ownership fractions------------------------

g.scatter <- ggplot(data = df.cs, aes(own.frac, rent.frac)) +
    geom_point() +
    geom_text(aes(label = input.good)) +
    scale_x_sqrt(label = percent) +
    scale_y_sqrt(label = percent) +
    theme_bw() +
    xlab("Fraction Owning") +
    ylab("Fraction Renting")

if (interactive() && SHOW.PLOTS){
    print(g.scatter)
}
writeImage(g.scatter, "raw_scatter_rent_v_own", width = 7, height = 7, path = PLOTS.DIR)

#---------------------------
#  Ownership by income bands
#---------------------------

df$income.band <- with(df,
                       cut(income.index, breaks = quantile(df$income.index, probs = seq(0, 1, 1/3), na.rm = TRUE),
                           labels = c("Lower 1/3 of Income", "Mid Income", "Top 1/3 of Income"))
                       )
    
# stat summary 
g.own.inc <- ggplot(data = df,
                aes(x = input.good, y = as.numeric(own), colour = factor(income.band))) +
    stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean) + 
    coord_flip() +
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent)


df.tmp <- data.table(subset(df, !is.na(income.band)))
df.tmp[, frac.own.total := mean(own, na.rm = TRUE), by = input.good]

df.by.good.by.inc <- df.tmp[, list(frac.own = mean(own, na.rm = TRUE),
                                   delta.own = mean(own, na.rm = TRUE) - mean(frac.own.total)),
                                    by = list(input.good, income.band)]


# order by variance 
df.by.good.by.inc$input.good <- with(df.by.good.by.inc, reorder(input.good, delta.own, var))

g.own.inc <- ggplot(data = df.by.good.by.inc,
                aes(x = input.good, y = delta.own)) +
    geom_point() +
    facet_wrap(~income.band, ncol = 4) + 
    geom_line(aes(group = income.band)) +
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed") + 
    coord_flip() +
    theme_bw() +
    xlab("") +
    ylab("Difference in ownership % from population mean ownership") +
    scale_y_continuous(label = percent)

print(g.own.inc)

writeImage(g.own.inc, "ownership_fractions_inc", width = 8, height = 8, path = "../writeup/plots/")
    
#####################
# Ownership and Usage 
#####################

# Goods that are owned by between 10% & 90% of the population-------------------       
df.own <- df[, list(own.frac = mean(own, na.rm = TRUE),
                    use.no.own = mean(!own & x != 0, na.rm = TRUE)), by = input.good]
middle.goods <- subset(df.own, own.frac > 0.10 & own.frac < 0.90)$input.good 

g.own.distro <- ggplot(data = subset(df, input.good %in% middle.goods),
                       aes(x = x, fill = factor(own), linetype = factor(own))) +
    geom_density(alpha = 0.25) +
    facet_wrap(~input.good.short, ncol = 4) +
    theme_bw() +
    scale_x_log10(breaks = c(   1/(365*24),      0.5/(24*12),      1/(7*24),         1/24),
                  labels = c("1 hour/year", "1/2 hour/month", "1 hour/week", "1 hour/day")) +
    scale_fill_discrete("Own's the good") +
    scale_linetype_discrete("Own's the good") + 
    xlab("Fraction of available time used (log scale)") +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))
if (interactive() && SHOW.PLOTS){
    print(g.own.distro)
}
writeImage(g.own.distro, "ownership_distro",
           width = 8, height = 8, path = PLOTS.DIR)


# Regression approach to usage and ownership------------------------------------

#' For the x == 0 case, need to replace with a dummy value (OK because of inclusion
#' of fixed effect for that observation value.

df$x.t <- df$x
df$x.t[df$x == 0] <- 10
df.realistic <- subset(df, x < 0.50)
m.1 <- lmer(own ~ log(x.t) + as.numeric(x == 0) + (1|input.good),
            data = df.realistic)
m.1.fe <- felm(own ~ log(x.t) + as.numeric(x == 0) + G(input.good), clustervar = "input.good",
                           data = df.realistic)
m.2 <- lmer(own ~ log(x.t) + as.numeric(x == 0) +  (1 + income.index | input.good) + (1 |worker.id),
            data = df.realistic)
out.file <- "../writeup/tables/ownership.tex"
s <- stargazer(m.1, m.1.fe, m.2,
               title = "Estimated product usage and ownership",
               label = "tab:ownership",
               column.labels = c("Good and Consumer REs", "Good FEs", "(1)+RE slope on income"),  
               dep.var.labels = c("Item is owned"),
               covariate.labels = c("Log frac. usage, $\\log x$", "No Use indicator, $1\\cdot\\{x = 0\\}$")
               )
AddTableNote(s, out.file, note = "\\\\{\\footnotesize \\begin{minipage}{0.90 \\linewidth} \\emph{Notes:}
This table reports regressions of an indicator for whether a respondent reported owning a good on that same respondent's estimate of how much they would use that good.
In Column~(1), I estimate a multi-level model with good-specific random effects.
In Column~(2), I instead use good-specific fixed effects and cluster at the good level.
Equation~\\ref{eq:base_own} shows the estimated equation for Columns~(1) and (2). 
In Column~(3), I re-run the estimate from Column~(1) but also allow for random slope on the respondendent's income index, as per Equation~\\ref{eq:usage_and_income} shows the estimated equation for Column~(3). 
\\starlanguage \\end{minipage} }")
                   
#############################
# Good attributes & ownership
#############################

df.no.brush <- subset(df, !(input.good %in% c("toothbrush", "back-up electric generator")))
m.1.re <- lmer(own ~ predict.index + (1|worker.id), data = df.no.brush)
m.2.re <- lmer(own ~ granular.index + (1|worker.id), data = df.no.brush)
m.3.re <- lmer(own ~ granular.index*predict.index + (1|worker.id),
               data = df.no.brush)
out.file <- "../writeup/tables/ownership_attr.tex"
s <- stargazer(m.1.re, m.2.re, m.3.re, 
               dep.var.labels = c("Item is owned"),
               covariate.labels = c("Unpredictability index (UI)", "UI x GI", "Granularity index (GI)"),
               title = "Good attributes and ownership---usage predictibility and granularity (toothbrush and electric generator excluded)",
               label = "tab:ownership_attr"
               )
AddTableNote(s, out.file, note = "\\\\ {\\footnotesize  \\begin{minipage}{0.75 \\linewidth} \\emph{Notes:} This table reports regressions of an indicator for whether the respondent owns a good on that same respondent's estimates of the unpredictability and granularity of usage for that good.
The two indices are just standardized (mean 0, standard deviation 1) of the 1-5 scale on granularity and predictability.
See Appendix~\\ref{sec:survey} for the actual survey language and responses.
In each regression, worker-specific random effect is included. 
\\end{minipage} }")

# Granularity verus unpredictability--------------------------------------------

df.gran <- subset(df, !is.na(granular.index)
                  & !is.na(predict.index))[,
                                           list(
                                               mean.gran.index = mean(granular.index),
                                               gran.index.se = sd(granular.index)/sqrt(.N),
                                               mean.predict.index = mean(predict.index),
                                               predict.index.se = sd(predict.index)/sqrt(.N)
                                           ), by = list(input.good)] 

g.scatter <- ggplot(data = df.gran,
                    aes(x = mean.gran.index,
                        y = mean.predict.index)
                    ) + geom_point() +
    geom_text(aes(label = input.good), size = 4,vjust = 1.5) +
        xlab("Mean granularity index") +
        ylab("Mean unpredictability index") +
        theme_bw()

if (interactive() && SHOW.PLOTS){
    print(g.scatter)
}
writeImage(g.scatter, "raw_granularity_versus_predictability",
           width = 7,
           height = 7, path = PLOTS.DIR)

# Granularity by item-----------------------------------------------------------

df.gran$input.good <- with(df.gran, reorder(input.good, mean.gran.index, mean))

g.gran <- ggplot(data = df.gran, aes(x = input.good, y = mean.gran.index)) +
    geom_point() +
    theme_bw() +
    geom_linerange(aes(ymin = mean.gran.index - 2*gran.index.se, ymax = mean.gran.index + 2*gran.index.se)) + 
    theme(axis.text.x=element_text(angle = -75, hjust = 0)) +
    xlab("") +
    ylab("Mean granularity index") +
if (interactive() & SHOW.PLOTS){
    print(g.gran)
}
writeImage(g.gran, "granularity", width = 7, height = 4, path = PLOTS.DIR)


# Predictability by item--------------------------------------------------------

df.gran$input.good <- with(df.gran, reorder(input.good, mean.predict.index, mean))
df.predict <- df.gran
g.predict <- ggplot(data = df.predict, aes(x = input.good, y = mean.predict.index)) +
    geom_point() +
    geom_linerange(aes(ymin = mean.predict.index - 2 * predict.index.se,
                       ymax = mean.predict.index + 2 * predict.index.se)) + 
    theme_bw() +
    theme(axis.text.x=element_text(angle = -75, hjust = 0)) +
    xlab("") +
    ylab("Mean unpredictability index") 
if (interactive() & SHOW.PLOTS){
    print(g.gran)
}
writeImage(g.predict, "predictability", width = 7, height = 4, path = PLOTS.DIR)

#########################
# Reasons for not owning
########################

df.no.own <- data.table(subset(df, !is.na(answer.no_own_reason)))
df.no.own.summary <- df.no.own[, list(num.by.reason = .N), by = list(answer.no_own_reason, input.good)]
df.no.own.summary[, total := sum(num.by.reason), by = list(input.good)]

df.no.own.summary[, reason.pct := Hmisc::binconf(num.by.reason, total)[1],
                  by = list(input.good, answer.no_own_reason)]
df.no.own.summary[, reason.lower := Hmisc::binconf(num.by.reason, total)[2],
                  by = list(input.good, answer.no_own_reason)]
df.no.own.summary[, reason.upper := Hmisc::binconf(num.by.reason, total)[3],
                  by = list(input.good, answer.no_own_reason)]
levels(df.no.own.summary$answer.no_own_reason) <- c("Don't have the money",
                                                    "Too little use to justify the purchase",
                                                    "No space")
g.reasons <- ggplot(data = df.no.own.summary,
                    aes(x = answer.no_own_reason, y = reason.pct)) +
           geom_point() +
           geom_linerange(aes(ymin = reason.lower, ymax = reason.upper)) + 
           scale_y_continuous(label = percent) + 
           facet_wrap(~input.good, ncol = 4) +
           coord_flip() +
           theme_bw() +
           ylab("Percentange of Respondents") +
           xlab("Reason given")

if (interactive() & SHOW.PLOTS) {
    print(g.reasons)
}
writeImage(g.reasons, "reasons", width = 9, height = 10, path = "../writeup/plots/")



