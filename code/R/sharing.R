#! /usr/bin/Rscript --vanilla

###############################################
# Author: John J. Horton 
# Created Date: 2014-05-30 
# Project: sharing 
###################################################

rm(list = ls(all = T))
gc(reset = T)
set.seed(12345)

library(devtools)

devtools::install("../shaRing")
library(shaRing)

df <- shaRing::GetDF()
SHOW.PLOTS <- FALSE 
#library(ggplot2)
#library(scales)
#library(testthat)

###############################################################################
#
# Sharing / John Horton
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

# Libraries
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(stringr)
library(data.table)
library(lme4)
library(stargazer)
library(scales)
library(Hmisc)
library(lfe)
library(Hmisc)
library(ggrepel)

library(JJHmisc) # non-cran resource 

# Settings----------------------------------------------------------------------

#SHOW.PLOTS <- FALSE 
#PLOTS.DIR <- "../../writeup/plots/" 

## ConvertFactor <- function(x) {
##   # Converts factors with "na" levels to factor with missing values
##   # 
##   # Args:
##   #   x: factor vector
##   #
##   # Returns:
##   #   Factor with missing values
  
##   lev <- levels(x)
##   lev <- lev[!lev %in% c("", "na")]
  
##   x <- as.character(x) %>% tolower()
##   x[!x %in% lev] <- NA
    
##   factor(x)
## }

## #######################################
## # Read and clean data from MTurk Survey
## #######################################

## .data.file <- "../../data/sharing_survey_results_pilot.csv"

## df.raw <- read.table(.data.file, header = T, sep = ",", fill = NA) %>%
##   select(worker.id = WorkerId,
##          assignment.id = AssignmentId,         
##          assignment.status = AssignmentStatus,
##          work.time = WorkTimeInSeconds,
##          input.good = Input.good,
##          answer.own = Answer.own,
##          answer.lent = Answer.lent,
##          answer.borrowed = Answer.borrowed,
##          answer.rent = Answer.rent,
##          answer.usage = Answer.usage,
##          answer.granularity = Answer.granularity,
##          answer.predictability = Answer.predictability,
##          answer.no_own_reason = Answer.no_own_reason,
##          answer.own_income = Answer.own_income) %>% tbl_df()

## # Converts factors-------------------------------------------------------------- 
## df <- df.raw %>%
##     mutate_each(funs(ConvertFactor),
##                 answer.own,
##                 answer.lent,
##                 answer.borrowed,
##                 answer.rent,
##                 answer.no_own_reason)

## # ecode usage levels------------------------------------------------------------- 
## usage.levels <- 0:11
## usage.labels <- c(
##     "We would not use this at all",
##     "1 minute a week (about 1 hour a year)",
##     "5 minutes a week (about 4 hours a year)",
##     "1/2 an hour a week",
##     "1 hour a week",
##     "1/2 an hour a day",
##     "1 hour a day",
##     "2 hours a day",
##     "4 hours a day",
##     "8 hours a day",
##     "16 hours a day",
##     "24 hours a day (I would continuously be using this good")
## minute.per.week <- (60/7)/(24*60*60*7)
## hour.per.day <- 1.0/24.0
## day.frac <- c(0,
##               minute.per.week, 
##               5 * minute.per.week,
##               30 * minute.per.week, 
##               60 * minute.per.week,
##               0.5 * hour.per.day,
##               hour.per.day,
##               2 * hour.per.day,
##               4 * hour.per.day,
##               8 * hour.per.day,
##               15 * hour.per.day,
##               24 * hour.per.day)

## df <- within(df, {
##     usage = factor(answer.usage, levels = usage.levels, labels = usage.labels)
##     x = as.numeric(as.character(factor(answer.usage, levels = usage.levels, labels = day.frac)))
##     own = answer.own == "yes"
##     borrowed = answer.borrowed == "yes"
##     lent = answer.lent == "yes"
##     usage.index = scale(as.numeric(answer.usage))
##     income.index = scale(as.numeric(answer.own_income))
##     predict.index = scale(as.numeric(answer.predictability))
##     granular.index = scale(as.numeric(answer.granularity))
##     rent = answer.rent == "yes"
## })

## # imput family household income 
## y.levels <- c(10/2, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 200)
## df$y <- with(df, as.numeric(sapply(answer.own_income, function (x) as.numeric(y.levels[x + 1]) )))

## # fix typo
## levels(df$input.good)[16] <- "kid's bouncy castle"

## df <- data.table(df)

## df$input.good.short <- revalue(df$input.good, c("high-end digitial camera"="high-end\ndigitial camera",
##                                           "kitchen timer (or egg timer)" = "kitchen timer\n(or egg timer)",
##                                           "portable air conditioner" = "portable\nair conditioner",
##                                           "cat carrier (for transporting cats)" = "cat carrier\n(for transporting cats)",
##                                           "back-up electric generator" = "back-up\nelectric generator",
##                                           "high-end audio headphones" = "high-end\naudio headphones")
##                               )

##################################################
# Fractions owning, renting, lending and borrowing
##################################################

# Fraction of respondents owning the various goods------------------------------ 

df.by.good <- subset(df, !is.na(answer.own))[,
                             list(lower = Hmisc::binconf(sum(own), .N)[2],
                                  upper = Hmisc::binconf(sum(own), .N)[3],
                                  frac.own = mean(own)),
                             by = input.good]

library(dplyr)

df.by.good <- df %>% filter(!is.na(answer.own)) %>%
    group_by(input.good) %>%
    dplyr::summarise(
        lower = Hmisc::binconf(sum(own), n())[2],
        upper = Hmisc::binconf(sum(own), n())[3],
        frac.own = mean(own)
    ) %>%
    ungroup %>%
    mutate(input.good = reorder(input.good, frac.own, mean))


#df.by.good$input.good <- with(df.by.good, reorder(input.good, frac.own, mean))

library(ggplot2)
library(scales)

g.own <- ggplot(data = df.by.good, aes(x = input.good, y = frac.own)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = scales::percent) +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))

print(g.own)

if (interactive() && SHOW.PLOTS){
    print(g.own)
}

writeImage(g.own, "ownership_fractions", width = 6, height = 4)

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


library(JJHmisc)

g.rent <- df %>% filter(!is.na(answer.rent)) %>%
    group_by(input.good) %>%
    dplyr::summarise(
        lower = Hmisc::binconf(sum(rent), n())[2],
        upper = Hmisc::binconf(sum(rent), n())[3],
        frac.rent = mean(rent)
    ) %>%
    ungroup %>%
    mutate(input.good = reorder(input.good, frac.rent, mean)) %>%
    ggplot(aes(x = input.good, y = frac.rent)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
        theme(axis.text.x = element_text(angle = -75, hjust = 0))

writeImage(g.rent, "rental_fractions", width = 6, height = 4)

if (interactive() && SHOW.PLOTS){
    print(g.rent)
}
writeImage(g.rent, "rental_fractions", width = 6, height = 4)

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

df.by.good.lent <- df %>%
    filter(!is.na(answer.lent)) %>%
    group_by(input.good) %>%
    dplyr::summarise(
        lower = Hmisc::binconf(sum(lent), n())[2],
        upper = Hmisc::binconf(sum(lent), n())[3],
        frac.lent = mean(rent)
    ) %>%
    ungroup %>%
    mutate(input.good = reorder(input.good, frac.lent, mean))

g.lent <- ggplot(data = df.by.good.lent, aes(x = input.good, y = frac.lent)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))

print(g.lent)

if (interactive() && SHOW.PLOTS){
    print(g.lent)
}
writeImage(g.lent, "lent_fractions", width = 6, height = 4)


# Relationship between renting and ownership------------------------------------ 
df.cs <- data.table(df)[, list(own.frac =  mean(own, na.rm = TRUE),
                               rent.frac = mean(rent, na.rm = TRUE),
                               lent.frac = mean(rent, na.rm = TRUE),
                               borrow.frac = mean(answer.borrowed == "yes", na.rm = TRUE)
                               ),
                        by = input.good]
m.1 <- lm(rent.frac ~ own.frac, data = df.cs)
m.2 <- lm(rent.frac ~ own.frac, data = subset(df.cs, input.good != "car"))
out.file <- "../../writeup/tables/own_vs_rent.tex"
s <- stargazer(m.1, m.2,
               title = "Fraction of respondents owning a good versus fraction having rented a good",
               label = "tab:own_vs_rent",
               omit.stat = c("aic", "f", "adj.rsq", "bic", "ser"),
               font.size = "footnotesize",
               no.space = TRUE,
               align = TRUE, 
               add.lines = list(c("Sample", "\\multicolumn{1}{c}{All Goods}", "\\multicolumn{1}{c}{Cars Excluded}")),
               dep.var.labels = c("Fraction reporting renting the good (\\textsc{FracRental})"),
               covariate.labels = c("Fraction reporting owning the good")
               )
AddTableNote(s, out.file, note = "\\\\{\\footnotesize \\begin{minipage}{0.85 \\linewidth} \\emph{Notes:}
The unit of observation for the regressions in this table is the individual good.
The dependent variable is the fraction of respondents reporting having rented that good, while the independent variable is the fraction reporting owning that good. 
Column~(1) includes all goods surveyed, while Column~(2) excludes cars.
For the full list of goods and the survey language, see Appendix~\\ref{sec:survey}. 
\\starlanguage \\end{minipage} }")

# Scatter plot showing rental versus ownership fractions------------------------

#install.packages("ggrepel")




short.list <- c("BBQ Grill" = "BBQ",
                "a men's suit" = "suit",
                "back-up electric generator" = "back-up gen",
                "blender",
                "canoe",
                "car",
                "cat carrier (for transporting cats)" = "cat carrier",
                "cordless power drill" = "drill",
                "diamond necklace" = "necklace",
                "food processor" = "food processor", 
                "hammer",
                "high-end audio headphones" = "headphones",
                "high-end digitial camera" = "camera", 
                "iPad or tablet" = "iPad",
                "jet ski",
                "kid's bouncy castle" = "bouncy castle",
                "kitchen timer (or egg timer)" = "egg timer", 
                "mountain bike" = "mt. bike",
                "pick-up truck" = "pick-up",
                "portable air conditioner" = "AC", 
                "push lawnmower",
                "RO lawnmower",
                "sewing machine",
                "toothbrush", 
                "tuxedo",
                "vacation home")

break.list <- c(0.0, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0)

g.scatter <- ggplot(data = df.cs, aes(own.frac, rent.frac)) +
    geom_point() +
        scale_x_sqrt(label = percent, breaks = break.list) +
            scale_y_sqrt(label = percent, breaks = break.list) +
                geom_label_repel(data = df.cs %>% mutate(input.good = revalue(input.good, short.list)),
                                 aes(label = input.good), force = 4, max.iter = 1000) + 
        theme_bw() + 
    xlab("Fraction Owning") +
    ylab("Fraction Renting")


#with(df.cs, revalue(input.good, ))

print(g.scatter)

if (interactive() && SHOW.PLOTS){
    print(g.scatter)
}

writeImage(g.scatter, "scatter_rent_v_own", width = 9, height = 4)

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

#print(g.own.inc)

writeImage(g.own.inc, "ownership_fractions_inc", width = 8, height = 8)
    
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
           width = 8, height = 8)


# Regression approach to usage and ownership------------------------------------

#' For the x == 0 case, need to replace with a dummy value (OK because of inclusion
#' of fixed effect for that observation value.

df$x.t <- df$x
df$x.t[df$x == 0] <- 10
df.realistic <- subset(df, x < 0.50 & x != 0 & !is.na(y))

## m.1 <- lmer(own ~ log(x.t) + (1|input.good),
##             data = df.realistic)

m.1 <- felm(own ~  log(x.t) | input.good | 0 | input.good, data = df.realistic)
m.2 <- felm(own ~  log(x.t) + log(y) | input.good | 0 | input.good, data = df.realistic)
m.3 <- felm(own ~  log(x.t) | input.good + worker.id | 0 | input.good, data = df.realistic)

ggplot(data = df.realistic, aes(x = log(x.t), colour = own, linetype = own)) + geom_density() + facet_wrap(~input.good)

out.file <- "../../writeup/tables/ownership.tex"
s <- stargazer(m.1,  m.2, m.3, 
               title = "Respondent estimates of the fraction of time spent using a good and whether they own that good",
               label = "tab:ownership",
               align = TRUE,
               font.size = "footnotesize",
               omit.stat = c("aic", "f", "adj.rsq", "bic", "ser"),
               no.space = TRUE,
               digits = 4, 
               add.lines = list(c(
                   "Good FE",
                   "\\multicolumn{1}{c}{Y}",
                   "\\multicolumn{1}{c}{Y}",
                   "\\multicolumn{1}{c}{Y}"),
                   c("Respondent FE",
                     "\\multicolumn{1}{c}{N}",
                     "\\multicolumn{1}{c}{N}",
                     "\\multicolumn{1}{c}{Y}")
                   ),            
               dep.var.labels = c("Respondent owns the item?, ($\\textsc{Own}_{ig}=1$)"),
               covariate.labels = c("Log estimated usage, $\\log x_{ig}$", "Log household income, $\\log y_i$")
               )
AddTableNote(s, out.file, note = "\\\\{\\footnotesize \\begin{minipage}{0.75 \\linewidth} \\emph{Notes:}
This table reports OLS regressions where the dependent variable is an indicator for whether a respondent reported owning a particular good.
In Column~(1), the independent variable is that respondent's estimate of what fraction of time he or she would spend using that good (in logs).
In Column~(2), a regressor for the log of the respondent's self-reported household income is added to the Column~(1) specification.
Column~(3) uses the same specification as Column~(1), but a respondent-specific fixed effect is added. 
The sample is restricted to respondents who report some positive amount of predicted usage of the good and reported their household income.
All regressions include good-specific fixed effects, and standard errors are clustered at the good level. 
\\starlanguage \\end{minipage} }")

###############
# Normal Goods? 
###############

m <- lm(own ~ log(y) * input.good, data = df.realistic)


#############################
# Good attributes & ownership
#############################

df.no.brush <- subset(df, !is.na(predict.index)
                      & !is.na(granular.index)
                      & !is.na(own)
                      & !(input.good %in% c("toothbrush", "back-up electric generator")))

#m.1.re <- lmer(own ~ predict.index + (1|worker.id), data = df.no.brush)
#m.2.re <- lmer(own ~ granular.index + (1|worker.id), data = df.no.brush)
#m.3.re <- lmer(own ~ granular.index*predict.index + (1|worker.id),
#               data = df.no.brush)

m.1 <- felm(own ~  predict.index | worker.id | 0 | worker.id, data = df.no.brush)
m.2 <- felm(own ~  granular.index | worker.id | 0 | worker.id, data = df.no.brush)
m.3 <- felm(own ~  predict.index * granular.index | worker.id | 0 | worker.id, data = df.no.brush)
m.4 <- felm(own ~  predict.index * granular.index | worker.id + input.good | 0 | worker.id, data = df.no.brush)


out.file <- "../../writeup/tables/ownership_attr.tex"
s <- stargazer(m.1, m.2, m.3, m.4, 
               dep.var.labels = c("Item is owned"),
               covariate.labels = c("Unpredictability Score (US)", "Chunkiness Score (CS)", "US x CS"),
               title = "Good usage unpredictability and chunkiness and its association with good ownership.",
               label = "tab:ownership_attr",
                              align = TRUE,
               font.size = "footnotesize",
               omit.stat = c("aic", "f", "adj.rsq", "bic", "ser"),
               no.space = TRUE,
               add.lines = list(
                   c("Respondent FE",
                     "\\multicolumn{1}{c}{Y}",
                     "\\multicolumn{1}{c}{Y}",
                     "\\multicolumn{1}{c}{Y}",
                     "\\multicolumn{1}{c}{Y}"),
                   c("Good FE",
                     "\\multicolumn{1}{c}{N}",
                     "\\multicolumn{1}{c}{N}",
                     "\\multicolumn{1}{c}{N}",
                     "\\multicolumn{1}{c}{Y}")
                  )) 
AddTableNote(s, out.file, note = "\\\\ {\\footnotesize  \\begin{minipage}{0.85 \\linewidth} \\emph{Notes:}
This table reports regressions of an indicator for whether the respondent owns a good on that same respondent's estimates of the unpredictability and granularity of usage for that good.
The two indices are normalized responses to the 1-5 scale questions on usage chunkiness and unpredictability, pooled over all respondents and goods.
Toothbrushes and backup generators are excluded from the sample. 
See Appendix~\\ref{sec:survey} for the actual survey language and responses.
In each regression, a respondent-specific fixed effect is included.
Standard errors are clustered at the level of the individual respondent.
\\starlanguage
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
    geom_label_repel(aes(label = input.good), force = 2, max.iter = 10000) +
        ylab("Mean unpredictability score") +
        xlab("Mean chunkiness score") +
        theme_bw()

print(g.scatter)

if (interactive() && SHOW.PLOTS){
    print(g.scatter)
}

writeImage(g.scatter, "granularity_versus_predictability",
           width = 8.0,
           height = 6.0)

# Granularity by item-----------------------------------------------------------

df.gran$input.good <- with(df.gran, reorder(input.good, mean.gran.index, mean))

g.gran <- ggplot(data = df.gran, aes(x = input.good, y = mean.gran.index)) +
    geom_point() +
    theme_bw() +
    geom_linerange(aes(ymin = mean.gran.index - 2*gran.index.se, ymax = mean.gran.index + 2*gran.index.se)) + 
    theme(axis.text.x=element_text(angle = -75, hjust = 0)) +
    xlab("") +
    ylab("Mean chunkiness score") +
if (interactive() & SHOW.PLOTS){
    print(g.gran)
}

writeImage(g.gran, "granularity", width = 7, height = 4)


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
writeImage(g.predict, "predictability", width = 7, height = 4)

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

writeImage(g.reasons, "reasons_raw", width = 4.5, height = 5)


#' Modified version
df.no.own <- data.table(subset(df, !is.na(answer.no_own_reason) & answer.no_own_reason != "space"))

library(dplyr)

## df.tmp <- df %>% group_by(input.good) %>%
##     mutate(frac.not.owning = mean(!is.na(answer.no_own_reason))) %>%
##     ungroup() %>% 
##     filter(!is.na(answer.no_own_reason) & answer.no_own_reason != "space") %>%
##     group_by(input.good) %>%
##     summarise(frac.not.owning = frac.not.owning[1],
##               num.obs = dplyr::n(),
##               frac.income = mean(answer.no_own_reason = "expensive"),
##               frac.use = mean(answer.no_own_reason = "little_use")
##     ) %>% ungroup()

                         
df.no.own.summary <- df.no.own[, list(
    num.obs = .N,
    num.income = sum(answer.no_own_reason == "expensive"), 
    frac.income = mean(answer.no_own_reason == "expensive"),
    frac.use = mean(answer.no_own_reason == "little_use")), by = list(input.good)]

g.reasons <- ggplot(data = subset(df.no.own.summary, num.obs > 7),  aes(x = frac.income, y = frac.use)) +
    geom_point() +
        geom_text_repel(aes(label = input.good)) + 
            theme_bw() +
                xlab("Fraction non-owners citing income") +
    ylab("Fraction non-owners citing usage") +
    geom_abline(intercept = 1, slope = -1)


library(magrittr)

df.tmp <- df.no.own.summary %>% filter(num.obs > 7) 
df.tmp$input.good <- with(df.tmp, reorder(input.good, frac.income, mean))

# lower = Hmisc::binconf(sum(own), .N)[2],

df.tmp <- df.tmp %>% cbind(with(df.tmp, Hmisc::binconf(num.income, num.obs)) %>% as.data.frame)
                

g.reasons <- ggplot(data = df.tmp,
       aes(y = input.good, x = frac.income)) +
           geom_point() +
               geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0, colour = "grey") + 
               theme_bw() +
                   scale_x_continuous(label = scales::percent) +
                       xlab("Fraction citing income = (1 - Fraction citing usage)") +
                           ylab("")

writeImage(g.reasons, "reasons", width = 6, height = 3)


#print(g.reasons)
#writeImage(g.reasons, "reasons_raw_2", width = 8, height = 4)




