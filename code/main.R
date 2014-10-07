#!/usr/bin/Rscript
###############################################################################
#
# Sharing / John Horton
# oDesk Contract #13552832
#
# Code for import & basic analysis
#
# Author: Alexander Gedranovich & John Horton 
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

library(JJHmisc) # non-cran resource 

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
.data.file <- "../data/sharing_survey_results_pilot.csv"

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
         answer.own_income = Answer.own_income) %>% tbl_df()

# Converts factors
df <- df %>% mutate_each(funs(ConvertFactor), answer.own, answer.lent,
                         answer.borrowed, answer.rent, answer.no_own_reason)

# ecode usage levels 
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
hour.per.day <- (1/24)

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
              
df$usage <- with(df, factor(answer.usage, levels = usage.levels, labels = usage.labels))
df$x <- as.numeric(as.character(with(df, factor(answer.usage, levels = usage.levels, labels = day.frac))))

df <- within(df, {
  own = answer.own == "yes"
  borrowed = answer.borrowed == "yes"
  lent = answer.lent == "yes"
  usage.index = scale(as.numeric(answer.usage))
  income.index = scale(as.numeric(answer.own_income))
  predict.index = scale(as.numeric(answer.predictability))
  granular.index = scale(as.numeric(answer.granularity))
  rent = answer.rent == "yes"
})
   

#-------------------------------------------------
# Fraction of respondents owning the various goods 
#-------------------------------------------------

df.by.good <- data.table(df)[, list(frac.own = mean(answer.own == "yes", na.rm = TRUE)), by = input.good]
df.by.good$input.good <- with(df.by.good, reorder(input.good, frac.own, mean))

# dplyr way
# df.by.good <- df %>% 
#   group_by(input.good) %>% 
#   summarise(frac.own = mean(answer.own == "yes", na.rm = T)) %>%
#   mutate(input.good = reorder(input.good, frac.own, mean))

g.own <- ggplot(data = df.by.good,
                aes(x = input.good, y = frac.own)) +
    geom_point() +
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x=element_text(angle = -75, hjust = 0))

if (interactive()){
    print(g.own)
}
    
writeImage(g.own, "ownership_fractions", width = 6, height = 4, path = "../writeup/plots/")


#--------
# Renting 
#--------

df.by.good.rent <- data.table(df)[, list(frac.rent = mean(answer.rent == "yes", na.rm = TRUE)), by = input.good]
df.by.good.rent$input.good <- with(df.by.good.rent, reorder(input.good, frac.rent, mean))


g.rent <- ggplot(data = df.by.good.rent,
                aes(x = input.good, y = frac.rent)) +
    geom_point() +
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x=element_text(angle = -75, hjust = 0))

writeImage(g.rent, "rental_fractions", width = 6, height = 4, path = "../writeup/plots/")

#--------
# Lending
#--------

df.by.good.lent <- data.table(df)[, list(frac.lent = mean(answer.lent == "yes", na.rm = TRUE)), by = input.good]
df.by.good.lent$input.good <- with(df.by.good.lent, reorder(input.good, frac.lent, mean))


g.lent <- ggplot(data = df.by.good.lent,
                aes(x = input.good, y = frac.lent)) +
    geom_point() +
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
    theme(axis.text.x=element_text(angle = -75, hjust = 0))

writeImage(g.lent, "lent_fractions", width = 6, height = 4, path = "../writeup/plots/")


# Scatter plot
df.cs <- data.table(df)[, list(own.frac =  mean(answer.own=="yes", na.rm = TRUE),
                               rent.frac = mean(answer.rent=="yes", na.rm = TRUE),
                               lent.frac = mean(answer.lent=="yes", na.rm = TRUE),
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
The dependent variable is the graction of respondents reporting having rented that good, while the indendent variable is the fraction reporting owning that good. 
Column~(1) includes all goods surveyed, while Column~(2) excludes cars.
For the full list of goods and the survey langugage, see Appendix~\\ref{sec:survey}. 
\\starlanguage \\end{minipage} }")

g.scatter <- ggplot(data = df.cs, aes(own.frac, rent.frac)) +
    geom_point() +
    geom_text(aes(label = input.good)) +
    scale_x_sqrt() +
        scale_y_sqrt() +
            theme_bw() +
                geom_smooth(method = "lm") +
                    xlab("Fraction Owning") + ylab("Fraction renting")

writeImage(g.scatter, "raw_scatter_rent_v_own", width = 7, height = 7, path = "../writeup/plots/")


#
#

g.scatter <- ggplot(data = df.cs, aes(own.frac, borrow.frac)) +
    geom_point() +
    geom_text(aes(label = input.good)) +
    scale_x_sqrt() +
        scale_y_sqrt() +
            theme_bw() +
                geom_smooth() +
                    xlab("Fraction Owning") + ylab("Fraction Lending")


writeImage(g.scatter, "raw_scatter_lent_v_own", width = 7, height = 7, path = "../writeup/plots/")

# writeImage(g.scatter, "lent_v_own", width = 7, height = 7, path = "../writeup/plots/")



print(g.scatter)



g.scatter <- ggplot(data = df.cs, aes(own.frac, lent.frac)) +
    geom_point() +
    geom_text(aes(label = input.good)) +
    scale_x_sqrt() +
        scale_y_sqrt() +
            theme_bw() +
                geom_smooth(method = "lm") +
                    xlab("Fraction Owning") + ylab("Fraction Lending")


print(g.scatter)


#---------------------------
#  Ownership by oncome bands
#---------------------------

df$income.band <- with(df,
                       cut(income.index, breaks = quantile(df$income.index, na.rm = TRUE),
                           labels = c("0-25", "25-50", "50-75", "75-100"))
                       )

df$income.band <- with(df,
                       cut(income.index, breaks = quantile(df$income.index, probs = seq(0, 1, 1/3), na.rm = TRUE),
                           labels = c("Lower 1/3 of Income", "Mid Income", "Top 1/3 of Income"))
                       )
    

# why are the quantiles not equally sized?

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
df.tmp[, frac.own.total := mean(answer.own == "yes", na.rm = TRUE), by = input.good]

df.by.good.by.inc <- df.tmp[, list(frac.own = mean(answer.own == "yes", na.rm = TRUE),
                                   delta.own = mean(answer.own == "yes", na.rm = TRUE) - mean(frac.own.total)),
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
    

## df.by.good.by.inc$input.good <- with(df.by.good.by.inc, reorder(input.good, frac.own, mean))

## g.own.inc <- ggplot(data = df.by.good.by.inc,
##                 aes(x = input.good, y = frac.own)) +
##     geom_point() +
##     geom_line(aes(group = income.band)) +
##     facet_wrap(~income.band, ncol = 4) + 
##     coord_flip() +
##     theme_bw() +
##     xlab("") +
##     ylab("Fraction of respondents") +
##     scale_y_continuous(label = percent)



## print(g.own.inc)


## writeImage(g.own.inc, "ownership_fractions_inc", width = 8, height = 8, path = "../writeup/plots/")

#-------------------
# Ownership analysis
#-------------------

g.own.distro <- ggplot(data = df, aes(x = x, fill = factor(own))) +
    geom_density(alpha = 0.5) +
        facet_wrap(~input.good, ncol = 4) +
            theme_bw() +
                scale_x_log10() +
                    scale_fill_discrete("Ownership") + 
                    xlab("Fraction of available time used (log scale)")

writeImage(g.own.distro, "ownership_distro", width = 7, height = 7, path = "../writeup/plots/")


m <- lmer(own ~ log(x) + (1 + income.index | input.good) + (1 |worker.id), data = subset(df, x > 0 & x < 0.50))

df$x.t <- df$x
df$x.t[df$x == 0] <- 10

library(lfe)

m.1 <- lmer(own ~ log(x.t) + as.numeric(x == 0) + (1|input.good), data = subset(df, x < 0.50))
m.1.fe <- felm(own ~ log(x.t) + as.numeric(x == 0) + G(input.good), clustervar = "input.good", data = subset(df, x < 0.50))
m.2 <- lmer(own ~ log(x.t) + as.numeric(x == 0) +  (1 + income.index | input.good) + (1 |worker.id), data = subset(df, x < 0.50))

out.file <- "../writeup/tables/ownership.tex"

s <- stargazer(m.1, m.1.fe, m.2,
               title = "Estimated product usage and ownership",
               label = "tab:ownership",
               column.labels = c("Good and Consumer REs", "Good FEs", "(1) + Good-specific income slope"),  
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


# Goods that are owned by between 10 & 90% of the population 
df.own <- data.table(df)[, list(own.frac = mean(own, na.rm = TRUE)), by = input.good]
middle.goods <- subset(df.own, own.frac > 0.10 & own.frac < 0.90)$input.good 
with(df, table(input.good, usage == "We would not use this at all"))
m <- lmer(own ~ usage.index + I(usage == "We would not use this at all") + (1 + income.index | input.good),
          data = subset(df, input.good %in% middle.goods))
m <- lmer(own ~ usage.index + I(usage == "We would not use this at all") + (1 + income.index | input.good),
          data = subset(df, input.good %in% middle.goods))
m <- lmer(own ~ usage.index + granular.index * predict.index + (income.index|input.good), data = df)
m <- lm(own ~ income.index * input.good, data = df)

# Are there goods where income effects matter more? 
m.1 <- lm(own ~ usage.index, data = subset(df, input.good %in% middle.goods))
m.1 <- lm(own ~ usage.index*income.index, data = subset(df, input.good %in% middle.goods &
                                           usage != "We would not use this at all"))

ggplot(data = subset(df, input.good %in% middle.goods), aes(x = usage.index, as.numeric(own))) +
    geom_smooth(method = "lm") +
    geom_point(position = "jitter") +
        facet_wrap(~input.good, ncol = 4) +
            theme_bw()

ggplot(data = subset(df, input.good %in% middle.goods & usage != "We would not use this at all"),
       aes(x = usage.index, as.numeric(own))) +
    geom_smooth(method = "lm") +
    geom_point(position = "jitter") +
        facet_wrap(~input.good, ncol = 4) +
            theme_bw()

ggplot(data = df, aes(x = usage.index)) +
    geom_histogram() +
    facet_wrap(~input.good, ncol = 4)

df <- subset(df, !is.na(own))
    
    
df$decision <- with(df, ifelse(own, "Owns", "Does not own"))

g.usage.by.own <- ggplot(data = df,
                         aes(x = decision, y = usage.index)) +
    geom_boxplot() +
    facet_wrap(~input.good, ncol = 4) +
    theme_bw() +
    xlab("")

if (interactive()){
    print(g.usage.by.own)
}

writeImage(g.usage.by.own, "usage_by_own", width = 7, height = 7, path = "../writeup/plots/")


df$input.good <- revalue(df$input.good, c("high-end digitial camera"="high-end\ndigitial camera",
                                          "kitchen timer (or egg timer)" = "kitchen timer\n(or egg timer)",
                                          "portable air conditioner" = "portable\nair conditioner",
                                          "cat carrier (for transporting cats)" = "cat carrier\n(for transporting cats)",
                                          "back-up electric generator" = "back-up\nelectric generator",
                                          "high-end audio headphones" = "high-end\naudio headphones")
                         )

m.1 <- lmer(own ~ usage.index + (1|input.good), data = df)
m.2 <- lm(own ~ usage.index + income.index, data = df)
m.3 <- lmer(own ~ usage.index + income.index + (1|worker.id), data = df)

m.4 <- lmer(own ~ usage.index + income.index + (1|input.good), data = subset(df, answer.usage !=0))
m.1.a <- lmer(own ~ usage.index + income.index + (1|input.good), data = subset(df, answer.usage != 0))
m.1.b <- lmer(own ~ usage.index + income.index + (1|input.good), data = df)


m.1 <- lm(own ~ usage.index, data = subset(df, input.good %in% middle.goods))
m.2 <- lm(own ~ usage.index + I(usage == "We would not use this at all"), data = subset(df, input.good %in% middle.goods))

m.3 <- lmer(own ~ usage.index + I(usage == "We would not use this at all") + (1 + income.index | input.good),
            data = subset(df, input.good %in% middle.goods))

p <- 0.10
middle.goods <- subset(df.own, own.frac > p & own.frac < 1 - p)$input.good 

m <- lmer(own ~ log(x) + (1 + income.index | input.good),
            data = subset(df, input.good %in% middle.goods
                & x > 0))
summary(m)

ggplot(data = df, aes(sqrt(x))) + geom_histogram()

m <- lmer(own ~ sqrt(x) + (1 + income.index | input.good), data = subset(df, input.good %in% middle.goods &
                                                                         x < 1))
summary(m)


m <- lmer(own ~ sqrt(x) + (1 + income.index | input.good), data = subset(df, input.good %in% middle.goods &
                                                                         x < 1 & x > 0))
summary(m)



ggplot(data = df, aes(
           x = usage,
           fill = factor(answer.own))) +
    facet_wrap(~input.good, ncol = 5) + 
    geom_bar(position = "dodge") + coord_flip()

#----------------------------
# Good attributes & ownership
#----------------------------

m.1 <- lm(own ~ predict.index, data = df)
m.2 <- lm(own ~ granular.index, data = df)
m.3 <- lm(own ~ granular.index*predict.index, data = df)

df.tmp <- subset(df, !(input.good %in% c("toothbrush", "back-up\nelectric generator")))

m.1.re <- lmer(own ~ predict.index + (1|worker.id), data = df.tmp)
m.2.re <- lmer(own ~ granular.index + (1|worker.id), data = df.tmp)
m.3.re <- lmer(own ~ granular.index*predict.index + (1|worker.id), data = df.tmp)

#m.3.re.limit <- lmer(own ~ granular.index*predict.index + (1|worker.id), data = df.tmp)

#stargazer(m.1.re, m.2.re, m.3.re, type = "text")

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

#---------------------
# Granularity by item
#---------------------

df.gran <- data.table(df)[, list(mean.gran.index = mean(granular.index, na.rm = TRUE),
                                 mean.predict.index = mean(predict.index, na.rm = TRUE)
                                 ), by = list(input.good)] 


g.scatter <- ggplot(data = df.gran,
                    aes(x = mean.gran.index,
                        y = mean.predict.index)
                    ) + geom_point() +
    geom_text(aes(label = input.good), size = 4,vjust = 1.5) +
        xlab("Mean granularity index") +
            ylab("Mean unpredictability index") +
                theme_bw()

writeImage(g.scatter, "raw_granularity_versus_predictability",
           width = 7,
           height = 7, path = "../writeup/plots/")

df.gran$input.good <- with(df.gran, reorder(input.good, mean.gran.index, mean))

g.gran <- ggplot(data = df.gran, aes(x = input.good, y = mean.gran.index)) +
    geom_point() + theme_bw() +
        theme(axis.text.x=element_text(angle = -75, hjust = 0))

writeImage(g.gran, "granularity", width = 7, height = 4, path = "../writeup/plots/")

#-----------------------
# Predictability by item
#-----------------------

df.gran$input.good <- with(df.gran, reorder(input.good, mean.predict.index, mean))

g.predict <- ggplot(data = df.gran, aes(x = input.good, y = mean.predict.index)) +
    geom_point() +
        theme_bw() +
            theme(axis.text.x=element_text(angle = -75, hjust = 0))

writeImage(g.predict, "predictability", width = 7, height = 4, path = "../writeup/plots/")

#----------------------------
# Good attributes and lending
#----------------------------

df.own <- data.table(subset(df, own))

df.own[, frac.lent := mean(lent), by = list(input.good)]
df.own[, num.own := .N, by = list(input.good)]

m.1 <- lm(lent ~ predict.index, data = subset(df.own, num.own > 10))
m.1 <- lm(lent ~ predict.index, data = subset(df, own & input.good != "car"))

g <- ggplot(data = subset(df.own, num.own > 5),
   aes(x = predict.index, y = as.numeric(lent))) +
   geom_smooth(method="lm") +
   geom_point() +
   facet_wrap(~input.good, ncol = 4) 

g <- ggplot(data = subset(df.own, num.own > 5),
   aes(x = granular.index, y = as.numeric(lent))) +
   geom_smooth(method="lm") +
   geom_point() +
   facet_wrap(~input.good, ncol = 4) 

print(g)

m <- lm(as.numeric(lent) ~ predict.index, data = subset(df.own, num.own > 5))

m <- lm(as.numeric(lent) ~ granular.index, data = subset(df.own, num.own > 10))

m <- lm(as.numeric(lent) ~ granular.index * predict.index, data = subset(df.own, num.own > 5))


print(g)

g <- ggplot(data = subset(df.own, num.own > 10), aes(x = predict.index)) + geom_histogram()


df.own$y <- as.numeric(df.own$lent)

g <- ggplot(data = subset(df.own, num.own > 10), aes(x = predict.index, y = y)) + geom_histogram()


print(g)

m.2 <- lm(lent ~ granular.index, data = subset(df, own))



#----------
# Borrowing 
#----------

m.1 <- lm(borrowed ~ usage.index + income.index, data = subset(df, !own))
m.2 <- lm(borrowed ~ granular.index*predict.index, data = subset(df, !own))

#--------
# Renting 
#--------

m.1 <- lm(rent ~ usage.index + income.index, data = subset(df, !own))


df.no.own <- data.table(subset(df, !is.na(answer.no_own_reason)))
df.no.own.summary <- df.no.own[, list(num.by.reason = .N), by = list(answer.no_own_reason, input.good)]
df.no.own.summary[, total := sum(num.by.reason), by = list(input.good)]

df.no.own.summary[, reason.pct := Hmisc::binconf(num.by.reason, total)[1], by = list(input.good, answer.no_own_reason)]
df.no.own.summary[, reason.lower := Hmisc::binconf(num.by.reason, total)[2], by = list(input.good, answer.no_own_reason)]
df.no.own.summary[, reason.upper := Hmisc::binconf(num.by.reason, total)[3], by = list(input.good, answer.no_own_reason)]




levels(df.no.own.summary$answer.no_own_reason) <- c("Don't have the money","Too little use to justify the purchase", "No space")

g.reasons <- ggplot(data = df.no.own.summary,
                    aes(x = answer.no_own_reason, y = reason.pct)) +
           geom_point() +
               geom_linerange(aes(ymin = reason.lower, ymax = reason.upper)) + 
               scale_y_continuous(label = percent) + 
                   facet_wrap(~input.good, ncol = 4) +
                       coord_flip() +
                           theme_bw() + ylab("Percentange of Respondents") +
                               xlab("Reason given")

print(g.reasons)

writeImage(g.reasons, "reasons", width = 9, height = 10, path = "../writeup/plots/")

# The less predictable, the more like a good is to be owned 
m <- lm(I(answer.own == "yes") ~ as.numeric(answer.predictability), data = df)

# the more it is broken into small chunks 
m <- lm(I(answer.own == "yes") ~ scale(as.numeric(answer.granularity)), data = df)


m <- lm(I(answer.lent == "yes") ~ as.numeric(answer.predictability), data = subset(df, answer.own == "yes"))

# Are owners of 'input.good' ever borrowed or rent it?
df.own_rent_borrowed <- df %>%
  select(input.good, answer.own, answer.borrowed, answer.rent) %>% 
  gather(action, answer, -input.good, -answer.own) %>%
  rowwise() %>%
  mutate(answer = sum(answer == "yes", na.rm = T)) %>%
  group_by(input.good, answer.own, action) %>%
  summarise(answer = sum(answer)) %>%
  na.omit %>% ungroup() %>%
  mutate(answer.own = factor(answer.own, levels = c("no", "yes"), 
                             labels = c("Does not own", "Owns")))

g.owned.borrowed <- ggplot(df.own_rent_borrowed, aes(x = answer.own, y = answer, fill = action)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ input.good, ncol = 4) + 
  xlab("") + ggtitle("Rents & borrows anong owners/non-owners") + 
  theme_bw() + coord_flip()
g.owned.borrowed

writeImage(g.owned.borrowed, "rents_borrows", width = 9, height = 10, path = "../writeup/plots/")



# # Number of owners of 'input.good'
# df %>% 
#   select(input.good, answer.own) %>% 
#   group_by(input.good, answer.own) %>% 
#   summarise(count = n()) %>%
#   spread(answer.own, count, fill = 0) %>%
#   mutate(total = no + yes + `NA`)
# 
# Reasons for not owning 'input.good'
# df %>% 
#   select(input.good, answer.no_own_reason) %>% 
#   group_by(input.good, answer.no_own_reason) %>% 
#   summarise(count = n()) %>%
#   spread(answer.no_own_reason, count, fill = 0) %>%
#   mutate(total = expensive + little_use + space + `NA`)

