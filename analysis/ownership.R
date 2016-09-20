#!/usr/bin/env Rscript


# Regression approach to usage and ownership------------------------------------

#' For the x == 0 case, need to replace with a dummy value (OK because of inclusion
#' of fixed effect for that observation value.
library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()

df$x.t <- df$x
df$x.t[df$x == 0] <- 10
df.realistic <- subset(df, x < 0.50 & x != 0 & !is.na(y))

m.1 <- felm(own ~  log(x.t) | input.good | 0 | input.good, data = df.realistic)
m.2 <- felm(own ~  log(x.t) + log(y) | input.good | 0 | input.good, data = df.realistic)
m.3 <- felm(own ~  log(x.t) | input.good + worker.id | 0 | input.good, data = df.realistic)

out.file <- "../writeup/tables/ownership.tex"
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
JJHmisc::AddTableNote(s, out.file, note = "\\\\{\\footnotesize \\begin{minipage}{0.75 \\linewidth} \\emph{Notes:}
This table reports OLS regressions where the dependent variable is an indicator for whether a respondent reported owning a particular good.
In Column~(1), the independent variable is that respondent's estimate of what fraction of time he or she would spend using that good (in logs).
In Column~(2), a regressor for the log of the respondent's self-reported household income is added to the Column~(1) specification.
Column~(3) uses the same specification as Column~(1), but a respondent-specific fixed effect is added. 
The sample is restricted to respondents who report some positive amount of predicted usage of the good and reported their household income.
All regressions include good-specific fixed effects, and standard errors are clustered at the good level. 
\\starlanguage \\end{minipage} }")
