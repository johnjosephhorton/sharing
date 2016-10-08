#!/usr/bin/env Rscript

library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()


# Relationship between renting and ownership------------------------------------ 
df.cs <- data.table(df)[, list(own.frac =  mean(own, na.rm = TRUE),
                               rent.frac = mean(rent, na.rm = TRUE),
                               lent.frac = mean(rent, na.rm = TRUE),
                               borrow.frac = mean(answer.borrowed == "yes", na.rm = TRUE)
                               ),
                        by = input.good]
m.1 <- lm(rent.frac ~ own.frac, data = df.cs)
m.2 <- lm(rent.frac ~ own.frac, data = subset(df.cs, input.good != "car"))

out.file <- "../writeup/tables/own_v_rent.tex"

s <- stargazer(m.1, m.2,
               title = "Fraction of respondents owning a good versus fraction having rented a good",
               label = "tab:own_v_rent",
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
