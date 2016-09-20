#!/usr/bin/env Rscript


# Regression approach to usage and ownership------------------------------------

#' For the x == 0 case, need to replace with a dummy value (OK because of inclusion
#' of fixed effect for that observation value.
library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()

df.no.brush <- subset(df, !is.na(predict.index)
                      & !is.na(granular.index)
                      & !is.na(own)
                      & !(input.good %in% c("toothbrush", "back-up electric generator")))

m.1 <- felm(own ~  predict.index | worker.id | 0 | worker.id, data = df.no.brush)
m.2 <- felm(own ~  granular.index | worker.id | 0 | worker.id, data = df.no.brush)
m.3 <- felm(own ~  predict.index * granular.index | worker.id | 0 | worker.id, data = df.no.brush)
m.4 <- felm(own ~  predict.index * granular.index | worker.id + input.good | 0 | worker.id, data = df.no.brush)

out.file <- "../writeup/tables/ownership_attr.tex"
s <- stargazer(m.1, m.2, m.3, m.4, 
               dep.var.labels = c("Item is owned"),
               covariate.labels = c("Unpredictability Score (US)", "Chunkiness Score (CS)", "US x CS"),
               title = "Unpredictability and chunkiness of good usage and their association with good ownership.",
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
