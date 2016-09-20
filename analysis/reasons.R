#!/usr/bin/env Rscript

library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()

df.no.own <- data.table(subset(df, !is.na(answer.no_own_reason) & answer.no_own_reason != "space"))
                         
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

df.tmp <- df.no.own.summary %>% filter(num.obs > 7) 
df.tmp$input.good <- with(df.tmp, reorder(input.good, frac.income, mean))

df.tmp <- df.tmp %>% cbind(with(df.tmp, Hmisc::binconf(num.income, num.obs)) %>% as.data.frame)
                

g.reasons <- ggplot(data = df.tmp,
       aes(y = input.good, x = frac.income)) +
           geom_point() +
               geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0, colour = "grey") + 
               theme_bw() +
                   scale_x_continuous(label = scales::percent) +
                       xlab("Fraction citing income = (1 - Fraction citing usage)") +
                           ylab("")

JJHmisc::writeImage(g.reasons, "reasons", path = "../writeup/plots/", width = 6, height = 3)
