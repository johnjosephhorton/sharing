#!/usr/bin/env Rscript

library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()



df.gran <- subset(df, !is.na(granular.index)
                  & !is.na(predict.index))[,
                                           list(
                                               mean.gran.index = mean(granular.index),
                                               gran.index.se = sd(granular.index)/sqrt(.N),
                                               mean.predict.index = mean(predict.index),
                                               predict.index.se = sd(predict.index)/sqrt(.N)
                                           ), by = list(input.good)] 


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

JJHmisc::writeImage(g.predict, "predictability", path = "../writeup/plots/", width = 7, height = 4)
