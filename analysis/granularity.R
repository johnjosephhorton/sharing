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



df.gran$input.good <- with(df.gran, reorder(input.good, mean.gran.index, mean))

g.gran <- ggplot(data = df.gran, aes(x = input.good, y = mean.gran.index)) +
    geom_point() +
    theme_bw() +
    geom_linerange(aes(ymin = mean.gran.index - 2*gran.index.se, ymax = mean.gran.index + 2*gran.index.se)) + 
    theme(axis.text.x=element_text(angle = -75, hjust = 0)) +
    xlab("") +
    ylab("Mean chunkiness score") 
        
JJHmisc::writeImage(g.gran, "granularity", path = "../writeup/plots/", width = 7, height = 4)
