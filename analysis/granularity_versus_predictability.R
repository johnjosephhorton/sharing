#!/usr/bin/env Rscript

library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()

# Granularity verus unpredictability--------------------------------------------

df.gran <- subset(df, !is.na(granular.index)
                  & !is.na(predict.index))[,
                                           list(
                                               mean.gran.index = mean(granular.index),
                                               gran.index.se = sd(granular.index)/sqrt(.N),
                                               mean.predict.index = mean(predict.index),
                                               predict.index.se = sd(predict.index)/sqrt(.N)
                                           ), by = list(input.good)] 

chunky.right <- annotation_custom(
    grob = textGrob(label = "Small usage chunks", hjust = 0,
        gp = gpar(fontsize = 16, fontface = "bold", col = "blue")),
    ymin = -1.4,      
    ymax = -1.1,
    xmin = 0.2,         
    xmax = 1)

chunky.left <-  annotation_custom(
    grob = textGrob(label = "Large usage chunks", hjust = 0,
        gp = gpar(fontsize = 16, fontface = "bold", col = "blue")),
    ymin = -1.4,      
    ymax = -1.1,
    xmin = -1.2,         
    xmax =  -1.2)

predictable.upper <- annotation_custom(
    grob = textGrob(label = "Unpredictable", hjust = 0, rot = 90, 
        gp = gpar(fontsize = 16, fontface = "bold", col = "blue")),
    ymin = 0.8,      
    ymax = 0.8,
    xmin = -1.2,         
    xmax = -1.2)

predictable.lower <- annotation_custom(
    grob = textGrob(label = "Predictable", hjust = 0, rot = 90, 
        gp = gpar(fontsize = 16, fontface = "bold", col = "blue")),
    ymin = -1.0,      
    ymax = -1.0,
    xmin = -1.2,         
    xmax = -1.2)
  

g.scatter <- ggplot(data = df.gran,
                    aes(x = mean.gran.index,
                        y = mean.predict.index)
                    ) + geom_point() +
    geom_label_repel(aes(label = input.good), force = 3, max.iter = 100000) +
        ylab("Predictable usage........Mean unpredictability score......Unpredictable usage") +
        xlab("Large usage chunks....................Mean chunkiness score...................Small usage chunks") +
            theme_bw()

pdf("../writeup/plots/granularity_versus_predictability.pdf", width = 8, height = 6)
                                        #grid.draw(gt)
print(g.scatter)
dev.off()
