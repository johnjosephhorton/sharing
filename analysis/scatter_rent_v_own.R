#!/usr/bin/env Rscript

library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()

df.cs <- data.table(df)[, list(own.frac =  mean(own, na.rm = TRUE),
                               rent.frac = mean(rent, na.rm = TRUE),
                               lent.frac = mean(rent, na.rm = TRUE),
                               borrow.frac = mean(answer.borrowed == "yes", na.rm = TRUE)
                               ),
                        by = input.good]

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
        scale_x_sqrt(label = percent, breaks = break.list, expand = c(0,0)) +
            scale_y_sqrt(label = percent, breaks = break.list, expand = c(0,0)) +
                geom_label_repel(data = df.cs %>% mutate(input.good = revalue(input.good, short.list)),
                                 aes(label = input.good), force = 4, max.iter = 1000) + 
        theme_bw() + 
    xlab("Fraction Owning") +
    ylab("Fraction Renting")

JJHmisc::writeImage(g.scatter, "scatter_rent_v_own", path = "../writeup/plots/", width = 9, height = 4)
