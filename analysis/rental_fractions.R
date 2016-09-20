#!/usr/bin/env Rscript

library(shaRing)
library(JJHmisc)

df <- shaRing::GetDF()

df.by.good.rent <- subset(df, !is.na(answer.rent))[,
                                  list(
                                      lower = Hmisc::binconf(
                                          sum(rent), .N)[2],
                                      upper = Hmisc::binconf( 
                                          sum(rent), .N)[3],
                                      frac.rent = mean(rent)),
                                  by = input.good]
df.by.good.rent$input.good <- with(df.by.good.rent, reorder(input.good, frac.rent, mean))

df.rent <- df %>% filter(!is.na(answer.rent)) %>%
    group_by(input.good) %>%
    dplyr::summarise(
        lower = Hmisc::binconf(sum(rent), length(input.good))[2],
        upper = Hmisc::binconf(sum(rent), length(input.good))[3],
        frac.rent = mean(rent)
    ) %>%
    ungroup %>%
    mutate(input.good = reorder(input.good, frac.rent, mean)) 


g.rent <- ggplot(data = df.rent, aes(x = input.good, y = frac.rent)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = percent) +
        theme(axis.text.x = element_text(angle = -75, hjust = 0))


JJHmisc::writeImage(g.rent, "rental_fractions", path = "../writeup/plots/", width = 6, height = 4)
