#!/usr/bin/env Rscript

library(shaRing)

df <- shaRing::GetDF()

df.by.good <- subset(df, !is.na(answer.own))[,
                             list(lower = Hmisc::binconf(sum(own), .N)[2],
                                  upper = Hmisc::binconf(sum(own), .N)[3],
                                  frac.own = mean(own)),
                             by = input.good]

df.by.good <- df %>% filter(!is.na(answer.own)) %>%
    group_by(input.good) %>%
    dplyr::summarise(
        lower = Hmisc::binconf(sum(own), length(own))[2],
        upper = Hmisc::binconf(sum(own), length(own))[3],
        frac.own = mean(own)
    ) %>%
    ungroup %>%
    mutate(input.good = reorder(input.good, frac.own, mean))


g.own <- ggplot(data = df.by.good, aes(x = input.good, y = frac.own)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) + 
    theme_bw() +
    xlab("") +
    ylab("Fraction of respondents") +
    scale_y_continuous(label = scales::percent) +
    theme(axis.text.x = element_text(angle = -75, hjust = 0))

JJHmisc::writeImage(g.own, "ownership_fractions", width = 6, height = 4, path = "../writeup/plots/")
