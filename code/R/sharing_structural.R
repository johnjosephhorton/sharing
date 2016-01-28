devtools::install("../shaRing")
library(shaRing)
library(lfe)
library(magrittr)
library(dplyr)
#library(plyr)

df <- shaRing::GetDF() 

detach("package:plyr", unload=TRUE, force = TRUE)

shared.goods <- df %>%
group_by(input.good) %>%
    summarise(any.borrowing = any(borrowed & !own, na.rm = TRUE),
              any.own = any(own, na.rm = TRUE)) %>%
    filter(any.borrowing & any.own) %$%
    input.good %>% as.character

df$outcome <- with(df, ifelse(own, "own", ifelse(borrowed, "borrow", "nothing")))

df.shared <- droplevels(subset(df, as.character(input.good) %in% shared.goods))

with(df.shared, table(input.good, outcome))

#install.packages("mlogit")

library(mlogit)

#data("Heating", package = "mlogit")
#H <- mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))
#mlogit(depvar ~ ic + oc, data 

#df %<>% mutate(outcome = ifelse(own, "own", ifelse(borrowed, "borrow", "nothing")))
#df %<>% mutate(outcome.binary = ifelse(own, "own", "nothing"))
#library(dplyr)
#library(plyr)

m <- lm(own ~ x, data = df)
summary(m)

m <- felm(own ~ log(x)|input.good|0|0, data = subset(df, x > 0))
summary(m)

m <- felm(own ~ x + I(x==0)|input.good|0|0, data = df.shared)
summary(m)

price.list <- list("a men's suit" = 200,
  "back-up electric generator" = 200, 
  "BBQ Grill" = 120,
  "blender" = 45,
  "canoe" = 400,
  "car" = 20000,
  "cat carrier (for transporting cats)" = 45, 
  "cordless power drill" = 80,
  "diamond necklace" = 1000,
  "food processor" = 50, 
  "hammer" = 12,
  "high-end audio headphones" = 200,
  "high-end digitial camera"= 300, 
  "iPad or tablet" = 500,
  "jet ski" = 4000,
  "kid's bouncy castle" = 700,
  "kitchen timer (or egg timer)" = 5, 
  "mountain bike" = 250,
  "pick-up truck" = 25000,
  "portable air conditioner" = 400, 
  "push lawnmower" = 300,
  "ride-on lawnmower" = 700,
  "sewing machine" = 100,
  "toothbrush" = 1, 
  "tuxedo" = 350,
  "vacation home" = 200000)

df$p <- with(df, unlist(price.list[input.good]))

#shared.goods <- c("vacation home")

df.tmp <- df %>% filter(input.good %in% shared.goods) %>%
            select(input.good, granular.index, predict.index, income.index, x, outcome, p) %>%
                mutate(p.own = p,         p.borrow = 0,                        p.nothing = 0,     
                       benefit.own = as.numeric(I(x > 0)),  benefit.borrow = as.numeric(I(x > 0)),  benefit.nothing = 0,
                       granular.own = 0,   granular.borrow = granular.index,    granular.nothing = 0,
                       predict.own = 0,    predict.borrow = predict.index,      predict.nothing = 0) %>%
                           select(outcome,
                                  p.own,        p.borrow,        p.nothing, 
                                  benefit.own,  benefit.borrow,  benefit.nothing,
                                  granular.own, granular.borrow, granular.nothing,
                                  predict.own,  predict.borrow,  predict.nothing, income.index, input.good) %>%
                                  mutate(outcome = factor(outcome)) %>% na.omit 

for(i in 1:length(shared.goods)){
    print(i)
    H <- mlogit.data(df.tmp %>% filter(input.good == shared.goods[i]), shape = "wide", choice = "outcome", varying = c(2:13))
    m <- mlogit(outcome ~ benefit + predict + granular|0, data = H)
    print(coef(m))
}

shared.goods[6]

df.tmp %>% filter(input.good == "BBQ Grill") %$% table(outcome)/20

apply(fitted(m, outcome = FALSE), 2, mean)

#apply(fitted(m, outcome = FALSE)[which(as.character(df.tmp$input.good) == "BBQ Grill"),], 2, mean)
#apply(fitted(m, outcome = FALSE)[which(as.character(df.tmp$input.good) == "hammer"),], 2, mean)


which(as.character(H$input.good) == "vacation home", arr.ind = TRUE)

which(as.character(H$input.good) == "BBQ Grill")

dim(fitted(m, outcome = FALSE))

apply(, 2, mean)





apply(, 2, mean)

summary(m)

m <- mlogit(outcome ~ p + benefit + predict|income.index, data = H)


library(stargazer)

stargazer(m, type = "text")

summary(m)


df.tmp <- df %>% 
            select(input.good, granular.index, predict.index, income.index, x, outcome.binary) %>%
                mutate(p.own = 1,         p.nothing = 0,     
                       benefit.own = x,    benefit.nothing = 0,
                       ) %>%
                           select(outcome.binary,
                                  p.own,               p.nothing, 
                                  benefit.own,         benefit.nothing,
                                  income.index) %>%
                                      mutate(outcome = factor(outcome.binary)) %>% na.omit 


H <- mlogit.data(df.tmp, shape = "wide", choice = "outcome", varying = c(2:5))

m <- mlogit(outcome ~ p + benefit | income.index, data = H)

summary(m)



                                     
                                      borrow nothing own
  BBQ Grill                                1       5  14
  a men's suit                             0       8  12
  back-up electric generator               1      17   2
  blender                                  0       0  20
  canoe                                    0      19   1
  car                                      0       1  18
  cat carrier (for transporting cats)      0      11   9
  cordless power drill                     1       7  12
  diamond necklace                         0      15   5
  food processor                           0       8  12
  hammer                                   0       0  40
  high-end audio headphones                0      10  10
  high-end digitial camera                 3       6  11
  iPad or tablet                           0       8  12
  jet ski                                  2      15   0
  kid's bouncy castle                      0      19   0
  kitchen timer (or egg timer)             0       7  13
  mountain bike                            0      16   4
  pick-up truck                            9       8   3
  portable air conditioner                 0      15   1
  push lawnmower                           2       8   8
  ride-on lawnmower                        0      13   7
  sewing machine                           0      11   8
  toothbrush                               0       0  20
  tuxedo                                   0      15   3
  vacation home                            1      17   1
