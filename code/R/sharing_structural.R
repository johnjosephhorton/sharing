devtools::install("../shaRing")
library(shaRing)

df <- shaRing::GetDF()

library(magrittr)



install.packages("mlogit")

library(mlogit)


data("Heating", package = "mlogit")

H <- mlogit.data(Heating, shape = "wide", choice = "depvar", varying = c(3:12))

df %<>% mutate(outcome = ifelse(own, "own", ifelse(borrowed | rent, "borrow", "nothing")))

table(df$input.good, df$outcome)

filter(input.good == "a men's suit") %>%
    
df.tmp <- df %>% 
            select(input.good, granular.index, predict.index, income.index, x, outcome) %>%
                mutate(p.own = 10,         p.borrow = 0,                        p.nothing = 0,     
                       benefit.own = x,    benefit.borrow = x,                  benefit.nothing = 0,
                       granular.own = 0,   granular.borrow = granular.index,    granular.nothing = 0,
                       predict.own = 0,    predict.borrow = predict.index,      predict.nothing = 0) %>%
                           select(outcome,
                                  p.own,        p.borrow,        p.nothing, 
                                  benefit.own,  benefit.borrow,  benefit.nothing,
                                  granular.own, granular.borrow, granular.nothing,
                                  predict.own,  predict.borrow,  predict.nothing, income.index) %>%
                                      mutate(outcome = factor(outcome)) %>% na.omit 


H <- mlogit.data(df.tmp %>% filter(input.good == "pick-up truck"), shape = "wide", choice = "outcome", varying = c(2:13)) 
m <- mlogit(outcome ~ p + benefit + granular + predict | income.index, data = H)
summary(m)


> 
                                     
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
