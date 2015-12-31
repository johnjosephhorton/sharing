#' Builds and returns the main data frame
#'
#' Build and returns the main data frame
#'
#' @export

library(magrittr)
library(dplyr)

GetDF <- function(){
    ConvertFactor <- function(x) {
        lev <- levels(x)
        lev <- lev[!lev %in% c("", "na")]
        x <- as.character(x) %>% tolower()
        x[!x %in% lev] <- NA
        factor(x)
    }
    file.name <- system.file("extdata/sharing_survey_results_pilot.csv", package = "shaRing")
    df <- read.table(file.name, header = T, sep = ",", fill = NA) %>%
        dplyr::select(worker.id = WorkerId,
                      assignment.id = AssignmentId,         
                      assignment.status = AssignmentStatus,
                      work.time = WorkTimeInSeconds,
                      input.good = Input.good,
                      answer.own = Answer.own,
                      answer.lent = Answer.lent,
                      answer.borrowed = Answer.borrowed,
                      answer.rent = Answer.rent,
                      answer.usage = Answer.usage,
                      answer.granularity = Answer.granularity,
                      answer.predictability = Answer.predictability,
                      answer.no_own_reason = Answer.no_own_reason,
                      answer.own_income = Answer.own_income) %>%
        mutate_each(funs(ConvertFactor),
                    answer.own,
                    answer.lent,
                    answer.borrowed,
                    answer.rent,
                    answer.no_own_reason) %>%   
        tbl_df()
    usage.levels <- 0:11
    usage.labels <- c(
        "We would not use this at all",
        "1 minute a week (about 1 hour a year)",
        "5 minutes a week (about 4 hours a year)",
        "1/2 an hour a week",
        "1 hour a week",
        "1/2 an hour a day",
        "1 hour a day",
        "2 hours a day",
        "4 hours a day",
        "8 hours a day",
        "16 hours a day",
        "24 hours a day (I would continuously be using this good")
    minute.per.week <- (60/7)/(24*60*60*7)
    hour.per.day <- 1.0/24.0
    day.frac <- c(0,
                  minute.per.week, 
                  5 * minute.per.week,
                  30 * minute.per.week, 
                  60 * minute.per.week,
                  0.5 * hour.per.day,
                  hour.per.day,
                  2 * hour.per.day,
                  4 * hour.per.day,
                  8 * hour.per.day,
                  15 * hour.per.day,
                  24 * hour.per.day)    
    df <- within(df, {
        usage = factor(answer.usage, levels = usage.levels, labels = usage.labels)
        x = as.numeric(as.character(factor(answer.usage, levels = usage.levels, labels = day.frac)))
        own = answer.own == "yes"
        borrowed = answer.borrowed == "yes"
        lent = answer.lent == "yes"
        usage.index = scale(as.numeric(answer.usage))
        income.index = scale(as.numeric(answer.own_income))
        predict.index = scale(as.numeric(answer.predictability))
        granular.index = scale(as.numeric(answer.granularity))
        rent = answer.rent == "yes"
    })
    ## imput family household income 
    y.levels <- c(10/2, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 200)
    df$y <- with(df, as.numeric(sapply(answer.own_income, function (x) as.numeric(y.levels[x + 1]) )))
    ## fix typo
    levels(df$input.good)[16] <- "kid's bouncy castle"
    df <- data.table(df)
    df$input.good.short <- plyr::revalue(df$input.good, c("high-end digitial camera"="high-end\ndigitial camera",
                                          "kitchen timer (or egg timer)" = "kitchen timer\n(or egg timer)",
                                          "portable air conditioner" = "portable\nair conditioner",
                                          "cat carrier (for transporting cats)" = "cat carrier\n(for transporting cats)",
                                          "back-up electric generator" = "back-up\nelectric generator",
                                          "high-end audio headphones" = "high-end\naudio headphones")
                                   )
    df
}
