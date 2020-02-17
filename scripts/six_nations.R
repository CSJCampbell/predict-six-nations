
# Analysis of Six Nations Rugby Tournament
# Manchester R 12 Feb 2020
# Chris Campbell

# dependencies

library(PlayerRatings)
library(dplyr)
library(tidyr)
library(ggplot2)

# plot styling

theme_at <- function(
    base_size = 20, 
    base_family = "AT Fabriga Medium", 
    base_line_size = base_size / 22, 
    base_rect_size = base_size / 22) {
    
    theme_bw(
        base_size = base_size, 
        base_family = base_family, 
        base_line_size = base_line_size, 
        base_rect_size = base_rect_size) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
}

# import 3 datasets
# * results 2010-2019
# * results 2010-2019 with number of individual scores
# * fixtures 2020

source("scripts/six_nations_prepare_data.R")

# scores by Team

ggplot(mutate(dat, Season = factor(Season)), aes(x = Season, y = For)) + 
    geom_boxplot(fill = rep(c("#FEFEFEFF", "#3672F5FF", "#35784AFF", 
        "#458AF6FF", "#25325AFF", "#E43E35"), each = 11)) +
    ylab("Score For") +
    facet_wrap(~Team) +
    theme_at()
#ggsave("01_six_nations_score.png", height = 6, width = 10, dpi = 124)

# drop match double entries, split
train <- filter(dat, Pick & Season < 19)
test <- filter(dat, Pick & Season == 19)

# Elo explanation
logistic <- function(ra, rb = 2000) { 1 / (1 + 10^((rb - ra) / 400)) }
logistic(ra = 2200)
# [1] 0.7597469
ra <- (100:300) * 10
qplot(x = ra, y = logistic(ra = ra), geom = "line") +
    xlab("Player A Rating") + 
    ylab("Probability A Wins") + 
    theme_at()
#ggsave("02_logit_elo.png", height = 6, width = 10, dpi = 124)

# default gamma is optimised for chess white
# find optimum home advantage for home stadium
source("scripts/minimize_home_advantage.R")

hmin <- optimize(homeAdv, interval = c(1, 300), 
    data = train, tol = 0.1)
hmin
#$minimum
#[1] 58.64539
#
#$objective
#[1] 4.970883

# same rating
PlayerRatings:::predict.rating(
    object = list(
        ratings = tibble(
            Player = c("A", "B"), 
            Rating = c(2000, 2000), 
            Games = 100), 
        type = "Elo"), 
    newdata = tibble(round = 1, p1 = "A", p2 = "B"), gamma = 0)
# [1] 0.5

# different by 1 standard deviation
PlayerRatings:::predict.rating(
    object = list(
        ratings = tibble(
            Player = c("A", "B"), 
            Rating = c(2200, 2000), 
            Games = 100), 
        type = "Elo"), 
    newdata = tibble(round = 101, p1 = "A", p2 = "B"), gamma = 0)
# [1] 0.7597469

# rating A
2000 + 27 * (0 - logistic(ra = 2000, rb = 2000))
# [1] 1986.5
elo(x = tibble(
        Season = 1,
        Player1 = "A", 
        Player2 = "B", 
        Result = 0), 
    init = 2000)$rating
#   Player Rating Games Win Draw Loss Lag
# 1      B 2013.5     1   1    0    0   0
# 2      A 1986.5     1   0    0    1   0

# https://en.wikipedia.org/wiki/Glicko_rating_system
# 1. update Deviation to increase variability since last observed
# 2. update Rating
# 3. update Deviation to decrease variability based on games observed
glicko(x = tibble(
        Season = 1, Player1 = "A", 
        Player2 = "B", Result = 0), 
    status = tibble(Player = c("A", "B"), 
        Rating = c(2000, 2000), 
        Deviation = c(10, 1000), 
        Games = c(1, 1)))$rating
#   Player   Rating Deviation Games Win Draw Loss Lag
# 1      B 2174.997  246.7786     2   1    0    0   0
# 2      A 1999.375   18.0169     2   0    0    1   0

# Stephenson method 
steph(x = tibble(
        Season = 1, Player1 = "A", 
        Player2 = "B", Result = 0), 
    status = tibble(Player = c("A", "B"), 
        Rating = c(2000, 2000), 
        Deviation = c(10, 1000), 
        Games = c(1, 1)))$rating

# equivalent for Steph
PlayerRatings:::predict.rating(
    object = list(
        ratings = tibble(
            Player = c("A", "B"), 
            Rating = c(2200, 2000), 
            Deviation = c(100, 100),
            Games = 100), 
        type = "Stephenson"), 
    newdata = tibble(round = 101, p1 = "A", p2 = "B"), gamma = 0)
# [1] 0.7408417

# create ratings
rating <- steph(train[, 
    c("Season", "Team", "Opposition", "Res")], gamma = hmin$minimum * (train$Home * 2 - 1))
rating$ratings

train$Prediction <- predict(rating, 
    train[, c("Season", "Team", "Opposition", "Res")], 
    gamma = hmin$minimum * (train$Home * 2 - 1))

# distribution uniformish?
qplot(data = train, x = Prediction, bins = 12)

# model training calibration
# create bins, then calculate proportion of wins
pred_quantiles <- quantile(train$Prediction, probs = (0:10) / 10)
train <- mutate(train, 
    `Prediction Bin` = cut(Prediction, 
        breaks = pred_quantiles, 
        include.lowest = TRUE))

train_binned <- train %>% 
    group_by(`Prediction Bin`) %>% 
    summarise(`Observed` = sum(Res) / n()) %>% 
    mutate(Prediction = c(
        pred_quantiles[-length(pred_quantiles)] + 
            diff(pred_quantiles) / 2)[as.numeric(`Prediction Bin`)])

ggplot(train_binned, 
    aes(x = Prediction, y = Observed)) +
    geom_abline(slope = 1) +
    geom_smooth(method = "lm", se = FALSE) + 
    geom_point() + 
    xlim(0:1) + 
    ylim(0:1) +
    coord_fixed() + 
    theme_at()
#ggsave("03_calibration.png", height = 6, width = 10, dpi = 124)

# predictions in test set
test <- test %>% 
    mutate( 
        Prediction = predict(rating, 
            select(., Season, Team, Opposition), 
            tng = 0, gamma = hmin$minimum * (test$Home * 2 - 1)))

# proportion draws (~3%)
draws <- sum(train$Res == 0.5) / nrow(train)
draw_bins <- c(0, 0.5 - draws / 2, 0.5 + draws / 2, 1)

# confusion matrix
tab <- test %>% 
    group_by(Observed = Res, 
        Predicted = as.character(cut(Prediction, 
            breaks = draw_bins, 
            labels = c(0, 0.5, 1), 
            include.lowest = TRUE))) %>%  
    summarise(n = n())
rbind(tab, list(
        Observed = c(0.5, 0.5, 1, 1), 
        Predicted = c("0", "0.5", "0", "0.5"), 
        n = c(0, 0, 0, 0))) %>% 
    pivot_wider(names_from = Predicted, values_from = n)


###########################################################

# incorporate individual scores

dat_score <- dat_score %>% 
    mutate(
        Predict_Team = predict(rating,
            tibble(Season, Team, Opposition),
            tng = 0, gamma = hmin$minimum * (Home * 2 - 1)),
        Predict_Opp = 1 - Predict_Team,
        Odds_Team = Predict_Team / (1 - Predict_Team),
        Logit_Team = log(Odds_Team), 
        Odds_Opp = Predict_Opp / (1 - Predict_Opp),
        Logit_Opp = log(Odds_Opp))

# split
score_train <- filter(dat_score, Season < 19)
score_test <- filter(dat_score, Season == 19)


# score model
score_data <- rbind(
    select(score_train, Team, For, Logit_Team, Opposition),
    transmute(score_train, 
        Team = Opposition,
        For = Aga, 
        Logit_Team = Logit_Opp, 
        Opposition = score_train$Team))

ggplot(score_data,
    aes(x = Logit_Team, y = For)) + 
    geom_bin2d(binwidth = c(0.5, 5)) +
    geom_smooth(method = lm) + 
    theme_at() +
    ylab("Score For") +
    xlab("Prediction Log Odds") +
    scale_fill_viridis_c(direction = -1, alpha = 0.8) + 
    facet_wrap(~Team)
#ggsave("04_score.png", height = 6, width = 10, dpi = 124)

# model shown in plot with interaction terms on gradient
score_model <- lm(For ~ Logit_Team * Team, data = score_data)
summary(score_model)
AIC(score_model)
# including opponent does not improve fit
score_model_opp <- lm(For ~ Logit_Team * Team + Opposition, data = score_data)
summary(score_model_opp)
AIC(score_model_opp)

# Poisson?
score_model_pois <- glm(For ~ Logit_Team * Team, data = score_data, family = poisson)
summary(score_model_pois)
AIC(score_model_pois)


# predict based on score
score_train <- score_train %>% 
    mutate(
        Predict_For = predict(score_model, 
            newdata = tibble(Logit_Team, Team)), 
        Predict_Aga = predict(score_model, 
            newdata = tibble(Logit_Team = Logit_Opp, Team = Opposition)))

# predict for test
score_test <- score_test %>% 
    mutate(
        Predict_For = predict(score_model, 
            newdata = tibble(Logit_Team, Team)), 
        Predict_Aga = predict(score_model, 
            newdata = tibble(Logit_Team = Logit_Opp, Team = Opposition)))

# stack and flip
score_data19 <- rbind(
    select(score_test, Team, For, Predict_For),
    transmute(score_test, 
        Team = Opposition,
        For = Aga,
        Predict_For = Predict_Aga))

# scores look okay
ggplot(score_data19,
    aes(x = Predict_For, y = For)) + 
    geom_bin2d(binwidth = c(5, 5)) +
    geom_smooth(method = lm, se = FALSE) + 
    theme_at() +
    xlab("Predicted Score For") +
    ylab("Score For") +
    coord_fixed() + 
    geom_abline() + 
    scale_fill_viridis_c(direction = -1, alpha = 0.8, begin = 0.5, breaks = 1:2) + 
    facet_wrap(~Team)
#ggsave("05_test_score.png", height = 6, width = 10, dpi = 124)

summarise(transmute(score_data19, For, Predict_For = round(Predict_For)),
    `Mean Abs Error` = mean(abs(For - Predict_For)), 
    `Root Mean Square` = sqrt(mean((For - Predict_For)^2)))
#    `Mean Abs Error` `Root Mean Square`
#               <dbl>              <dbl>
#  1             5.87               8.29

tries_data <- rbind(
    select(score_train, Team, Try_Team, Logit_Team),
    transmute(score_train, Team = Opposition, Try_Team = Try_Opp, Logit_Team = Logit_Opp))
ggplot(tries_data,
    aes(x = Logit_Team, y = Try_Team)) + 
    geom_bin2d(binwidth = c(0.5, 1)) +
    geom_smooth(method = lm) + 
    theme_at() +
    ylab("Tries For") + 
    xlab("Predicted Log Odds") + 
    scale_fill_viridis_c(direction = -1, alpha = 0.8) + 
    facet_wrap(~Team)
#ggsave("06_train_tries.png", height = 6, width = 10, dpi = 124)

# tries model
tries_model <- lm(Try_Team ~ Logit_Team * Team, data = tries_data)
# country terms need gradient
summary(tries_model)
AIC(tries_model)
# [1] 998.0515
tries_model2 <- glm(Try_Team ~ Logit_Team + Team, data = tries_data, family = poisson)
summary(tries_model2)
AIC(tries_model2)
# [1] 898.6857

con_data <- rbind(
    select(score_train, Team, Con_Team, Logit_Team),
    transmute(score_train, Team, Con_Team = Con_Opp, Logit_Team = Logit_Opp))

con_model0 <- glm(Try_Team ~ Team, data = tries_data, family = poisson)
AIC(con_model0)
con_model1 <- glm(Try_Team ~ Logit_Team, data = tries_data, family = poisson)
AIC(con_model1)
con_model2 <- glm(Try_Team ~ Logit_Team + Team, data = tries_data, family = poisson)
AIC(con_model2)
con_model3 <- glm(Try_Team ~ Logit_Team * Team, data = tries_data, family = poisson)
AIC(con_model3)

ggplot(con_data,
    aes(x = Logit_Team, y = Con_Team)) + 
    geom_bin2d(binwidth = c(0.5, 1)) +
    geom_smooth(method = lm) + 
    theme_at() +
    ylab("Conversions For") +
    xlab("Predicted Log Odds") +
    scale_fill_viridis_c(direction = -1, alpha = 0.8) + 
    facet_wrap(~Team)
#ggsave("07_train_con.png", height = 6, width = 10, dpi = 124)

pen_data <- rbind(
    select(score_train, Team, Pen_Team, Logit_Team),
    transmute(score_train, Team, Pen_Team = Pen_Opp, Logit_Team = Logit_Opp))

ggplot(pen_data,
    aes(x = Logit_Team, y = Pen_Team)) + 
    geom_bin2d(binwidth = c(0.5, 0.5)) +
    geom_smooth(method = lm) + 
    theme_at() +
    ylab("Penalties For") +
    xlab("Predicted Log Odds") +
    scale_fill_viridis_c(direction = -1, alpha = 0.8) + 
    facet_wrap(~Team)
#ggsave("08_train_pen.png", height = 6, width = 10, dpi = 124)

score_test <- score_test %>% 
    mutate(
        Tries_For = floor(predict(tries_model2, 
            newdata = tibble(Logit_Team, Team), 
            type = "response")), 
        Tries_Aga = floor(predict(tries_model2, 
            newdata = tibble(Logit_Team = Logit_Opp, Team = Opposition), 
            type = "response")),
        Score_For_Tries = Tries_For * 5,
        Score_Aga_Tries = Tries_Aga * 5,
        Con_For = min(Tries_For, 
            floor(predict(con_model2, 
                newdata = tibble(Logit_Team, Team), 
                type = "response"))), 
        Con_Aga = min(Tries_Aga, 
            floor(predict(con_model2, 
                newdata = tibble(Logit_Team = Logit_Opp, Team = Opposition), 
                type = "response"))), 
        Score_For_Con_Tries = Score_For_Tries + Con_For * 2,
        Score_Aga_Con_Tries = Score_Aga_Tries + Con_Aga * 2,
        Score_For_Final = Score_For_Con_Tries + 
            min(0, 3 * ceiling((Predict_For - Score_For_Con_Tries)/3)), 
        Score_Aga_Final = Score_Aga_Con_Tries + 
            min(0, 3 * ceiling((Predict_Aga - Score_Aga_Con_Tries)/3)),
        Correct = Score_For_Final == For & Score_Aga_Final == Aga)
        
ggplot(rbind(
        select(score_test, For, Score_For_Final, Team),
        transmute(score_test, For = Aga, Score_For_Final = Score_Aga_Final, Team = Opposition)),
    aes(x = Score_For_Final, y = For)) + 
    geom_bin2d(binwidth = c(5, 5)) +
    geom_smooth(method = lm, se = FALSE) + 
    theme_at() +
    xlab("Predicted Score For") +
    ylab("Score For") +
    coord_fixed() + 
    geom_abline() + 
    scale_fill_viridis_c(direction = -1, alpha = 0.8, begin = 0.5, breaks = 1:3) + 
    facet_wrap(~Team)
#ggsave("09_test_score2.png", height = 6, width = 10, dpi = 124)

###########################################################

# update ratings with 2019 results
rating19 <- dat %>% 
    filter(Pick) %>% 
    select(Season, Team, Opposition, Res) %>% 
    steph(gamma = hmin$minimum * (filter(dat, Pick)$Home * 2 - 1))

# score model
score_data19 <- mutate(dat, 
    Predict = predict(rating19,
        tibble(Season, Team, Opposition),
        tng = 0, gamma = hmin$minimum * (Home * 2 - 1)),
    Odds = Predict / (1 - Predict),
    Logit = log(Odds))

# model shown in plot with interaction terms on gradient
score_model19 <- lm(For ~ Logit * Team, data = score_data19)
summary(score_model19)

score_data19 <- mutate(score_data19,
    Predict_For_Raw = fitted(score_model19), 
    Predict_Aga_Raw = predict(score_model19, 
        newdata = tibble(Logit = log(1 / Odds), Team = Opposition), 
        type = "response"), 
    Predict_For = round(Predict_For_Raw), 
    Predict_Aga = round(Predict_Aga_Raw), 
    Predict_For18_Raw = predict(score_model, 
        newdata = tibble(Logit_Team = Logit, Team), 
        type = "response"), 
    Predict_Diff_Raw = Predict_For_Raw - Predict_Aga_Raw)

ggplot(
    select(score_data19, For, Predict_For, Team),
    aes(x = Predict_For, y = For)) + 
    geom_bin2d(binwidth = c(5, 5)) +
    geom_smooth(method = lm, se = FALSE) + 
    theme_at() +
    xlab("Predicted Score For") +
    ylab("Score For") +
    coord_fixed() + 
    geom_abline() + 
    scale_fill_viridis_c(direction = -1, alpha = 0.8) + 
    facet_wrap(~Team)

# how many times is score correct?
summarise(score_data19, 
    correct = sum(For == Predict_For), 
    diff = sum((For - Against) == round(Predict_Diff_Raw)))
# # A tibble: 1 x 1
# correct  diff
# <int> <int>
#     1       8     6

common_scores <- score_data19 %>% 
    group_by(For) %>% 
    summarise(N = n())

sum(!score_data19$Predict_For %in% common_scores$For)
# [1] 10

#' replace scores with nearest likely neighbour
#' @param x numeric vector scores to update
#' @param table data frame with two columns For and n
#' where For is score value, and N is value frequency
#' @return numeric vector of length x where all values have been updated
#' @examples 
#' adjust_score(x = 1:3, table = tibble(For = 3, N = 1))
#' adjust_score(x = c(0, 1, 10, 100), table = tibble(For = c(0, 3, 5, 7), N = c(1, 3, 2, 1)))
adjust_score <- function(x, table) {
    stopifnot(is.numeric(x))
    out <- numeric(length(x))
    x[x < min(table$For)] <- min(table$For)
    x[x > max(table$For)] <- max(table$For)
    unique_x <- unique(x)
    all_rows <- seq.int(from = min(table$For), to = max(table$For))
    all_rows <- all_rows[!all_rows %in% table$For]
    table <- rbind(table, 
        tibble(For = all_rows, N = rep(-1, times = length(all_rows)))) %>% 
        arrange(For)
    for (ii in seq_along(unique_x)) {
        # scores in neighbourhood
        nearby <- slice(
            table, 
            seq.int(
                max(1, unique_x[ii] - 2), 
                min(nrow(table), unique_x[ii] + 2)))
        # force length 1
        score <- filter(nearby,
            N == max(N, na.rm = TRUE))$For
        if (length(score) > 1) {
            score <- score[sample.int(n = length(score), size = 1)]
        }
        out[x == unique_x[ii]] <- score
    }
    out
}

set.seed(3534)
score_data19 <- mutate(score_data19,
    Predict_For_Adj = adjust_score(x = Predict_For, table = common_scores), 
    Predict_Aga_Adj = adjust_score(x = Predict_Aga, table = common_scores))

sum(!score_data19$Predict_For_Adj %in% common_scores$For)

summarise(score_data19, 
    correct = sum(For == Predict_For_Adj))
# # A tibble: 1 x 1
#  correct
#    <int>
# 1     18

###########################################################

# 2020 season
head(fixtures)

set.seed(23526)
fixtures <- fixtures %>% 
    mutate(
        Predict_Team <- predict(rating19, 
            tibble(Season, Team, Opposition), 
            # Team is Home
            tng = 0, gamma = hmin$minimum),
        Predict_Opp = 1 - Predict_Team,
        Odds_Team = Predict_Team / (1 - Predict_Team),
        Logit_Team = log(Odds_Team), 
        Odds_Opp = Predict_Opp / (1 - Predict_Opp),
        Logit_Opp = log(Odds_Opp),
        Predict_For = round(predict(score_model19, 
            newdata = tibble(Logit = Logit_Team, Team))), 
        Predict_Aga = round(predict(score_model19, 
            newdata = tibble(Logit = Logit_Opp, Team = Opposition))), 
        Predict_For_Adj = adjust_score(x = Predict_For, table = common_scores), 
        Predict_Aga_Adj = adjust_score(x = Predict_Aga, table = common_scores), 
        Result_Rating = cut(Predict_Team, 
            breaks = draw_bins, 
            labels = c("loss", "draw", "win"), 
            include.lowest = TRUE), 
        Result_Score = case_when(
            Predict_For_Adj > Predict_Aga_Adj ~ "win",
            Predict_For_Adj == Predict_Aga_Adj ~ "draw", 
            Predict_For_Adj < Predict_Aga_Adj ~ "loss"))

transmute(fixtures, 
    Date = as.Date(paste(Date, "2020"), format = "%A %d %B %Y"), 
    Team, Opposition, 
    Predict_For_Adj, Predict_Aga_Adj, 
    Result_Rating, Result_Score)

#    Date       Team  Opposition Predict_For_Adj Predict_Aga_Adj Result_Rating
#    <date>     <chr> <chr>                <dbl>           <dbl> <fct>        
#  1 2020-02-01 Wales Italy                   38               9 win          
#  2 2020-02-01 Irel… Scotland                26              13 win          
#  3 2020-02-02 Fran… England                 16              21 loss         
#  4 2020-02-08 Irel… Wales                   16              13 win          
#  5 2020-02-08 Scot… England                 16              23 loss         
#  6 2020-02-09 Fran… Italy                   29              13 win          
#  7 2020-02-22 Italy Scotland                16              21 loss         
#  8 2020-02-22 Wales France                  22              13 win          
#  9 2020-02-23 Engl… Ireland                 21              13 win          
# 10 2020-03-07 Irel… Italy                   38              13 win          
# 11 2020-03-07 Engl… Wales                   21              13 win          
# 12 2020-03-08 Scot… France                  16              21 loss         
# 13 2020-03-14 Wales Scotland                26              13 win          
# 14 2020-03-14 Italy England                 13              38 loss         
# 15 2020-03-14 Fran… Ireland                 16              16 loss  

