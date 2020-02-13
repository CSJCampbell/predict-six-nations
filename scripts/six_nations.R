
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

# 2020 season
head(fixtures)

fixtures$Prediction <- predict(rating, 
    select(fixtures, Season, Team, Opposition), 
    # Team is Home
    tng = 0, gamma = hmin$minimum)

fixtures <- fixtures %>% 
    mutate(
        Result = cut(Prediction, 
            breaks = draw_bins, 
            labels = c("loss", "draw", "win"), 
            include.lowest = TRUE))
select(fixtures, Date, Team, Opposition, Result)
#    Date                 Team     Opposition Result
#    <chr>                <chr>    <chr>      <fct> 
#  1 Saturday 1 February  Wales    Italy      win   
#  2 Saturday 1 February  Ireland  Scotland   win   
#  3 Sunday 2 February    France   England    loss  
#  4 Saturday 8 February  Ireland  Wales      win   
#  5 Saturday 8 February  Scotland England    loss  
#  6 Sunday 9 February    France   Italy      win   
#  7 Saturday 22 February Italy    Scotland   loss  
#  8 Saturday 22 February Wales    France     win   
#  9 Sunday 23 February   England  Ireland    win   
# 10 Saturday 7 March     Ireland  Italy      win   
# 11 Saturday 7 March     England  Wales      win   
# 12 Sunday 8 March       Scotland France     draw  
# 13 Saturday 14 March    Wales    Scotland   win   
# 14 Saturday 14 March    Italy    England    loss  
# 15 Saturday 14 March    France   Ireland    loss  

###########################################################

# incorporate individual scores

dat_score <- dat_score %>% 
    mutate(
        Prediction = predict(rating,
            tibble(Season, Team, Opposition),
            tng = 0, gamma = hmin$minimum * (Home * 2 - 1)),
        Odds = Prediction / (1 - Prediction),
        Logit = log(Odds))

# split
score_train <- filter(dat_score, Season < 19)
score_test <- filter(dat_score, Season == 19)

# stack
score_train_rev <- transmute(score_train, 
    Team = Opposition,
    For = Aga,
    Try_Team = Try_Opp, 
    Con_Team = Con_Opp,
    Pen_Team = Pen_Opp,
    Drop_Team = Drop_Opp,
    Prediction = 1 - Prediction,
    Odds = Prediction / (1 - Prediction),
    Logit = log(Odds))

# score model
score_data <- rbind(
    select(score_train, Team, For, Logit),
    select(score_train_rev, Team, For, Logit))

ggplot(score_data,
    aes(x = Logit, y = For)) + 
    geom_bin2d(binwidth = c(0.5, 5)) +
    geom_smooth(method = lm) + 
    theme_at() +
    ylab("Score For") +
    xlab("Prediction Log Odds") +
    scale_fill_viridis_c(direction = -1, alpha = 0.8) + 
    facet_wrap(~Team)
#ggsave("04_score.png", height = 6, width = 10, dpi = 124)

# model shown in plot with interaction terms on gradient
score_model <- lm(For ~ Logit * Team, data = score_data)
summary(score_model)

score_train <- score_train %>% 
    mutate(
        Odds_Team = Prediction / (1 - Prediction),
        Logit_Team = log(Odds_Team), 
        Odds_Opp = (1 - Prediction) / Prediction,
        Logit_Opp = log(Odds_Opp), 
        Predict_For = predict(score_model, 
            newdata = tibble(Logit = Logit_Team, Team)), 
        Predict_Aga = predict(score_model, 
            newdata = tibble(Logit = Logit_Opp, Team = Opposition)))

# predict for test
score_test <- score_test %>% 
    mutate(
        Odds_Team = Prediction / (1 - Prediction),
        Logit_Team = log(Odds_Team), 
        Odds_Opp = (1 - Prediction) / Prediction,
        Logit_Opp = log(Odds_Opp), 
        Predict_For = predict(score_model, 
            newdata = tibble(Logit = Logit_Team, Team)), 
        Predict_Aga = predict(score_model, 
            newdata = tibble(Logit = Logit_Opp, Team = Opposition)))

# stack and flip
score_data <- rbind(
    select(score_test, Team, For, Predict_For),
    transmute(score_test, 
        Team = Opposition,
        For = Aga,
        Predict_For = Predict_Aga))

# scores look okay
ggplot(score_data,
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

summarise(transmute(score_data, For, Predict_For = round(Predict_For)),
    `Mean Abs Error` = mean(abs(For - Predict_For)), 
    `Root Mean Square` = sqrt(mean((For - Predict_For)^2)))
#    `Mean Abs Error` `Root Mean Square`
#               <dbl>              <dbl>
#  1              5.9               8.57

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
