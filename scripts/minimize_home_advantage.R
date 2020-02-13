

library(PlayerRatings)

#' @title Estimate Home Advantage Sum of Square Difference
#' @description Use Stephenson method to rank teams for a given home advantage.
#' Pass to \code{steph} and make predictions.
#' Predict Result for all games, then calculate sum of square difference, 
#' versus weight if available.
#' @param hm single numeric home advantage parameter to pass to gamma argument
#' of  \code{steph}, multiplied by data$home 
#' @param data data frame with the columns named in the subsequent arguments
#' @param season single character season column to pass to \code{steph} 
#' @param team single character team column to pass to \code{steph} 
#' @param opponent single character opposition column to pass to \code{steph} 
#' @param result single character result column to pass to \code{steph} (value 0-1)
#' @param weight single character weighting column to penalize close games (value 0-1)
#' @param home single character hpme column to pass to \code{steph} (value 0:1)
#' @return single numeric
#' @import PlayerRatings
#' @examples
#' homeAdv(data = aflodds, season = "Week", team = "HomeTeam", opponent = "AwayTeam", 
#'     result = "Score", weight = NULL, home = NULL, hm = 10)
#' aflodds$scorefrac <- aflodds$HomeScore / (aflodds$AwayScore + aflodds$HomeScore)
#' homeAdv(data = aflodds, season = "Week", team = "HomeTeam", opponent = "AwayTeam", 
#'     result = "Score", home = NULL, hm = 10)
#' optimize(homeAdv, interval = c(1, 2000), 
#'     data = aflodds, season = "Week", team = "HomeTeam", opponent = "AwayTeam", 
#'     result = "Score", home = NULL, tol = 0.1)
#' load("sixnations.RData")
#' homeAdv(data = dat, hm = 20)

#    datA <- data.frame("round" = rep(1:4, each = 5), 
#        "p1" = "A", "p2" = "B", "result" = 1, 
#        stringsAsFactors = FALSE)
#    ratingA <- steph(datA)
#    datAB <- data.frame("round" = rep(1:4, each = 7), 
#        "p1" = c("A", "B"), "p2" = c("B", "A"), 
#        "result" = 1, stringsAsFactors = FALSE)
#    ratingAB <- steph(datAB)

homeAdv <- function(hm, data, season = "Season", team = "Team",  
    opponent = "Opposition", result = "Res", 
    weight = "Scorefrac", home = "Home") {
    
    # check and fix columns
    missingCols <- !c(season, team, opponent, result) %in% colnames(data)
    if (any(missingCols)) {
        stop("missing columns in data: ", 
            paste(c(season, team, opponent, result)[missingCols], collapse = ", "))
    }
    
    if (is.null(home)) { home <- "home" }
    
    if (is.null(data[[home]])) { data[[home]] <- 1 }
    
    if (is.logical(data[[home]])) { data[[home]] <- (data[[home]] * 2) - 1 }
    
    if (is.null(weight)) { weight <- "weight" }
    
    if (is.null(data[[weight]])) { data[[weight]] <- data[[result]] }
    
    if (!(all(data[[weight]] >= 0) && all(data[[weight]] <= 1))) {
        stop("weight must be in range c(0, 1)") }
    
    rating <- steph(data[, c(season, team, opponent, result), drop = FALSE], 
        gamma = hm * data[[home]])
    
    selftest <- predict(rating, 
        data[, c(season, team, opponent), drop = FALSE], 
        tng = 0, trat = c(2200, 300), 
        gamma = hm * data[[home]])
    
    selftest[is.na(selftest)] <- 0.5
    stat <- sum(((data[[weight]] - 0.5) - (selftest - 0.5))^2)

    return(stat)
}

