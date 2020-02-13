
# dependencies

library(readxl)
library(dplyr)
library(forcats)

#' Split a vector
#' 
#' Splits sequential pairs in a vector randomly into two groups.
#'  
#' @param n single even integer
#' @return logical vector length n
#' @export
#' @examples
#' splitPairs(n = 2)
#' splitPairs(n = 4)

splitPairs <- function(n = 10L) {
    if (length(n) != 1L) { 
        stop("n must be length one")
    }
    if (n %% 2 != 0) { 
        warning("n should be even")
    }
    rand <- runif(n / 2)
    cl <- numeric(n)
    cl[seq_len(n / 2) * 2] <- rand
    cl[(seq_len(n / 2) * 2) - 1] <- 1 - rand
    cl[cl >= 0.5] <- 1
    cl[cl < 0.5] <- 0
    return(as.logical(cl))
}

dat <- read_excel("data/sixnations.xlsx")

set.seed(35234)
dat <- transmute(dat, 
    Team,
    Opposition,
    Result,
    For,
    Against = Aga,
    TeamGround = fct_collapse(dat$Ground, 
        Ireland = c("Lansdowne Road", "Croke Park"), 
        England = "Twickenham", 
        Scotland = "Murrayfield", 
        Wales = "Millennium Stadium", 
        France = c("Stade de France", "Marseille"), 
        Italy = "Rome"),
    Home = Team == TeamGround, 
    `Match Date`, 
    Season = as.numeric(format(`Match Date`, format = "%y")), 
    Res = (as.numeric(factor(Result, levels = c("lost", "draw", "won"))) - 1) / 2,
    Scorefrac = dat$For / (dat$For + dat$Aga), 
    Match = rep(seq.int(to = n() / 2), each = 2), 
    Pick = splitPairs(n = n()))

###########################################################

# individual scores included

# scale the result
dat_score <- read_excel("data/sixnations_scores.xlsx")

# validate score data
stopifnot(dat_score %>% 
        select(-Diff, -HTf, -HTa, -Ground) %>% 
        mutate(
            For_Score = Try_Team * 5 + Con_Team * 2 + Pen_Team * 3 + Drop_Team * 3, 
            Aga_Score = Try_Opp * 5 + Con_Opp * 2 + Pen_Opp * 3 + Drop_Opp * 3) %>% 
        filter(For != For_Score | Aga != Aga_Score) %>% 
        nrow == 0)


dat_score <-  dat_score %>% 
    mutate(
        TeamGround = fct_collapse(Ground, 
            Ireland = c("Lansdowne Road", "Croke Park"), 
            England = "Twickenham", 
            Scotland = "Murrayfield", 
            Wales = "Millennium Stadium", 
            France = c("Stade de France", "Marseille"), 
            Italy = "Rome"),
        Home = Team == TeamGround, 
        Season = as.numeric(format(`Match Date`, format = "%y")), 
        Res = (as.numeric(factor(Result, levels = c("lost", "draw", "won"))) - 1) / 2)


###########################################################


fixtures <- tribble(
    ~Round, ~Date, ~Season, ~Team, ~Opposition, ~Ground, ~GroundCountry, ~Kickoff, ~Station,
    1, "Saturday 1 February", 20, "Wales", "Italy", "Principality Stadium", "Cardiff", "2:15pm", "BBC",
    1, "Saturday 1 February", 20, "Ireland", "Scotland", "Aviva Stadium", "Dublin", "4:45pm", "ITV",
    1, "Sunday 2 February", 20, "France", "England", "Stade de France", "Paris", "3pm", "BBC",
    2, "Saturday 8 February", 20, "Ireland", "Wales", "Aviva Stadium", "Dublin", "2:15pm", "ITV",
    2, "Saturday 8 February", 20, "Scotland", "England", "Murrayfield", "Edinburgh", "4:45pm", "BBC",
    2, "Sunday 9 February", 20, "France", "Italy", "Stade de France", "Paris", "3pm", "BBC",
    3, "Saturday 22 February", 20, "Italy", "Scotland", "Stadio Olimpico", "Rome", "2:15pm", "ITV",
    3, "Saturday 22 February", 20, "Wales", "France", "Principality Stadium", "Cardiff", "4:45pm", "BBC",
    3, "Sunday 23 February", 20, "England", "Ireland", "Twickenham", "London", "3pm", "ITV",
    4, "Saturday 7 March", 20, "Ireland", "Italy", "Aviva Stadium", "Dublin", "2:15pm", "ITV",
    4, "Saturday 7 March", 20, "England", "Wales", "Twickenham", "London", "4:45pm", "ITV",
    4, "Sunday 8 March", 20, "Scotland", "France", "Murrayfield", "Edinburgh", "3pm", "BBC",
    5, "Saturday 14 March", 20, "Wales", "Scotland", "Principality Stadium", "Cardiff", "2:15pm", "BBC",
    5, "Saturday 14 March", 20, "Italy", "England", "Stadio Olimpico", "Rome", "4:45pm", "ITV",
    5, "Saturday 14 March", 20, "France", "Ireland", "Stade de France", "Paris", "8pm", "BBC")


# https://www.independent.co.uk/sport/rugby/rugby-union/international/six-nations-2020-fixtures-schedule-matches-dates-kick-off-times-odds-rugby-a9272381.html
odds <- matrix(c(
    8, 13,
    4, 1,
    11, 2,
    7, 1,
    25, 1,
    500, 1),
    ncol = 2, byrow = TRUE,
    dimnames = list(c("England", "Ireland", "Wales", "France", "Scotland", "Italy"), NULL))
prob <- apply(odds, MARGIN = 1, FUN = function(x) x[2] / sum(x))
#     England     Ireland       Wales      France    Scotland       Italy 
# 0.619047619 0.200000000 0.153846154 0.125000000 0.038461538 0.001996008 
