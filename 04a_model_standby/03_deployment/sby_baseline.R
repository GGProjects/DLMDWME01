#' Title
#'
#' @param data 
#' @param train_weeks 
#'
#' @returns
#' @export
#'
#' @examples
apply_sby_baseline <- function(data, train_weeks = 104) {
  require(dplyr)
  require(fpp3)
  
  ##### MQ7 berechnen und zu Wochendaten Daten aggregieren #####
  ts_sby_week <- data %>%
    mutate(
      MQ7 = slider::slide_dbl(sby_need, 
                              ~ quantile(x = .x,
                                         probs = 0.99),
                              .before = 3, .after = 3,
                              .complete = TRUE)) %>%
    filter(!is.na(MQ7)) %>%
    index_by(week = ~ yearweek(.)) %>%
    summarise(w_need = max(MQ7)) %>%
    filter(!is.na(w_need)) %>%
    select(week, w_need) %>%
    slice(n():(n() - train_weeks))
  
  ##### Training #####
  t1 <- system.time(
    progressr::with_progress(
      sby_basemodel <- ts_sby_week %>%
        filter(year(week) >= 2017) %>%
        model(
          tslm = TSLM(w_need ~ trend() + season())
        ) 
    ),  gcFirst = TRUE
  )
  
  ##### Vorhersage #####
  t2 <- system.time(
    progressr::with_progress(
      sby_basefc <- sby_basemodel %>%
        forecast(h = "2 months", level = c(99))
    ), gcFirst = TRUE
  )
  
  ##### in Tagesdaten konvertieren #####
  fc <- data %>%
    index_by(week = ~ yearweek(.)) %>%
    select(date, week) %>%
    as_tibble() %>%
    right_join(as_tibble(sby_basefc), by = "week") %>%
    ungroup() %>%
    select(date, sby_need = .mean)
  
  ##### Ergebnisausgabe #####
  calctime <- t1[3] + t2[3]
  result <- list(fc = fc,
                 calctime = calctime)
  return(result)
}


