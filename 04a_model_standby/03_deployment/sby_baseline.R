#' Title
#'
#' @param data 
#' @param train_weeks 
#'
#' @returns
#' @export
#'
#' @examples
apply_sby_baseline <- function(data, train_weeks = 104, fcperiod = "2 months") {
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
        model(
          tslm = TSLM(w_need ~ trend() + season())
        ) 
    ),  gcFirst = TRUE
  )
  
  ##### Vorhersage #####
  t2 <- system.time(
    progressr::with_progress(
      sby_basefc <- sby_basemodel %>%
        forecast(h = fcperiod, level = c(80, 90, 99))
    ), gcFirst = TRUE
  )
  
  ##### in Tagesdaten konvertieren #####
  daysequence <- seq.Date(from = range(as.Date(sby_basefc$week))[1],
                   to = range(as.Date(sby_basefc$week))[2]+6,
                   by = 1)
  
  week2day <- data.frame(date = daysequence,
                         week = yearweek(daysequence))
  
  fc <- sby_basefc %>% 
    as_tibble() %>% 
    left_join(week2day, by = "week") %>%
    filter(!is.na(date)) %>%
    mutate(sby_need = w_need) %>%
    select(.model, date, sby_need, .mean) %>%
    as_fable(index = date, key = .model, response = "sby_need", distribution = sby_need)
  
  
  ##### Ergebnisausgabe #####
  calctime <- t1[3] + t2[3]
  result <- list(fc = fc,
                 calctime = calctime)
  return(result)
}


