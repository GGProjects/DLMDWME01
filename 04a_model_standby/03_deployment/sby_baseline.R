#' Standby Prediction - Baseline Model
#'
#' Creates a StandBy Personel forecast for a given period, using a Baseline
#' model, based on the TSLM algorithm. Predictions are calculated using weekly 
#' maxima.
#' 
#' This model is part of a series of models. Therefore some of the parameters 
#' are not being used in this particular model.
#'
#' @param data Training data used for retraining
#' @param train_days Number of days used for training (default = 720)
#' @param train_weeks Number of weeks used for training (default = 104)
#' @param fcperiod Period to forecast
#' @param onduty Planned personel on duty for the forecast period
#' @param dutyoffset Model finetuning; the higher the offset the higher the
#' prediction (default = 100)
#' @param confidence Model finetuning; Confidence intervall to use for prediction
#' @param sby_min Minimum standby personal to be used (default = 35)
#'
#' @returns A list, containing the calculation time and the prediction results
#' @export
#'
#' @examples
#' @author Georg Grunsky, \email{georg.grunsky@@iu-study.org}
apply_model <- function(data,
                        train_weeks = 104,
                        train_days = 720, 
                        fcperiod = "2 months",
                        onduty = 1900,
                        dutyoffset = 100,
                        sby_min = 35,
                        confidence = 90) {
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
  
  sby_pred_base <- fc %>%
    as_tibble() %>%
    mutate(sby = pmax(sby_min,
                      round(.mean,0))) %>%
    select(date, sby)
  
  ##### Ergebnisausgabe #####
  calctime <- t1[3] + t2[3]
  result <- list(sby_pred = sby_pred_base,
                 calctime = calctime)
  return(result)
}


