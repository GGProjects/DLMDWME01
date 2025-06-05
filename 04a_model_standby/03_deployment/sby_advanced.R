#' Standby Prediction - Advanced Model
#'
#' Creates a StandBy Personel forecast for a given period, using an Advanced
#' model, based on the STL algorithm. The model forecasts incoming calls and
#' then uses a linear correlation to predict the needed standby personel.
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
  
  train_data <- data %>%
    slice((n() - train_days):n())
  
  ##### Lineares Modell erzeugen ##### 
  lm_sby = lm(sby_need ~ (pmax(0,(calls - (n_duty - 1700) * 5) - 8150)), 
              data = train_data)

  ##### Training #####
  decomp_spec <- decomposition_model(
    STL(calls ~ trend(window = 365) +
          season(period = "1 year", window = 540) + 
          season(period = "1 month", window = 61) +
          season(period = "1 week", window = 28)),
    RW(season_adjust ~ drift())
  )
  
  ##### Training ##### 
  t1 <- system.time(
    progressr::with_progress(
      sby_model_adv <- train_data %>%
        model(
          stl = decomp_spec,
        )
    ), gcFirst = TRUE
  )
  
  ##### Vorhersage calls #####
  t2 <- system.time(
    progressr::with_progress(
      sby_fc_adv <- sby_model_adv %>%
        forecast(h = fcperiod, level = c(80))
    ), gcFirst = TRUE
  )
  
  ##### Vorhersage sby_need #####
  
  # vorgesehenes diensthabendes Personal für Vorhersage modifiziert
  duty_pers <- onduty - dutyoffset
  
  # neuer Datensatz mit gleitenden Maxima zu Generalisierung (Prediction-Dataset)
  sby_pred_adv <- sby_fc_adv %>%
    filter(.model == "stl") %>%
    as_tibble() %>%
    select(date, .mean) %>%
    mutate(n_duty = duty_pers,
           calls = slider::slide_dbl(.mean, 
                                     max,
                                     .before = 6, .after = 7,
                                     .complete = TRUE)) %>%
    filter(!is.na(calls)) %>%
    as_tsibble(index = date)
  
  # Anwendung lineares Modell 
  sby_pred <- predict(lm_sby, sby_pred_adv, interval = "prediction")
  
  # Hinzufügen von sby zu Prediction-Dataset mit Bereitschafts-Mindestwert
  sby_pred_adv$sby <- pmax(sby_min,
                            round(sby_pred[,3],0))
  
  sby_pred_adv <- sby_pred_adv %>%
    select(date, sby)
  
  ##### Ergebnisausgabe #####
  calctime <- t1[3] + t2[3]
  result <- list(sby_pred = sby_pred_adv,
                 calctime = calctime)
  return(result)
}


