#' Standby Prediction - Prophet Model
#'
#' Creates a StandBy Personel forecast for a given period, using the Prophet
#' model, based on the FACEBOOK PROPHET algorithm. The model forecasts incoming calls and
#' then uses a linear correlation to predict the needed standby personel.
#'
#'
#' @param data Training data used for retraining
#' @param train_days Number of days used for training (default = 720)
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
apply_sby_prophet <- function(data,
                              train_weeks = 104,
                              train_days = 720, 
                              fcperiod = "2 months",
                              onduty = 1900,
                              dutyoffset = 100,
                              sby_min = 35,
                              confidence = 90) {
  require(dplyr)
  require(fpp3)
  require(fable.prophet)
  
  train_data <- data %>%
    slice((n() - train_days):n())
  
  ##### Lineares Modell erzeugen ##### 
  lm_sby = lm(sby_need ~ (pmax(0,(calls - (n_duty - 1700) * 5) - 8150)), 
              data = train_data)

  ##### Training #####
  t1 <- system.time(
    progressr::with_progress(
      sby_model_prophet <- train_data %>%
        model(
          prophet = prophet(calls ~ growth("linear",
                                           n_changepoints = 15) +
                              season(period = 7,
                                     order = 20,
                                     type = "additive",
                                     name = "week") +
                              season(period = 28,
                                     order = 10,
                                     type = "multiplicative",
                                     name = "month") +
                              season(period = "year",
                                     order = 15,
                                     type = "multiplicative",
                                     name = "year")
          )
        )
    ), gcFirst = TRUE
  )
  
  ##### Vorhersage calls #####
  t2 <- system.time(
    progressr::with_progress(
      sby_fc_prophet <- sby_model_prophet %>%
        forecast(h = fcperiod)
    ), gcFirst = TRUE
  )
  
  ##### Verwendung des Konfidenzintervalls #####
  sby_fc_prophet <- sby_fc_prophet %>%
    mutate(.mean = hilo(calls,
                        confidence)$upper)
  
  
  ##### Vorhersage sby_need #####
  
  # vorgesehenes diensthabendes Personal für Vorhersage modifiziert
  duty_pers <- onduty - dutyoffset
  
  # neuer Datensatz mit gleitenden Maxima zu Generalisierung (Prediction-Dataset)
  sby_pred_prophet <- sby_fc_prophet %>%
    as.tibble() %>%
    select(date, .mean) %>%
    mutate(n_duty = duty_pers,
           calls = .mean) %>%
    filter(!is.na(calls)) %>%
    as_tsibble(index = date)
  
  # Anwendung lineares Modell 
  sby_pred <- predict(lm_sby, sby_pred_prophet, interval = "prediction")
  
  # Hinzufügen von sby zu Prediction-Dataset mit Bereitschafts-Mindestwert
  sby_pred_prophet$sby <- pmax(sby_min,
                            round(sby_pred[,3],0))
  
  sby_pred_prophet <- sby_pred_prophet %>%
    select(date, sby)
  
  ##### Ergebnisausgabe #####
  calctime <- t1[3] + t2[3]
  result <- list(sby_pred = sby_pred_prophet,
                 calctime = calctime)
  return(result)
}


