#' Standby Prediction - Call Model
#'
#' Creates a StandBy Personnel forecast for a given period, using one of the
#' deployed models from the model-deployment folder. 
#'  
#' This function calls the specified model and handles parameter and data handover, 
#' logging of calculation time and result output.
#'
#' @param data Data to handover to the model
#' @param model Chosen model to call
#' @param train_days Time period used for training (prophet and advanced)
#' @param train_weeks Time period used for training (baseline)
#' @param fcperiod Forecast period
#' @param onduty Number of personnel on duty in forecast period (prophet and advanced)
#' @param dutyoffset Offset to subtract from onduty param to fine tune prediction (prophet and advanced)
#' @param sby_min Minimum number of StandBy personnel to use in prediction
#' @param confidence Used quantile for calculation output (prophet only)
#'
#' @returns Returns a data.frame with date and predicted Standby personnel.
#' Writs prediction time to log output.
#' @export
#'
#' @examples
predict_sby <- function(data,
                        model = "prophet",
                        train_days = 720,
                        train_weeks = 104, 
                        fcperiod = "2 months",
                        onduty = 1900,
                        dutyoffset = 100,
                        sby_min = 35,
                        confidence = 90) {
  # Pakete laden
  require(dplyr)
  require(fpp3)
  require(fable.prophet)
  
  
  # in tsibble konvertieren
  ts_sby <- data %>%
    select(-c("n_sby", "dafted")) %>%
    mutate(date = as.Date(date)) %>%
    as_tsibble(index = date)
  
  # gewähltes Modell laden
  source(paste0("../../../04a_model_standby/03_deployment/sby_", model,".R"))  
  
  # Modell anwenden
  result <- suppressWarnings(apply_model(data = ts_sby))
  
  # Prediction-Time loggen
  write(paste0(Sys.time(), " - Prediction time: ", 
               round(result[[2]],2), " seconds, using Model: ", model),
        file = paste0("../../04_logs/prediction_", Sys.Date(), ".log"),
        append = TRUE)
  
  # Ergebnis ausgeben
  return(result[[1]])
}