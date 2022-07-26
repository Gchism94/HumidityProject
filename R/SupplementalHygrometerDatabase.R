#' Supplemental Hygrometer Data
#'
#' A dataset the raw H.O.B.O. hygrometer data collecting Temperature (Celcius) and relative humidity
#'  every 45 minutes. The variables are as follows:
#'
#' \itemize{
#'   \item Colony. Unique experimental colony identifiers (1--20)
#'   \item TrialNumber. Sequential trial number that is NOT unique for each colony (1--4)
#'   \item Salt. Saturated salt solution used (K2SO4, LiCl, KCl, P2O5, NaAmm, MgCl, MgNO3, NaCl, MgAce, KCO3)
#'   \item Date Time, GMT-07:00. Specific time collected in GMT-7:00 format
#'   \item Temp. Temperature in Celsius (18.129--26.304)
#'   \item RH. Relative humidity (\1%--\100%)
#'   \item Trial. The trial number for each individual colony (1--2)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name SupplementalHygrometerDatabase
#' @usage data(SupplementalHygrometerDatabase)
#' @format A data frame with 21165 rows and 7 variables
