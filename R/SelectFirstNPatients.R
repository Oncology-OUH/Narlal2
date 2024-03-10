#' Select from the data the first n included patients
#'
#' @param df the original data frame obtain by using LoadAndPrepareData
#' @param n the number of patients to be selected e.g. 350 as for the primary endpoint
#'
#' @return a data frame similar to the original but only including the initial n patients based on the date of randomization. All patients randomised on the last date will be included in the data
#' @export SelectFirstNPatients
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' df <- SelectFirstNPatients(df,350)
SelectFirstNPatients <- function(df,n){
  index<-df$redcap_event_name=='registration_arm_1'
  cprData<-df[index,]
  if (n>nrow(cprData)){
    n<-nrow(cprData)
  }
  index<-cprData$d_registrering<=sort(cprData$date_of_randomization)[n]
  cprToInclude<-cprData[index,]$patient_id
  index<-df$patient_id %in% cprToInclude
  return(df[index,])
}
