#' Extract recurrence dates from the medical image information
#' @description ‘ExtractImagingRecurrenceDates’ extracts the dates of recurrence from the medical images. The dates are local recurrence or any recurrence.
#'
#' @param df the data.frame obtained from LoadAndPrepareData
#'
#' @return data.frame that contain recurrence dates
#' @export ExtractImagingRecurrenceDates
#'
#' @examples  file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' RecurrenceDates <- ExtractImagingRecurrenceDates(df)
ExtractImagingRecurrenceDates <- function(df){
  pts<-unique(df$patient_id)
  df1 <- df[df$redcap_event_name=='haendelser_arm_1',c('patient_id','d_mors')]
  df2 <- df[df$redcap_event_name=='registration_arm_1',c('patient_id','date_of_randomization')]
  df3 <- df[df$redcap_event_name=='clinical_update_da_arm_1',c('patient_id','clinical_update_date','image_update_date')]
  datadf <- data.frame(patient_id = pts, dateLocoRegional = as.Date(NA), dateDistant = as.Date(NA), dateLastImaging = as.Date(NA), numberImageSessions = 0)
  datadf <- merge(datadf, df1, by = "patient_id", all = TRUE)
  datadf <- merge(datadf, df2, by = "patient_id", all = TRUE)
  datadf <- merge(datadf, df3, by = "patient_id", all = TRUE)
  #Start by reducing the data
  df <- df[df$redcap_event_name=='medical_imaging_arm_1',]
  for (iPts in seq_along(pts)){

    dftemp <- df[df$patient_id==pts[iPts],]
    dateLocoRegional <- NA
    dateDistant <- NA
    dateLastImaging <- NA
    if (nrow(dftemp)>0){
      index<- !is.na(dftemp$date_of_imaging)
      if (any(index)){
        datadf[iPts,'numberImageSessions'] <- sum(index)
        dateLastImaging <- max(dftemp$date_of_imaging[index])
        indexLocoRegional <- dftemp$recurence_type___1.factor=='Checked'
        indexLocoRegional[is.na(indexLocoRegional)] <- FALSE
        indexDistant <- dftemp$recurence_type___2.factor=='Checked'
        indexDistant[is.na(indexDistant)] <- FALSE
        if (any(indexLocoRegional)){
          dateLocoRegional<-min(dftemp$date_of_imaging[indexLocoRegional])
        }
        if (any(indexDistant)){
          dateDistant<-min(dftemp$date_of_imaging[indexDistant])
        }
      }
    }
    indexPt <- datadf$patient_id==pts[iPts]
    datadf[indexPt,'dateLocoRegional'] <- dateLocoRegional
    datadf[indexPt,'dateDistant'] <- dateDistant
    datadf[indexPt,'dateLastImaging'] <- dateLastImaging
  }
  #datadf$text1 <- as.numeric(sub("-.*", "", datadf$patient_id))
  #datadf$text2 <- as.numeric(sub(".*-", "", datadf$patient_id))
  #datadf$sort_order <- datadf$text1 * 1000 + datadf$text2
  #datadf <- datadf[order(datadf$sort_order), ]
  #datadf$text1 <- NULL
  #datadf$text2 <- NULL
  #datadf$sort_order <- NULL
  return(datadf)
}
