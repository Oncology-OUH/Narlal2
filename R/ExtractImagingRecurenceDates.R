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
    indexPt <- datadf$patient_id==pts[iPts]
    if (nrow(dftemp)>0){
      index<- !is.na(dftemp$date_of_imaging)
      if (any(index)){

        datadf[indexPt,'numberImageSessions'] <- sum(index)
        dateLastImaging <- max(dftemp$date_of_imaging[index])

        #For each possible recurrence position, the imaging recurrence status is selected. However, if there is a related biopsy with the result “rejected image information” or “Inconclusive result”, the image recurrence observation is disregarded. For both “Rejected” and “Inconclusive,” the image result was not validated within the biopsy and is thus ignored.
        localRecureceBiopsyVariables<-c('same_lobe_biopsy_val','mediastinal_biopsy_val','ipsi_hilus_biopsy_val','contra_hilus_biopsy_val','ipsi_supraclav_biopsy_val')
        indexLocoRegional<-rep(FALSE,nrow(dftemp))
        for (iBiopsy in seq_along(localRecureceBiopsyVariables)){
          localRecurrence<-dftemp[[paste('local_recurrence_location___',iBiopsy,'.factor',sep='')]]=='Checked'
          localRecurrence[is.na(localRecurrence)]<-FALSE
          rejectByBiopsy<-dftemp[[localRecureceBiopsyVariables[iBiopsy]]]==3 | dftemp[[localRecureceBiopsyVariables[iBiopsy]]]==4
          rejectByBiopsy[is.na(rejectByBiopsy)]<-FALSE
          indexLocoRegional<-indexLocoRegional | (localRecurrence & !rejectByBiopsy)
        }
        distantRecureceBiopsyVariables<-c('diffhist_biopsy_val','otherlobe_biopsy_val','contra_lung_biopsy_val','contra_supra_biopsy_val',
                                          'mult_lungmet_biopsy_val','pleura_efus_biopsy_val','brain_biopsy_val','liver_biopsy_val',
                                          'andrenal_biopsy_val','bone_biopsy_val','cutis_biopsy_val','other_recurrence_type')
        indexDistant<-rep(FALSE,nrow(dftemp))
        for (iBiopsy in seq_along(distantRecureceBiopsyVariables)){
          distantRecurrence<-dftemp[[paste('distant_recurrence_loc___',iBiopsy,'.factor',sep='')]]=='Checked'
          distantRecurrence[is.na(distantRecurrence)]<-FALSE
          rejectByBiopsy<-dftemp[[distantRecureceBiopsyVariables[iBiopsy]]]==3 | dftemp[[distantRecureceBiopsyVariables[iBiopsy]]]==4
          rejectByBiopsy[is.na(rejectByBiopsy)]<-FALSE
          indexDistant<-indexDistant | (distantRecurrence & !rejectByBiopsy)
        }

        if (any(indexLocoRegional)){
          dateLocoRegional<-min(dftemp$date_of_imaging[indexLocoRegional])
        }
        if (any(indexDistant)){
          dateDistant<-min(dftemp$date_of_imaging[indexDistant])
        }
      }
    }
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
