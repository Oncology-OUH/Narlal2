#' Extract patient characteristics
#' @description 'ExtracPatientCharacteristic' extract the patient characteristics from the initial raw data obtained using LoadAndPrepareData
#' @param rawdata the data.frame obtained from LoadAndPrepareData
#'
#' @return data.frame that contains patient characteristic
#' @export ExtractPatientCharacteristic
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtChar <- ExtractPatientCharacteristic(df)
ExtractPatientCharacteristic <- function(rawdata){
  patient_id <- unique((rawdata$patient_id))
  index <- !is.na(patient_id)
  patient_id <- patient_id[index]
  df <-data.frame(patient_id)
  df$arm <- NA
  df$arm <- factor(df$arm,levels = levels(rawdata$randomisering.factor))
  df$age <- NA
  df$gender <- NA
  df$gender <- factor(df$gender,levels = levels(rawdata$gender.factor))
  df$histology <- NA
  df$histology <- factor(df$histology,levels = levels(rawdata$hist.factor))
  df$T <- NA
  df$T <- factor(df$T,levels = levels(rawdata$t.factor))
  df$N <- NA
  df$N <- factor(df$N,levels = levels(rawdata$n.factor))
  df$Stadium <- NA
  df$Stadium <- factor(df$Stadium,levels = levels(rawdata$stadium.factor))
  df$Perform <- NA
  df$Perform <- factor(df$Perform,levels = levels(rawdata$ps.factor))
  df$FEV1 <- NA
  df$FVC <- NA
  df$weight <- NA
  df$height <- NA
  df$MeanDose_T <- NA
  df$MeanDose_N <- NA
  df$MeanLung <- NA
  df$Vol_T <- NA
  df$Vol_N <- NA
  df$durvalumab <- NA
  df$durvalumab <- factor(df$durvalumab,levels = levels(rawdata$durvalumab.factor))

  index_registration <- rawdata$redcap_event_name == 'registration_arm_1'
  index_haendelse <- rawdata$redcap_event_name == 'haendelser_arm_1'
  index_followup1 <- rawdata$redcap_event_name == '1_followup_arm_1'


  for (i in seq_len(nrow(df))){
    index <- rawdata$patient_id==df$patient_id[i]
    #ptdata <- rawdata[index,]
    #index_registration <- ptdata$redcap_event_name == 'registration_arm_1'
    #index_haendelse <- ptdata$redcap_event_name == 'haendelser_arm_1'
    #index_followup1 <- ptdata$redcap_event_name == '1_followup_arm_1'
    df$arm[[i]] <- rawdata$randomisering.factor[index_registration & index]
    df$age[[i]] <- rawdata$age[index_registration & index]
    df$gender[[i]] <- rawdata$gender.factor[index_registration & index]
    df$histology[[i]] <- rawdata$hist.factor[index_registration & index]
    df$T[[i]] <- rawdata$t.factor[index_registration & index]
    df$N[[i]] <- rawdata$n.factor[index_registration & index]
    df$Stadium[[i]] <- rawdata$stadium.factor[index_registration & index]
    df$Perform[[i]] <- rawdata$ps.factor[index_registration & index]
    df$FEV1[[i]] <- rawdata$fev1[index_registration & index]
    df$FVC[[i]] <- rawdata$fvc[index_registration & index]
    df$weight[[i]] <- rawdata$weight[index_registration & index]
    df$height[[i]] <- rawdata$height[index_registration & index]
    df$durvalumab[[i]] <- rawdata$durvalumab.factor[index_haendelse & index]
    df$MeanDose_T[[i]] <- rawdata$gy_t_mean[index_followup1 & index]
    df$MeanDose_N[[i]] <- rawdata$gy_n_mean[index_followup1 & index]
    df$MeanLung[[i]] <- rawdata$gy_lung_mean[index_followup1 & index]
    df$Vol_T[[i]] <- rawdata$vol_gtv_t[index_followup1 & index]
    df$Vol_N[[i]] <- rawdata$vol_gtv_n[index_followup1 & index]
  }
  return(df)
}
