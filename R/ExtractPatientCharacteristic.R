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
  df$Stadium <- factor(df$Stadium,levels = levels(rawdata$std.factor))
  df$Perform <- NA
  df$Perform <- factor(df$Perform,levels = levels(rawdata$ps.factor))
  df$FEV1 <- NA
  df$FEV1_percent <- NA
  df$FVC <- NA
  df$DLCO <- NA
  df$DLCO_percent <- NA
  df$weight <- NA
  df$height <- NA
  df$MeanDose_T <- NA
  df$MeanDose_N <- NA
  df$MeanLung <- NA
  df$Vol_T <- NA
  df$Vol_N <- NA
  df$durvalumab <- NA
  df$durvalumab <- factor(df$durvalumab,levels = levels(rawdata$durvalumab.factor))
  df$histology_squamous <- NA
  df$histology_squamous <- factor(df$durvalumab,levels = levels(rawdata$histology_squamous.factor))
  df$n_cis_rt<-NA
  df$n_carbo_rt<-NA
  df$n_nav_rt<-NA
  df$n_platin_rt<-NA
  df$fx<-NA
  df$smoking_baseline<-NA
  df$smoking_baseline<-factor(df$smoking_baseline,levels = levels(rawdata$smoking_rt.factor))
  df$previous_smoker<-NA
  df$previous_smoker<-factor(df$previous_smoker,levels=c('Nej','Ja'))
  df$d_rt <- as.Date(NA)
  df$d_rtend <- as.Date(NA)
  df$daysRT<-NA

  index_registration <- rawdata$redcap_event_name == 'registration_arm_1'
  index_haendelse <- rawdata$redcap_event_name == 'haendelser_arm_1'
  index_followup1 <- rawdata$redcap_event_name == '1_followup_arm_1'
  index_uge1 <- rawdata$redcap_event_name == 'uge_1_arm_1'

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
    df$Stadium[[i]] <- rawdata$std.factor[index_registration & index]
    df$Perform[[i]] <- rawdata$ps.factor[index_registration & index]
    df$FEV1[[i]] <- rawdata$fev1[index_registration & index]
    df$FVC[[i]] <- rawdata$fvc[index_registration & index]
    df$DLCO[[i]] <- rawdata$dlco[index_registration & index]
    df$DLCO_percent[[i]] <- rawdata$dlcoref[index_registration & index]
    df$weight[[i]] <- rawdata$weight[index_registration & index]
    df$height[[i]] <- rawdata$height[index_registration & index]
    df$durvalumab[[i]] <- if (sum(index_haendelse & index)>0) rawdata$durvalumab.factor[index_haendelse & index] else NA
    df$MeanDose_T[[i]] <- if (sum(index_followup1 & index)>0) rawdata$gy_t_mean[index_followup1 & index] else NA
    df$MeanDose_N[[i]] <- if (sum(index_followup1 & index)>0) rawdata$gy_n_mean[index_followup1 & index] else NA
    df$MeanLung[[i]] <- if (sum(index_followup1 & index)>0) rawdata$gy_lung_mean[index_followup1 & index] else NA
    df$Vol_T[[i]] <- if (sum(index_followup1 & index)>0) rawdata$vol_gtv_t[index_followup1 & index] else NA
    df$Vol_N[[i]] <- if (sum(index_followup1 & index)>0) rawdata$vol_gtv_n[index_followup1 & index] else NA
    df$n_cis_rt[[i]]<-if (sum(index_followup1 & index)>0) rawdata$n_cis_rt[index_followup1 & index] else NA
    df$n_carbo_rt[[i]]<- if (sum(index_followup1 & index)>0) rawdata$n_carbo_rt[index_followup1 & index] else NA
    df$n_nav_rt[[i]]<-if (sum(index_followup1 & index)>0) rawdata$n_nav_rt[index_followup1 & index] else NA
    df$fx[[i]]<-if (sum(index_followup1 & index)>0) rawdata$fx[index_followup1 & index] else NA
    df$histology_squamous[[i]] <- if (sum(index_registration & index)>0) rawdata$histology_squamous.factor[index_registration & index] else NA
    df$smoking_baseline[[i]] <- if (sum(index_uge1 & index)>0) rawdata$smoking_rt.factor[index_uge1 & index] else NA
    if (!is.na(rawdata$pack_years[index_registration & index])){
      df$previous_smoker[[i]] <-if (rawdata$pack_years[index_registration & index]>1) 'Ja' else 'Nej'
    }
    df$d_rt[[i]] <- if (sum(index_followup1 & index)>0) rawdata$d_rt[index_followup1 & index] else NA
    df$d_rtend[[i]] <- if (sum(index_followup1 & index)>0) rawdata$d_rtsl[index_followup1 & index] else NA
  }
  #Calculate FEV1_percent since only available as absolute in the database
  #The equation for reference values are obtained from Loekke A, et al. "New Danish reference values for spirometry" Clin Respir J. 2013;7(2):153-67.
  index<-df$gender=="Kvinde"
  df$FEV1_percent[index] <- 100*df$FEV1[index]/(-1.35015-0.00024*df$age[index]^2+0.02923*df$height[index])
  df$FEV1_percent[!index] <- 100*df$FEV1[!index]/(-2.87615-0.00026*df$age[!index]^2+0.04201*df$height[!index])
  #Extract as one variable the sum of n_carbo_rt and n_cis_rt
  #df$n_platin_rt <- apply(cbind(df$n_cis_rt,df$n_carbo_rt),1,max,na.rm=TRUE)
  df$n_platin_rt <- df$n_cis_rt
  indexCis<-!is.na(df$n_cis_rt)
  indexCarbo<-!is.na(df$n_carbo_rt)
  df$n_platin_rt<-df$n_cis_rt
  df$n_platin_rt[!indexCis & indexCarbo]<-df$n_carbo_rt[!indexCis & indexCarbo]
  df$n_platin_rt[indexCis & indexCarbo]<-df$n_cis_rt[indexCis & indexCarbo]+df$n_carbo_rt[indexCis & indexCarbo]
  #Calculate number of days during RT
  df$daysRT<-as.numeric(df$d_rtend-df$d_rt)
  return(df)
}
