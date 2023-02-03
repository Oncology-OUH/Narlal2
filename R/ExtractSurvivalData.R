#' Extract survival data
#' @description 'ExtracSurvivalData' extract the survival data from the initial raw data obtained using LoadAndPrepareData
#' @param rawdata the data.frame obtained from LoadAndPrepareData
#' @param censor_after_rt value in month of the latest value to censor after RT. Needs to be fixed then the database has a censor date included
#'
#' @return data.frame that contain survival data
#' @export ExtractSurvivalData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtSurv <- ExtractSurvivalData(df,12*5)
ExtractSurvivalData <- function(rawdata,censor_after_rt){
  patient_id <- unique((rawdata$patient_id))
  index <- !is.na(patient_id)
  patient_id <- patient_id[index]
  df <-data.frame(patient_id)
  df$arm <- NA
  df$arm <- factor(df$arm,levels = levels(rawdata$randomisering.factor))
  df$d_mors <- as.Date(NA)
  df$d_rt <- as.Date(NA)
  df$d_registrering <- as.Date(NA)
  df$d_pd <- as.Date(NA)
  df$d_pd_cns <- as.Date(NA)
  df$d_pd_hep <- as.Date(NA)
  df$d_pd_bon <- as.Date(NA)
  df$d_pd_skin <- as.Date(NA)
  df$d_pd_and <- as.Date(NA)
  df$d_censoring <- as.Date(NA)
  df$event_localcontrol <- NA
  df$t_localcontrol <- NA
  df$event_progression <- NA
  df$t_progression <- NA
  df$event_os <- NA
  df$t_os <- NA
  df$event_firstevent <- NA
  df$t_firstevent <- NA
  df$durvalumab <- NA
  df$durvalumab <- factor(df$durvalumab,levels = levels(rawdata$durvalumab.factor))
  df$histology_squamous <- NA
  df$histology_squamous <- factor(df$durvalumab,levels = levels(rawdata$histology_squamous.factor))


  index_registration <- rawdata$redcap_event_name == 'registration_arm_1'
  index_haendelse <- rawdata$redcap_event_name == 'haendelser_arm_1'
  index_followup1 <- rawdata$redcap_event_name == '1_followup_arm_1'

  for (i in seq_len(nrow(df))){
    index <- rawdata$patient_id==df$patient_id[i]
    df$arm[[i]] <- rawdata$randomisering.factor[index_registration & index]
    df$d_mors[[i]] <- rawdata$d_mors[index_haendelse & index]
    df$d_rt[[i]] <- rawdata$d_rt[index_followup1 & index]
    df$d_registrering[[i]] <- rawdata$d_registrering[index_registration & index]
    df$d_pd[[i]] <- rawdata$d_pd[index_haendelse & index]
    df$d_pd_cns[[i]] <- rawdata$d_pd_cns[index_haendelse & index]
    df$d_pd_hep[[i]] <- rawdata$d_pd_hep[index_haendelse & index]
    df$d_pd_bon[[i]] <- rawdata$d_pd_bon[index_haendelse & index]
    df$d_pd_skin[[i]] <- rawdata$d_pd_skin[index_haendelse & index]
    df$d_pd_and[[i]] <- rawdata$d_pd_and[index_haendelse & index]
    df$d_censoring[[i]] <- as.Date(as.numeric(rawdata$d_rt[index_followup1 & index])+censor_after_rt*365.25/12,origin="1970-01-01")
    df$durvalumab[[i]] <- rawdata$durvalumab.factor[index_haendelse & index]
    df$histology_squamous[[i]] <- rawdata$histology_squamous.factor[index_registration & index]
  }
  #locale control
  temp<-cbind(df$d_mors,df$d_pd,df$d_censoring)
  temp[is.na(temp)] <- Inf
  first_event_time <- apply(temp,1,min)
  index_event <- as.numeric(df$d_pd) <= first_event_time
  index_event[is.na(index_event)] <- FALSE
  df$event_localcontrol <- index_event
  df$t_localcontrol <- (first_event_time-as.numeric(df$d_registrering))*12/365.25

  #progression time
  temp <- cbind(df$d_pd_cns,df$d_pd_hep,df$d_pd_bon,df$d_pd_skin,df$d_pd_and,df$d_pd,df$d_censoring,df$d_mors)
  temp[is.na(temp)] <- Inf
  progression_event_time <- apply(temp,1,min)

  temp <- cbind(df$d_pd_cns,df$d_pd_hep,df$d_pd_bon,df$d_pd_skin,df$d_pd_and,df$d_pd,df$d_mors) #death is also event
  temp[is.na(temp)] <- Inf
  temp_event_time <- apply(temp,1,min)

  index_event <- temp_event_time <= progression_event_time
  df$event_progression <- index_event
  df$t_progression <- (progression_event_time-as.numeric(df$d_registrering))*12/365.25

  #overall survival time
  temp <- cbind(df$d_censoring,df$d_mors)
  temp[is.na(temp)] <- Inf
  os_event_time <- apply(temp,1,min)
  index_event <- df$d_mors <= os_event_time
  df$event_os <- index_event
  df$t_os <- (os_event_time-as.numeric(df$d_registrering))*12/365.25

  #first event - death local control or progressive disease (the below-created variables are used to create all the competing risk tables)
  temp <- cbind(df$d_pd_cns,df$d_pd_hep,df$d_pd_bon,df$d_pd_skin,df$d_pd_and)
  temp[is.na(temp)] <- Inf
  first_met_event_time <- apply(temp,1,min)
  temp <- cbind(df$d_pd_cns,df$d_pd_hep,df$d_pd_bon,df$d_pd_skin,df$d_pd_and,df$d_mors,df$d_pd,df$d_censoring)
  temp[is.na(temp)] <- Inf
  first_event_time <- apply(temp,1,min)
  first_event <- c(matrix("censoring",length(first_event_time),1))
  index <- as.numeric(df$d_pd) <= first_event_time
  index[is.na(index)] <- FALSE
  first_event[index] <- 'local'
  index <- first_met_event_time <= first_event_time
  index[is.na(index)] <- FALSE
  first_event[index] <- 'met'
  index <- (as.numeric(df$d_pd) == first_met_event_time) & (first_met_event_time <= first_event_time)
  index[is.na(index)] <- FALSE
  first_event[index] <- 'local+met'
  index <- as.numeric(df$d_mors) <= first_event_time
  index[is.na(index)] <- FALSE
  first_event[index] <- 'mors'
  df$event_firstevent <- factor(first_event)
  df$t_firstevent <- (first_event_time-as.numeric(df$d_registrering))*12/365.25

  return(df)
}
