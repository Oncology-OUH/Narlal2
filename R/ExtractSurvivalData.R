#' Extract survival data
#' @description 'ExtracSurvivalData' extract the survival data from the initial raw data obtained using LoadAndPrepareData
#' @param rawdata the data.frame obtained from LoadAndPrepareData
#' @param maxFollowUpTime time in months, after which all data are censored. Default time is infinity
#'
#' @return data.frame that contain survival data
#' @export ExtractSurvivalData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtSurv <- ExtractSurvivalData(df)
ExtractSurvivalData <- function(rawdata,maxFollowUpTime=Inf){
  patient_id <- unique((rawdata$patient_id))
  index <- !is.na(patient_id)
  patient_id <- patient_id[index]
  df <-data.frame(patient_id)
  df$arm <- NA
  df$arm <- factor(df$arm,levels = levels(rawdata$randomisering.factor))
  #df$d_mors <- as.Date(NA)
  df$d_rt <- as.Date(NA)
  df$d_registrering <- as.Date(NA)
  df$d_pd <- as.Date(NA)
  df$d_pd_cns <- as.Date(NA)
  df$d_pd_hep <- as.Date(NA)
  df$d_pd_bon <- as.Date(NA)
  df$d_pd_skin <- as.Date(NA)
  df$d_pd_and <- as.Date(NA)
  #df$d_censoring_mors <- as.Date(NA)
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
  index_clinicalupdate <- rawdata$redcap_event_name == 'clinical_update_da_arm_1'
  recurenceDates<-ExtractImagingRecurrenceDates(rawdata)
  for (i in seq_len(nrow(df))){
    index <- rawdata$patient_id==df$patient_id[i]
    df$arm[[i]] <- if (sum(index_registration & index)>0) rawdata$randomisering.factor[index_registration & index] else NA
    #df$d_mors[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_mors[index_haendelse & index] else NA
    df$d_rt[[i]] <- if (sum(index_followup1 & index)>0) rawdata$d_rt[index_followup1 & index] else NA
    df$d_registrering[[i]] <- if (sum(index_registration & index)>0) rawdata$d_registrering[index_registration & index] else NA
    df$d_pd[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd[index_haendelse & index] else NA
    df$d_pd_cns[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_cns[index_haendelse & index] else NA
    df$d_pd_hep[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_hep[index_haendelse & index] else NA
    df$d_pd_bon[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_bon[index_haendelse & index] else NA
    df$d_pd_skin[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_skin[index_haendelse & index] else NA
    df$d_pd_and[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_and[index_haendelse & index] else NA
    #df$d_censoring_mors[[i]] <- if (sum(index_clinicalupdate & index)>0) rawdata$clinical_update_date[index_clinicalupdate & index] else NA
    df$durvalumab[[i]] <- if (sum(index_haendelse & index)>0) rawdata$durvalumab.factor[index_haendelse & index] else NA
    df$histology_squamous[[i]] <- if (sum(index_registration & index)>0) rawdata$histology_squamous.factor[index_registration & index] else NA
  }
  #Set all durvalumab NA to NO. This can occur if the page has not been fill in. In that case we will assume that Durvalumab has not been administrated
  df$durvalumab[is.na(df$durvalumab)]<- 'No'
  df <- merge(df, recurenceDates, by = "patient_id", all.x = TRUE)

  #Locale control
  #Date of event is the local event time
  #If no event use last image date
  #If no last image date use randomization date since patient did not make it until first imaging session
  indexEvent<-!is.na(df$dateLocoRegional)
  eventDate<-rep(as.Date(NA),nrow(df))
  eventDate[indexEvent] <- df$dateLocoRegional[indexEvent]
  #If no event use date of last imaging
  eventDate[!indexEvent] <- df$dateLastImaging[!indexEvent]
  #If no imaging date use randomization date
  indexNoImaging<-is.na(eventDate)
  eventDate[indexNoImaging]<-df$date_of_randomization[indexNoImaging]
  df$t_localcontrol <- as.numeric(difftime(eventDate,df$date_of_randomization,units='days'))*12/365.25
  df$event_localcontrol <- indexEvent


  #Progression time. Event if either local or distant event
  #Date to event the first of local or distant event
  #If no event use last image date
  #If no last image date use randomization date since patient did not make it until first imaging session
  tempData<-df[,c('dateLocoRegional','dateDistant')]
  indexEvent<-!apply(tempData, 1,function(x) all(is.na(x)))
  eventDate<-rep(as.Date(NA),nrow(tempData))
  eventDate[indexEvent] <- as.Date(apply(tempData[indexEvent,], 1,min,na.rm=TRUE))
  #If no event use date of last imaging
  eventDate[!indexEvent] <- df$dateLastImaging[!indexEvent]
  #If no imaging date use randomization date
  indexNoImaging<-is.na(eventDate)
  eventDate[indexNoImaging]<-df$date_of_randomization[indexNoImaging]
  df$event_progression<-indexEvent
  df$t_progression<-as.numeric(difftime(eventDate,df$date_of_randomization,units='days'))*12/365.25

  #overall survival time
  #Date of event is the date of death
  #If no event use clinical update date
  #If no clincal update date use day of randomisation. This should not be possible to happen since clinical update date should be defined for all patients
  indexEvent<-!is.na(df$d_mors)
  eventDate<-rep(as.Date(NA),nrow(df))
  eventDate[indexEvent] <- df$d_mors[indexEvent]
  #If no event use date of last imaging
  eventDate[!indexEvent] <- df$clinical_update_date[!indexEvent]
  #If no imaging date use randomization date
  indexNoImaging<-is.na(eventDate)
  eventDate[indexNoImaging]<-df$date_of_randomization[indexNoImaging]
  df$event_os <- indexEvent
  df$t_os <- as.numeric(difftime(eventDate,df$date_of_randomization,units='days'))*12/365.25

  #first event - death local control or progressive disease (the below-created variables are used to create all the competing risk tables)

  #There are five different categories Censoring, local, met, local+met and mors as first event
  #Star by defining the event variable
  first_event<-rep('censoring',nrow(df))
  #find those with first local event
  indexLocal <- !is.na(df$dateLocoRegional) & (is.na(df$dateDistant) | df$dateLocoRegional<df$dateDistant)
  first_event[indexLocal]<-'local'
  #find those with first distant event
  indexMet <- !is.na(df$dateDistant) & (is.na(df$dateLocoRegional) | df$dateDistant<df$dateLocoRegional)
  first_event[indexMet]<-'met'
  #find those with simultaneous met and local
  indexLocalMet <- !is.na(df$dateDistant) & !is.na(df$dateLocoRegional) & df$dateDistant==df$dateLocoRegional
  first_event[indexLocalMet]<-'local+met'
  #find those that has mors as first event
  indexMors <- is.na(df$dateDistant) & is.na(df$dateLocoRegional) & !is.na(df$d_mors)
  first_event[indexMors]<-'mors'
  #Now find the related event dates
  date_first_event<-rep(as.Date(NA),nrow(df))
  index<-first_event=='censoring'
  date_first_event[index]<-df$clinical_update_date[index]
  index<-first_event=='local' | first_event=='local+met'
  date_first_event[index]<-df$dateLocoRegional[index]
  index<-first_event=='met'
  date_first_event[index]<-df$dateDistant[index]
  index<-first_event=='mors'
  date_first_event[index]<-df$d_mors[index]
  df$event_firstevent <- factor(first_event)

  df$t_firstevent<-as.numeric(difftime(date_first_event,df$date_of_randomization,units='days'))*12/365.25

  #Make time censoring
  #local control
  index<-df$t_localcontrol>maxFollowUpTime
  df$t_localcontrol[index]<-maxFollowUpTime
  df$event_localcontrol[index]<-FALSE
  #progression
  index<-df$t_progression>maxFollowUpTime
  df$t_progression[index]<-maxFollowUpTime
  df$event_progression[index]<-FALSE
  #survival
  index<-df$t_os>maxFollowUpTime
  df$t_os[index]<-maxFollowUpTime
  df$event_os[index]<-FALSE
  #firstevent
  index<-df$t_firstevent>maxFollowUpTime
  df$t_firstevent[index]<-maxFollowUpTime
  df$event_firstevent[index]<-'censoring'

  return(df)
}
