#' Extract toxicity data
#' @description 'ExtracToxicityData' extract the patient toxicity from the initial raw data obtained using LoadAndPrepareData
#' @param rawdata the data.frame obtained from LoadAndPrepareData
#'
#' @return data.frame that contain toxicity data
#' @export ExtractToxicityData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df)
ExtractToxicityData <- function(rawdata){

  tox_variable_rt <- c('ps_rt','fatigue_rt','cough_rt','dyspnoe_rt',
                       'constipation_rt','nausea_rt','vomiting_rt',
                       'dysphagia_rt','pain_rt','sens_neuropati_rt',
                       'infection_rt','diaria_rt','skinreaction_rt','ototox_rt')
  tox_variable_fu <- c('ps_fu','fatigue_fu','cough_fu','dyspnoe_fu','diare_fu',
                       'nausea_fu','vomiting_fu','dysphagia_fu','pain_fu',
                       'sens_neuropati_fu','infection_fu')


  week_number <- seq_len(8)
  month_number <- c(3,6,9,12,15,18,21,24,30,36,42,48,54,60)
  patient_id <- unique((rawdata$patient_id))
  index <- !is.na(patient_id)
  patient_id <- patient_id[index]
  df <-data.frame(patient_id)
  df$arm <- NA
  df$arm <- factor(df$arm,levels = levels(rawdata$randomisering.factor))
  df$durvalumab <- NA
  df$durvalumab <- factor(df$durvalumab,levels = levels(rawdata$durvalumab.factor))
  df$histology_squamous <- NA
  df$histology_squamous <- factor(df$durvalumab,levels = levels(rawdata$histology_squamous.factor))
  df$d_rt <- as.Date(NA)
  df$d_registrering <- as.Date(NA)
  df$d_pd <- as.Date(NA)
  df$d_pd_cns <- as.Date(NA)
  df$d_pd_hep <- as.Date(NA)
  df$d_pd_bon <- as.Date(NA)
  df$d_pd_skin <- as.Date(NA)
  df$d_pd_and <- as.Date(NA)

  for (j in week_number){
    redcap_name <- paste("uge_",as.character(j),"_arm_1",sep="")
    for (k in tox_variable_rt){
      varname_name <- paste(k,"_uge_",as.character(j),sep="")
      df[[varname_name]] <- NA
      df[[varname_name]] <- factor(df[[varname_name]],levels = levels(rawdata[[paste(k,'.factor',sep='')]]))
    }
  }

  for (j in month_number){
    redcap_name <- paste(as.character(j),"_mdr_followup_arm_1",sep="")
    for (k in tox_variable_fu){
      varname_name <- paste(k,"_mdr_",as.character(j),sep="")
      df[[varname_name]] <- NA
      df[[varname_name]] <- factor(df[[varname_name]],levels = levels(rawdata[[paste(k,'.factor',sep='')]]))
    }
  }
  index_registration <- rawdata$redcap_event_name == 'registration_arm_1'
  index_haendelse <- rawdata$redcap_event_name == 'haendelser_arm_1'
  index_followup1 <- rawdata$redcap_event_name == '1_followup_arm_1'
  for (i in seq_len(nrow(df))){
    index <- rawdata$patient_id==df$patient_id[i]
    #ptdata <- rawdata[index,]
    #index_registration <- ptdata$redcap_event_name == 'registration_arm_1'
    #index_haendelse <- ptdata$redcap_event_name == 'haendelser_arm_1'
    #index_followup1 <- ptdata$redcap_event_name == '1_followup_arm_1'
    for (j in week_number){
      redcap_name <- paste("uge_",as.character(j),"_arm_1",sep="")
      redcap_index <- rawdata$redcap_event_name == redcap_name
      for (k in tox_variable_rt){
        varname_name <- paste(k,"_uge_",as.character(j),sep="")
        indextemp <- redcap_index & index
        if (sum(indextemp)==1){
          if (!is.null(rawdata[[paste(k,'.factor',sep='')]])){
            df[[varname_name]][[i]] <- rawdata[[paste(k,'.factor',sep='')]][indextemp]
          }
        }
      }
    }
    for (j in month_number){
      redcap_name <- paste(as.character(j),"_mdr_followup_arm_1",sep="")
      redcap_index <- rawdata$redcap_event_name == redcap_name
      for (k in tox_variable_fu){
        varname_name <- paste(k,"_mdr_",as.character(j),sep="")
        indextemp <- redcap_index & index
        if (sum(indextemp)==1){
          if (!is.null(rawdata[[paste(k,'.factor',sep='')]])){
            df[[varname_name]][[i]] <- rawdata[[paste(k,'.factor',sep='')]][indextemp]
          }
        }
      }
    }
    df$arm[[i]] <- rawdata$randomisering.factor[index_registration & index]
    df$durvalumab[[i]] <- rawdata$durvalumab.factor[index_haendelse & index]
    df$histology_squamous[[i]] <- rawdata$histology_squamous.factor[index_registration & index]
    df$d_rt[[i]] <- rawdata$d_rt[index_followup1 & index]
    df$d_registrering[[i]] <- rawdata$d_registrering[index_registration & index]
    df$d_pd[[i]] <- rawdata$d_pd[index_haendelse & index]
    df$d_pd_cns[[i]] <- rawdata$d_pd_cns[index_haendelse & index]
    df$d_pd_hep[[i]] <- rawdata$d_pd_hep[index_haendelse & index]
    df$d_pd_bon[[i]] <- rawdata$d_pd_bon[index_haendelse & index]
    df$d_pd_skin[[i]] <- rawdata$d_pd_skin[index_haendelse & index]
    df$d_pd_and[[i]] <- rawdata$d_pd_and[index_haendelse & index]
  }
  #Add first progression date (need to only count tox before progression)
  temp <- data.frame(df$d_pd_cns,df$d_pd_hep,df$d_pd_bon,df$d_pd_skin,df$d_pd_and,df$d_pd)
  temp[is.na(temp)] <- Sys.Date()+500*365 #Current time plus 500 years
  df$d_progression <- as.Date(apply(temp,1,min))

  #Add variable with maximum degree during RT
  for (k in tox_variable_rt){
    data_max<-data.frame(matrix(NA,nrow=nrow(df),ncol=0))
    data_before<-data.frame(matrix(NA,nrow=nrow(df),ncol=0))
    for (j in week_number){
      early_var <- paste(k,"_uge_",as.character(j),sep="")
      data_max[[as.character(j)]] <- factor(df[[early_var]],levels = c(0,1,2,3,4,5),ordered = TRUE)
      data_before[[as.character(j)]] <- factor(df[[early_var]],levels = c(0,1,2,3,4,5),ordered = TRUE)
      #index those with current time prior to progression
      index<-(df$d_rt+j*7)<df$d_progression
      data_before[[as.character(j)]][!index] <-0
    }
    data_max[is.na(data_max)] <- 0
    data_before[is.na(data_before)] <- 0
    varname <- paste("DuringAll_",k,sep="")
    df[[varname]]<- MaxOrderedFactorsPerRow(data_max)
    varname <- paste("DuringBeforeProgres_",k,sep="")
    df[[varname]]<- MaxOrderedFactorsPerRow(data_before)
  }
  #Add variable with maximum degree during 3 and 6 month followup
  Early_followup <- c(3,6)
  for (k in tox_variable_fu){
    data_max<-data.frame(matrix(NA,nrow=nrow(df),ncol=0))
    data_before<-data.frame(matrix(NA,nrow=nrow(df),ncol=0))
    for (j in Early_followup){
      early_var <- paste(k,"_mdr_",as.character(j),sep="")
      data_max[[as.character(j)]] <- factor(df[[early_var]],levels = c(0,1,2,3,4,5),ordered = TRUE)
      data_before[[as.character(j)]] <- factor(df[[early_var]],levels = c(0,1,2,3,4,5),ordered = TRUE)
      #index those with current time prior to progression
      index<-(df$d_rt+j*365.25/12)<df$d_progression
      data_before[[as.character(j)]][!index] <-0
    }
    data_max[is.na(data_max)] <- 0
    data_before[is.na(data_before)] <- 0
    varname <- paste("EarlyAll_",k,sep="")
    df[[varname]]<- MaxOrderedFactorsPerRow(data_max)
    varname <- paste("EarlyBeforeProgres_",k,sep="")
    df[[varname]]<- MaxOrderedFactorsPerRow(data_before)
  }
  #Add variable with maximum degree after 6 months

  for (k in tox_variable_fu){
    data_max<-data.frame(matrix(NA,nrow=nrow(df),ncol=0))
    data_before<-data.frame(matrix(NA,nrow=nrow(df),ncol=0))
    for (j in setdiff(month_number,Early_followup)){
      early_var <- paste(k,"_mdr_",as.character(j),sep="")
      data_max[[as.character(j)]] <- factor(df[[early_var]],levels = c(0,1,2,3,4,5),ordered = TRUE)
      data_before[[as.character(j)]] <- factor(df[[early_var]],levels = c(0,1,2,3,4,5),ordered = TRUE)
      #index those with current time prior to progression
      index<-(df$d_rt+j*365.25/12)<df$d_progression
      data_before[[as.character(j)]][!index] <-0
    }
    data_max[is.na(data_max)] <- 0
    data_before[is.na(data_before)] <- 0
    varname <- paste("LateAll_",k,sep="")
    df[[varname]]<- MaxOrderedFactorsPerRow(data_max)
    varname <- paste("LateBeforeProgres_",k,sep="")
    df[[varname]]<- MaxOrderedFactorsPerRow(data_max)
  }
  #Find variables that start with the name During, Early, or Late and contains at least two underscores and extract the part between the first and the last underscore
  extractedNames<-stringr::str_match(names(df), "^(?:DuringAll|DuringBeforeProgres|EarlyAll|EarlyBeforeProgres|LateAll|LateBeforeProgres)_(.+)_[^_]+$")
  varNames<-names(df)[!is.na(extractedNames[,1])]
  for (i in seq_along(varNames)){
    if(is.factor(df[[varNames[i]]])){
      levellist<-paste(paste(levels(df[[varNames[i]]]),collapse=";"),";",sep="")
      checklevels<-regexpr("^(0;)?(1;)?(2;)?(3;)?(4;)(5;)?$",levellist) #Check that the levels are 0 to 5 and order from 0 to 5
      if (!checklevels){
        warning('Some of the levels of toxicity seem not to be in the range 0 to 5')
      }
      else{
        df[[varNames[i]]]<-ordered(df[[varNames[i]]],levels=c(0,1,2,3,4,5)) #Force all levels in also if they are not present and make ensure that they are ordered
      }
    } else{
      warning('Some of the levels of toxicity are not set as a factor')
    }
  }
  return(df)
}
