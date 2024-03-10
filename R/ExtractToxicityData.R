#' Extract toxicity data
#' @description 'ExtracToxicityData' extract the patient toxicity from the
#'   initial raw data obtained using LoadAndPrepareData
#' @param rawdata the data.frame obtained from LoadAndPrepareData
#' @param SAERelationToInclude Describe which SAEs to include in the toxicity
#'   data. The SAE are classified by their relation to the radiotherapy in the
#'   groups No, Yes, and  Potentially. The returned toxicity data will only
#'   include SAEs with RT relations as those specified in this variable. The
#'   default is to return all SAE toxicity information.
#' @param variablesToImpute is a list of toxicity varibales to impute.
#'   Imputation is performed per toxicity. The imputation will be performed if
#'   all toxicity values (per patient/toxicity) are grade 0 or 1. In that case,
#'   all data points that are in time before the last entered values will be
#'   imputed with the value grade 0. This imputation is made since some of those
#'   entering data in the database might have thought that grade zero and no
#'   value (NA) are the same within the database. Thus, to correct this "bias",
#'   the described imputation will be performed in the current code. The
#'   default list of toxicity variables that will be imputed is those scored
#'   prospectively during the study.
#' @param removeToxAfterLocalFailure will, if set to TRUE, set all toxicity
#'   scores after local failure to NA. The default is to keep all toxicity.
#' @param removeToxAfterDistantFailure will, if set to TRUE, set all toxicity
#'   scores after distant failure to NA. The default is to keep all toxicity.
#'
#' @return data.frame that contain toxicity data
#' @export ExtractToxicityData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df,SAERelationToInclude=c('No','Yes','Potentially'))
ExtractToxicityData <- function(rawdata,SAERelationToInclude=c('No','Yes','Potentially'),
                                variablesToImpute=c('fatigue','cough','dyspnoe','constipation','nausea','vomiting','dysphagia','pain','sens_neuropati','infection','diaria','skinreaction','other_tox'),
                                removeToxAfterLocalFailure=FALSE,removeToxAfterDistantFailure=FALSE){


  tox_variable_rt <- c('ps_rt','fatigue_rt','cough_rt','dyspnoe_rt',
                       'constipation_rt','nausea_rt','vomiting_rt',
                       'dysphagia_rt','pain_rt','sens_neuropati_rt',
                       'infection_rt','diaria_rt','skinreaction_rt','ototox_rt',
                       'other_tox1_rt','other_tox2_rt','other_tox3_rt')
  tox_variable_fu <- c('ps_fu','fatigue_fu','cough_fu','dyspnoe_fu','diaria_fu',
                       'nausea_fu','vomiting_fu','dysphagia_fu','pain_fu',
                       'sens_neuropati_fu','infection_fu','other_tox1_fu',
                       'other_tox2_fu','other_tox3_fu')


  week_number <- seq_len(8)
  month_number <- c(3,6,9,12,15,18,21,24,30,36,42,48,54,60,72,84,96,108,120)


  #Adding some standard variables with value NA to the tox data.frame (df)

  patient_id <- unique((rawdata$patient_id))
  index <- !is.na(patient_id)
  patient_id <- patient_id[index]
  df <-data.frame(patient_id)
  df$arm <- NA
  df$arm <- factor(df$arm,levels = levels(rawdata$randomisering.factor))
  df$fx_standard <- NA
  df$fx_escalated <- NA
  df$arm_main_treated <- NA
  df$arm_main_treated <- factor(df$arm_main_treated,levels = levels(rawdata$randomisering.factor))
  df$durvalumab <- NA
  df$durvalumab <- factor(df$durvalumab,levels = levels(rawdata$durvalumab.factor))
  df$date_durvalumab <- as.Date(NA)
  df$histology_squamous <- NA
  df$histology_squamous <- factor(df$durvalumab,levels = levels(rawdata$histology_squamous.factor))
  df$d_rt <- as.Date(NA)
  df$d_rtsl <- as.Date(NA)
  df$d_registrering <- as.Date(NA)
  df$date_of_randomization <- as.Date(NA)
  df$d_pd <- as.Date(NA)
  df$d_pd_cns <- as.Date(NA)
  df$d_pd_hep <- as.Date(NA)
  df$d_pd_bon <- as.Date(NA)
  df$d_pd_skin <- as.Date(NA)
  df$d_pd_and <- as.Date(NA)

  #Add the tox varibales with value NA to df

  for (j in week_number){
    redcap_name <- paste("uge_",as.character(j),"_arm_1",sep="")
    for (k in tox_variable_rt){
      varname_name <- paste(k,"_uge_",as.character(j),sep="")
      df[[varname_name]] <- NA
      df[[varname_name]] <- factor(df[[varname_name]],levels = levels(rawdata[[paste(k,'.factor',sep='')]]),ordered = TRUE)
    }
  }

  for (j in month_number){
    redcap_name <- paste(as.character(j),"_mdr_followup_arm_1",sep="")
    for (k in tox_variable_fu){
      varname_name <- paste(k,"_mdr_",as.character(j),sep="")
      df[[varname_name]] <- NA
      df[[varname_name]] <- factor(df[[varname_name]],levels = levels(rawdata[[paste(k,'.factor',sep='')]]),ordered = TRUE)
    }
  }
  #Add the values of all the toxicities
  index_registration <- rawdata$redcap_event_name == 'registration_arm_1'
  index_haendelse <- rawdata$redcap_event_name == 'haendelser_arm_1'
  index_followup1 <- rawdata$redcap_event_name == '1_followup_arm_1'
  for (i in seq_len(nrow(df))){
    index <- rawdata$patient_id==df$patient_id[i] #index is used to identify the specifc patient in the rawdata
    #ptdata <- rawdata[index,]
    #index_registration <- ptdata$redcap_event_name == 'registration_arm_1'
    #index_haendelse <- ptdata$redcap_event_name == 'haendelser_arm_1'
    #index_followup1 <- ptdata$redcap_event_name == '1_followup_arm_1'
    for (j in week_number){
      redcap_name <- paste("uge_",as.character(j),"_arm_1",sep="")
      redcap_index <- rawdata$redcap_event_name == redcap_name
      for (k in tox_variable_rt){
        varname_name <- paste(k,"_uge_",as.character(j),sep="")
        indextemp <- redcap_index & index #index of specific patient and variable line in rawdata
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
    #Add variable values for some of the other variables in the returned data.frame (df)
    df$arm[[i]] <- if (sum(index_registration & index)>0) rawdata$randomisering.factor[index_registration & index] else NA

    df$fx_standard[[i]] <- if (sum(index_followup1 & index)>0) rawdata$fx_standard[index_followup1 & index] else NA
    df$fx_escalated[[i]] <- if (sum(index_followup1 & index)>0) rawdata$fx_escalated[index_followup1 & index] else NA

    df$durvalumab[[i]] <- if (sum(index_haendelse & index)>0) rawdata$durvalumab.factor[index_haendelse & index] else NA
    df$date_durvalumab[[i]] <- if (sum(index_haendelse & index)>0) rawdata$date_durvalumab[index_haendelse & index] else NA
    df$histology_squamous[[i]] <- if (sum(index_registration & index)>0) rawdata$histology_squamous.factor[index_registration & index] else NA
    df$d_rt[[i]] <- if (sum(index_followup1 & index)>0) rawdata$d_rt[index_followup1 & index] else NA
    df$d_rtsl[[i]] <- if (sum(index_followup1 & index)>0) rawdata$d_rtsl[index_followup1 & index] else NA
    df$d_registrering[[i]] <- if (sum(index_registration & index)>0) rawdata$d_registrering[index_registration & index] else NA
    df$date_of_randomization[[i]] <- if (sum(index_registration & index)>0) rawdata$date_of_randomization[index_registration & index] else NA
    df$d_pd[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd[index_haendelse & index] else NA
    df$d_pd_cns[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_cns[index_haendelse & index] else NA
    df$d_pd_hep[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_hep[index_haendelse & index] else NA
    df$d_pd_bon[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_bon[index_haendelse & index] else NA
    df$d_pd_skin[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_skin[index_haendelse & index] else NA
    df$d_pd_and[[i]] <- if (sum(index_haendelse & index)>0) rawdata$d_pd_and[index_haendelse & index] else NA
  }

  indexTreatedMainStandard <- df$fx_standard>df$fx_escalated
  df$arm_main_treated[indexTreatedMainStandard] <- 'Standard'
  indexTreatedMainEscalated <- df$fx_standard<=df$fx_escalated
  df$arm_main_treated[indexTreatedMainEscalated] <- 'Eskaleret'

  #Add first progression date (need to only count tox before progression)
  temp <- data.frame(df$d_pd_cns,df$d_pd_hep,df$d_pd_bon,df$d_pd_skin,df$d_pd_and,df$d_pd)
  temp[is.na(temp)] <- Sys.Date()+500*365 #Current time plus 500 years
  df$d_progression <- as.Date(apply(temp,1,min))


  #Add recurrence dates from medical image information
  recurrenceDates<-ExtractImagingRecurrenceDates(rawdata)
  keepVar <- setdiff(names(recurrenceDates),names(df))
  keepVar <- unique(c('patient_id',keepVar))
  df <- merge(df,recurrenceDates[,keepVar], all.x = TRUE, all.y=FALSE,by.x='patient_id',by.y='patient_id')

  #Combine the three other_tox groups into one using the maximum value. This is done since a given tox can be in group 1 on one visit but in group 2 on another visit. Thus, they are combined into one group to avoid overseeing specific toxicity. The SAP states that other_tox are not reported unless some particular toxicity patterns are evident within the other tox group.

  weekCombineVar <- c('other_tox1_rt','other_tox2_rt','other_tox3_rt')
  for (iWeek in seq_along(week_number)){
    var1<-paste(weekCombineVar[1],'_uge_',as.character(week_number[iWeek]),sep='')
    var2<-paste(weekCombineVar[2],'_uge_',as.character(week_number[iWeek]),sep='')
    var3<-paste(weekCombineVar[3],'_uge_',as.character(week_number[iWeek]),sep='')
    data_max <- df[,c(var1,var2,var3)]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("other_tox_rt_uge_",as.character(week_number[iWeek]),sep="")
    df[[varname]]<- maxValue
    df[[var1]] <- NULL
    df[[var2]] <- NULL
    df[[var3]] <- NULL
  }
  monthsCombineVar <- c('other_tox1_fu','other_tox2_fu','other_tox3_fu')

  for (iMonths in seq_along(month_number)){
    var1<-paste(monthsCombineVar[1],'_mdr_',as.character(month_number[iMonths]),sep='')
    var2<-paste(monthsCombineVar[2],'_mdr_',as.character(month_number[iMonths]),sep='')
    var3<-paste(monthsCombineVar[3],'_mdr_',as.character(month_number[iMonths]),sep='')
    data_max <- df[,c(var1,var2,var3)]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("other_tox_fu_mdr_",as.character(month_number[iMonths]),sep="")
    df[[varname]]<- maxValue
    df[[var1]] <- NULL
    df[[var2]] <- NULL
    df[[var3]] <- NULL
  }

  #Start the imputation
  if (length(variablesToImpute)>0){
    pts<-unique(df$patient_id)
    for (iPts in seq_along(pts)){
      indexPt <- df$patient_id == pts[iPts]
      for (iVarImpute in seq_along(variablesToImpute)){
        tempImputeVars <- character(length(week_number)+length(month_number))
        for (iWeekNr in seq_along(week_number)){
          tempImputeVars[iWeekNr] <- paste(variablesToImpute[iVarImpute],'_rt_uge_',as.character(week_number[iWeekNr]),sep='')
        }
        if (!(variablesToImpute[iVarImpute] %in% c('constipation','skinreaction','ototox'))){
          for (iMonthNr in seq_along(month_number)){
            tempImputeVars[iMonthNr+length(week_number)] <- paste(variablesToImpute[iVarImpute],'_fu_mdr_',as.character(month_number[iMonthNr]),sep='')
          }
        }else{
          tempImputeVars <- tempImputeVars[1:length(week_number)]
        }

        dataValues <- df[indexPt,tempImputeVars]
        if (any(!is.na(dataValues))){
          maxValue <- apply(dataValues,1,max,na.rm=TRUE)
          if (maxValue<="1"){
            indexMax<-max(which(!is.na(dataValues)))
            indexImpute<-c(rep(TRUE,indexMax),rep(FALSE,length(tempImputeVars)-indexMax)) & is.na(dataValues)
            if (any(indexImpute)){
              df[indexPt,tempImputeVars[indexImpute]] <-'0'
            }
          }
        }
      }
    }
  }
  #Done with imputation

  #Add pneumonitis tox from retrotox
  pneuminitVarNames<-character(length(week_number)+length(month_number))
  daysSinceRandomization <- numeric(length(week_number)+length(month_number))
  for (iWeekNr in seq_along(week_number)){
    pneuminitVarNames[iWeekNr] <- paste('pneumonitis_rt_uge',as.character(week_number[iWeekNr]),sep='_')
    daysSinceRandomization[iWeekNr] <- week_number[iWeekNr]*7
  }
  for (iMonthNr in seq_along(month_number)){
    pneuminitVarNames[iMonthNr+length(week_number)] <- paste('pneumonitis_fu_mdr',as.character(month_number[iMonthNr]),sep='_')
    daysSinceRandomization[iMonthNr+length(week_number)] <- month_number[iMonthNr]*365.25/12
  }

  #Start initialize pneumonit data

  #Find the minimum date of date mors and clinical_update_date (NA if both are NA)
  redcapToxName <- 'pneumonitis_w_degree.factor'
  dateEndObservation<-apply(df[,c("d_mors","clinical_update_date")],1,function(x) { if (all(is.na(x))) NA else min(x,na.rm=TRUE)})
  observationDays <- data.frame(obsTime=as.numeric(difftime(dateEndObservation,df$date_of_randomization,units='days')))
  indexNoPneumonit <- apply(observationDays,1,function(x){if (is.na(x)) NA else max(which(daysSinceRandomization<x)) })
  #Start by setting all observation to NA
  df[,pneuminitVarNames] <- NA
  levelNames <- levels(rawdata[[redcapToxName]])
  df[,pneuminitVarNames] <- lapply(df[,pneuminitVarNames],factor,levels = levelNames,ordered = TRUE)

  for (iRow in seq_len(nrow(df))){
    #Insert the lowest tox level for all the time point for which the patient have been observed
    if (!is.na(indexNoPneumonit[iRow])){
      df[iRow,pneuminitVarNames[seq_len(indexNoPneumonit[iRow])]] <- levelNames[1] #Assign the lowest level to all variables within the observation time. If there is no SAE, the value is assumed to be the lowest level.
      #df[iRow,toxVarNames] <- lapply(df[iRow,toxVarNames],factor,levels = levelNames,ordered = TRUE)
    }
  }





  #End initialize pneumonit data



  #df[,pneuminitVarNames] <- NA
  #df[,pneuminitVarNames] <- lapply(df[,pneuminitVarNames],factor,levels = levels(rawdata$pneumonitis_w_degree.factor),ordered = TRUE)



  dftempPneumonitis<-as.data.frame(rawdata[rawdata$redcap_event_name=='retro_tox_arm_1',c("patient_id","pneumonitis_w_degree.factor","pneumonitis_w_date")])
  for (iPneumonit in seq_len(nrow(dftempPneumonitis))){
    if (!is.na(dftempPneumonitis[iPneumonit,'pneumonitis_w_date'])){
      indexPt <- df$patient_id %in% dftempPneumonitis[iPneumonit,'patient_id']
      pneumonitTime <- difftime(dftempPneumonitis[iPneumonit,'pneumonitis_w_date'],df[indexPt,"date_of_randomization"],units="days")
      pnumonitIndex<-which.min(abs(daysSinceRandomization-pneumonitTime))
      df[indexPt,pneuminitVarNames[pnumonitIndex]] <- dftempPneumonitis[iPneumonit,'pneumonitis_w_degree.factor']
    } else{
      #Here we need to deal with no pneumonia date which can be correct if the pneumonia score is zero
      if (!is.na(dftempPneumonitis[iPneumonit,'pneumonitis_w_degree.factor'])){
        if (dftempPneumonitis[iPneumonit,'pneumonitis_w_degree.factor']=='0'){
          indexPt <- df$patient_id %in% dftempPneumonitis[iPneumonit,'patient_id']
          df[indexPt,pneuminitVarNames] <- dftempPneumonitis[iPneumonit,'pneumonitis_w_degree.factor']
        }
      }
    }
  }

  #Add the six additional SAE groups
  varType<-c('neutropeni','hemoptysis','heart','emboli','infection','othertox')
  for (jVarType in seq_along(varType)){

    toxVarNames<-character(length(week_number)+length(month_number))
    daysSinceRandomization <- numeric(length(week_number)+length(month_number))
    for (iWeekNr in seq_along(week_number)){
      toxVarNames[iWeekNr] <- paste('SAE_',varType[jVarType],'_rt_uge_',as.character(week_number[iWeekNr]),sep='')
      daysSinceRandomization[iWeekNr] <- week_number[iWeekNr]*7
    }
    for (iMonthNr in seq_along(month_number)){
      toxVarNames[iMonthNr+length(week_number)] <- paste('SAE_',varType[jVarType],'_fu_mdr_',as.character(month_number[iMonthNr]),sep='')
      daysSinceRandomization[iMonthNr+length(week_number)] <- month_number[iMonthNr]*365.25/12
    }
    redcapToxName <- paste(varType[jVarType],'1_degree.factor',sep='')

    #Initialize SAE tox variables. NA after the valid observation time and lowest level before that

    #Find the minimum date of date mors and clinical_update_date (NA if both are NA)
    dateEndObservation<-apply(df[,c("d_mors","clinical_update_date")],1,function(x) { if (all(is.na(x))) NA else min(x,na.rm=TRUE)})
    observationDays <- data.frame(obsTime=as.numeric(difftime(dateEndObservation,df$date_of_randomization,units='days')))
    indexNoSAE <- apply(observationDays,1,function(x){if (is.na(x)) NA else max(which(daysSinceRandomization<x)) })
    #Start by setting all observation to NA
    df[,toxVarNames] <- NA
    levelNames <- levels(rawdata[[redcapToxName]])
    df[,toxVarNames] <- lapply(df[,toxVarNames],factor,levels = levelNames,ordered = TRUE)

    for (iRow in seq_len(nrow(df))){
      #Insert the lowest tox level for all the time point for which the patient have been observed
      if (!is.na(indexNoSAE[iRow])){
        df[iRow,toxVarNames[seq_len(indexNoSAE[iRow])]] <- levelNames[1] #Assign the lowest level to all variables within the observation time. If there is no SAE, the value is assumed to be the lowest level.
        #df[iRow,toxVarNames] <- lapply(df[iRow,toxVarNames],factor,levels = levelNames,ordered = TRUE)
      }
    }
    #End initialize SAE tox-variables

    #There are three independent version of each type of SAE loop over these
    for (jEvent in 1:3){
      redcapToxName <- paste(varType[jVarType],as.character(jEvent),'_degree.factor',sep='')
      redcapToxDate <- paste(varType[jVarType],as.character(jEvent),'_date',sep='')
      redcapToxRelated <- paste(varType[jVarType],as.character(jEvent),'_related.factor',sep='')
      dftempTox<-as.data.frame(rawdata[rawdata$redcap_event_name=='sae_reports_arm_1',c("patient_id",redcapToxName,redcapToxDate,redcapToxRelated)])
      dftempTox[,redcapToxName]<-factor(dftempTox[,redcapToxName],levels=levels(dftempTox[,redcapToxName]),ordered = TRUE)
      for (iTox in seq_len(nrow(dftempTox))){
        includeSAE <- dftempTox[iTox,redcapToxRelated] %in% SAERelationToInclude
        includeSAE[is.na(includeSAE)] <- FALSE
        if (includeSAE){
          if (!is.na(dftempTox[iTox,redcapToxDate])){
            indexPt <- df$patient_id %in% dftempTox[iTox,'patient_id']
            toxTime <- difftime(dftempTox[iTox,redcapToxDate],df[indexPt,"date_of_randomization"],units="days")
            toxIndex<-which.min(abs(daysSinceRandomization-toxTime))
            temp<-c(df[indexPt,toxVarNames[toxIndex]],dftempTox[iTox,redcapToxName])
            temp<-temp[!is.na(temp)]
            if (length(temp)>0){
              df[indexPt,toxVarNames[toxIndex]] <- max(temp)
            }
          }
        }
      }
    }
  }

  #If requested by the input remove tox after tumor failures


  if (removeToxAfterLocalFailure | removeToxAfterDistantFailure){
    indexVariables <- grepl('_fu_mdr_',names(df)) | grepl('_rt_uge_',names(df))
    variableToRemoveTox <- unique(sub("_rt_uge_.*", "",sub("_fu_mdr_.*", "", names(df)[indexVariables])))

    pts<-unique(df$patient_id)
    for (iPts in seq_along(pts)){
      indexPt <- df$patient_id == pts[iPts]
      dateDelteAfter<-Sys.Date()+500*365 #Current time plus 500 years
      if (removeToxAfterLocalFailure){
        dateDelteAfter <- c(dateDelteAfter,df[indexPt,'dateLocoRegional'])
      }
      if (removeToxAfterLocalFailure){
        dateDelteAfter <- c(dateDelteAfter,df[indexPt,'dateDistant'])
      }
      dateDelteAfter <- min(dateDelteAfter,na.rm=TRUE)
      daysDelteAfter <- as.numeric(difftime(dateDelteAfter,df[indexPt,'date_of_randomization'],units='days'))
      for (iVarRemove in seq_along(variableToRemoveTox)){
        tempRemoveVars <- character(length(week_number)+length(month_number))
        daysSinceRandomization <- numeric(length(week_number)+length(month_number))
        for (iWeekNr in seq_along(week_number)){
          tempRemoveVars[iWeekNr] <- paste(variableToRemoveTox[iVarRemove],'_rt_uge_',as.character(week_number[iWeekNr]),sep='')
          daysSinceRandomization <- numeric(length(week_number)+length(month_number))
        }
        if (!(variableToRemoveTox[iVarRemove] %in% c('constipation','skinreaction','ototox'))){
          for (iMonthNr in seq_along(month_number)){
            tempRemoveVars[iMonthNr+length(week_number)] <- paste(variableToRemoveTox[iVarRemove],'_fu_mdr_',as.character(month_number[iMonthNr]),sep='')
            daysSinceRandomization[iMonthNr+length(week_number)] <- month_number[iMonthNr]*365.25/12
          }
        }else{
          tempRemoveVars <- tempRemoveVars[1:length(week_number)]
          daysSinceRandomization <- daysSinceRandomization[1:length(week_number)]
        }
        indexRemove<- daysSinceRandomization >= daysDelteAfter
        df[indexPt,tempRemoveVars[indexRemove]] <- NA
      }
    }
  }

  #Done remove tox

  #Add organ system as used in early tox article
  df<-AddOrganSystemToxicity(df)


  #Add variable with maximum degree during RT

  indexVariables <-  grepl('_rt_uge_',names(df))
  weekToxVar <-unique(sub("_uge_.*", "", names(df)[indexVariables]))
  indexVariables <-  grepl('_fu_mdr_',names(df))
  monthToxVar <-unique(sub("_mdr_.*", "", names(df)[indexVariables]))

  for (k in weekToxVar){
    during_vars<-character(length(week_number))
    for (j in seq_along(week_number)){
      during_vars[j] <- paste(k,"_uge_",as.character(week_number[j]),sep="")
    }
    data_max <- df[,during_vars]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("During_",k,sep="")
    df[[varname]]<- maxValue
  }
  #Add variable with maximum degree during 3 and 6 month followup
  for (k in monthToxVar){
    early_vars<-character(2)
    early_vars[1] <- paste(k,"_mdr_",as.character(3),sep="")
    early_vars[2] <- paste(k,"_mdr_",as.character(6),sep="")

    data_max <- df[,early_vars]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("Early_",k,sep="")
    df[[varname]]<- maxValue
  }

  #Add variable with maximum degree during RT and months 3 and 6
  indexVariables <- grepl('_fu_mdr_',names(df)) | grepl('_rt_uge_',names(df))
  variablesToMaxOver <- unique(sub("_rt_uge_.*", "",sub("_fu_mdr_.*", "", names(df)[indexVariables])))
  for (k in variablesToMaxOver){
    all_vars<-character(length(week_number))
    for (j in seq_along(week_number)){
      all_vars[j] <- paste(k,"_rt_uge_",as.character(week_number[j]),sep="")
    }

    if (!(k %in% c('constipation','skinreaction','ototox'))){
      all_vars <-c(all_vars, paste(k,"_fu_mdr_",as.character(3),sep=""))
      all_vars <-c(all_vars, paste(k,"_fu_mdr_",as.character(6),sep=""))
    }

    data_max <- df[,all_vars]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("DuringAndEarly_",k,'_rtfu',sep="")
    df[[varname]]<- maxValue
  }


  #Add variable with maximum degree after 6 months

  for (k in monthToxVar){
    late_vars<-c()
    for (j in seq_along(month_number)){
      if (month_number[j]!=3 & month_number[j]!=6){
        late_vars <- c(late_vars,paste(k,"_mdr_",as.character(month_number[j]),sep=""))
      }
    }
    data_max <- df[,late_vars]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("Late_",k,sep="")
    df[[varname]]<- maxValue

  }

  #Add variable for max over all time points
  indexVariables <- grepl('_fu_mdr_',names(df)) | grepl('_rt_uge_',names(df))
  variablesToMaxOver <- unique(sub("_rt_uge_.*", "",sub("_fu_mdr_.*", "", names(df)[indexVariables])))
  for (k in variablesToMaxOver){
    all_vars<-character(length(week_number)+length(month_number))
    for (j in seq_along(week_number)){
      all_vars[j] <- paste(k,"_rt_uge_",as.character(week_number[j]),sep="")
    }
    for (j in seq_along(month_number)){
      if (!(k %in% c('constipation','skinreaction','ototox'))){

        all_vars[j+length(week_number)] <- paste(k,'_fu_mdr_',as.character(month_number[j]),sep='')

      }else{
        all_vars <- all_vars[1:length(week_number)]
      }
    }
    data_max <- df[,all_vars]
    indexRowAllNA<- apply(is.na(data_max), 1,all)
    data_max[is.na(data_max)]<-levels(data_max[[names(data_max)[1]]])[1]
    maxValue <- factor(apply(data_max, 1,max),levels=levels(data_max[[names(data_max)[1]]]),ordered=TRUE)
    maxValue[indexRowAllNA] <- NA
    varname <- paste("AllMax_",k,'_rtfu',sep="")
    df[[varname]]<- maxValue

  }

  #Find variables that start with the name During, Early, or Late and contains at least two underscores and extract the part between the first and the last underscore
  # extractedNames<-stringr::str_match(names(df), "^(?:DuringAll|DuringBeforeProgres|EarlyAll|EarlyBeforeProgres|LateAll|LateBeforeProgres)_(.+)_[^_]+$")
  # varNames<-names(df)[!is.na(extractedNames[,1])]
  # for (i in seq_along(varNames)){
  #   if(is.factor(df[[varNames[i]]])){
  #     levellist<-paste(paste(levels(df[[varNames[i]]]),collapse=";"),";",sep="")
  #     checklevels<-regexpr("^(0;)?(1;)?(2;)?(3;)?(4;)(5;)?$",levellist) #Check that the levels are 0 to 5 and order from 0 to 5
  #     if (!checklevels){
  #       warning('Some of the levels of toxicity seem not to be in the range 0 to 5')
  #     }
  #     else{
  #       #df[[varNames[i]]]<-ordered(df[[varNames[i]]],levels=c(0,1,2,3,4,5)) #Force all levels in also if they are not present and make ensure that they are ordered
  #     }
  #   } else{
  #     warning('Some of the levels of toxicity are not set as a factor')
  #   }
  # }



  #Sort df
  df$text1 <- as.numeric(sub("-.*", "", df$patient_id))
  df$text2 <- as.numeric(sub(".*-", "", df$patient_id))
  df$sort_order <- df$text1 * 1000 + df$text2
  df <- df[order(df$sort_order), ]
  df$text1 <- NULL
  df$text2 <- NULL
  df$sort_order <- NULL
  rownames(df)<-NULL

  return(df)
}
AddOrganSystemToxicity<-function(rawdata){
  weekNumbers<-c(1,2,3,4,5,6,7,8)
  monthNumbers <- c(3,6,9,12,15,18,21,24,30,36,42,48,54,60,72,84,96,108,120)
  #Add the variables Lung, gastro and max (Esophageal is identical to dysphagia)
  for (iWeek in seq_along(weekNumbers)){
    #Make lung organ combination
    varName1<-paste('cough_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varName2<-paste('dyspnoe_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varName3<-paste('pneumonitis_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varNameRes<-paste('lungOrganGroup_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    dftemp<-rawdata[,c(varName1,varName2,varName3)]
    index<-apply(dftemp,1,function(x){all(is.na(x))})
    dftemp[index,varName1]<-0
    toxValue<-factor(apply(dftemp,1,function(x){max(x,na.rm=TRUE)}),levels=levels(rawdata$cough_rt_uge_1),ordered=TRUE)
    toxValue[index]<-NA
    rawdata[,varNameRes]<-toxValue

    #Make garsto organ group
    varName1<-paste('constipation_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varName2<-paste('vomiting_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varName3<-paste('diaria_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varName4<-paste('nausea_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    varNameRes<-paste('gastroOrganGroup_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    dftemp<-rawdata[,c(varName1,varName2,varName3,varName4)]
    index<-apply(dftemp,1,function(x){all(is.na(x))})
    dftemp[index,varName1]<-0
    toxValue<-factor(apply(dftemp,1,function(x){max(x,na.rm=TRUE)}),levels=levels(rawdata$cough_rt_uge_1),ordered=TRUE)
    toxValue[index]<-NA
    rawdata[,varNameRes]<-toxValue

    #Make group over all tox values
    allWeekTox<-c('fatigue_rt_uge_','cough_rt_uge_','dyspnoe_rt_uge_','constipation_rt_uge_',
                  'nausea_rt_uge_','vomiting_rt_uge_','dysphagia_rt_uge_','pain_rt_uge_',
                  'infection_rt_uge_','diaria_rt_uge_','skinreaction_rt_uge_',
                  'pneumonitis_rt_uge_','SAE_neutropeni_rt_uge_','SAE_hemoptysis_rt_uge_',
                  'SAE_heart_rt_uge_','SAE_emboli_rt_uge_','SAE_infection_rt_uge_')
    varNames<-paste(allWeekTox,as.character(weekNumbers[iWeek]),sep='')
    varNameRes<-paste('allToxGroup_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
    dftemp<-rawdata[,c(varNames)]
    index<-apply(dftemp,1,function(x){all(is.na(x))})
    dftemp[index,varName1]<-0
    toxValue<-factor(apply(dftemp,1,function(x){max(x,na.rm=TRUE)}),levels=levels(rawdata$cough_rt_uge_1),ordered=TRUE)
    toxValue[index]<-NA
    rawdata[,varNameRes]<-toxValue
  }
  for (iMonth in seq_along(monthNumbers)){
    #Make lung organ combination
    varName1<-paste('cough_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    varName2<-paste('dyspnoe_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    varName3<-paste('pneumonitis_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    varNameRes<-paste('lungOrganGroup_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    dftemp<-rawdata[,c(varName1,varName2,varName3)]
    index<-apply(dftemp,1,function(x){all(is.na(x))})
    dftemp[index,varName1]<-0
    toxValue<-factor(apply(dftemp,1,function(x){max(x,na.rm=TRUE)}),levels=levels(rawdata$cough_rt_uge_1),ordered=TRUE)
    toxValue[index]<-NA
    rawdata[,varNameRes]<-toxValue

    #Make garsto organ group
    varName2<-paste('vomiting_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    varName3<-paste('diaria_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    varName4<-paste('nausea_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    varNameRes<-paste('gastroOrganGroup_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    dftemp<-rawdata[,c(varName2,varName3,varName4)]
    index<-apply(dftemp,1,function(x){all(is.na(x))})
    dftemp[index,varName1]<-0
    toxValue<-factor(apply(dftemp,1,function(x){max(x,na.rm=TRUE)}),levels=levels(rawdata$cough_rt_uge_1),ordered=TRUE)
    toxValue[index]<-NA
    rawdata[,varNameRes]<-toxValue

    #Make group over all tox values
    allWeekTox<-c('fatigue_fu_mdr_','cough_fu_mdr_','dyspnoe_fu_mdr_',
                  'nausea_fu_mdr_','vomiting_fu_mdr_','dysphagia_fu_mdr_','pain_fu_mdr_',
                  'infection_fu_mdr_','diaria_fu_mdr_',
                  'pneumonitis_fu_mdr_','SAE_neutropeni_fu_mdr_','SAE_hemoptysis_fu_mdr_',
                  'SAE_heart_fu_mdr_','SAE_emboli_fu_mdr_','SAE_infection_fu_mdr_')
    varNames<-paste(allWeekTox,as.character(monthNumbers[iMonth]),sep='')
    varNameRes<-paste('allToxGroup_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
    dftemp<-rawdata[,c(varNames)]
    index<-apply(dftemp,1,function(x){all(is.na(x))})
    dftemp[index,varName1]<-0
    toxValue<-factor(apply(dftemp,1,function(x){max(x,na.rm=TRUE)}),levels=levels(rawdata$cough_rt_uge_1),ordered=TRUE)
    toxValue[index]<-NA
    rawdata[,varNameRes]<-toxValue
  }
  return(rawdata)
}
