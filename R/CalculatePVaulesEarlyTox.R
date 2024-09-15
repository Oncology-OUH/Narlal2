#' Calculate P-values related to early toxicity reporting
#'
#' @param rawdata The input data frame containing toxicity data. The data is the
#'   output from the function ExtractToxicityData
#' @param maxMonth The maximum month that will provide data for the after RT
#'   regression (See details - default is 6)
#' @param nBootImputation The number of multiple imputations that are performed
#'   in case baseline toxicity is missing (default is 20)
#' @param randomSeed is a seed value to the random generator used during imputation (default 42)
#'
#' @return P-values related to whether there is a statistically significant
#'   difference in toxicity between the two arms of the study
#' @export CalculatePVaulesEarlyTox
#' @details The function calculates the p-values related to early toxicity
#'   reporting of the Narlal2 trial. Toxicity was scored during all weeks of
#'   radiotherapy and at months 3 and 6 (the toxicity was also scored at later
#'   months, but 3 and 6 months are included in the early toxicity reporting).
#'   The returned p-values are related to either during RT or 3 and 6 months
#'   after randomization. The p-values are adjusted for the baseline value and
#'   whether the patient has received Durvalumab. The main regression is
#'   ordinal::clmm(toxScore~ treatmentArm + baseLineTox + durvalumab +
#'   (1|patient_id)). The returned p-values are the p-values related to the
#'   likelihood ratio test between the described model and the same model
#'   without including treatmentArm (thus testing if treatmentArm add statically
#'   significant information to the model). The regression includes all the
#'   different levels of toxScore (e.g. 0-1,2,3,4,5), and the predictors are the
#'   treatment arm, administration of  Durvalumab, and the initial baseline
#'   values (the value noted at week 1). Each toxicity during RT or months 3 and
#'   6 will provide an observation for the regression. The value of Durvalumab
#'   will only be true if the patient has had Durvalumab and the toxicity is
#'   scored after the initialization of Durvalumab. The regression is made as a
#'   mixed model (the term (1|patient_id)). This term adds a random effect to
#'   the model for each patient. The random effect compensates for some patients
#'   having more missing toxicity scores than others, which otherwise would make
#'   the individual patients contribute differently to the regression. In some
#'   cases, the patient baseline value might be missing. If so, the value will
#'   be imputed by a random sampling of the same value among the other patients
#'   from the same institution (unless the institution has provided less than 20
#'   patients, then the sampling will be performed from all the patients in the
#'   study). If there is a need for baseline imputation, the regression is
#'   performed 20 times (multiple imputation), and the returned p-value will be
#'   the median p-value related to the 20 regressions. Due to quite a low number
#'   of toxicity observations, the regression will quite often result in perfect
#'   separation of the toxicity scores. The perfect separation makes the code
#'   slow since there is a need for many iterations to stabilize the calculation
#'   of the likelihood. Therefore, the example code removes many toxicity types
#'   to avoid taking too much time.
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df)
#' # remove all toxicity except for fatigue to make a quick example code
#' index<-grepl('_fu_mdr_',names(PtTox)) | grepl('_rt_uge_',names(PtTox))
#' index<-index & !(grepl('fatigue_fu_mdr_',names(PtTox)) | grepl('fatigue_rt_uge_',names(PtTox)))
#' PtTox[,index]<-NULL
#' p_values <- CalculatePVaulesEarlyTox(PtTox,maxMonth=6,nBootImputation=1)

CalculatePVaulesEarlyTox<-function(rawdata,maxMonth=6,nBootImputation=20,randomSeed=42){
  set.seed(randomSeed)
  print(nBootImputation)
  weekNumbers<-c(1,2,3,4,5,6,7)
  monthNumbers <- c(3,6,9,12,15,18,21,24,30,36,42,48,54,60,72,84,96,108,120)
  index<-monthNumbers<=maxMonth
  monthNumbers<-monthNumbers[index]

  index<-grepl('_fu_mdr_',names(rawdata)) | grepl('_rt_uge_',names(rawdata))
  toxVariables <- names(rawdata)[index]
  toxVariables <- gsub("_rt_uge_\\d+.*", "", toxVariables)
  toxVariables <- gsub("_fu_mdr_\\d+.*", "", toxVariables)
  toxVariables <- unique(toxVariables)

  #Remove late prospective tox variables e.g. heart_late_pro
  indexLatePro<-grepl('_late_pro',toxVariables)
  toxVariables<-toxVariables[!indexLatePro]


  result<-list()
  #Calculate probabilities during RT

  for (iTox in seq_along(toxVariables)){
    varName <- toxVariables[iTox]
    print(varName)
    #Start by collecting the relevant data

    #For SAE tox, allToxGroup and pneumonitis there should not be made a during RT analysis
    if (!grepl('^SAE_',varName) & varName!='pneumonitis' &varName!='allToxGroup'){


      #Start creating data frame for the probability calculation for during rt
      dftemp <- data.frame(patient_id=NA,toxScore=NA,baseLineTox=NA,treatmentArm=NA,durvalumab=as.logical(NA))
      toxLevels=levels(rawdata[[paste(varName,'_rt_uge_1',sep='')]])
      dftemp$toxScore <- factor(dftemp$toxScore, levels=toxLevels,ordered = TRUE)
      dftemp$baseLineTox <- factor(dftemp$baseLineTox, levels=toxLevels,ordered = TRUE)
      treatmentArmLevels <- levels(rawdata$arm)
      dftemp$treatmentArm <- factor(dftemp$treatmentArm, levels=treatmentArmLevels,ordered = FALSE)

      counter <- 0
      for (jPt in seq_along(rawdata$patient_id)){
        for (iWeek in seq_along(weekNumbers)){
          if (iWeek>1){
            counter <- counter+1
            dftemp[counter,'patient_id'] <- rawdata[jPt,'patient_id']
            dftemp[counter,'toxScore'] <- rawdata[jPt,paste(varName,'_rt_uge_',as.character(weekNumbers[iWeek]),sep='')]
            dftemp[counter,'baseLineTox'] <- rawdata[jPt,paste(varName,'_rt_uge_1',sep='')]
            dftemp[counter,'treatmentArm'] <- rawdata[jPt,'arm']
            dftemp[counter,'durvalumab'] <- FALSE
            index <- rawdata$durvalumab
            if (!is.na(rawdata[jPt,'durvalumab']) & rawdata[jPt,'durvalumab'] =='Yes' & !is.na(rawdata[jPt,'date_durvalumab'])){
              #Patient have had durvalumab now decide whether it is before or after the time of the current tox time
              timeDurvalumab <- difftime(rawdata[jPt,'date_durvalumab'],rawdata[jPt,'date_of_randomization'],units = 'days')
              if (as.numeric(timeDurvalumab) <= weekNumbers[iWeek]*7){
                dftemp[counter,'durvalumab'] <- TRUE
              }
            }

          }
        }
      }
      #Data collected in dftemp now call the fit routine makeMixedFit
      result[[varName]]$DuringRT<-makeMixedFit(dftemp,nBootImputation)
    }
  }

  #Calculate probability for early tox
  for (iTox in seq_along(toxVariables)){
    varName <- toxVariables[iTox]
    print(varName)
    if (paste(varName,'_fu_mdr_3',sep='') %in% names(rawdata)){
      #Start create data frame for the probability calculation for during rt
      dftemp <- data.frame(patient_id=NA,toxScore=NA,baseLineTox=NA,treatmentArm=NA,durvalumab=as.logical(NA))
      toxLevels=levels(rawdata[[paste(varName,'_rt_uge_1',sep='')]])
      dftemp$toxScore <- factor(dftemp$toxScore, levels=toxLevels,ordered = TRUE)
      dftemp$baseLineTox <- factor(dftemp$baseLineTox, levels=toxLevels,ordered = TRUE)
      treatmentArmLevels <- levels(rawdata$arm)
      dftemp$treatmentArm <- factor(dftemp$treatmentArm, levels=treatmentArmLevels,ordered = FALSE)

      counter <- 0
      for (jPt in seq_along(rawdata$patient_id)){
        for (iMonth in seq_along(monthNumbers)){
          counter <- counter+1
          dftemp[counter,'patient_id'] <- rawdata[jPt,'patient_id']
          dftemp[counter,'toxScore'] <- rawdata[jPt,paste(varName,'_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')]
          dftemp[counter,'baseLineTox'] <- rawdata[jPt,paste(varName,'_rt_uge_1',sep='')]
          dftemp[counter,'treatmentArm'] <- rawdata[jPt,'arm']
          dftemp[counter,'durvalumab'] <- FALSE
          index <- rawdata$durvalumab
          if (!is.na(rawdata[jPt,'durvalumab']) & rawdata[jPt,'durvalumab'] =='Yes' & !is.na(rawdata[jPt,'date_durvalumab'])){
            #Patient have had durvalumab now decide whether it is before or after the time of the current tox time
            timeDurvalumab <- difftime(rawdata[jPt,'date_durvalumab'],rawdata[jPt,'date_of_randomization'],units = 'days')
            if (as.numeric(timeDurvalumab) <= monthNumbers[iMonth]*(365.25/12)){
              dftemp[counter,'durvalumab'] <- TRUE
            }
          }
        }
      }
      #If SAE tox, allToxGroup, or pneumonitis also the tox during RT should be included
      if (grepl('^SAE_',varName) | varName=='pneumonitis' | varName=='allToxGroup'){
        for (jPt in seq_along(rawdata$patient_id)){
          for (iWeek in seq_along(weekNumbers)){
            if (iWeek>1){
              counter <- counter+1
              dftemp[counter,'patient_id'] <- rawdata[jPt,'patient_id']
              dftemp[counter,'toxScore'] <- rawdata[jPt,paste(varName,'_rt_uge_',as.character(weekNumbers[iWeek]),sep='')]
              dftemp[counter,'baseLineTox'] <- rawdata[jPt,paste(varName,'_rt_uge_1',sep='')]
              dftemp[counter,'treatmentArm'] <- rawdata[jPt,'arm']
              dftemp[counter,'durvalumab'] <- FALSE
              index <- rawdata$durvalumab
              if (!is.na(rawdata[jPt,'durvalumab']) & rawdata[jPt,'durvalumab'] =='Yes' & !is.na(rawdata[jPt,'date_durvalumab'])){
                #Patient have had durvalumab now decide whether it is before or after the time of the current tox time
                timeDurvalumab <- difftime(rawdata[jPt,'date_durvalumab'],rawdata[jPt,'date_of_randomization'],units = 'days')
                if (as.numeric(timeDurvalumab) <= weekNumbers[iWeek]*7){
                  dftemp[counter,'durvalumab'] <- TRUE
                }
              }
            }
          }
        }
      }
      #Data collected in dftemp now call the fit routine makeMixedFit
      result[[varName]]$Early<-makeMixedFit(dftemp,nBootImputation)
    }
  }
  return(result)
}
makeMixedFit<-function(df,nBootImputation){
  if (!any(is.na(df$baseLineTox))){
    nBootImputation<-1
  }
  df$patient_id<-factor(df$patient_id)
  df<-df[!is.na(df$toxScore),]
  df$toxScore<-droplevels(df$toxScore)

  res<-rep(NA,nBootImputation)
  reschi2<-rep(NA,nBootImputation)
  for (iBoot in seq_len(nBootImputation)){
    dffit<-ImputeBaselineTox(df)
    dffit$baseLineTox<-droplevels(dffit$baseLineTox)
    formString='toxScore~(1|patient_id)'
    formStringBackup<-formString
    dffit$baseLineTox<-factor(dffit$baseLineTox,ordered = TRUE)
    if (length(unique(dffit$baseLineTox))>1){
      formString<-paste(formString,'+baseLineTox',sep='')
    }
    if (length(unique(dffit$durvalumab))>1){
      formString<-paste(formString,'+durvalumab',sep='')
    }
    formFull<-as.formula(paste(formString,'+treatmentArm',sep=''))
    formNoArm<-as.formula(formString)
    #Suppress the warning that the Hessian is numerically singular, resulting
    #from a perfect separation of the toxScore based on some of the predictors.
    #The likelihood values can still be compared, which is the only use in the
    #current setting.
    pkgcond::suppress_warnings(resFull <- ordinal::clmm(formFull,data=dffit,control = ordinal::clmm.control(innerCtrl = c("noWarn"))), "Hessian is numerically singular")
    pkgcond::suppress_warnings(resNoArm <- ordinal::clmm(formNoArm,data=dffit,control = ordinal::clmm.control(innerCtrl = c("noWarn"))), "Hessian is numerically singular")
    #browser()
    reschi2[iBoot]<-2*abs(resFull$logLik-resNoArm$logLik)
    res[iBoot]<-stats::pchisq(2*abs(resFull$logLik-resNoArm$logLik), 1, lower.tail = FALSE)

  }
  #Implementation of the aggregation method by Li K-H, Meng X-L, Raghunathan TE, et al. SIGNIFICANCE LEVELS FROM REPEATED p-VALUES WITH MULTIPLY-IMPUTED DATA. Statistica Sinica. 1991;1:65-92.
  #Not planed to be used in the publication but implemented to check that these p-values will be larger than when using the median p-value
  #browser()

  # if (nBootImputation>1){
  #   #No imputation was needed
  #   d<-reschi2
  #   m<-length(d)
  #   k<-1
  #   r2<-(1+1/m)*var(sqrt(d))
  #   v2<-0.5*(1+1/k)*(m-1)*(1+1/r2)^2
  #   W2<-(1/(1+r2))*((mean(d)/k)-r2*(m+1)/(m-1))
  #   combined_p<-1-pf(W2,k,v2)
  # }else{
  #   combined_p<-res[1]
  # }
  # print(combined_p)
  # if (is.na(combined_p)){
  #   browser()
  # }
  # return(combined_p)

  print(stats::median(res))
  return(stats::median(res))
}
ImputeBaselineTox <- function(df){
  #Find patients that need baseline imputation
  pts <- unique(df[is.na(df$baseLineTox),'patient_id'])
  #Loop over relevant patients
  for (iPt in seq_along(pts)){
    #Find the data from the related institution
    tempInst <- gsub('-.*', "", df$patient_id)
    tempInstPt <- gsub('-.*', "", pts[iPt])
    index <- tempInst == tempInstPt
    #If less than 20 patients from the institution use all patients as source of imputation
    if (length(unique(df$patient_id[index]))<20){
      index[] <-TRUE
    }
    #Select the relevant part of the cohort
    tempdf<-df[index,]
    #Identify the values that should be used as source for imputation
    valuesToImputeFrom <- tempdf[!duplicated(tempdf$patient_id), ]$baseLineTox
    valuesToImputeFrom <- valuesToImputeFrom[!is.na(valuesToImputeFrom)]
    #Select a random value from the source of impute values
    index<-round(stats::runif(1,min=1,max=length(valuesToImputeFrom)))
    imputedValue <- valuesToImputeFrom[index]
    df[df$patient_id==pts[iPt],'baseLineTox'] <- imputedValue
  }
  return(df)
}

