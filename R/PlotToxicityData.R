#' Write word files containing patient toxicity data
#' @description
#' Creates patient toxicity tables divided by treatment arm and saves these as word files. For all plots, three versions are created: one for all patients and two for patients with tumours of histology squamous or non-squamous. Furthermore, eight different versions of the table are made for each of the three histologies. Four different versions are created for the different time windows: During RT, Early, combined During and Early, and late. For each of these four, two versions are created one that includes toxicity until the first recurrence and one that includes all toxicity independent of recurrence status.
#' @param df the output from the the function ExtractToxicityData
#' @param filepath the filepath to the directory where the patient toxicity shall be stored
#' @param ChangeText is a variable used to change the text on plots just before they are plotted. The variable is defined in three part (see example below) that is used to change the text in variable/names, levels, and labels on the tables and plots. Note that ChangeText can also be used to combine different levels by mapping them to a common name (see the example in which toxicity levels 0, 1, and 2 are mapped to the same name)
#' @param VariablesInclInTox is used to select a subset of the available variables for the toxicity table. The variable is divided into four parts (see example below) such that variable selection can be made independently for toxicity during RT, early after RT, a combination of during and early, and late toxicity. For all tables, the values are the maximum score for the patient within the period. During is the toxicity scored during the RT course. Early includes all scheduled evaluations after RT until and including the 6-months follow-up visit. Late is all toxicity score after the 6-month follow-up visit (the 6-month visit is not included in late toxicity). If VariablesInclInTox includes the string  ‘All’, all available toxicity parameters will be included (also if other toxicity names are listed in VariablesInclInTox). Similarly, if VariablesInclInTox is not provided to the function, all toxicity variables will be included in the table.
#'
#' @return Is returning word files with tables of patient toxicity data
#' @export PlotToxicityData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df)
#' ChangeText<-c()
#' ChangeText$ChangeLabels <- c(
#'   y='Cumulative distribution',
#'   `Time [months]`='Time since randomisation [months]'
#' )
#' ChangeText$ChangeVar <-c(
#'   arm='Treatment Arm',
#'   fatigue='Fatigue',
#'   dysphagia='Dysphagia',
#'   ps='Performance status'
#' )
#' ChangeText$ChangeLevels <- c(
#'   Standard='Standard',
#'   Eskaleret='Escalated',
#'   Mand='Male',
#'   Kvinde='Female',
#'   `0`='0-2',
#'   `1`='0-2',
#'   `2`='0-2'
#' )

#' VariablesInclInTox<-c()
#' VariablesInclInTox$During<-c('dysphagia','fatigue','ps')
#' VariablesInclInTox$Early<-c('dysphagia','fatigue')
#' VariablesInclInTox$DuringAndEarly<-c('dysphagia','fatigue')
#' VariablesInclInTox$Late<-c('ps')
#' VariablesInclInTox$AllMax<-c('dysphagia','fatigue','ps')

#' PlotToxicityData(df=PtTox,filepath='c:/home/cab/temp',ChangeText=ChangeText,
#'                     VariablesInclInTox=VariablesInclInTox)
PlotToxicityData <- function(df,filepath,ChangeText=c(),VariablesInclInTox=c()){

  #DurvalumabLabel <- c('AllDurvalumab','YesDurvalumab','NoDurvalumab')
  DurvalumabLabel <- c('AllDurvalumab')
  HistologyLabel <- c('AllHistology','Squamous','NonSquamous')
  TimeSinceRTLabel <-c('During','Early','DuringAndEarly','Late','AllMax')

  # if ("During" %in% names(VariablesInclInTox)){
  #   During<-VariablesInclInTox$During
  # } else{
  #   During<-c('All')
  # }
  # if ("Early" %in% names(VariablesInclInTox)){
  #   Early<-VariablesInclInTox$Early
  # } else{
  #   Early<-c('All')
  # }
  # if ("DuringAndEarly" %in% names(VariablesInclInTox)){
  #   DuringAndEarly<-VariablesInclInTox$DuringAndEarly
  # } else{
  #   DuringAndEarly<-c('All')
  # }
  # if ("Late" %in% names(VariablesInclInTox)){
  #   Late<-VariablesInclInTox$Late
  # } else{
  #   Late<-c('All')
  # }
  index<-grep("^patient_id$|^arm$|^durvalumab$|^histology_squamous$|^During_.*|^Early_.*|^DuringAndEarly_.*|^Late_.*|^AllMax_.*",names(df))
  df<-df[,index]
  #Loop over Durvalumab status
  for (i in seq_along(DurvalumabLabel)){
    indexDurvalumab<-rep(TRUE,nrow(df))
    if (DurvalumabLabel[i]=='YesDurvalumab'){
      indexDurvalumab[df$durvalumab!='Yes'] <- FALSE
    }
    if (DurvalumabLabel[i]=='NoDurvalumab') {
      indexDurvalumab[df$durvalumab!='No'] <- FALSE
    }
    #loop over histology status
    for (j in seq_along(HistologyLabel)){
      indexHistology <- rep(TRUE,nrow(df))
      if (HistologyLabel[j]=='Squamous'){
        indexHistology[df$histology_squamous!='Squamous'] <- FALSE
      }
      if (HistologyLabel[j]=='NonSquamous'){
        indexHistology[df$histology_squamous=='Squamous'] <- FALSE
      }
      #loop over time since RT
      for (k in seq_along(TimeSinceRTLabel)){
        if (TimeSinceRTLabel[k]=='During'){
          #greppattern <- "^DuringAll_(.+)_[^_]+$"
          greppattern <- "^During_.*"
          VarSelection<-VariablesInclInTox$During
        }
        if (TimeSinceRTLabel[k]=='Early'){
          greppattern <- "^Early_.*"
          VarSelection<-VariablesInclInTox$Early
        }
        if (TimeSinceRTLabel[k]=='DuringAndEarly'){
          greppattern <- "^DuringAndEarly_.*"
          VarSelection<-VariablesInclInTox$DuringAndEarly
        }

        if (TimeSinceRTLabel[k]=='Late'){
          greppattern <- "^Late_.*"
          VarSelection<-VariablesInclInTox$Late
        }
        if (TimeSinceRTLabel[k]=='AllMax'){
          greppattern <- "^AllMax_.*"
          VarSelection<-VariablesInclInTox$AllMax
        }
        if (length(VariablesInclInTox)==0){
          VarSelection<-c('AllVar')
        }

        # The grep pattern will select the relevant toxicity based on their time since RT:
        # During, Early, Late, or During and Early. The extracted names will in the first column
        # have the names that match the grep pattern, and in the second column, the toxicity
        # name by removing characters before the first underscore and after the last underscore.
        # The “During and Early” will leave two scores per toxicity (During and Early), which
        # need to be combined using the max function. These scores are identified in a loop below
        # with the variable similarData, and based on that, the help function
        # MaxOrderedFactorsPerRow is called to produce the max value per patient.

        indexSelect <- indexDurvalumab & indexHistology
        dfSelected<-df[indexSelect,]
        indexVar <- grepl(greppattern,names(dfSelected))
        #extractedNames<-stringr::str_match(names(dfSelected), greppattern)
        dftemp<-dfSelected[,indexVar]
        # dftemp<-data.frame(matrix(NA,nrow=nrow(dfSelected),ncol=0))
        # for(m in seq_len(nrow(extractedNames))){
        #   if (!is.na(extractedNames[m,2])){
        #     index<-extractedNames[,2]==extractedNames[m,2]
        #     index[is.na(index)]<-FALSE
        #     similarData<-dfSelected[,index,drop=FALSE]
        #     #similarData[is.na(similarData)]<-0
        #     dftemp[[extractedNames[m,2]]]<-MaxOrderedFactorsPerRow(similarData)
        #
        #     #maxvalue<-rep(NA,nrow(similarData))
        #     #maxvalue<-factor(maxvalue,levels=c(0,1,2,3,4,5),ordered = TRUE)
        #     #for (n in seq_len(nrow(similarData))){
        #     #  if (!all(is.na(similarData[n,]))){
        #     #   temp<-unlist(similarData[n,])
        #     #    temp<-ordered(temp,levels=c(0,1,2,3,4,5))
        #     #    maxvalue[n]<-max(temp,na.rm=TRUE)
        #     #  }
        #     #}
        #     #dftemp[[extractedNames[m,2]]]<-maxvalue
        #     extractedNames[index,]<-NA
        #   }
        # }
        dftemp$arm<-dfSelected$arm
        #extracted_text <- sub("^[^_]+_(.*)_[^_]+$", "\\1", names(dftemp))

        names(dftemp) <- sub("^[^_]+_(.*)_[^_]+$", "\\1", names(dftemp)) #Extract the partt of the name between the first and the last underscore
        listVars <- names(dftemp)
        listVars <- setdiff(listVars,c('arm'))
        if (length(listVars)>0){
          if(!(tolower("AllVar") %in% tolower(VarSelection))){
            listVars<-intersect(listVars,VarSelection)
          }
          catVars <- listVars

          dftemp<-ChageVarAndLevels_dataframe(dftemp,ChangeText=ChangeText)
          catVars <-  ChangeVar_vector(catVars,ChangeText=ChangeText)
          treatarm<-ChangeVar_vector(c("arm"),ChangeText=ChangeText)
          listVars<-ChangeVar_vector(listVars,ChangeText=ChangeText)
          #table1 <- tableone::CreateTableOne(vars = listVars, data = dftemp, factorVars = catVars,strata = treatarm,includeNA = FALSE,test=TRUE,addOverall=TRUE)

          table1 <- tableone::CreateTableOne(vars = listVars, data = dftemp,strata = treatarm,includeNA = TRUE,test=TRUE,addOverall=TRUE)
          tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F, exact = catVars,
                             dropEqual = F,explain = T,showAllLevels = TRUE)
          tab1_word <- cbind(rownames(tab1_word),tab1_word)
          colnames(tab1_word)[1] <- "Variable"
          tab1_df <- as.data.frame(tab1_word)
          tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
          customtab_defaults_Narlal()
          header<-paste('Toxicity: ',TimeSinceRTLabel[k],sep='')
          if (i>1){
            header<-paste(header,'; ', DurvalumabLabel[i],sep='')
          }
          if (j>1){
            header<-paste(header,'; ', HistologyLabel[j],sep='')
          }
          footer <- c()
          flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
          filename <- file.path(filepath,paste('Toxicity_',TimeSinceRTLabel[k],'_',DurvalumabLabel[i],'_',HistologyLabel[j],'.docx',sep=''))
          tryCatch({
            flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))
          },
          error=function(cond){
            message("Error during save of patient toxicity file:")
            message(cond)
            return(NA)
          })
        }else{
          message("For some of the toxicities, no variables were left based on the selected criteria in VariablesInclInTox")
        }
      }
    }
  }
}
