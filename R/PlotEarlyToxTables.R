#' Plot early toxicity tables
#'
#' @param rawdata The input data frame containing toxicity data. The data is the
#'   output from the function ExtractToxicityData
#' @param maxMonth The maximum month that will contribute data for the after RT
#'   values (See details - default is 6)
#' @param nBootImputation The number of multiple imputations that are performed
#'   when calculating the p-values (default is 20)
#' @param p_values calculated from a previous run (can be helpful to store
#'   p_values since they take quite a long time to calculate; if provided, the
#'   p_values will not be recalculated; default is an empty string which will
#'   trigger p-value calculation)
#' @param p_value_save_path a path to save the calculated p_values. The default
#'   is an empty string that will prevent the p_values from being saved.
#' @param Q is the false discovery rate used to compensate for multiple testing.
#'   The multiple testing procedure is based on Benjamini Y, Hochberg Y.
#'   Controlling the False Discovery Rate: A Practical and Powerful Approach to
#'   Multiple Testing. Journal of the Royal Statistical Society Series B
#'   (Methodological). 1995;57:289-300. (the default value is 0.1)
#' @param plotVariables is a list of variables that should be plotted in the
#'   table. The variables will be plotted in the order they appear in
#'   plotVariables. If not provided, all toxicity variables will be plotted
#' @param ChangeText is a character list of variable names and the names that
#'   should be used in the plot e.g. ChangeText = c(pneumonitis='Pneumonitis',
#'   ps='Performance status')
#' @param OutPutFileName is the filename (including full path) for where the
#'   toxicity tables are stored
#' @return A flextable that can be saved as a Word file
#' @export PlotEarlyToxTables
#' @details This function produces the table values needed to compare the
#'   toxicity of the two treatment arms for the Early toxicity article. Details
#'   of how the presented p_values are calculated can be found in the function
#'   CalculatePVaulesEarlyTox. The function returns a flextable, which then can
#'   be modified or saved directly as a Word file.
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df)
#' index<-grepl('_fu_mdr_',names(PtTox)) | grepl('_rt_uge_',names(PtTox))
#' index<-index & !(grepl('diaria_fu_mdr_',names(PtTox)) |
#'    grepl('diaria_rt_uge_',names(PtTox)))
#' PtTox[,index]<-NULL
#' ft<-PlotEarlyToxTables(PtTox,nBootImputation=1)
#' flextable::save_as_docx(ft, path = 'c:/home/cab/temp/EarlyToxTable.docx',
#'   pr_section =  officer::prop_section(page_size =
#'   officer::page_size(orient = "portrait"), type = "continuous"))

PlotEarlyToxTables <- function(rawdata,maxMonth=6,nBootImputation=20,p_values=NULL,p_value_save_path=NULL,Q=0.1,plotVariables=NULL,ChangeText=c(),OutPutFileName='c:/home/cab/temp/EarlyToxTable.docx'){
  weekNumbers<-c(1,2,3,4,5,6,7)
  monthNumbers <- c(3,6,9,12,15,18,21,24,30,36,42,48,54,60,72,84,96,108,120)
  monthNumbers <- monthNumbers[monthNumbers<=maxMonth]

  #start by removing sens_neuropati and ototox which according to thee SAP
  #should not be reported
  index<-grepl('sens_neuropati_fu_mdr_',names(rawdata)) | grepl('sens_neuropati_rt_uge_',names(rawdata))
  index<-index | grepl('ototox_fu_mdr_',names(rawdata)) | grepl('ototox_rt_uge_',names(rawdata))
  rawdata[,index]<-NULL

  index<-grepl('_fu_mdr_',names(rawdata)) | grepl('_rt_uge_',names(rawdata))
  toxVariables <- names(rawdata)[index]
  toxVariables <- gsub("_rt_uge_\\d+.*", "", toxVariables)
  toxVariables <- gsub("_fu_mdr_\\d+.*", "", toxVariables)
  toxVariables <- unique(toxVariables)
  dftemp<-data.frame(Toxicity=NA,Level=NA,Total1=NA,Standard1=NA,Escalated1=NA,p_val1=NA,Total2=NA,Standard2=NA,Escalated2=NA,p_val2=NA)
  counter<-0
  namesInDuring<-setdiff(toxVariables,c('sens_neuropati','ototox','pneumonitis',
                                        'SAE_neutropeni','SAE_hemoptysis','SAE_heart',
                                        'SAE_emboli','SAE_infection','SAE_othertox','allToxGroup'))
  namesInEarly<-setdiff(toxVariables,c('sens_neuropati','ototox','constipation','skinreaction','pneumonitis',
                                       'SAE_neutropeni','SAE_hemoptysis','SAE_heart',
                                       'SAE_emboli','SAE_infection','SAE_othertox','allToxGroup'))
  namesInCombined<-c('pneumonitis','SAE_neutropeni','SAE_hemoptysis','SAE_heart',
                     'SAE_emboli','SAE_infection','SAE_othertox','allToxGroup')


  if (length(plotVariables)==0){
    plotVariables<-toxVariables
  }

  #Start by combining tox level in accordance with the description in the SAP
  index<-grepl('_rt_uge_',names(rawdata))
  for (month in monthNumbers){
    index<-index | grepl(paste('_fu_mdr_',as.character(month),'$',sep=''),names(rawdata))
  }
  toxVar <- names(rawdata)[index]
  toxType <- gsub("_rt_uge_\\d+.*", "", toxVar)
  toxType <- gsub("_fu_mdr_\\d+.*", "", toxType)
  listRecoding<-data.frame(varName=toxVar,toxType=toxType)

  for (toxType in unique(toxType)){
    varName<-paste('During_',toxType,'_rt',sep='')
    if (varName %in% names(rawdata)){
      listRecoding[nrow(listRecoding)+1,'varName']<-varName
      listRecoding[nrow(listRecoding),'toxType']<-toxType
    }
    varName<-paste('Early_',toxType,'_fu',sep='')
    if (varName %in% names(rawdata)){
      listRecoding[nrow(listRecoding)+1,'varName']<-varName
      listRecoding[nrow(listRecoding),'toxType']<-toxType
    }
    varName<-paste('DuringAndEarly_',toxType,'_rtfu',sep='')
    if (varName %in% names(rawdata)){
      listRecoding[nrow(listRecoding)+1,'varName']<-varName
      listRecoding[nrow(listRecoding),'toxType']<-toxType
    }
  }
  index<-listRecoding$toxType!='ps'
  listRecoding<-listRecoding[index,]
  for (varName in listRecoding$varName){
    rawdata[[varName]][rawdata[[varName]]==1]<-0
    rawdata[[varName]]<-factor(rawdata[[varName]],levels=setdiff(levels(rawdata[[varName]]),'1'),ordered = TRUE)
    levelNames<-levels(rawdata[[varName]])
    levelNames[1]<-"0-1"
    levels(rawdata[[varName]])<-levelNames
    if (grepl('^SAE_',varName) | grepl('_SAE_',varName)){
      rawdata[[varName]][rawdata[[varName]]==2]<-'0-1'
      rawdata[[varName]]<-factor(rawdata[[varName]],levels=setdiff(levels(rawdata[[varName]]),'2'),ordered = TRUE)
      levelNames<-levels(rawdata[[varName]])
      levelNames[1]<-"0-2"
      levels(rawdata[[varName]])<-levelNames
    }
  }

  #End re-coding

  if(length(p_values)==0){
    p_values<-CalculatePVaulesEarlyTox(rawdata,maxMonth=maxMonth,nBootImputation=nBootImputation)
    if (length(p_value_save_path)!=0){
      save(p_values,file=p_value_save_path)
    }
  }

  BHCriticalValue<-BenjaminiHochbergCriticalValue(p_values,Q=Q)
  boldIndexDuringRT<-c()
  boldIndexEarly<-c()

  for (iTox in seq_along(plotVariables)){
    varName<-plotVariables[iTox]
    varNameDuringRT<-paste('During_',varName,'_rt',sep='')
    varNameEarly<-paste('Early_',varName,'_fu',sep='')
    varNameCombined<-paste('DuringAndEarly_',varName,'_rtfu',sep='')
    #If there is no level 4 and 5 tox combine level 4 and 5 for more compact plotting
    if (sum(rawdata[[varNameCombined]]==4 | rawdata[[varNameCombined]]==5,na.rm = TRUE)==0 & (5 %in% levels(rawdata[[varNameCombined]]))){
      for (varTemp in c(varNameDuringRT,varNameEarly,varNameCombined)){
        if (varTemp %in% names(rawdata)){
          rawdata[[varTemp]]<-factor(rawdata[[varTemp]],levels=setdiff(levels(rawdata[[varTemp]]),'5'),ordered = TRUE)
          levelNames<-levels(rawdata[[varTemp]])
          levelNames[length(levelNames)]<-"4-5"
          levels(rawdata[[varTemp]])<-levelNames
        }
      }
    }
    if ((varName %in% namesInDuring) | (varName %in% namesInEarly) | (varName %in% namesInCombined)){

      counter<-counter+1
      #Here add p values
      if ('DuringRT' %in% names(p_values[[varName]])){
        temp<-p_values[[varName]]$DuringRT
        if (temp<=0.05){
          boldIndexDuringRT<-c(boldIndexDuringRT,counter)
        }
        addStar=FALSE
        if (temp<BHCriticalValue){
          addStar=TRUE
        }
        if (temp <.001){
          temp<-'<0.001'
        }else{
          temp<-sprintf("%.3f", round(temp*1000)/1000)
        }
        if (addStar){
          temp<-paste(temp,'*',sep='')
        }
        dftemp[counter,'p_val1']<-temp
      }

      if ('Early' %in% names(p_values[[varName]])){
        temp<-p_values[[varName]]$Early
        if (temp<=0.05){
          boldIndexEarly<-c(boldIndexDuringRT,counter)
        }
        addStar=FALSE
        if (temp<BHCriticalValue){
          addStar=TRUE
        }
        if (temp <.001){
          temp<-'<0.001'
        }else{
          temp<-sprintf("%.3f", round(temp*1000)/1000)
        }
        if (addStar){
          temp<-paste(temp,'*',sep='')
        }
        dftemp[counter,'p_val2']<-temp
      }
      plotVarName<-varName
      if (length(ChangeText)>0){
        if (varName %in% names(ChangeText)){
          plotVarName<-ChangeText[[varName]]
        }
      }
      dftemp[counter,'Toxicity']<-plotVarName
      indexStandard<-rawdata$arm=='Standard'
      if ((varName %in% namesInDuring) | (varName %in% namesInEarly)){
        varLevels<-levels(rawdata[[varNameDuringRT]])
        for (iLevel in seq_along(varLevels)){
          counter<-counter+1
          if (varName %in% namesInDuring){
            dftemp[counter,'Level']<-varLevels[iLevel]
            index <- rawdata[[varNameDuringRT]]==varLevels[iLevel]
            tot<-sum(!is.na(rawdata[[varNameDuringRT]]))
            n<-sum(index,na.rm=TRUE)


            dftemp[counter,'Total1']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
            dftemp[counter,'Total1']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
            tot<-sum(!is.na(rawdata[[varNameDuringRT]][indexStandard]))
            n<-sum(index & indexStandard,na.rm=TRUE)
            dftemp[counter,'Standard1']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
            tot<-sum(!is.na(rawdata[[varNameDuringRT]][!indexStandard]))
            n<-sum(index & !indexStandard,na.rm=TRUE)
            dftemp[counter,'Escalated1']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
          }
          if (varName %in% namesInEarly){
            index <- rawdata[[varNameEarly]]==varLevels[iLevel]
            tot<-sum(!is.na(rawdata[[varNameEarly]]))
            n<-sum(index,na.rm=TRUE)
            dftemp[counter,'Total2']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
            tot<-sum(!is.na(rawdata[[varNameEarly]][indexStandard]))
            n<-sum(index & indexStandard,na.rm=TRUE)
            dftemp[counter,'Standard2']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
            tot<-sum(!is.na(rawdata[[varNameEarly]][!indexStandard]))
            n<-sum(index & !indexStandard,na.rm=TRUE)
            dftemp[counter,'Escalated2']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
          }

        }
      }
      if (varName %in% namesInCombined){
        varLevels<-levels(rawdata[[varNameCombined]])
        for (iLevel in seq_along(varLevels)){
          counter<-counter+1
          dftemp[counter,'Level']<-varLevels[iLevel]
          index <- rawdata[[varNameCombined]]==varLevels[iLevel]
          tot<-sum(!is.na(rawdata[[varNameCombined]]))
          n<-sum(index,na.rm=TRUE)
          dftemp[counter,'Total2']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
          tot<-sum(!is.na(rawdata[[varNameCombined]][indexStandard]))
          n<-sum(index & indexStandard,na.rm=TRUE)
          dftemp[counter,'Standard2']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
          tot<-sum(!is.na(rawdata[[varNameCombined]][!indexStandard]))
          n<-sum(index & !indexStandard,na.rm=TRUE)
          dftemp[counter,'Escalated2']<-paste(as.character(n),' (',sprintf("%.1f",round(1000*n/tot)/10),'%)',sep='')
        }
      }
    }
  }
  ft <- flextable::flextable(dftemp)
  ft <- flextable::add_header_row(ft,colwidths = c(2,4,4),values = c(" ","During RT", "After RT"))
  ft <- flextable::set_header_labels(ft,
                                     Toxicity = "Toxicity",
                                     Level = "Grade",
                                     Total1 = "Total",
                                     Standard1 = "Standard",
                                     Escalated1 = "Escalated",
                                     p_val1 = "p",
                                     Total2 = "Total",
                                     Standard2 = "Standard",
                                     Escalated2 = "Escalated",
                                     p_val2 = "p")
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::fontsize(ft, size = 7, part = "header")
  ft <- flextable::fontsize(ft, size = 7)
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::autofit(ft,part = c("body","header"))
  ft <- flextable::fit_to_width(ft, max_width = 16,unit = 'cm')
  ft <- flextable::height(ft, height=.5, part = "body", unit = "in")
  ft <- flextable::height(ft, i = NULL, height=.6, part = "body", unit = "cm")
  ft <- flextable::hrule(ft, rule = "exact", part = "body")
  ft <- flextable::valign(ft, i = NULL, j = NULL, valign = "center", part = "body")
  if (length(boldIndexDuringRT)>0){
    ft<-flextable::bold(ft, i = boldIndexDuringRT, j=6, bold = TRUE, part = "body")
  }
  if (length(boldIndexEarly)>0){
    ft<-flextable::bold(ft, i = boldIndexEarly, j=10, bold = TRUE, part = "body")
  }
  flextable::save_as_docx(ft, path = OutPutFileName,
                          pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

}
BenjaminiHochbergCriticalValue<-function(p_values,Q=0.1){
  #Implemented using
  ##https://stats.libretexts.org/Bookshelves/Applied_Statistics/Biological_Statistics_(McDonald)/06%3A_Multiple_Tests/6.01%3A_Multiple_Comparisons
  #as the guideline

  #Extract all p values
  p_array<-c()
  for (iTox in seq_along(names(p_values)) ){
    varName<-names(p_values)[iTox]
    if (varName!="other_tox" & varName!="SAE_othertox"){
      if ('DuringRT' %in% names(p_values[[varName]])){
        p_array<-c(p_array,p_values[[varName]]$DuringRT)
      }
      if ('Early' %in% names(p_values[[varName]])){
        p_array<-c(p_array,p_values[[varName]]$Early)
      }
    }
  }
  #Sort p-values and compare with (i/m)Q
  p_array<-sort(p_array)
  p_compare<-(1:length(p_array))/length(p_array)*.1
  index<-which(p_array<p_compare)[1]
  #Set the threshold between the last significant and the first non-significant
  #to avoid comparison issues between doubles when deciding whether a specific
  #value is significant.
  threshold<-0
  if (!is.na(index)){
    if (index<length(p_array)){
      threshold<-(p_array[index]+p_array[index+1])/2
    }else{
      threshold<-1
    }
  }
  return(threshold)
}

#library(tictoc)
#file <- 'C:/home/cab/Lunge protokoler/NarlalII/DataCuration/OP106NARLAL2_DATA_2024-03-05_2004.csv'
#df <- LoadAndPrepareData(filename=file)
#df <- SelectFirstNPatients(df,350)


#PtTox <- ExtractToxicityData(df)
#Move actual treatment arm to the randomized treatment arm to calculate the result as the patients were treated
#PtTox$arm<-PtTox$arm_main_treated

#index<-grepl('_fu_mdr_',names(PtTox)) | grepl('_rt_uge_',names(PtTox))
#index<-index & !(grepl('infection_fu_mdr_',names(PtTox)) | grepl('infection_rt_uge_',names(PtTox)))
#PtTox[,index]<-NULL

#tic()
#ft<-PlotEarlyToxTables(PtTox,nBootImputation=1)
#toc()
#flextable::save_as_docx(ft, path = 'c:/home/cab/temp/EarlyToxTable.docx', pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))
