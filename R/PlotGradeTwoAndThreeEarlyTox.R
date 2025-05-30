#' Plot grade two and three early toxicity
#'
#' @param rawdata The input data frame containing toxicity data. The data is the
#'   output from the function ExtractToxicityData
#' @param outputDirectory is a path to where the plots are saved
#' @param ChangeText a variable used to change the text on plots just before they are plotted (see example below).
#' @param plotVariables a variable that defines which toxicities should be included in the output.
#'
#' @return The output is the saved plots, and no R-data is returned
#' @export PlotGradeTwoAndThreeEarlyTox
#' @details The function produces the time-dependent toxicity graphs for the early toxicity paper of the Narlal2 trial.
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df)
#' ChangeText <-c(
#' lungOrganGroup='Lunge group',
#' gastroOrganGroup='Gastro group',
#' dysphagia='Esophagitis')
#' plotVariables<-c('lungOrganGroup','gastroOrganGroup','dysphagia','pneumonitis')
#' PlotGradeTwoAndThreeEarlyTox(PtTox,outputDirectory='c:/home/cab/temp')
PlotGradeTwoAndThreeEarlyTox<-function(rawdata,outputDirectory='c:/home/cab/temp',ChangeText=c(),plotVariables=c()){
  weekNumbers<-c(1,2,3,4,5,6,7)
  monthNumbers <- c(3,6)

  index<-grepl('_fu_mdr_',names(rawdata)) | grepl('_rt_uge_',names(rawdata))
  toxVariables <- names(rawdata)[index]
  toxVariables <- gsub("_rt_uge_\\d+.*", "", toxVariables)
  toxVariables <- gsub("_fu_mdr_\\d+.*", "", toxVariables)
  toxVariables <- unique(toxVariables)
  plot<-list()
  plotCounter<-0
  if (length(plotVariables)==0){
    plotVariables<-toxVariables
  }



  for (iTox in seq_along(plotVariables)){
    varName <- plotVariables[iTox]
    if (! varName %in% c('sens_neuropati','ototox')){
      plotCounter<-plotCounter+1

      dftemp<-data.frame()
      indexEscalated<-rawdata$arm=='Eskaleret'
      counter<-0

      for (iWeek in seq_along(weekNumbers)){
        counter<-counter+1
        varNameTime<-paste(varName,'_rt_uge_',as.character(weekNumbers[iWeek]),sep='')
        dftemp[counter,'toxType'] <-varName
        dftemp[counter,'timeWeeks'] <- weekNumbers[iWeek]
        dftemp[counter,'n_Standard']<-sum(!is.na(rawdata[!indexEscalated,varNameTime]))
        dftemp[counter,'n_Escalated']<-sum(!is.na(rawdata[indexEscalated,varNameTime]))
        index<-rawdata[!indexEscalated,varNameTime]>=2
        dftemp[counter,'G2plus_Standard']<-sum(index,na.rm=TRUE)
        index<-rawdata[indexEscalated,varNameTime]>=2
        dftemp[counter,'G2plus_Escalated']<-sum(index,na.rm=TRUE)
        index<-rawdata[!indexEscalated,varNameTime]>=3
        dftemp[counter,'G3plus_Standard']<-sum(index,na.rm=TRUE)
        index<-rawdata[indexEscalated,varNameTime]>=3
        dftemp[counter,'G3plus_Escalated']<-sum(index,na.rm=TRUE)
      }
      for (iMonth in seq_along(monthNumbers)){

        varNameTime<-paste(varName,'_fu_mdr_',as.character(monthNumbers[iMonth]),sep='')
        if (varNameTime %in% names(rawdata)){


          counter<-counter+1
          dftemp[counter,'toxType'] <-varName
          dftemp[counter,'timeWeeks'] <- monthNumbers[iMonth]*((365.25/7)/12)
          dftemp[counter,'n_Standard']<-sum(!is.na(rawdata[!indexEscalated,varNameTime]))
          dftemp[counter,'n_Escalated']<-sum(!is.na(rawdata[indexEscalated,varNameTime]))
          index<-rawdata[!indexEscalated,varNameTime]>=2
          dftemp[counter,'G2plus_Standard']<-sum(index,na.rm=TRUE)
          index<-rawdata[indexEscalated,varNameTime]>=2
          dftemp[counter,'G2plus_Escalated']<-sum(index,na.rm=TRUE)
          index<-rawdata[!indexEscalated,varNameTime]>=3
          dftemp[counter,'G3plus_Standard']<-sum(index,na.rm=TRUE)
          index<-rawdata[indexEscalated,varNameTime]>=3
          dftemp[counter,'G3plus_Escalated']<-sum(index,na.rm=TRUE)
        }
      }
      plot[[plotCounter]]<-makeGplot(dftemp,ChangeText)
    }
  }
  plotPerPage<-5
  while( length(plot) %% 5 !=0){
    #Add empty plot such that the total number is dividable by 5
    plot[[length(plot)+1]]<-ggplot2::ggplot() + ggplot2::theme_void()
  }

  startIndex<-1
  counter<-0
  while (startIndex<length(plot)) {
    counter<-counter+1
    endIndex<-startIndex+plotPerPage-1
    if (endIndex>length(plot)){
      endIndex<-length(plot)
    }
    plotForPage<-cowplot::plot_grid(plotlist=plot[startIndex:endIndex], ncol=1)
    filename <- file.path(outputDirectory,paste('ToxPlotNr_',as.character(counter),'.pdf',sep=''))
    #ggplot2::ggsave(filename,plot=plotForPage,device = ragg::agg_png,bg ="white",width=21, height=29, units="cm", res =300, scaling=1)
    Cairo::CairoPDF(width=21,height=29,file=filename)
    print(plotForPage)
    grDevices::dev.off()
    startIndex<-endIndex+1
  }

}
makeGplot <- function(df,ChangeText){
  resultsG2Standard <- lapply(seq_along(df$n_Standard), function(i) {if (df$n_Standard[i]>0) stats::binom.test(df$G2plus_Standard[i], df$n_Standard[i]) else NA})
  resultsG2Escalated <- lapply(seq_along(df$n_Escalated), function(i) {if (df$n_Escalated[i]>0) stats::binom.test(df$G2plus_Escalated[i], df$n_Escalated[i]) else NA})
  resultsG3Standard <- lapply(seq_along(df$n_Standard), function(i) {if (df$n_Standard[i]>0) stats::binom.test(df$G3plus_Standard[i], df$n_Standard[i]) else NA})
  resultsG3Escalated <- lapply(seq_along(df$n_Escalated), function(i) {if (df$n_Escalated[i]>0) stats::binom.test(df$G3plus_Escalated[i], df$n_Escalated[i]) else NA})

  plotData <- data.frame(time = df$timeWeeks)

  plotData$G2Standard_est<-sapply(resultsG2Standard,function(x) if(length(x)>1) x[["estimate"]] else 0)
  plotData$G2Standard_lower<-sapply(resultsG2Standard,function(x) if(length(x)>1) x[["conf.int"]][1] else 0)
  plotData$G2Standard_upper<-sapply(resultsG2Standard,function(x) if(length(x)>1) x[["conf.int"]][2] else 0)

  plotData$G2Escalated_est<-sapply(resultsG2Escalated,function(x) if(length(x)>1) x[["estimate"]] else 0)
  plotData$G2Escalated_lower<-sapply(resultsG2Escalated,function(x) if(length(x)>1) x[["conf.int"]][1] else 0)
  plotData$G2Escalated_upper<-sapply(resultsG2Escalated,function(x) if(length(x)>1) x[["conf.int"]][2] else 0)

  plotData$G3Standard_est<-sapply(resultsG3Standard,function(x) if(length(x)>1) x[["estimate"]] else 0)
  plotData$G3Standard_lower<-sapply(resultsG3Standard,function(x) if(length(x)>1) x[["conf.int"]][1] else 0)
  plotData$G3Standard_upper<-sapply(resultsG3Standard,function(x) if(length(x)>1) x[["conf.int"]][2] else 0)

  plotData$G3Escalated_est<-sapply(resultsG3Escalated,function(x) if(length(x)>1) x[["estimate"]] else 0)
  plotData$G3Escalated_lower<-sapply(resultsG3Escalated,function(x) if(length(x)>1) x[["conf.int"]][1] else 0)
  plotData$G3Escalated_upper<-sapply(resultsG3Escalated,function(x) if(length(x)>1) x[["conf.int"]][2] else 0)
  plotData$time_offset<-plotData$time+.3
  plot<-list()
  #The following lines are a hack in order to avoid notes then checking the
  #library. It is related to ggplot.
  time<-time_offset<-NULL
  G2Standard_est<-G2Standard_lower<-G2Standard_upper<-NULL
  G3Standard_est<-G3Standard_lower<-G3Standard_upper<-NULL
  G2Escalated_est<-G2Escalated_lower<-G2Escalated_upper<-NULL
  G3Escalated_est<-G3Escalated_lower<-G3Escalated_upper<-NULL

  plotVarName<-df$toxType[1]
  if (length(ChangeText)>0){
    if (plotVarName %in% names(ChangeText)){
      plotVarName<-ChangeText[[plotVarName]]
    }
  }
  yScaleVar<-max(c(plotData$G2Standard_upper,plotData$G2Escalated_upper))+.01
  plot[[1]]<-ggplot2::ggplot(plotData) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = G2Standard_est,color="Standard"), show.legend = TRUE,size = 1) +
    ggplot2::geom_line(ggplot2::aes(x = time_offset, y = G2Escalated_est,color="Escalated"), show.legend = TRUE,size = 1)+
    ggplot2::geom_errorbar(ggplot2::aes(x = time, ymin = G2Standard_lower, ymax = G2Standard_upper), width = 0.2, color = 'blue',size=1)+
    ggplot2::geom_errorbar(ggplot2::aes(x = time_offset, ymin = G2Escalated_lower, ymax = G2Escalated_upper), width = 0.2, color = 'red',size=1)+
    ggplot2::labs(x = "Time [weeks]", y = "Fraction", color = "Curve") +
    ggplot2::scale_color_manual(values = c("Standard" = "blue", "Escalated"= "red"),name = "")+
    #ggplot2::ggtitle(paste(plotVarName,' grade \u2265','2',sep='')) +
    ggplot2::ggtitle(bquote(.(plotVarName) ~ "grade" ~ "\u2265" ~ 2)) +
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "top")+
    ggplot2::scale_x_continuous( limits = c(0,30),breaks=seq(0,30,5),minor_breaks=seq(0,30,1))+
    ggplot2::scale_y_continuous(limits = c(0, yScaleVar))+
    ggplot2::guides(x = ggprism::guide_prism_minor())+
    ggplot2::theme(axis.text = ggplot2::element_text(size = 24),  # Scale axis tick labels
                   axis.title = ggplot2::element_text(size = 24), # Scale axis titles
                   legend.text = ggplot2::element_text(size = 24), # Scale legend text
                   legend.title = ggplot2::element_text(size = 24), # Scale legend title)
                   plot.title = ggplot2::element_text(size = 24)) #Scale title of plot


  yScaleVar<-max(c(plotData$G3Standard_upper,plotData$G3Escalated_upper))+.01
  plot[[2]]<-ggplot2::ggplot(plotData) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = G3Standard_est,color="Standard"), show.legend = TRUE,size=1) +
    ggplot2::geom_line(ggplot2::aes(x = time_offset, y = G3Escalated_est,color="Escalated"), show.legend = TRUE,size=1)+
    ggplot2::geom_errorbar(ggplot2::aes(x = time, ymin = G3Standard_lower, ymax = G3Standard_upper), width = 0.2, color = 'blue',size=1)+
    ggplot2::geom_errorbar(ggplot2::aes(x = time_offset, ymin = G3Escalated_lower, ymax = G3Escalated_upper), width = 0.2, color = 'red',size=1)+
    ggplot2::labs(x = "Time [weeks]", y = "Fraction", color = "Curve") +
    ggplot2::scale_color_manual(values = c("Standard" = "blue", "Escalated"= "red"),name = "")+
    #ggplot2::ggtitle(paste(plotVarName,' grade \u2265','3',sep='') ) +
    ggplot2::ggtitle(bquote(.(plotVarName) ~ "grade" ~ "\u2265" ~ 3) ) +
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_x_continuous( limits = c(0,30),breaks=seq(0,30,5),minor_breaks=seq(0,30,1))+
    ggplot2::scale_y_continuous(limits = c(0, yScaleVar))+
    ggplot2::guides(x = ggprism::guide_prism_minor())+
    ggplot2::theme(axis.text = ggplot2::element_text(size = 24),  # Scale axis tick labels
                   axis.title = ggplot2::element_text(size = 24), # Scale axis titles
                   legend.text = ggplot2::element_text(size = 24), # Scale legend text
                   legend.title = ggplot2::element_text(size = 24),# Scale legend title
                   plot.title = ggplot2::element_text(size = 24)) #Scale title of plot


  return(cowplot::plot_grid(plotlist=plot, ncol=2))
}
#file <- 'C:/home/cab/Lunge protokoler/NarlalII/DataCuration/OP106NARLAL2_DATA_2024-03-05_2004.csv'
#df <- LoadAndPrepareData(filename=file)
#df <- SelectFirstNPatients(df,350)
#PtTox <- ExtractToxicityData(df)
#Move actual treatment arm to the randomised treatment arm to calculate the result as the patients were treated
#PtTox$arm<-PtTox$arm_main_treated
#set.seed(42)
#PlotGradeTwoAndThreeEarlyTox(PtTox)
#save(temp,file='C:/home/cab/temp/temp.Rdata')
