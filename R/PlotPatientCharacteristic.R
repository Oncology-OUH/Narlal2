#' Write word files containing patient characteristics
#' @description
#' Creates patient characteristics tables divided by treatment arm and saves these as word files. For all plots, three versions are created: one for all patients and two for patients with tumours of histology squamous or non-squamous
#' @param df the output from the the function ExtracPatientCharacteristic
#' @param filepath the filepath to the directory where the patient characteristics shall be stored
#' @param ChangeText a variable used to change the text on plots just before they are plotted. The variable is defined in three part (see example below) that is used to change the text in variable/names, levels, and labels on the tables and plots. Note that ChangeText can also be used to combine different levels by mapping them to a common name (see the example in which sublevels of T1 are mapped to the same name)
#' @return Is returning word files with tables of patient characteristics data and cumulative plots of all non-categorical parameters separated in the two treatment groups (the plots are stored both separately but also combined in one figure)
#' @export PlotPatientCharacteristic
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtChar <- ExtractPatientCharacteristic(df)
#' ChangeText<-c()
#' ChangeText$ChangeLabels <- c(
#'   y='Cumulative distribution'
#' )
#' ChangeText$ChangeVar <-c(
#'   arm='Treatment Arm',
#'   age='Age [years]',
#'   gender='Sex'
#' )
#' ChangeText$ChangeLevels <- c(
#'   Standard='Standard',
#'   Eskaleret='Escalated',
#'   T1A='T1',
#'   T1B='T1',
#'   Mand='Male',
#'   Kvinde='Female'
#' )
#' PlotPatientCharacteristic(df=PtChar,filepath='c:/home/cab/temp',ChangeText=ChangeText)
PlotPatientCharacteristic <- function(df,filepath,ChangeText=c()){

  listVars <- c("age", "gender","histology","T","N","Stadium","Perform","n_nav_rt","n_cis_rt","n_carbo_rt","FEV1","FVC","weight","height","MeanDose_T","MeanDose_N","MeanLung","Vol_T","Vol_N")
  catVars <- c("gender","histology","T","N","Stadium","Perform","n_nav_rt","n_cis_rt","n_carbo_rt")
  nonNormalVars <- c("age","FEV1","FVC","weight","height","MeanDose_T","MeanDose_N","MeanLung","Vol_T","Vol_N")


  #DurvalumabLabel <- c('AllDurvalumab','YesDurvalumab','NoDurvalumab')
  DurvalumabLabel <- c('AllDurvalumab')
  HistologyLabel <- c('AllHistology','Squamous','NonSquamous')
  dforg<-df
  for (i in seq_along(DurvalumabLabel)){
    for (j in seq_along(HistologyLabel)){
      df<-dforg
      headerlabel<-c()
      indexDurvalumab<-rep(TRUE,nrow(df))
      if (DurvalumabLabel[i]=='YesDurvalumab'){
        indexDurvalumab[df$durvalumab!='Yes'] <- FALSE
        headerlabel<-c(headerlabel, 'Received durvalumab')
      }
      if (DurvalumabLabel[i]=='NoDurvalumab'){
        indexDurvalumab[df$durvalumab!='No'] <- FALSE
        headerlabel<-c(headerlabel, 'Did not receive durvalumab')
      }

      indexHistology <- rep(TRUE,nrow(df))
      if (HistologyLabel[j]=='Squamous'){
        indexHistology[df$histology_squamous!='Squamous'] <- FALSE
        headerlabel<-c(headerlabel,'Histology: Squamous')
      }
      if (HistologyLabel[j]=='NonSquamous'){
        indexHistology[df$histology_squamous=='Squamous'] <- FALSE
        headerlabel<-c(headerlabel,'Histology: Non-squamous')
      }
      df<-dforg[indexDurvalumab & indexHistology,]

      #Change variable and level names as indicated in ChangeText
      tempdf<-ChageVarAndLevels_dataframe(df,ChangeText)
      templistVars<-ChangeVar_vector(listVars,ChangeText)
      tempcatVars<-ChangeVar_vector(catVars,ChangeText)
      tempstratavar<-ChangeVar_vector(c("arm"),ChangeText)
      tempnonNormalVars<-ChangeVar_vector(nonNormalVars,ChangeText)

      table1 <- tableone::CreateTableOne(vars = templistVars, data = tempdf, factorVars = tempcatVars,strata = tempstratavar,includeNA = FALSE,test=FALSE,addOverall=TRUE)
      tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = tempcatVars,test = T, contDigits = 1, printToggle = F,nonnormal=tempnonNormalVars,
                     dropEqual = F,explain = T)
      tab1_word <- cbind(rownames(tab1_word),tab1_word)
      colnames(tab1_word)[1] <- "Variable"
      tab1_df <- as.data.frame(tab1_word)
      tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
      customtab_defaults_Narlal()
      header <- paste("Patient characteristics.",paste(headerlabel,collapse=", "),sep=" ")
      filename<-gsub(':', '',header)
      filename<-gsub('\\.', '',filename)
      filename<-gsub(',', '',filename)
      filename<-gsub(' ', '_',filename)
      filename<-paste(filename,'.docx',sep='')
      filename <- file.path(filepath,filename)
      footer <- ""
      #flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
      flextable_1 <- custom_tab_Narlal(tab1_df, header)
      tryCatch({
        flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))
      },
      error=function(cond){
        message("Error during save of patient characteristics file:")
        message(cond)
        return(NA)
      })
      #Start plotting cumulative distributions
      p_combined<-list()
      varnames<-setdiff(templistVars,tempcatVars)
      for (k in seq_along(varnames)){
        palette_temp<-c("red","blue")
        #p<-ggplot2::ggplot(tempdf, ggplot2::aes(ggplot2::.data[[varnames[k]]], colour = ggplot2::.data[[tempstratavar]])) + ggplot2::theme_classic()+ggplot2::stat_ecdf()  +ggplot2::scale_colour_manual(values=palette_temp)
        p<-ggplot2::ggplot(tempdf, ggplot2::aes( !!ggplot2::sym(varnames[k]), colour =  !!ggplot2::sym(tempstratavar))) + ggplot2::theme_classic()+ggplot2::stat_ecdf()  +ggplot2::scale_colour_manual(values=palette_temp)
        p<-p+ ggplot2::theme(legend.position="top")+ggplot2::guides(color=ggplot2::guide_legend(nrow=2, byrow=TRUE))
        #p<-p+ theme(legend.justification = c(1, 0),legend.position = c(1,0))
        p<-ChageLabels_ggplot(p,ChangeText=ChangeText)
        p_combined[[length(p_combined)+1]]<-p
        filename<-paste('PatientCharacteristicsPlot',paste(headerlabel,collapse=", "),'Variable',varnames[k],sep='_')
        filename<-gsub(':', '',filename)
        filename<-gsub('\\.', '',filename)
        filename<-gsub(',', '',filename)
        filename<-gsub(' ', '_',filename)
        filename<-paste(filename,'.png',sep='')
        filename <- file.path(filepath,filename)
        ggplot2::ggsave(filename,plot=p,device = ragg::agg_png,bg ="white",width=6, height=6, units="cm", res =300, scaling=.5)

      }
      nCol <- floor(sqrt(length(p_combined)))
      p_combined<-cowplot::plot_grid(plotlist=p_combined,ncol=nCol)
      filename<-paste('PatientCharacteristicsPlot',paste(headerlabel,collapse=", "),'Variables_combined',sep='_')
      filename<-gsub(':', '',filename)
      filename<-gsub('\\.', '',filename)
      filename<-gsub(',', '',filename)
      filename<-gsub(' ', '_',filename)
      filename<-paste(filename,'.png',sep='')
      filename <- file.path(filepath,filename)
      ggplot2::ggsave(filename,plot=p_combined,device = ragg::agg_png,bg ="white",width=12, height=24, units="cm", res =300, scaling=.6)
      #End plotting cumulative distributions
    }
  }
}

#source('./R/LoadAndPrepareData.R')
#source('./R/ExtractPatientCharacteristic.R')
#source('./R/PlotPatientCharacteristic.R')
#library(flextable)
#file <- system.file('extdata','DemoData.csv',package="Narlal2")
#df <- LoadAndPrepareData(filename=file)
#PtChar <- ExtractPatientCharacteristic(df)
#PlotPatientCharacteristic(df=PtChar,filepath='c:/home/cab/temp')
