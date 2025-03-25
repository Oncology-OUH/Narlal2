#' Write word files containing patient characteristics
#' @description
#' Creates patient characteristics tables divided by treatment arm and saves these as word files. For all plots, three versions are created: one for all patients and two for patients with tumours of histology squamous or non-squamous
#' @param df the output from the the function ExtracPatientCharacteristic
#' @param filepath the filepath to the directory where the patient characteristics shall be stored
#' @param ChangeText a variable used to change the text on plots just before they are plotted. The variable is defined in three part (see example below) that is used to change the text in variable/names, levels, and labels on the tables and plots. Note that ChangeText can also be used to combine different levels by mapping them to a common name (see the example in which sublevels of T1 are mapped to the same name)
#' @param listVars is a list of variables to include in the patient's characteristics. If left empty, it will be the list created for the primary endpoint
#' @param catVars is a list of variables that should be handled as categorical variables within the patient characteristics
#' @param plotTotalColumn is a Boolean to define whether a total column is plotted. Default is TRUE
#' @return Is returning word files with tables of patient characteristics data and cumulative and differential plots of all non-categorical parameters separated in the two treatment groups
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
PlotPatientCharacteristic <- function(df,filepath,ChangeText=c(),listVars=c(),catVars=c(),plotTotalColumn=TRUE){

  if (length(listVars)==0){
    listVars <- c("gender","age", "histology_squamous","Stadium","Perform","FEV1_percent","DLCO_percent","previous_smoker","fx_factor","daysRT","n_nav_rt","n_platin_rt","durvalumab","Vol_T","Vol_N","vol_ptv_total","MeanDose_T","MeanDose_N","MeanLung")
  }
  if (length(catVars)==0){
    catVars <- c("gender","histology_squamous","Stadium","Perform","previous_smoker","fx_factor","n_platin_rt","durvalumab")
  }
  #nonNormalVars <- c("age","FEV1_percent","DLCO_percent","daysRT","MeanDose_T","MeanDose_N","MeanLung","Vol_T","Vol_N","n_nav_rt")
  nonNormalVars <-setdiff(listVars,catVars)

  #DurvalumabLabel <- c('AllDurvalumab','YesDurvalumab','NoDurvalumab')
  DurvalumabLabel <- c('AllDurvalumab')
  HistologyLabel <- c('AllHistology','Squamous','NonSquamous')
  #Create a factor variable based on the number of treatment fractions with the levels <30, 30-32,33, >33
  df$fx_factor<-NA
  df$fx_factor<-factor(df$fx_factor,levels=c('<30','30-32','33','>33'),ordered=TRUE)
  index<-df$fx<30
  df$fx_factor[index]<-'<30'
  index<-df$fx>=30 & df$fx<=32
  df$fx_factor[index]<-'30-32'
  index<-df$fx==33
  df$fx_factor[index]<-'33'
  index<-df$fx>33
  df$fx_factor[index]<-'>33'

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

      table1 <- tableone::CreateTableOne(vars = templistVars, data = tempdf, factorVars = tempcatVars,strata = tempstratavar,includeNA = TRUE,test=FALSE,addOverall=plotTotalColumn)
      tab1_word <- print(table1, quote = F, noSpaces = T,cramVars = tempcatVars,test = T, contDigits = 1, printToggle = F,nonnormal=tempnonNormalVars,
                     dropEqual = T,explain = F)
      #Remove the text median [IQR] from all the lines
      #rownames(tab1_word)<-gsub(' \\(median \\[IQR\\]\\)','',rownames(tab1_word))
      #Remove the text (%) from all the lines
      #rownames(tab1_word)<-gsub(' \\(%\\)','',rownames(tab1_word))
      #Change = to :
      rownames(tab1_word)<-gsub('=',':',rownames(tab1_word))
      tab1_word <- cbind(rownames(tab1_word),tab1_word)
      colnames(tab1_word)[1] <- "Variable"
      tab1_df <- as.data.frame(tab1_word)
      tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
      #tab1_df[1,1]<-'N'
      customtab_defaults_Narlal()
      header <- paste("Patient characteristics and treatment details.",paste(headerlabel,collapse=", "),sep=" ")


      #filename<-gsub(':', '',header)
      #filename<-gsub('\\.', '',filename)
      #filename<-gsub(',', '',filename)
      #filename<-gsub(' ', '_',filename)
      #filename<-paste(filename,'.docx',sep='')
      #filename <- file.path(filepath,filename)
      filename <- file.path(filepath,paste('Patient_characteristics_',DurvalumabLabel[i],'_',HistologyLabel[j],'.docx',sep=''))

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
      p_combined_differential<-list()
      varnames<-setdiff(templistVars,tempcatVars)
      for (k in seq_along(varnames)){
        palette_temp<-c("blue","red")
        #p<-ggplot2::ggplot(tempdf, ggplot2::aes(ggplot2::.data[[varnames[k]]], colour = ggplot2::.data[[tempstratavar]])) + ggplot2::theme_classic()+ggplot2::stat_ecdf()  +ggplot2::scale_colour_manual(values=palette_temp)

        indexNonNa<-!is.na(tempdf[[varnames[k]]])
        p<-ggplot2::ggplot(tempdf[indexNonNa,], ggplot2::aes( !!ggplot2::sym(varnames[k]), colour =  !!ggplot2::sym(tempstratavar))) + ggplot2::theme_classic()+ggplot2::stat_ecdf()  +ggplot2::scale_colour_manual(values=palette_temp)
        p<-p+ ggplot2::theme(legend.position="top",legend.title=ggplot2::element_blank(),axis.title=ggplot2::element_text(size=8,face="bold"))+ ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult=c(0,0.1)))+ ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult=c(0,0.1)))
        #p<-p+ theme(legend.justification = c(1, 0),legend.position = c(1,0))
        p<-ChageLabels_ggplot(p,ChangeText=ChangeText)
        #p_combined[[length(p_combined)+1]]<-p
        p_combined[[k]]<-p
        boundary<-min(tempdf[[varnames[k]]],na.rm=TRUE)
        binwidth<-(max(tempdf[[varnames[k]]],na.rm=TRUE)-boundary)/15
        index<-tempdf[[tempstratavar]]==levels(tempdf[[tempstratavar]])[1]
        #Method 1
        # p_combined_differential[[k]]<-ggplot2::ggplot() +
        #   ggplot2::geom_histogram(ggplot2::aes(x=!!ggplot2::sym(varnames[k]), color = !!ggplot2::sym(tempstratavar),fill=!!ggplot2::sym(tempstratavar)),data=tempdf, size=1,position= "identity",bins=20) +
        #   ggplot2::scale_colour_manual(values=palette_temp)+
        #   #ggplot2::scale_fill_manual(values = ggplot2::alpha(palette_temp, 0.1))+
        #   ggplot2::scale_fill_manual(values = c('#ff000030','#0000ff00'))+
        #   ggplot2::theme_classic()+
        #   ggplot2::theme(legend.position="top",legend.title=ggplot2::element_blank())
        #p_combined_differential[[k]]<-ChageLabels_ggplot(p_combined_differential[[k]],ChangeText=ChangeText)

        #Method 2
        # p_combined_differential[[k]]<-ggplot2::ggplot() +
        #   ggplot2::geom_histogram(ggplot2::aes(x=!!ggplot2::sym(varnames[k]), color = !!ggplot2::sym(tempstratavar),fill=!!ggplot2::sym(tempstratavar)),data=tempdf, binwidth=binwidth,size=.3,position= ggplot2::position_dodge(width=0.65*binwidth)) +
        #   ggplot2::scale_colour_manual(values=c('#ff000000','#0000ff00'))+
        #   #ggplot2::scale_fill_manual(values = ggplot2::alpha(palette_temp, 0.1))+
        #   ggplot2::scale_fill_manual(values = c('#ff0000FF','#0000ffFF'))+
        #   ggplot2::theme_classic()+
        #   ggplot2::theme(legend.position="top",legend.title=ggplot2::element_blank())
        # p_combined_differential[[k]]<-ChageLabels_ggplot(p_combined_differential[[k]],ChangeText=ChangeText)

        #Method 3
        maxval<-max(tempdf[[varnames[k]]][indexNonNa])
        minval<-min(tempdf[[varnames[k]]][indexNonNa])
        ngroups<-10
        breaks<-(seq_len(ngroups+1)-1)/ngroups
        breaks<-breaks*(maxval-minval)+minval
        breaks[1]<-breaks[1]-0.0001*(maxval-minval)
        breaks[length(breaks)]<-breaks[length(breaks)]+0.0001*(maxval-minval)
        binwidth<-breaks[2]-breaks[1]

        boxdata<-data.frame(x=rep(NA,2*(length(breaks)-1)),y=rep(NA,2*(length(breaks)-1)),col=rep(NA,2*(length(breaks)-1)))
        boxoffset<-0.35*binwidth
        for (m in 1:(length(breaks)-1)){
          for (n in seq_along(levels(tempdf[[tempstratavar]]))){
            boxdata$col[m+(n-1)*(length(breaks)-1)]<-levels(tempdf[[tempstratavar]])[n]
            boxdata$x[m+(n-1)*(length(breaks)-1)]<-mean(c(breaks[m+1],breaks[m]))-(2*n-3)*boxoffset/2
            index<-tempdf[[tempstratavar]]==levels(tempdf[[tempstratavar]])[n]
            boxdata$y[m+(n-1)*(length(breaks)-1)]<-sum(tempdf[[varnames[k]]][index]<breaks[m+1] & tempdf[[varnames[k]]][index]>=breaks[m],na.rm=TRUE)
          }
        }
        boxdata$col<-factor(boxdata$col,levels = levels(tempdf[[tempstratavar]]))
        x<-y<-NULL #Hack to prevent check error related to ggplot
        p_combined_differential[[k]]<-ggplot2::ggplot(data=boxdata, ggplot2::aes(x=x, y=y, fill = col)) +
          ggplot2::geom_bar(width =0.3*binwidth,stat = "identity") +
          #ggplot2::scale_x_continuous() +
          ggplot2::scale_fill_manual(values = c('#0000ffFF','#ff0000FF'))+
          ggplot2::theme_classic()+
          ggplot2::labs(y= "Number of observations", x = varnames[k]) +
          ggplot2::theme(legend.position="top",legend.title=ggplot2::element_blank(),axis.title=ggplot2::element_text(size=8,face="bold"))+
          ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult=c(0,0.1)))+ ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult=c(0,0.1)))

        #filename<-paste('PatientCharacteristicsPlot',paste(headerlabel,collapse=", "),'Variable',varnames[k],sep='_')
        #filename<-gsub(':', '',filename)
        #filename<-gsub('\\.', '',filename)
        #filename<-gsub(',', '',filename)
        #filename<-gsub(' ', '_',filename)
        #filename<-paste(filename,'.png',sep='')
        #filename <- file.path(filepath,filename)
        #ggplot2::ggsave(filename,plot=p,device = ragg::agg_png,bg ="white",width=6, height=6, units="cm", res =300, scaling=.5)

      }
      nCol <- floor(sqrt(length(p_combined)))
      p_combined<-cowplot::plot_grid(plotlist=p_combined,ncol=nCol)
      p_combined_differential<-cowplot::plot_grid(plotlist=p_combined_differential,ncol=nCol)
      p_combined<-cowplot::plot_grid(p_combined,p_combined_differential,ncol=2)
      #filename<-paste('PatientCharacteristicsPlot',paste(headerlabel,collapse=", "),'Variables_combined',sep='_')
      #filename<-gsub(':', '',filename)
      #filename<-gsub('\\.', '',filename)
      #filename<-gsub(',', '',filename)
      #filename<-gsub(' ', '_',filename)
      #filename<-paste(filename,'.pdf',sep='')
      #filename <- file.path(filepath,filename)
      filename <- file.path(filepath,paste('Patient_characteristics_plots',DurvalumabLabel[i],'_',HistologyLabel[j],'.pdf',sep=''))
      #ggplot2::ggsave(filename,plot=p_combined,device = ragg::agg_png,bg ="white",width=20, height=24, units="cm", dpi =1200, scaling=.3)
      grDevices::pdf(width=20,height=24,file=filename)
      print(p_combined)
      grDevices::dev.off()

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
