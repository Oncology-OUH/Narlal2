#' Creates survival plots
#' @description
#' Creates survival plots divided by treatment arm of local control, progression-free survival, and overall survival. Furthermore, a competing risk plot of the first event among local failure, distant failure and death is also created – the competing risk plots are made in two versions “standard”, including confidence intervals and a stacked version. For all plots, three versions are created: one for all patients and two for patients with tumours of histology squamous or non-squamous.
#'
#' @param df the output from the the function ExtractSurvivalData
#' @param filepath the filepath to the directory where the patient survival data shall be stored
#' @param nboot number of bootstraps (nboot=1 will replace the bootstrap values with the standard values)
#' @param conf.int confidence intervals extracted from the bootstrap. Will typical be 0.95
#' @param seed a value to seed the random generator
#' @param ChangeText is a variable used to change the text on plots just before they are plotted. The variable is defined in three part (see example below) that is used to change the text in variable/names, levels, and labels on the plots.
#'
#' @return Is exporting survival figures
#' @export PlotSurvivalData
#' @importFrom survival strata
#' @importFrom stats as.formula pchisq
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtSurvival <- ExtractSurvivalData(df,12*5)
#' ChangeText<-c()
#' ChangeText$ChangeLabels <- c(
#'   `Time [months]`='Time since randomisation [Months]'
#' )
#' ChangeText$ChangeVar <-c(
#'   arm='Treatment Arm'
#' )
#' ChangeText$ChangeLevels <- c(
#'   Standard='Standard',
#'   Eskaleret='Escalated'
#' )
#' PlotSurvivalData(df=PtSurvival,filepath='c:/home/cab/temp',
#'                  nboot=10,conf.int=.95,seed=42,ChangeText=ChangeText)


PlotSurvivalData <- function(df,filepath,nboot=10,conf.int=.95,seed=42,ChangeText=c()){
  df<-ChageVarAndLevels_dataframe(df,ChangeText)
  stratavar<-ChangeVar_vector(c("arm"),ChangeText)
  #DurvalumabLabel <- c('AllDurvalumab','YesDurvalumab','NoDurvalumab')
  DurvalumabLabel <- c('AllDurvalumab')
  HistologyLabel <- c('AllHistology','Squamous','NonSquamous')
  formulatext_surv <- c(paste('survival::Surv(t_localcontrol,event_localcontrol)~get("',stratavar,'")',sep=''),
                   paste('survival::Surv(t_progression,event_progression) ~get("',stratavar,'")',sep=''),
                   paste('survival::Surv(t_os,event_os) ~get("',stratavar,'")',sep=''))
  survival_titles <- c('Local control rate','Progression free survival','Overall survival')
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
        indexHistology[df$histology_squamous!='Non-squamous'] <- FALSE
      }
      dftemp <- df[indexDurvalumab & indexHistology,]

      #Perform all the bootstrap calculations for the given plot both survival and competink risk
      formsurv <- list()
      for (k in seq_along(formulatext_surv)){
        formsurv <- append(formsurv, as.formula(formulatext_surv[[k]]))
      }
      formcox <- list()
      for (k in seq_along(formulatext_surv)){
        formcox <- append(formcox, as.formula(paste(formulatext_surv[[k]], '+ strata(durvalumab)',sep='')))
      }
      #Call function for all the bootstraping
      bootdata <- bootstrap_narlal(dftemp,formcox,formsurv,nboot=nboot,conf.int=conf.int,seed=seed)

      #Start making the plots

      #Section: Plot survival data ####
      splots <- list()
      for (k in seq_along(formulatext_surv)){
        cox_res<-bootdata$cox[[k]]$main
        pvalue <- pchisq(cox_res$score, df=1, lower.tail=FALSE)
        if(pvalue<.001){pvaluelable <- c('p = <0.001')} else {pvaluelable <- paste('p = ',sprintf("%.3f",round(pvalue,digits=3)),sep='')}
        h1lable <- sprintf("%.3f",round(exp(bootdata$cox[[k]]$boot$coefficients),digits=3))
        h2lable <- sprintf("%.3f",round(exp(bootdata$cox[[k]]$boot$coefficients_lower),digits=3))
        h3lable <- sprintf("%.3f",round(exp(bootdata$cox[[k]]$boot$coefficients_upper),digits=3))
        hlable <- paste('HR = ',h1lable,' (',h2lable,'-',h3lable,')',sep='')
        #fit<-survival::survfit(as.formula(formulatext[[k]]),data=dftemp)
        #fit$call$formula <- as.formula(formulatext[[k]])
        fit <- bootdata$surv[[k]]$main

        palette_temp = c("red","blue")

        #Make the primary plot that includes the curves of the two arms without confidence intervals
        temp <- survminer::ggsurvplot(fit,xlab="Time [months]",ylab=survival_titles[k],palette=palette_temp,
                                      risk.table = TRUE,fontsinze=1,break.time.by = 12,legend = c(0.8, 0.8),legend.title=ggplot2::element_blank(),legend.labs = levels(df[[stratavar]]),
                                      risk.table.height = .1,conf.int = FALSE,conf.int.style = "step",pval = FALSE,  data = dftemp)
        #Make confidence intervals for curve one
        time <- lower <- upper <- NULL
        temp_bootdata <- data.frame(time=bootdata$surv[[k]]$boot$strata[[1]]$time,
                               lower=bootdata$surv[[k]]$boot$strata[[1]]$lower_boot,
                               upper=bootdata$surv[[k]]$boot$strata[[1]]$upper_boot)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = lower, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[1],inherit.aes = FALSE,data=temp_bootdata)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = upper, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[1],inherit.aes = FALSE,data=temp_bootdata)

        #Make confidence intervals for curve two
        time <- lower <- upper <- NULL
        temp_bootdata <- data.frame(time=bootdata$surv[[k]]$boot$strata[[2]]$time,
                                    lower=bootdata$surv[[k]]$boot$strata[[2]]$lower_boot,
                                    upper=bootdata$surv[[k]]$boot$strata[[2]]$upper_boot)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = lower, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[2],inherit.aes = FALSE,data=temp_bootdata)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = upper, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[2],inherit.aes = FALSE,data=temp_bootdata)

        #Add the p-value and the hazard value as text labels
        temp$plot <- temp$plot + ggplot2::annotate("text", x = 0, y = 0, label = pvaluelable, cex=4.5, col="black", vjust=0, hjust = 0.0, fontface=2)
        temp$plot <- temp$plot + ggplot2::annotate("text", x = Inf, y = Inf, label = hlable, cex=4.5, col="black", vjust=1, hjust = 1, fontface=2)

        #Update labels on the plot as requested via ChangeText
        temp$plot<-ChageLabels_ggplot(temp$plot,ChangeText=ChangeText)
        temp$table<-ChageLabels_ggplot(temp$table,ChangeText=ChangeText)

        #Store plot in a list for combined printing/storage
        splots[[k]] <- temp
      }
      #Plot the survival plots
      survivalplot <- survminer::arrange_ggsurvplots(splots, print = FALSE,ncol = 3, nrow = 1, risk.table.height = 0.2)
      filename <- file.path(filepath,paste('Survival_',DurvalumabLabel[i],'_',HistologyLabel[j],'.png',sep=''))
      ggplot2::ggsave(filename,plot=survivalplot,device = ragg::agg_png,bg ="white",width=15, height=6.75, units="cm", res =300, scaling=.375)
      #End plotting survival plots

      #Section: Plot competing risk models
      #Start by small hack to silent note messages when cheeking code see e.g. https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

      #Start plot of each endpoint in separate windows with both arms in the same window ####
      est<-arm<-upper_boot<-lower_boot<-NULL
      splots <- list()
      plot_endpoints<-ChangeLevel_vector(c('local','mors','met','local+met'),ChangeText)
      plot_arms<-ChangeLevel_vector(c('Standard','Eskaleret'),ChangeText)
      palette_temp<-c()
      palette_temp[[plot_arms[1]]]="red"
      palette_temp[[plot_arms[2]]]="blue"
      palette_temp<-unlist(palette_temp)
      for (k in seq_along(plot_endpoints)){
        dataCurve<-list()
        p<-list()
        for(karm in 1:2){
          dataCurve[[karm]]<-data.frame(bootdata$comprisk$boot$strata[[paste(plot_arms[karm],plot_endpoints[k],sep=' ')]])
          if (nrow(dataCurve[[karm]])==0){
            dataCurve[[karm]]<-data.frame(time=c(0,1),est=c(0,0),lower_org=c(0,0),upper_org=c(0,0),lower_boot=c(0,0), upper_boot=c(0,0))
          }
          dataCurve[[karm]]$arm<-plot_arms[karm]
          dataCurve[[karm]]$arm<-factor(dataCurve[[karm]]$arm,levels=plot_arms)
        }
        #ggplot2::theme(panel.border=element_blank(),panel.background = element_blank())
        splots[[k]]<-ggplot2::ggplot()+ggplot2::theme_classic()+ ggplot2::xlab('Time [Months]') + ggplot2::ylab('Probability of first event')
        splots[[k]]<-splots[[k]]+ ggplot2::theme(legend.position="top",legend.title=ggplot2::element_blank())
        splots[[k]]<-splots[[k]]+ ggplot2::annotate("text", x = 0, y = .9, label = plot_endpoints[k], cex=4.5, col="black", vjust=0, hjust = 0.0, fontface=2)
        splots[[k]]<-splots[[k]]+ggplot2::ylim(0,1)
        for(karm in 1:2){
          splots[[k]]<-splots[[k]]+ggplot2::geom_step(ggplot2::aes(y = est, x=time,col=arm),direction = "hv", alpha = 1,size=.5,linetype=1,data=dataCurve[[karm]])
          splots[[k]]<-splots[[k]]+ggplot2::geom_step(ggplot2::aes(y = upper_boot, x=time,col=arm),direction = "hv",alpha = 0.5,size=.2,linetype=2,data=dataCurve[[karm]])
          splots[[k]]<-splots[[k]]+ggplot2::geom_step(ggplot2::aes(y = lower_boot, x=time,col=arm),direction = "hv",alpha = 0.5,size=.2,linetype=2,data=dataCurve[[karm]])
        }
        splots[[k]]<-splots[[k]]+ggplot2::scale_colour_manual(values=palette_temp)
        splots[[k]]<-ChageLabels_ggplot(splots[[k]],ChangeText=ChangeText)
      }
      title <- cowplot::ggdraw() + cowplot::draw_label('Cumulative incidence of the first event', fontface='bold',color="black") + ggplot2::theme_bw()
      title <-title +  ggplot2::theme(axis.line = ggplot2::element_blank(),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank())
      #combinedplot<-cowplot::plot_grid(splots[[1]],splots[[2]])
      nCol <- floor(sqrt(length(splots)))
      z<-cowplot::plot_grid(plotlist=splots, ncol=nCol)
      z<-cowplot::plot_grid(title, z, ncol=1, rel_heights=c(0.1, 1))

      filename <- file.path(filepath,paste('CompetingRisk_',DurvalumabLabel[i],'_',HistologyLabel[j],'.png',sep=''))
      ggplot2::ggsave(filename,plot=z,device = ragg::agg_png,bg ="white",width=15, height=10, units="cm", res =300, scaling=.7)
      #End plot of each endpoint in separate windows with both arms in the same window
      #Start plot of stacking of the individual endpoint in separate plots for the two arms ####

      x<-y_lower<-y_upper<-NULL #Hack to avoid notes in cheking related to ggplot

      cumlist<-list()
      for(karm in 1:2){
        alltimepoints<-c()
        datatemp<-list()
        palette_cumlist<-list()
        mycolor<-c("red","blue","green","orange")
        for (k in seq_along(plot_endpoints)){
          palette_cumlist[[plot_endpoints[k]]]<-mycolor[k]
          datatemp[[k]]<-data.frame(bootdata$comprisk$boot$strata[[paste(plot_arms[karm],plot_endpoints[k],sep=' ')]])
          if (nrow(datatemp[[k]])==0){
            datatemp[[k]]<-data.frame(time=c(0,1),est=c(0,0),lower_org=c(0,0),upper_org=c(0,0),lower_boot=c(0,0), upper_boot=c(0,0))
          }
          alltimepoints<-c(alltimepoints,datatemp[[k]]$time)
        }
        palette_cumlist<-unlist(palette_cumlist)
        alltimepoints<-sort(unique(alltimepoints))
        resampled_est<-list()
        for (k in seq_along(datatemp)){
          resampled_est[[k]]<-stats::approx(x=c(0,datatemp[[k]]$time),y=c(0,datatemp[[k]]$est),xout=alltimepoints,ties=mean,method="constant",f=0)
          index<-is.na(resampled_est[[k]]$y)
          resampled_est[[k]]$y[index]<-max(resampled_est[[k]]$y,na.rm=TRUE)
          if (k>1){
            resampled_est[[k]]$y_upper<-resampled_est[[k]]$y+resampled_est[[k-1]]$y_upper
            resampled_est[[k]]$y_lower<-resampled_est[[k-1]]$y_upper
          }else{
            resampled_est[[k]]$y_upper<-resampled_est[[k]]$y
            resampled_est[[k]]$y_lower<-0
          }
          resampled_est[[k]]$col<-plot_endpoints[k]
          resampled_est[[k]]<-data.frame(resampled_est[[k]])
        }
        cumlist[[karm]]<-ggplot2::ggplot()+ggplot2::theme_classic()+ggplot2::ylim(0,1)+ ggplot2::theme(legend.position="top",legend.title=ggplot2::element_blank())
        cumlist[[karm]]<-cumlist[[karm]]+ ggplot2::xlab('Time [Months]') + ggplot2::ylab('Stacked probability of first event')
        cumlist[[karm]]<-cumlist[[karm]]+ ggplot2::annotate("text", x = 0, y = .9, label = plot_arms[karm], cex=4.5, col="black", vjust=0, hjust = 0.0, fontface=2)
        cumlist[[karm]]<-cumlist[[karm]]+ ggplot2::guides(fill=ggplot2::guide_legend(nrow=2,byrow=TRUE))
        for (k in seq_len(length(resampled_est))){
          ytop<-paste("y",as.character(k),sep='')
          ybottom<-paste("y",as.character(k-1),sep='')
          cumlist[[karm]]<-cumlist[[karm]]+ggplot2::geom_ribbon(ggplot2::aes(x=x,ymin=y_lower,ymax=y_upper,fill=col),data=resampled_est[[k]])
        }
        #cumlist[[karm]]<-cumlist[[karm]]+ggplot2::scale_color_manual(values=palette_cumlist)+ggplot2::scale_fill_manual(values=palette_cumlist)
        cumlist[[karm]]<-cumlist[[karm]]+ggplot2::scale_fill_manual(values=palette_cumlist)

      }
      nCol <- 2
      z<-cowplot::plot_grid(plotlist=cumlist, ncol=nCol)

      filename <- file.path(filepath,paste('CompetingRiskStacked_',DurvalumabLabel[i],'_',HistologyLabel[j],'.png',sep=''))
      ggplot2::ggsave(filename,plot=z,device = ragg::agg_png,bg ="white",width=15, height=10, units="cm", res =300, scaling=.7)
      # End section plot competing risk ####
    }
  }
}
#' Bootstraping of all survival data of the NARLAL trial
#'
#' @param df data frame
#' @param formcox list of formula for Cox call
#' @param formsurv list of formula for survival call
#' @param nboot number of bootstraps (nboot=1 will replace the bootstrap values with the standard values)
#' @param conf.int confidence intervals extracted from the bootstrap. Will typpical be 0.95
#' @param seed a value to seed the random generator
#' @keywords internal
#' @return The returned list from bootstrap_narlal consists of three items: Cox,
#'  surv, comprisk. Each of these consists of a list reflecting the number of
#'  models provided to the bootstrap (e.g. local control, progression-free
#'  survival and overall survival). Within each of these, there is a main and a
#'  boot part. The main part will contain all the information provided from the
#'  Cox, survival or competing risk model performed on the entire data set. The
#'  boot section contains information related to the bootstrapping of the models
#'  (see below). The Cox result is based on the coxph function in the survival
#'  package, surv on the function survfit of the survival package and comprisk
#'  on the function cuminc from the cmprsk package.
#'
#'  Cox (boot section): Contains the Cox coefficient (copy from the main
#'  section) and the upper and lower bootstrapped confidence interval as
#'  requested by conf.int
#'
#'  Surv (boot section): Consist of a list of strata (e.g. standard and
#'  escalated arm), and within each of these, six items are present: time,
#'  survival, lower_org, upper_org, lower_boot, and upper_boot. The time,
#'  survival, lower_org, and upper_org are obtained directly from the main
#'  section (time, surv, lower, upper). The lower_boot and upper_boot are the
#'  confidence intervals based on the bootstrap result.
#'
#'  Comprisk (boot section): Consists of a list of strata. Each strata is named
#'  by the “strata” name (e.g. standard and escalated arm) followed by the name
#'  of the specific risk endpoint (e.g. local control, metastatic disease,
#'  death). So a name could e.g. be “escalated mors” (this is the layout
#'  provided by cuminc from the cmprsk package). Each strata has six items:
#'  time, est, lower_org, upper_org, lower_boot, upper_boot. The time, est,
#'  lower_org, upper_org are obtained from the main section, based on the main
#'  values time, est of var (upper_org and lower_org reflect the confidence
#'  interval provided by conf.int and uses est and var within that calculation).
#'  The upper_boot and lower_boot are the confidence intervals obtained from the
#'  bootstrapping.
#' @export
#'
bootstrap_narlal <- function(df,formcox,formsurv,nboot=10,conf.int=.95,seed=42){

  set.seed(seed)
  if (!is.list(formcox)){
    formcox=as.list(formcox)
  }
  if (!is.list(formsurv)){
    formsurv=as.list(formsurv)
  }
  nboot <- as.integer(nboot)
  if (nboot<1){nboot=1}
  nboot_times <- 1000
  bootres <- c()
  treatarm<-as.character(formsurv[[1]][[3]])[2] #Extract this name from the formula since the user might have changed it to something other than “arm”
  #Start calculating the point estimates
  bootres$cox <- vector(mode = "list", length = length(formcox))
  for (i in seq_len(length(formcox))){
    bootres$cox[[i]]$main <- survival::coxph(formcox[[i]],data=df)
  }
  bootres$surv <- vector(mode = "list", length = length(formsurv))
  for (i in seq_len(length(formsurv))){
    bootres$surv[[i]]$main <- survival::survfit(formsurv[[i]],conf.int=conf.int,data=df)
    #The next line is fix of a problem in survfit that it does not replace the calling function which creates problems in ggplot
    bootres$surv[[i]]$main$call$formula <-formsurv[[i]]
  }

  bootres$comprisk$main <- cmprsk::cuminc(ftime = df$t_firstevent, fstatus = df$event_firstevent, cencode = "censoring",group=df[[treatarm]])

  #Initialize the boot results
  for (k in seq_len(length(formcox))){
    bootres$cox[[k]]$boot$coefficients <- rep(NA,length(nboot))
  }
  for (k in seq_len(length(formsurv))){
    counterstart <- 1
    for (i in seq_along(bootres$surv[[k]]$main$strata)){
      counterend <- counterstart + as.vector(bootres$surv[[k]]$main$strata[i])-1
      index <- rep(FALSE,length(bootres$surv[[k]]$main$surv))
      index[c(counterstart:counterend)] <- TRUE
      time <- bootres$surv[[k]]$main$time[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$time <- time
      #bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$time_boot <-(0:(nboot_times-1))*(max(time)-min(time))+min(time)
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$survival <- bootres$surv[[k]]$main$surv[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$lower_org <- bootres$surv[[k]]$main$lower[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$upper_org <- bootres$surv[[k]]$main$upper[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$y <- matrix(NA,nboot,bootres$surv[[k]]$main$strata[i])

      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$n <- 0
      counterstart <- counterend+1
    }
  }
  cmp_strata_names <- names(bootres$comprisk$main)
  index <- cmp_strata_names != "Tests"
  cmp_strata_names <-cmp_strata_names[index]
  pconst <- stats::qnorm(1-(1-conf.int)/2)
  for (k in cmp_strata_names){

    time_org <- bootres$comprisk$main[[k]]$time
    bootres$comprisk$boot$strata[[k]]$time <-time_org
    #bootres$comprisk$boot$strata[[k]]$time_boot <- (0:(nboot_times-1))*(max(time_org)-min(time_org))+min(time_org)
    bootres$comprisk$boot$strata[[k]]$y <- matrix(NA,nboot,length(bootres$comprisk$main[[k]]$time))
    bootres$comprisk$boot$strata[[k]]$est <- bootres$comprisk$main[[k]]$est
    bootres$comprisk$boot$strata[[k]]$lower_org <- bootres$comprisk$main[[k]]$est - pconst*sqrt(bootres$comprisk$main[[k]]$var)
    bootres$comprisk$boot$strata[[k]]$upper_org <- bootres$comprisk$main[[k]]$est + pconst*sqrt(bootres$comprisk$main[[k]]$var)
    bootres$comprisk$boot$strata[[k]]$n <- 0
  }
  #Done with the intialising for the bootstrap

  #Perform the boot-strapping
  if (nboot >1){
    #Start loop over boots
    for (i in seq_len(nboot)){
      #Create the boot index in bootstrap
      bootstrap <- ceiling(stats::runif(n=nrow(df),min=0,max=nrow(df)))
      #Next two lines should not be needed but is as simple precaution for rounding errors of runif
      bootstrap[bootstrap<1] <- 1
      bootstrap[bootstrap>nrow(df)] <- nrow(df)
      #Select the bootstrapped data in tempdata
      tempdata=df[bootstrap,]
      #Calculate the Cox model for all requested models. Needed to define the confidence interval for the hazard ratio.
      for (k in seq_len(length(formcox))){
        bootres$cox[[k]]$boot$coefficients[i] <- tryCatch({7
          survival::coxph(formcox[[k]],data=tempdata)$coefficients[1]
        }, error = function(e) {
          NA
        }
        )
      }
      #Calculate the survival curves. Need to define the confidence interval of the survival curves
      for (k in seq_len(length(formsurv))){

        bootsurv <- survival::survfit(formsurv[[k]],data=tempdata)

        counterstart <-1
        #Loop over the two strata (the standard arm and the escalated arm)
        for (j in seq_along(bootsurv$strata)){
          #bootsurv$strata[j] is the number of time points for strata j
          counterend <- counterstart + as.vector(bootsurv$strata[j])-1
          index <- rep(FALSE,length(bootsurv$time))
          index[c(counterstart:counterend)] <- TRUE

          strata_name <- names(bootsurv$strata)[j]
          temp_time <- bootsurv$time[index] #The time values related to strata j
          temp_surv <- bootsurv$surv[index] #The survival values related to strata j

          # The following lines make an exponential fit (linear of the log-transformed survival
          # values). Due to bootstrap, the data might not cover all time points in
          # the original dataset (max or min value might not have been included in
          # the bootstrap). Thus the fit is used to extrapolate data to values outside
          # the range of time points in the specific bootstrap. When extrapolating
          # the fit, it is important to remember that the survival curve is monotonic.
          # Suppose the fit results in, e.g. a larger value for larger times than the
          # last time point in the bootstrap; then the value from the previous time
          # point will be used (and similar for small time values). Interpolations are
          # made with the last previous value within the bootstrap data. All
          # interpolation is made such that values are available for all time points
          # in the original data.

          #Ensure non-zero values to enable log transform
          temp_surv[temp_surv<.001] <- 0.001
          temp_surv <- log(temp_surv)

          # The time values used per bootstrap are a copy of the time point in the
          # original data. Thus resampled_times match the set of all time points in the original data
          resampled_times <- bootres$surv[[k]]$boot$strata[[strata_name]]$time

          #First step the interpolation
          temp_new_surv<-stats::approx(x=temp_time,y=temp_surv,xout=resampled_times,ties=mean,method="constant",f=0)
          temp_new_surv <- temp_new_surv$y
          #Exponential fit
          temp_lm <- stats::lm(temp_surv~temp_time)
          temp_lm <- stats::predict(temp_lm,newdata=data.frame(temp_time=resampled_times))

          #Find extrapolations prior to the first time point
          startpos <- which(!is.na(temp_new_surv))[1]
          indexfirst <- rep(TRUE,length(resampled_times))
          indexfirst[startpos:length(indexfirst)] <- FALSE
          z<-apply(rbind(as.vector(temp_lm),rep(max(temp_new_surv,na.rm=TRUE),length(temp_new_surv))),2,max)
          z<-apply(rbind(z,rep(0,length(temp_new_surv))),2,min)
          temp_new_surv[indexfirst] <- z[indexfirst]

          #Find extrapolations after the last time point
          endpos <- which(!is.na(temp_new_surv))
          endpos <- endpos[length(endpos)]
          indexlast <- rep(TRUE,length(resampled_times))
          indexlast[1:endpos] <- FALSE
          z<-apply(rbind(as.vector(temp_lm),rep(min(temp_new_surv,na.rm=TRUE),length(temp_new_surv))),2,min)
          temp_new_surv[indexlast] <- z[indexlast]

          #Transform the log times back to times
          temp_new_surv <- exp(temp_new_surv)

          #Place the time values and survival time and boot number in n y (y is an array to support the individual boots) and t
          n <- bootres$surv[[k]]$boot$strata[[strata_name]]$n + 1
          bootres$surv[[k]]$boot$strata[[strata_name]]$n <- n
          bootres$surv[[k]]$boot$strata[[strata_name]]$y[n,] <- temp_new_surv
          #bootres$surv[[k]]$boot$strata[[strata_name]]$time_boot <- resampled_times
          counterstart <- counterend+1
        }
      }
      #Finished all boots for the survival curves

      # Start the bootstrap calculation for the competing risk model. Comments for
      # the individual steps are similar to those provided above for the survival
      # curves (thus not repeated in the following lines)
      bootcomprisk <- cmprsk::cuminc(ftime = tempdata$t_firstevent, fstatus = tempdata$event_firstevent, cencode = "censoring",group=tempdata[[treatarm]])
      boot_cmp_strata_names <- names(bootcomprisk)
      index <- boot_cmp_strata_names != "Tests"
      boot_cmp_strata_names <- boot_cmp_strata_names[index]
      for (j in boot_cmp_strata_names){

        temp_time <- bootcomprisk[[j]]$time
        temp_surv <- bootcomprisk[[j]]$est
        temp_surv[temp_surv>.999] <- 0.999
        temp_surv <- log(1-temp_surv)
        resampled_times <- bootres$comprisk$main[[j]]$time

        temp_new_surv<-stats::approx(x=temp_time,y=temp_surv,xout=resampled_times,ties=min,method="constant",f=0)
        temp_new_surv <- temp_new_surv$y
        temp_lm <- stats::lm(temp_surv~temp_time)
        temp_lm <- stats::predict(temp_lm,newdata=data.frame(temp_time=resampled_times))

        startpos <- which(!is.na(temp_new_surv))[1]
        indexfirst <- rep(TRUE,length(resampled_times))
        indexfirst[startpos:length(indexfirst)] <- FALSE
        z<-apply(rbind(as.vector(temp_lm),rep(max(temp_new_surv,na.rm=TRUE),length(temp_new_surv))),2,max)
        z<-apply(rbind(z,rep(0,length(temp_new_surv))),2,min)
        temp_new_surv[indexfirst] <- z[indexfirst]

        endpos <- which(!is.na(temp_new_surv))
        endpos <- endpos[length(endpos)]
        indexlast <- rep(TRUE,length(resampled_times))
        indexlast[1:endpos] <- FALSE
        z<-apply(rbind(as.vector(temp_lm),rep(min(temp_new_surv,na.rm=TRUE),length(temp_new_surv))),2,min)
        temp_new_surv[indexlast] <- z[indexlast]

        temp_new_surv <- 1-exp(temp_new_surv)
        n <- bootres$surv[[k]]$boot$strata[[strata_name]]$n + 1
        n <- bootres$comprisk$boot$strata[[j]]$n + 1
        bootres$comprisk$boot$strata[[j]]$n <- n
        bootres$comprisk$boot$strata[[j]]$y[n,] <- temp_new_surv
        #bootres$comprisk$boot$strata[[j]]$time_boot <- resampled_times
      }
    }

    #Combine bootstrap result to confidence intervals

    #Combine the central part of the Cox coefficients based on the central part of
    #values as requested by the variable conf.int
    for (k in seq_len(length(formsurv))){
      temp <- as.vector(stats::quantile(bootres$cox[[k]]$boot$coefficients,probs=c((1-conf.int)/2,1-(1-conf.int)/2),na.rm=TRUE))
      bootres$cox[[k]]$boot$coefficients_lower <- temp[1]
      bootres$cox[[k]]$boot$coefficients_upper <- temp[2]
      bootres$cox[[k]]$boot$coefficients <- bootres$cox[[k]]$main$coefficients
    }
    #Combine the survival times (stored in y) to an upper and lower limit based on
    #the central part of values as requested by the variable conf.int
    for (k in seq_len(length(formsurv))){
      for (i in seq_along(bootres$surv[[k]]$boot$strata)){
        temp<-apply(bootres$surv[[k]]$boot$strata[[i]]$y,2,stats::quantile,probs=c((1-conf.int)/2,1-(1-conf.int)/2),na.rm=TRUE)
        bootres$surv[[k]]$boot$strata[[i]]$lower_boot <- temp[1,]
        bootres$surv[[k]]$boot$strata[[i]]$upper_boot <- temp[2,]
        bootres$surv[[k]]$boot$strata[[i]]$y <- NULL #Remove this since it was only used temporarily during the bootstrapping to store all survival curves
        bootres$surv[[k]]$boot$strata[[i]]$n <- NULL #Remove this since it was only used temporarily during the bootstrapping
      }
    }
    #Combine the cumulative risk for the competing risk models (stored in y) to an
    #upper and lower limit based on the central part of values as requested by the variable conf.int
    for (j in cmp_strata_names){
      temp<-apply(bootres$comprisk$boot$strata[[j]]$y,2,stats::quantile,probs=c((1-conf.int)/2,1-(1-conf.int)/2),na.rm=TRUE)
      bootres$comprisk$boot$strata[[j]]$lower_boot <- temp[1,]
      bootres$comprisk$boot$strata[[j]]$upper_boot <- temp[2,]
      bootres$comprisk$boot$strata[[j]]$y <- NULL
      bootres$comprisk$boot$strata[[j]]$n <- NULL
    }


  } else {
    #Bootstrapping is not performed since nboot=1
    #Since bootstrapping was not performed, the uncertainty values are calculated
    #by assuming stand Wald statistics utilizing the variance information provided from the original fit
    pconst <- stats::qnorm(1-(1-conf.int)/2)
    for (k in seq_len(length(formsurv))){
      bootres$cox[[k]]$boot$coefficients <- bootres$cox[[k]]$main$coefficients
      bootres$cox[[k]]$boot$coefficients_lower <- bootres$cox[[k]]$boot$coefficients - pconst*sqrt(bootres$cox[[k]]$main$var)
      bootres$cox[[k]]$boot$coefficients_upper <- bootres$cox[[k]]$boot$coefficients + pconst*sqrt(bootres$cox[[k]]$main$var)

    }
    for (k in seq_len(length(formsurv))){
      for (i in seq_along(bootres$surv[[k]]$boot$strata)){
        bootres$surv[[k]]$boot$strata[[i]]$lower_boot <- bootres$surv[[k]]$boot$strata[[i]]$lower_org
        bootres$surv[[k]]$boot$strata[[i]]$upper_boot <- bootres$surv[[k]]$boot$strata[[i]]$upper_org
      }
    }
    for (j in cmp_strata_names){
      bootres$comprisk$boot$strata[[j]]$lower_boot <- bootres$comprisk$boot$strata[[j]]$lower_org
      bootres$comprisk$boot$strata[[j]]$upper_boot <- bootres$comprisk$boot$strata[[j]]$upper_org
    }
  }
  return(bootres)
}
#source('R/LoadAndPrepareData.R')
#source('R/ExtractSurvivalData.R')
#source('R/SupportPlotFunctions.R')
#library(survival)
#file <- 'C:/home/cab/Lunge protokoler/NarlalII/RcodeNarlal/inst/extdata/DemoData.CSV'
#system.file('extdata','DemoData.csv',package="Narlal2")
#df <- LoadAndPrepareData(filename=file)
#PtSurvival <- ExtractSurvivalData(df,12*5)
#temp <- list(as.formula('survival::Surv(t_os,event_os) ~ arm'),as.formula('survival::Surv(t_progression,event_progression) ~ arm'))
#temp<-bootstrap_narlal(PtSurvival,as.formula('survival::Surv(t_os,event_os) ~ arm+ strata(durvalumab)'),temp,nboot=100,seed=42)
#PlotSurvivalData(df=PtSurvival,filepath='c:/home/cab/temp',nboot=10,seed=42,conf.int=.95)
