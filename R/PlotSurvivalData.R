#' Write word files containing patient toxicity data
#'
#' @param df the output from the the function ExtractSurvivalData
#' @param filepath the filepath to the directory where the patient survival data shall be stored
#' @param nboot number of bootstraps (nboot=1 will replace the bootstrap values with the standard values)
#' @param conf.int confidence intervals extracted from the bootstrap. Will typpical be 0.95
#' @param seed a value to seed the random generator
#'
#' @return Is exporting survival figures
#' @export PlotSurvivalData
#' @importFrom survival strata
#' @importFrom stats as.formula pchisq
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtSurvival <- ExtractSurvivalData(df,12*5)
#' PlotSurvivalData(df=PtSurvival,filepath='c:/home/cab/temp',nboot=10,conf.int=.95,seed=42)
PlotSurvivalData <- function(df,filepath,nboot=10,conf.int=.95,seed=42){

  DurvalumabLabel <- c('AllDurvalumab','YesDurvalumab','NoDurvalumab')
  HistologyLabel <- c('AllHistology','Squamous','NonSquamous')
  formulatext_surv <- c('survival::Surv(t_localcontrol,event_localcontrol)~ arm',
                   'survival::Surv(t_progression,event_progression) ~arm',
                   'survival::Surv(t_os,event_os) ~ arm')
  survival_titles <- c('Local control rate','Progression free rate','Overall survival')

  for (i in seq_along(DurvalumabLabel)){
    indexDurvalumab<-rep(TRUE,nrow(df))
    if (i==2){
      indexDurvalumab[df$durvalumab!='Yes'] <- FALSE
    }
    if (i==3) {
      indexDurvalumab[df$durvalumab!='No'] <- FALSE
    }
    for (j in seq_along(HistologyLabel)){
      indexHistology <- rep(TRUE,nrow(df))
      if (j==2){
        indexHistology[df$histology_squamous!='Squamous'] <- FALSE
      }
      if (j==3){
        indexHistology[df$histology_squamous!='Non-squamous'] <- FALSE
      }
      dftemp <- df[indexDurvalumab & indexHistology,]

      #Perform all the bootstrap calc for the given plot
      formsurv <- list()
      for (k in seq_along(formulatext_surv)){
        formsurv <- append(formsurv, as.formula(formulatext_surv[[k]]))
      }
      formcox <- list()
      for (k in seq_along(formulatext_surv)){
        formcox <- append(formcox, as.formula(paste(formulatext_surv[[k]], '+ strata(durvalumab)',sep='')))
      }

      bootdata <- bootstrap_narlal(dftemp,formcox,formsurv,nboot=nboot,conf.int=conf.int,seed=seed)
      #save(bootdata, file = paste("c:/home/cab/temp/data_",DurvalumabLabel[i],'_',HistologyLabel[j],".Rdata",sep=""))
      splots <- list()
      for (k in seq_along(formulatext_surv)){
        #tempform <- as.formula(paste(formulatext[[k]], '+ strata(durvalumab)',sep=''))
        #cox_res<-survival::coxph(tempform,data=dftemp)
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

        palette_temp = c("red",  "blue")

        temp <- survminer::ggsurvplot(fit,xlab="Time [months]",ylab=survival_titles[k],palette=palette_temp,
                                      risk.table = TRUE,fontsinze=1,break.time.by = 12,legend = c(0.8, 0.8),
                                      risk.table.height = .1,conf.int = FALSE,conf.int.style = "step",pval = FALSE,  data = dftemp)
        time <- lower <- upper <- NULL
        temp_bootdata <- data.frame(time=bootdata$surv[[k]]$boot$strata[[1]]$time,
                               lower=bootdata$surv[[k]]$boot$strata[[1]]$lower_boot,
                               upper=bootdata$surv[[k]]$boot$strata[[1]]$upper_boot)
        #temp$plot <- temp$plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax=upper, x=time),fill=palette_temp[1], alpha = 0.0,size=.5,linetype=2,color=palette_temp[1],inherit.aes = FALSE,data=temp_bootdata)

        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = lower, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[1],inherit.aes = FALSE,data=temp_bootdata)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = upper, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[1],inherit.aes = FALSE,data=temp_bootdata)


        time <- lower <- upper <- NULL
        temp_bootdata <- data.frame(time=bootdata$surv[[k]]$boot$strata[[2]]$time,
                                    lower=bootdata$surv[[k]]$boot$strata[[2]]$lower_boot,
                                    upper=bootdata$surv[[k]]$boot$strata[[2]]$upper_boot)
        #temp$plot <- temp$plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax=upper, x=time),fill=palette_temp[2], alpha = 0.0,size=.5,linetype=2,color=palette_temp[2],inherit.aes = FALSE,data=temp_bootdata)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = lower, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[2],inherit.aes = FALSE,data=temp_bootdata)
        temp$plot <- temp$plot + ggplot2::geom_step(ggplot2::aes(y = upper, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[2],inherit.aes = FALSE,data=temp_bootdata)

        temp$plot <- temp$plot + ggplot2::annotate("text", x = 0, y = 0, label = pvaluelable, cex=4.5, col="black", vjust=0, hjust = 0.0, fontface=2)
        temp$plot <- temp$plot + ggplot2::annotate("text", x = Inf, y = Inf, label = hlable, cex=4.5, col="black", vjust=1, hjust = 1, fontface=2)
        # annotate("text", -Inf, Inf, label = "Top-left", hjust = 0, vjust = 1)
        splots[[k]] <- temp
      }


      survivalplot <- survminer::arrange_ggsurvplots(splots, print = FALSE,ncol = 3, nrow = 1, risk.table.height = 0.2)
      filename <- file.path(filepath,paste('Survival_',DurvalumabLabel[i],'_',HistologyLabel[j],'.png',sep=''))
      ggplot2::ggsave(filename,plot=survivalplot,device = ragg::agg_png,width=15, height=6.75, units="cm", res =300, scaling=.375)


      splots <- list()
      #CR <- cmprsk::cuminc(ftime = dftemp$t_firstevent, fstatus = dftemp$event_firstevent, cencode = "censoring",group=dftemp$arm)
      CR <- bootdata$comprisk$main
      palette_temp = grDevices::palette.colors(n = (length(CR)-1)/2, palette = "Set 1",  alpha=1,recycle = FALSE)
      index <- grepl("Standard", names(CR), fixed = TRUE)
      index <- index | grepl("Test", names(CR), fixed = TRUE)
      CRtemp <- CR[index]
      #compsurvplot <- ggcompetingrisks_narlal(fit = CRtemp, multiple_panels = FALSE,conf.int = FALSE,palette=palette_temp,size=5) +
      #  ggplot2::scale_x_continuous(limits=c(0,60), breaks=seq(from=0,to=60,by=12))
      compsurvplot <- ggplot2::ggplot() + ggplot2::theme_classic() + ggplot2::xlab('Time [Months]') + ggplot2::ylab('Probability of first event') + ggplot2::xlim(0, 60) + ggplot2::ylim(0,1)
      index <- grepl("Standard", names(bootdata$comprisk$boot$strata), fixed = TRUE)

      tempdata <- bootdata$comprisk$boot$strata[index]
      legendnames<-sapply(strsplit(names(tempdata),split=' '),'[',2)
      for (k in seq_along(tempdata)){
        time <- lower <- upper <- NULL
        temp_bootdata <- data.frame(time=tempdata[[k]]$time,
                                    lower=tempdata[[k]]$lower_boot,
                                    upper=tempdata[[k]]$upper_boot)


        compsurvplot <-compsurvplot +  ggplot2::geom_step(ggplot2::aes(y = lower, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[k],inherit.aes = FALSE,data=temp_bootdata)+
          ggplot2::geom_step(ggplot2::aes(y = upper, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[k],inherit.aes = FALSE,data=temp_bootdata)
        time <- est <- ll <- NULL
        temp_data <- data.frame(time=tempdata[[k]]$time,
                                est=tempdata[[k]]$est,
                                ll=rep(legendnames[k],length(tempdata[[k]]$est)))

        compsurvplot <-compsurvplot + ggplot2::geom_step(ggplot2::aes(y = est, x=time,col = ll),direction = "hv", alpha = 1,size=.5,linetype=1,inherit.aes = FALSE,data=temp_data)+
          ggplot2::theme(legend.title=ggplot2::element_blank(),legend.position = c(0.25, 0.8))

      }
      compsurvplot <-compsurvplot+ggplot2::ggtitle("Standard") + ggplot2::scale_colour_manual(values=palette_temp)
      splots[[1]] <- compsurvplot

      index <- !grepl("Standard", names(CR), fixed = TRUE)
      index <- index | grepl("Test", names(CR), fixed = TRUE)
      CRtemp <- CR[index]
      #compsurvplot <- ggcompetingrisks_narlal(fit = CRtemp, multiple_panels = FALSE,conf.int = FALSE,palette=palette_temp,size=5) +
      #  ggplot2::scale_x_continuous(limits=c(0,60), breaks=seq(from=0,to=60,by=12))
      compsurvplot <- ggplot2::ggplot() + ggplot2::theme_classic() + ggplot2::xlab('Time [Months]') + ggplot2::ylab('Probability of first event')+ ggplot2::xlim(0, 60) + ggplot2::ylim(0,1)
      index <- !grepl("Standard", names(bootdata$comprisk$boot$strata), fixed = TRUE)
      tempdata <- bootdata$comprisk$boot$strata[index]
      legendnames<-sapply(strsplit(names(tempdata),split=' '),'[',2)
      for (k in seq_along(tempdata)){
        time <- lower <- upper <- NULL
        temp_bootdata <- data.frame(time=tempdata[[k]]$time,
                                    lower=tempdata[[k]]$lower_boot,
                                    upper=tempdata[[k]]$upper_boot)

        compsurvplot <-compsurvplot +  ggplot2::geom_step(ggplot2::aes(y = lower, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[k],inherit.aes = FALSE,data=temp_bootdata)+
                                      ggplot2::geom_step(ggplot2::aes(y = upper, x=time),direction = "hv",alpha = 0.5,size=.2,linetype=2,color=palette_temp[k],inherit.aes = FALSE,data=temp_bootdata)
        time <- est <- ll <- NULL
        temp_data <- data.frame(time=tempdata[[k]]$time,
                                est=tempdata[[k]]$est,
                                ll=rep(legendnames[k],length(tempdata[[k]]$est)))

        compsurvplot <-compsurvplot + ggplot2::geom_step(ggplot2::aes(y = est, x=time,col = ll),direction = "hv", alpha = 1,size=.5,linetype=1,inherit.aes = FALSE,data=temp_data)+
          ggplot2::theme(legend.title=ggplot2::element_blank(),legend.position = c(0.25, 0.8))
      }
      compsurvplot <-compsurvplot+ggplot2::ggtitle("Escalated") + ggplot2::scale_colour_manual(values=palette_temp)
      splots[[2]] <- compsurvplot
      title <- cowplot::ggdraw() + cowplot::draw_label('Cumulative incidence of the first event', fontface='bold',color="black") + ggplot2::theme_bw() +ggplot2::theme(axis.line = ggplot2::element_blank(),
                                                                                                                                                            panel.grid.major = ggplot2::element_blank(),
                                                                                                                                                            panel.grid.minor = ggplot2::element_blank(),
                                                                                                                                                            panel.border = ggplot2::element_blank(),
                                                                                                                                                            panel.background = ggplot2::element_blank())
      #+ggplot2::theme(panel.background = ggplot2::element_blank(),linetype=0)
      combinedplot<-cowplot::plot_grid(splots[[1]],splots[[2]])
      z<-cowplot::plot_grid(title, combinedplot, ncol=1, rel_heights=c(0.1, 1))

      filename <- file.path(filepath,paste('CompetingRisk_',DurvalumabLabel[i],'_',HistologyLabel[j],'.png',sep=''))
      ggplot2::ggsave(filename,plot=z,device = ragg::agg_png,width=15, height=10, units="cm", res =300, scaling=.8)

      cowplot::save_plot(filename,plot=z)

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
#' @return return a list containing all the standard acall and the related bootstrap values
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
  bootres$comprisk$main <- cmprsk::cuminc(ftime = df$t_firstevent, fstatus = df$event_firstevent, cencode = "censoring",group=df$arm)

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
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$time_boot <-(0:(nboot_times-1))*(max(time)-min(time))+min(time)
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$survival <- bootres$surv[[k]]$main$surv[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$lower_org <- bootres$surv[[k]]$main$lower[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$upper_org <- bootres$surv[[k]]$main$upper[index]
      bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$y <- matrix(NA,nboot,bootres$surv[[k]]$main$strata[i])
      #bootres$surv[[k]]$boot$strata[[names(bootres$surv[[k]]$main$strata[i])]]$y <- matrix(NA,nboot,nboot_times)

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
    bootres$comprisk$boot$strata[[k]]$time_boot <- (0:(nboot_times-1))*(max(time_org)-min(time_org))+min(time_org)
    bootres$comprisk$boot$strata[[k]]$y <- matrix(NA,nboot,length(bootres$comprisk$main[[k]]$time))
    #bootres$comprisk$boot$strata[[k]]$y <- matrix(NA,nboot,nboot_times)
    bootres$comprisk$boot$strata[[k]]$est <- bootres$comprisk$main[[k]]$est
    bootres$comprisk$boot$strata[[k]]$lower_org <- bootres$comprisk$main[[k]]$est - pconst*sqrt(bootres$comprisk$main[[k]]$var)
    bootres$comprisk$boot$strata[[k]]$upper_org <- bootres$comprisk$main[[k]]$est + pconst*sqrt(bootres$comprisk$main[[k]]$var)
    bootres$comprisk$boot$strata[[k]]$n <- 0
  }
  #perform the boot-strapping
  if (nboot >1){
    for (i in seq_len(nboot)){
      bootstrap <- ceiling(stats::runif(n=nrow(df),min=0,max=nrow(df)))
      #Next two lines should not be needed but is as simple precaution for rounding errors of runif
      bootstrap[bootstrap<1] <- 1
      bootstrap[bootstrap>nrow(df)] <- nrow(df)
      tempdata=df[bootstrap,]
      for (k in seq_len(length(formcox))){

        bootres$cox[[k]]$boot$coefficients[i] <- tryCatch({
          survival::coxph(formcox[[k]],data=tempdata)$coefficients[1]
        }, error = function(e) {
          NA
        }
        )

        #bootres$cox[[k]]$boot$coefficients[i] <- survival::coxph(formcox[[k]],data=tempdata)$coefficients[1]
      }
      for (k in seq_len(length(formsurv))){

        bootsurv <- survival::survfit(formsurv[[k]],data=tempdata)

        counterstart <-1
        for (j in seq_along(bootsurv$strata)){
          counterend <- counterstart + as.vector(bootsurv$strata[j])-1
          index <- rep(FALSE,length(bootsurv$time))
          index[c(counterstart:counterend)] <- TRUE

          strata_name <- names(bootsurv$strata)[j]
          temp_time <- bootsurv$time[index]
          temp_surv <- bootsurv$surv[index]
          temp_surv[temp_surv<.001] <- 0.001
          temp_surv <- log(temp_surv)
          #t0 <- min(bootres$surv[[k]]$boot$strata[[strata_name]]$time)
          #t1 <- max(bootres$surv[[k]]$boot$strata[[strata_name]]$time)
          #resampled_times <- (0:(nboot_times-1))*(t1-t0)/(nboot_times-1)+t0
          resampled_times <- bootres$surv[[k]]$boot$strata[[strata_name]]$time


          temp_new_surv<-stats::approx(x=temp_time,y=temp_surv,xout=resampled_times,ties=mean,method="constant",f=0)
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

          temp_new_surv <- exp(temp_new_surv)
          n <- bootres$surv[[k]]$boot$strata[[strata_name]]$n + 1
          bootres$surv[[k]]$boot$strata[[strata_name]]$n <- n
          bootres$surv[[k]]$boot$strata[[strata_name]]$y[n,] <- temp_new_surv
          bootres$surv[[k]]$boot$strata[[strata_name]]$time_boot <- resampled_times
          counterstart <- counterend+1
        }
      }

      bootcomprisk <- cmprsk::cuminc(ftime = tempdata$t_firstevent, fstatus = tempdata$event_firstevent, cencode = "censoring",group=tempdata$arm)
      boot_cmp_strata_names <- names(bootcomprisk)
      index <- boot_cmp_strata_names != "Tests"
      boot_cmp_strata_names <- boot_cmp_strata_names[index]
      for (j in boot_cmp_strata_names){

        temp_time <- bootcomprisk[[j]]$time
        temp_surv <- bootcomprisk[[j]]$est
        temp_surv[temp_surv>.999] <- 0.999
        temp_surv <- log(1-temp_surv)
        #t0 <- min(bootres$comprisk$main[[j]]$time)
        #t1 <- max(bootres$comprisk$main[[j]]$time)
        #resampled_times <- (0:(nboot_times-1))*(t1-t0)/(nboot_times-1)+t0
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
        bootres$comprisk$boot$strata[[j]]$time_boot <- resampled_times
      }
    }
    #Combine bootstrap result to confidence intervals
    for (k in seq_len(length(formsurv))){
      temp <- as.vector(stats::quantile(bootres$cox[[k]]$boot$coefficients,probs=c((1-conf.int)/2,1-(1-conf.int)/2),na.rm=TRUE))
      bootres$cox[[k]]$boot$coefficients_lower <- temp[1]
      bootres$cox[[k]]$boot$coefficients_upper <- temp[2]
      bootres$cox[[k]]$boot$coefficients <- bootres$cox[[k]]$main$coefficients
    }
    for (k in seq_len(length(formsurv))){
      for (i in seq_along(bootres$surv[[k]]$boot$strata)){
        temp<-apply(bootres$surv[[k]]$boot$strata[[i]]$y,2,stats::quantile,probs=c((1-conf.int)/2,1-(1-conf.int)/2),na.rm=TRUE)
        bootres$surv[[k]]$boot$strata[[i]]$lower_boot <- temp[1,]
        bootres$surv[[k]]$boot$strata[[i]]$upper_boot <- temp[2,]
        bootres$surv[[k]]$boot$strata[[i]]$y <- NULL
        bootres$surv[[k]]$boot$strata[[i]]$n <- NULL
      }
    }

    for (j in cmp_strata_names){
      temp<-apply(bootres$comprisk$boot$strata[[j]]$y,2,stats::quantile,probs=c((1-conf.int)/2,1-(1-conf.int)/2),na.rm=TRUE)
      bootres$comprisk$boot$strata[[j]]$lower_boot <- temp[1,]
      bootres$comprisk$boot$strata[[j]]$upper_boot <- temp[2,]
      bootres$comprisk$boot$strata[[j]]$y <- NULL
      bootres$comprisk$boot$strata[[j]]$n <- NULL
    }


  } else {
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
        #bootres$surv[[k]]$boot$strata[[i]]$y <- NULL
        #bootres$surv[[k]]$boot$strata[[i]]$n <- NULL
      }
    }
    for (j in cmp_strata_names){
      bootres$comprisk$boot$strata[[j]]$lower_boot <- bootres$comprisk$boot$strata[[j]]$lower_org
      bootres$comprisk$boot$strata[[j]]$upper_boot <- bootres$comprisk$boot$strata[[j]]$upper_org
      #bootres$comprisk$boot$strata[[j]]$y <- NULL
      #bootres$comprisk$boot$strata[[j]]$n <- NULL
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
