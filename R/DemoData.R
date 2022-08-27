#' Create demo data fro the Narlal2 clinical trial
#' @description
#' ‘Demodata’ writes a CSV file to disk containing demo data for the clinical trial Narlal2. The file can be imported to the Redcap data database used for the trial. The primary purpose of the demo data has been to develop the other functionalities utilising “realistic” data.
#' @param nrpts pt number to include in the demo data
#' @param uselabels can be set to TRUE if the values in the data table should be the label names (e.g. Male/Female versus 1/2). FALSE is the default and is needed if the data should be imported into a REdcap database
#' @param filepath path to the output file
#' @param seed is used for the random number generator. Two runs will for a fixed value of the seed produce the same demo data
#' @param firstid is the id of the first patient, and the following are consecutive. It can be used to divide a large cohort into subfiles which might be needed in upload to Redcap that seems to have a limit on the number of patients that can be imported (likely due to memory issues at the Redcap server)
#' @return write a csv file with demo data that can be loaded into a redcap database
#' @export DemoData
#'
#' @examples file <- file.path(dirname(tempdir()),'demo.csv')
#' DemoData(nrpts=100,uselabels=FALSE,seed=17,filepath=file)
DemoData <- function(nrpts=360,uselabels=FALSE,seed=42,firstid=1,filepath){
  set.seed(seed)
  CohortData <- GeneratePtCohortData(nrpts,uselabel=uselabels,firstid=firstid)
  df <- MakeCohortToDataFrame(CohortData,firstid=firstid)
  WriteToRedCapFormat(df,filename=filepath)
}

form_registrering <- function(id,uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x<-list()
  x$patient_id=id


  BirtsDayEarly <- as.numeric(as.Date("1940-01-01",origin="1970-01-01"))
  BirtsDayLate <- as.numeric(as.Date("1970-01-01",origin="1970-01-01"))
  birthday <- as.Date(stats::runif(1,BirtsDayEarly,BirtsDayLate),origin="1970-01-01")

  daystring <- as.character(lubridate::day(birthday))
  while(nchar(daystring)<2){daystring<-paste('0',daystring,sep='')}
  monthstring <- as.character(lubridate::month(birthday))
  while(nchar(monthstring)<2){monthstring<-paste('0',monthstring,sep='')}
  yearstring <- lubridate::year(birthday)
  yearstring <- yearstring-1900
  if (yearstring>99){yearstring <- yearstring -100}
  yearstring <- as.character(yearstring)
  while(nchar(yearstring)<2){yearstring<-paste('0',yearstring,sep='')}
  if (lubridate::year(birthday)<2000){
    fourlaststring <- floor(stats::runif(1,1,4990))
  } else {
    fourlaststring <- floor(stats::runif(1,5000,9990))
  }
  temp <- list(c('Mand','Kvinde'),c(1,2))
  if (fourlaststring %% 2 ==1){gender <- temp[[labelindex]][1]} else {gender <- temp[[labelindex]][2]}
  fourlaststring <- as.character(fourlaststring)
  while(nchar(fourlaststring)<4){fourlaststring<-paste('0',fourlaststring,sep='')}

  x$cpr_nummer <- paste(daystring,monthstring,yearstring,fourlaststring,sep='')
  x$initials <- paste( letters[ceiling(stats::runif(1,0,length(letters)))],
                       letters[ceiling(stats::runif(1,0,length(letters)))],sep='')
  registreringEarly <- as.numeric(as.Date("2010-01-01",origin="1970-01-01"))
  registreringLate <- as.numeric(as.Date("2020-01-01",origin="1970-01-01"))
  x$d_registrering <- as.Date(stats::runif(1,registreringEarly,registreringLate),origin="1970-01-01")
  centres <- list(c('RH','Herlev','Næstved','OUH','Aarhus','Vejle','Aalborg' ),
                  c(1,2,3,4,5,6,7))
  x$center <- centres[[labelindex]][floor(stats::runif(1,1,length(centres[[labelindex]])+1))]
  x$birthday<-birthday
  x$gender <- gender
  x$age <- floor(lubridate::time_length(difftime(x$d_registrering, x$birthday),"years"))
  return(x)
}


form_inklusion<- function(uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  temp <- list(c('IIB','IIIA','IIIB'),c(4,5,6))
  x$stadium <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*sqrt(stats::runif(1,0,1)))]
  temp <- list(c('Squamous','Non-squamous'),c(1,2))
  x$histology_squamous <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  temp<-list(c('Ja','Nej'),c(1,0))
  x$age18 <- temp[[labelindex]][1]
  x$stadium_inkl <- temp[[labelindex]][1]
  x$perform <- temp[[labelindex]][1]
  x$able_curative_rt <- temp[[labelindex]][1]
  x$able_followup <- temp[[labelindex]][1]
  x$antikonception <- temp[[labelindex]][1]
  x$informed_consent <- temp[[labelindex]][1]
  x$mulig_plan <- temp[[labelindex]][1]

  x$system_lidelse <- temp[[labelindex]][2]
  x$nasal_ilt <- temp[[labelindex]][2]
  x$thorakal_rt <- temp[[labelindex]][2]
  x$active_cancer <- temp[[labelindex]][2]
  x$oral_medicin <- temp[[labelindex]][2]
  x$active_ulcus <- temp[[labelindex]][2]
  x$lactation <- temp[[labelindex]][2]
  temp <- list(c('Standard','Eskaleret'),c(1,2))
  x$randomisering <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  return(x)
}

form_onstudy_skema <- function(ptdata,uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <-list()
  x$d_diag <- as.Date(as.numeric(ptdata$Registration$d_registrering)-stats::runif(1,30,60),origin = "1970-01-01")
  temp <- list(c('Planocellulær','Adenocarcinom','Adenosquamøs','Udifferentieret'),
               c(1,2,3,4))
  x$hist <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  temp <- list(c('T1A','T1B','T2','T2A','T2B','T3','T4'),
               c(3,4,5,6,7,8,9))
  x$t <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  temp <- list(c('0','1','2','3'),c(0,1,2,3))
  x$n <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  temp <- list(c('IB','IIA','IIB','IIIA','IIIB','IV'),
               c(2,3,4,5,6,7))
  x$std <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  temp <- list(c('Højre','Venstre','Undifferentieret','Mediastinalt'),
               c(1,2,3,4))
  x$slung <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  x$weight <- floor(stats::runif(1,50,110))
  x$height <- floor(stats::runif(1,150,198))
  temp <- list(c('0','1'),c(0,1))
  x$ps <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*stats::runif(1,0,1))]
  x$fev1 <- round(stats::runif(1,1,3),digits=2)
  x$fvc <- round(stats::runif(1,2,4),digits=2)
  x$dlcoref <- floor(stats::runif(1,30,80))
  x$dlco <- round(stats::runif(1,30,80),digits = 2)
  x$n_neocis <- as.integer(NA)
  x$n_neocarbo <- as.integer(NA)
  x$n_neonavelbine <- as.integer(NA)
  x$n_neovino <- as.integer(NA)
  x$hgb <- round(stats::runif(1,7,11),digits=1)
  x$leu <- round(stats::runif(1,5,10),digits=1)
  x$neu <- round(stats::runif(1,3,6),digits=1)
  x$tro <- round(stats::runif(1,100,350),digits=0)
  x$na <- round(stats::runif(1,120,180),digits=0)
  x$k <- round(stats::runif(1,2,5),digits=1)
  x$alb <- round(stats::runif(1,20,50),digits=0)
  x$creat_b <- round(stats::runif(1,60,80),digits=0)
  x$basp <- round(stats::runif(1,90,105),digits=0)
  x$alat <- round(stats::runif(1,30,40),digits=0)
  x$ldh <- round(stats::runif(1,90,180),digits=0)
  x$bili <- round(stats::runif(1,2,7),digits=0)

  return(x)
}


form_bivirkningsskema_strlebehandling <- function(uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  temp <- list(c('Nej','Ja'),c(0,1))
  x$smoking <- temp[[labelindex]][ceiling(0.7*length(temp[[labelindex]])*stats::runif(1,0,1))]
  if (x$smoking=='Ja' || x$smoking==1){x$n_cigarettes <- round(stats::runif(1,10,30),digits=0) }
  temp <- list(c('0','1','2','3','4'),c(0,1,2,3,4))
  x$ps_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$fatigue_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$cough_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$dyspnoe_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$constipation_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$nausea_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$vomiting_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$dysphagia_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$pain_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$sens_neuropati_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$infection_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$diaria_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$skinreaction_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$ototox_rt <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^14)]
  x$other_tox1_rt <- temp[[labelindex]][1]
  x$other_tox2_rt <- temp[[labelindex]][1]
  x$other_tox3_rt <- temp[[labelindex]][1]
  return(x)
}
form_blodprver_under_strlebehandling <- function(){
  x <- list()
  x$hgb_rt <- round(stats::runif(1,7,11),digits=1)
  x$leu_rt <- round(stats::runif(1,5,10),digits=1)
  x$neu_rt <- round(stats::runif(1,3,6),digits=1)
  x$tro_rt <- round(stats::runif(1,100,350),digits=0)
  x$na_rt <- round(stats::runif(1,120,180),digits=0)
  x$k_rt <- round(stats::runif(1,2,5),digits=1)
  x$alb_rt <- round(stats::runif(1,20,50),digits=0)
  x$creat_rt <- round(stats::runif(1,60,80),digits=0)
  x$basp_rt <- round(stats::runif(1,90,105),digits=0)
  x$alat_rt <- round(stats::runif(1,30,40),digits=0)
  x$ldh_rt <- round(stats::runif(1,90,180),digits=0)
  x$bili_rt <- round(stats::runif(1,2,7),digits=0)
  return(x)
}

form_rt_slut <- function(ptdata,uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  x$d_rt <- as.Date(as.numeric(ptdata$Registration$d_registrering)+stats::runif(1,14,21),origin="1970-01-01")
  x$d_rtsl <- as.Date(as.numeric(x$d_rt)+stats::runif(1,35,45),origin="1970-01-01")
  if (ptdata$Registration$randomisering=='Standard' || ptdata$Registration$randomisering==1){mean=66} else {mean=80}
  x$gy_t_mean <- round(stats::rnorm(1,mean=mean,sd=2),digits = 2)
  x$gy_n_mean <- round(stats::rnorm(1,mean=mean,sd=2),digits = 2)
  x$fx <- 33
  x$gy_lung_mean  <- round(stats::rnorm(1,mean=15,sd=2),digits = 2)
  x$d_rt_plan2 <- as.Date(NA)
  temp <- list(c('Midt ventilation','MIP'),c(1,2))
  x$gtv_dfn <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$vol_gtv_t <- round(stats::rnorm(1,mean=0,sd=5)^2,digits = 2)
  x$vol_gtv_n <- round(stats::rnorm(1,mean=0,sd=4)^2,digits = 2)

  temp <- list(c('30%','40%','50%','Udvidet'),
               c(1,2,3,4))
  x$gtv_t1_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_t2_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_t3_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_n1_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_n2_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_n3_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_n4_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_n5_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_n6_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$gtv_kongl_pet_suv <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  x$comments_rt_planning <- ''
  temp <- list(c('Nej','Ja'),c(0,1))
  x$cirro_export <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(1/4))]
  return(x)
}

form_kemoterapi_under_rt <- function(ptdata,uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  startdate <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+stats::runif(1,7,14),origin="1970-01-01")
  enddate <- as.Date(as.numeric(startdate)+stats::runif(1,7,14),origin="1970-01-01")
  temp<-list(c(as.character(NA)),c(as.integer(NA)))
  x$cis_rt_dosis <- temp[[labelindex]]
  x$carbo_rt_dosis <- temp[[labelindex]]
  if (stats::runif(1,0,1)<.8){
    x$n_cis_rt <- round(stats::runif(1,1,3),digits = 0)
    x$cis_rt_mg <- round(stats::runif(1,1,3),digits = 0)*100
    temp <- list(c('Ingen dosisreduktion','75% dosis','Seponeret'),c(1,2,3))
    x$cis_rt_dosis <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(4))]
    x$d_cis_rt <- startdate
    x$d_cis_rtsl <- enddate
    x$n_carbo_rt <- 0

  } else {
    x$carbo_rt_mg <- round(stats::runif(1,1,3),digits = 0)*100
    x$n_carbo_rt <- round(stats::runif(1,1,3),digits = 0)
    temp <- list(c('Ingen dosisreduktion (AUC4)','AUC3','Seponeret'),c(1,2,3))
    x$carbo_rt_dosis <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(4))]
    x$d_carbo_rt <- startdate
    x$d_carbo_rtsl <- enddate
    x$n_cis_rt <- 0
  }
  x$n_nav_rt <- round(stats::runif(1,1,4),digits = 0)
  x$nav_rt_mg  <- round(stats::runif(1,1,3),digits = 0)*100
  temp <- list(c('Ingen dosisreduktion (100%)','80%','Seponeret'),c(1,2,3))

  x$nav_rt_dosis <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(4))]
  startdate <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+stats::runif(1,7,14),origin="1970-01-01")
  enddate <- as.Date(as.numeric(startdate)+stats::runif(1,7,14),origin="1970-01-01")
  x$d_nav_rt <- startdate
  x$d_nav_rtsl <- enddate
  x$n_and_rt_type <- ''
  if ((x$cis_rt_dosis!='Ingen dosisreduktion' && x$cis_rt_dosis!=1) && (!is.na(x$cis_rt_dosis)) ){
    index <- matrix(1,10,1)
    index[ceiling((10*stats::runif(1,0,1)))] <- 2
    temp<- list(c('Nej','Ja'),c(0,1))
    x$no_kemo_rt_hemotol <- temp[[labelindex]][index[1]]
    x$no_kemo_rt_kvl <- temp[[labelindex]][index[2]]
    x$no_kemo_rt_ear <- temp[[labelindex]][index[3]]
    x$no_kemo_rt_neuro <- temp[[labelindex]][index[4]]
    x$no_kemo_rt_esofagit <- temp[[labelindex]][index[5]]
    x$no_kemo_rt_lunge2_8f7 <- temp[[labelindex]][index[6]]
    x$no_kemo_rt_and_rt_tox <- temp[[labelindex]][index[7]]
    x$no_kemo_rt_pt <- temp[[labelindex]][index[8]]
    x$no_kemo_rt_nefro <- temp[[labelindex]][index[9]]
    x$no_kemo_rt_at <- temp[[labelindex]][index[10]]
    x$no_kemo_rt_and <- temp[[labelindex]][1]
  }
  if ((x$carbo_rt_dosis!='Ingen dosisreduktion' && x$carbo_rt_dosis!=1) && (!is.na(x$carbo_rt_dosis)) ){
    index <- matrix(1,9,1)
    index[ceiling((9*stats::runif(1,0,1)))] <- 2
    temp<- list(c('Nej','Ja'),c(0,1))
    x$no_carbo_rt_hemotol <- temp[[labelindex]][index[1]]
    x$no_carbo_rt_kvl <- temp[[labelindex]][index[2]]
    x$no_carbo_rt_ear <- temp[[labelindex]][index[3]]
    x$no_carbo_rt_neuro <- temp[[labelindex]][index[4]]
    x$no_carbo_rt_esofagit <- temp[[labelindex]][index[5]]
    x$no_carbo_rt_lunge2_8f7 <- temp[[labelindex]][index[6]]
    x$no_carbo_rt_and_rt_tox <- temp[[labelindex]][index[7]]
    x$no_carbo_rt_pt <- temp[[labelindex]][index[8]]
    x$no_carbo_rt_at <- temp[[labelindex]][index[9]]
    x$no_carbo_rt_and <- temp[[labelindex]][1]
  }

  if ((x$nav_rt_dosis!='Ingen dosisreduktion (100%)' && x$nav_rt_dosis!=1) && (!is.na(x$nav_rt_dosis)) ){
    index <- matrix(1,8,1)
    index[ceiling((8*stats::runif(1,0,1)))] <- 2
    temp<- list(c('Nej','Ja'),c(0,1))
    x$no_nav_rt_hemotol <- temp[[labelindex]][index[1]]
    x$no_navel_rt_kvl <- temp[[labelindex]][index[2]]
    x$no_nav_rt_neuro <- temp[[labelindex]][index[3]]
    x$no_nav_rt_esofagit <- temp[[labelindex]][index[4]]
    x$no_nav_rt_lunge2_8f7 <- temp[[labelindex]][index[5]]
    x$no_nav_rt_and_rt_tox <- temp[[labelindex]][index[6]]
    x$no_nav_rt_pt <- temp[[labelindex]][index[7]]
    x$no_nav_rt_at <- temp[[labelindex]][index[8]]
    x$no_nav_rt_and <- temp[[labelindex]][1]
  }

  return(x)

}


form_durvalumab <- function(ptdata,uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  temp <- list(c('Yes','No'),c(1,0))
  x$durvalumab <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(1/4))]
  if (x$durvalumab == 'Yes' || x$durvalumab == 1){
    x$pdl1 <- round(stats::rnorm(1,50,10),digits=0)
    x$d_durvalumab <- as.Date(as.numeric(ptdata$FirstFollowup$d_rtsl)+stats::runif(1,20,60),origin="1970-01-01")
    x$d_durvalumab_end <- as.Date(as.numeric(x$d_durvalumab)+stats::runif(1,30,90),origin="1970-01-01")
    x$durvalumab_stop <- round(stats::runif(1,1,6),digits=0)
    temp <- list(c('Adverse event','Relapse','Other'),c(1,2,3))
    x$durvalumab_stop_reason <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  }
  return(x)
}
form_recidivmors <- function(ptdata,uselabel=TRUE,h_loc = c(3.75e-4,6.35e-4),h_death = 5.23e-4, h_loc_death = 0.004, h_dist_met=0.0001){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  t_death <- -log(stats::runif(1,0,1))/h_death
  if (ptdata$Registration$randomisering=="Standard" || ptdata$Registration$randomisering==1){
    t_loc <- -log(stats::runif(1,0,1))/h_loc[1]
  } else {
    t_loc <- -log(stats::runif(1,0,1))/h_loc[2]
  }
  local_rec <- FALSE
  if (t_loc<t_death){
    t_death <- t_loc -log(stats::runif(1,0,1))/h_loc_death
    local_rec <- TRUE
  }
  dist_rec <- FALSE
  t_dist_met <- -log(stats::runif(1,0,1))/h_dist_met
  if (t_dist_met < t_death) {dist_rec <- TRUE}
  if (local_rec){
    x$d_pd <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt) + t_loc ,origin="1970-01-01")
    temp <- list(c('CT','PET-CT','MR','Klinik alene'),c(1,2,3,4))
    x$pdskan <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(4))]
    temp <- list(c('Nej','Ja'),c(0,1))
    if (x$pdskan=='Klinik alene' || x$pdskan==4) {
      x$pd_klin <- temp[[labelindex]][2]
    } else {
      x$pd_klin <- temp[[labelindex]][1]
    }

    index <- ceiling((5*stats::runif(3,0,1)))
    if ((index[1]==1 || index[1]==5) && (index[2]==1 || index[2]==5) && (index[3]==1 || index[3]==5)){
      index[1]==2
    }
    tempfield <- list(c('Nej','I felt','Udenfor felt','Begge','Kan ikke afgøres'),
                      c(0,1,2,3,9))
    x$pd_pul <- tempfield[[labelindex]][index[1]]
    if (x$pd_pul!='Nej' && x$pd_pul!=0){
      x$d_pd_pul <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_loc,origin="1970-01-01")
      temp <- list(c('Nej','Ja'),c(0,1))
      x$pd_pul_biopsi <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(1/4))]
      if (x$pd_pul_biopsi=='Ja'){
        x$d_pd_pul_biopsi  <- as.Date(as.numeric(x$d_pd_pul)+stats::runif(1,7,14),origin="1970-01-01")
      }
    }
    x$pd_med <- tempfield[[labelindex]][index[2]]
    if (x$pd_med!='Nej' && x$pd_med!=0){
      #if also progressive disease in pulm use the same date to force some of these event to occur at the same time point
      if (x$pd_pul!='Nej' && x$pd_pul!=0){
        x$d_pd_med <-x$d_pd_pul
      } else {
        x$d_pd_med <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_loc,origin="1970-01-01")
      }
      temp <- list(c('Nej','Ja'),c(0,1))
      x$pd_med_biopsi <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(1/4))]
      if (x$pd_med_biopsi=='Ja' || x$pd_med_biopsi==1){
        #There is a mistake in the current version of the database so this field is not a date as i should
        #The following line is the one that should be used when the mistake is corrected. Also remember to correct in the lines below in which it is set to NA
        #x$d_pd_med_biopsi  <- as.Date(as.numeric(x$d_pd_med)+stats::runif(1,7,14),origin="1970-01-01")
        x$d_pd_med_biopsi <- x$pd_med_biopsi
      }
    }
    x$pd_scl <- tempfield[[labelindex]][index[3]]
    if (x$pd_scl!='Nej' && x$pd_scl!=0){
      x$d_pd_scl <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_loc,origin="1970-01-01")
      temp <- list(c('Nej','Ja'),c(0,1))
      x$pd_scl_biopsi <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^(1/4))]
      if (x$pd_scl_biopsi=='Ja' || x$pd_scl_biopsi==0){
        x$d_pd_scl_biopsi  <- as.Date(as.numeric(x$d_pd_scl)+stats::runif(1,7,14),origin="1970-01-01")
      }
    }
  } else {
    #No recurrence or progressive disease
    temp <- list(c(as.character(NA)),c(as.integer(NA)))
    x$d_pd <- as.Date(NA)
    x$pd_pul <- temp[[labelindex]]
    x$pdskan <- temp[[labelindex]]
    x$pd_klin <- temp[[labelindex]]
    x$pd_pul <- temp[[labelindex]]
    x$d_pd_pul <- as.Date(NA)
    x$pd_pul_biopsi <- temp[[labelindex]]
    x$d_pd_pul_biopsi <- as.Date(NA)
    x$pd_med <- temp[[labelindex]]
    x$d_pd_med <- as.Date(NA)
    x$pd_med_biopsi <- temp[[labelindex]]
    #This line need to be correct then the date mistake in the datbase is corrected.
    #x$d_pd_med_biopsi <- as.Date(NA)
    x$d_pd_med_biopsi <- temp[[labelindex]]
    x$pd_scl <- temp[[labelindex]]
    x$d_pd_scl <- as.Date(NA)
    x$pd_scl_biopsi <- temp[[labelindex]]
    x$d_pd_scl_biopsi <- as.Date(NA)
  }
  index <- ceiling(stats::runif(4,0,2))
  if (sum(index)==4){index[1]=2}
  if (dist_rec){
    temp <- list(c('Nej','Ja'),c(0,1))
    x$pd_cns <- temp[[labelindex]][index[1]]
    if (x$pd_cns=='Ja' || x$pd_cns==1){
      x$d_pd_cns <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_dist_met,origin ="1970-01-01")
      temp2 <- list(c('Nej','Ja'),c(0,1))
      x$pd_cns2_biopsi <-  temp2[[labelindex]][ceiling(length(temp2[[labelindex]])*(stats::runif(1,0,1)))]
      if (x$pd_cns2_biopsi=='Ja' || x$pd_cns2_biopsi==1){
        x$d_pd_cns_biopsi <- as.Date(as.numeric(x$d_pd_cns) +stats::runif(1,7,14),origin ="1970-01-01")
      }
    }
    x$pd_hep <- temp[[labelindex]][index[2]]
    if (x$pd_hep=='Ja' || x$pd_hep==1){
      x$d_pd_hep <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_dist_met,origin ="1970-01-01")
      temp2 <- list(c('Nej','Ja'),c(0,1))
      x$pd_hep_biopsi <-  temp2[[labelindex]][ceiling(length(temp2[[labelindex]])*(stats::runif(1,0,1)))]
      if (x$pd_hep_biopsi=='Ja'){
        x$d_pd_hep_biopsi <- as.Date(as.numeric(x$d_pd_hep) +stats::runif(1,7,14),origin ="1970-01-01")
      }
    }
    x$pd_bon <- temp[[labelindex]][index[3]]
    if (x$pd_bon=='Ja'){
      x$d_pd_bon <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_dist_met,origin ="1970-01-01")
      temp2 <- list(c('Nej','Ja'),c(0,1))
      x$pd_bon_biopsi <-  temp2[[labelindex]][ceiling(length(temp2[[labelindex]])*(stats::runif(1,0,1)))]
      if (x$pd_bon_biopsi=='Ja'){
        x$d_pd_bon_biopsi <- as.Date(as.numeric(x$d_pd_bon) +stats::runif(1,7,14),origin ="1970-01-01")
      }
    }
    x$pd_skin <- temp[[labelindex]][index[4]]
    if (x$pd_skin=='Ja' || x$pd_skin==1){
      x$d_pd_skin <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_dist_met,origin ="1970-01-01")
      temp2 <- list(c('Nej','Ja'),c(0,1))
      x$pd_skin_biopsi <-  temp2[[labelindex]][ceiling(length(temp2[[labelindex]])*(stats::runif(1,0,1)))]
      if (x$pd_skin_biopsi=='Ja' || x$pd_skin_biopsi==1){
        x$d_pd_skin_biopsi <- as.Date(as.numeric(x$d_pd_skin) +stats::runif(1,7,14),origin ="1970-01-01")
      }
    }
  } else {
    temp2 <- list(c('Nej','Ja'),c(0,1))
    x$pd_cns <- temp2[[labelindex]][1]
    x$pd_hep <- temp2[[labelindex]][1]
    x$pd_bon <- temp2[[labelindex]][1]
    x$pd_skin <- temp2[[labelindex]][1]
  }
  temp <- list(c('Nej','Ja'),c(0,1))
  x$pd_and <- temp[[labelindex]][1]
  x$d_mors <- as.Date(as.numeric(ptdata$FirstFollowup$d_rt)+t_death,origin ="1970-01-01")
  temp <- list(c('PD','Akut toksicitet','Sen toksicitet','PD+ toksicitet','Hjertesygdom','Anden cancer','Uoplyst'),
               c(1,2,3,4,5,6,9))
  x$mors <-  temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1)))]
  return(x)
}

form_follow_up <- function(ptdata,uselabel=TRUE,followupmonth=9){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  followupdate_num <- as.numeric(ptdata$FirstFollowup$d_rt)+followupmonth*365.25/12
  deathdate_num <- as.numeric(ptdata$Haendelse$d_mors)
  templist <- list("d_pd","d_pd_cns","d_pd_hep","d_pd_scl","d_pd_skin")
  recurencedate_num <- vector()
  recurence <- FALSE
  for (i in templist){
    if (! is.null(ptdata$Haendelse[[(i)]])){
      if (!is.na(ptdata$Haendelse[[(i)]])){
        recurencedate_num<-append(recurencedate_num,as.numeric(ptdata$Haendelse[[(i)]]))
        recurence <- TRUE
      }
    }
  }
  if (recurence){
    recurencedate_num <- min(recurencedate_num)
  } else{
    recurencedate_num <- as.numeric(as.Date("3000-12-12",origin="1970-01-01"))
  }
  if (followupdate_num<deathdate_num){
    x$fu_mdr <- followupmonth
    x$fev1_fu <- round(stats::runif(1,1,3),digits=2)
    x$fvc_fu <- round(stats::runif(1,2,4),digits=2)
    x$dlcoref_fu <- floor(stats::runif(1,30,80))
    temp <- list(c('Ja','Nej'),c(1,0))
    if (recurencedate_num < followupdate_num){
      x$recidiv <- temp[[labelindex]][1]
    } else {
      x$recidiv <- temp[[labelindex]][2]
    }

  }
  return(x)
}

form_bivirkning_fu <- function(uselabel=TRUE){
  labelindex <- 2
  if (uselabel){labelindex <- 1}
  x <- list()
  temp <- list(c('Nej','Ja'),c(0,1))
  x$smoking_fu <- temp[[labelindex]][ceiling(0.7*length(temp[[labelindex]])*stats::runif(1,0,1))]
  if (x$smoking_fu=='Ja'){x$n_cigarettes_fu <- round(stats::runif(1,10,30),digits=0) }
  temp <- list(c('0','1','2','3','4'),c(0,1,2,3,4))
  x$ps_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$fatigue_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$cough_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$dyspnoe_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$diare_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$nausea_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$vomiting_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$dysphagia_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$pain_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$sens_neuropati_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$infection_fu <- temp[[labelindex]][ceiling(length(temp[[labelindex]])*(stats::runif(1,0,1))^10)]
  x$other_tox1_fu <- temp[[labelindex]][1]
  x$other_tox2_fu <- temp[[labelindex]][1]
  x$other_tox3_fu <- temp[[labelindex]][1]
  return(x)
}

GenerateOnePtData <- function(id=1,uselabel=TRUE){
  ptdata <- list()
  ptdata$Registration <- form_registrering(id,uselabel=uselabel)
  ptdata$Registration <- c(ptdata$Registration,form_inklusion(uselabel=uselabel))
  ptdata$Registration <- c(ptdata$Registration,form_onstudy_skema(ptdata,uselabel=uselabel))
  ptdata$Registration$redcap_event_name <- 'registration_arm_1'

  tempdata<-ptdata
  FirstFollowup <- form_rt_slut(tempdata,uselabel=uselabel)
  tempdata$FirstFollowup <- FirstFollowup
  FirstFollowup <- c(form_kemoterapi_under_rt(tempdata,uselabel=uselabel),FirstFollowup)
  StartRTDate_num <- as.numeric(FirstFollowup$d_rt)

  Haendelser <- form_durvalumab(tempdata,uselabel=uselabel)
  Haendelser <- c(Haendelser,form_recidivmors(tempdata,uselabel=uselabel))
  DeathDate_num <- as.numeric(Haendelser$d_mors)
  index <- c(1:8)
  for (i in index){
    if ((i*7+StartRTDate_num)<DeathDate_num){
      weekname <- paste('Uge',as.character(i),sep='')
      ptdata[[weekname]] <- form_bivirkningsskema_strlebehandling(uselabel=uselabel)
      ptdata[[weekname]] <- c(ptdata[[weekname]],form_blodprver_under_strlebehandling())
      ptdata[[weekname]]$redcap_event_name <- paste('uge_',as.character(i),'_arm_1',sep='')
    }
  }

  ptdata$FirstFollowup <- FirstFollowup
  ptdata$FirstFollowup$redcap_event_name <- '1_followup_arm_1'
  ptdata$Haendelser <- Haendelser
  ptdata$Haendelser$redcap_event_name <- 'haendelser_arm_1'

  index <- c(3,6,9,12,15,18,21,24,30,36,42,48,54,60)
  for (i in index){
    if ((i*365.25/12+StartRTDate_num)<DeathDate_num){
      monthname <- paste('Month',as.character(i),sep='')
      ptdata[[monthname]] <- form_follow_up(ptdata=ptdata,uselabel=uselabel,followupmonth=i)
      ptdata[[monthname]] <- c(ptdata[[monthname]],form_bivirkning_fu(uselabel=uselabel))
      ptdata[[monthname]]$redcap_event_name <- paste(as.character(i),'_mdr_followup_arm_1',sep='')
    }
  }
  return(ptdata)
}

GeneratePtCohortData <- function(nrpts=350,uselabel=TRUE,firstid=1){
  CohortData <- list()
  for (i in seq_len(nrpts)){
    x <- GenerateOnePtData(id=i+(firstid-1),uselabel=uselabel)
    CohortData[[i]] <- x
  }
  return(CohortData)
}

MakeCohortToDataFrame <- function(CohortData,firstid=1){
  #Start by finding all Field names
  #Loop pts
  patient_id <- firstid
  redcap_event_name <- 'demo'
  dfempty<-data.table::data.table(patient_id,redcap_event_name)
  for (i in seq_len(length(CohortData))){
    ptdata <- CohortData[[i]]
    #Loop event_names
    for (j in names(ptdata)){
      eventdata <- ptdata[[j]]
      for (k in names(eventdata)){
        if (!is.element(k,names(dfempty))){
          dfempty[[k]] <- eventdata[[k]]
        }
      }
    }
  }
  dfempty[1,] <- NA
  dftotal<-dfempty[-1,]
  #Now input the data
  nrrows <- 0
  for (i in seq_along(CohortData)){
    nrrows <- nrrows + sum(sapply(CohortData[[i]],length))
  }
  #dftotal <-dftotal[1:nrrows]
  #Loop pts
  counter <- 1L
  for (i in seq_len(length(CohortData))){

    #ptdata <- CohortData[[i]]
    ptid <- CohortData[[i]]$Registration$patient_id
    #Loop event_names
    for (j in names(CohortData[[i]])){
      #eventdata <- CohortData[[i]][[j]]
      dftemp <- dfempty
      for (k in names(CohortData[[i]][[j]])){
        dftemp[[k]] <- CohortData[[i]][[j]][[k]]
      }
      dftemp$patient_id <- ptid
      if (counter > nrow(dftotal)){
        if (counter == 1){
          dftotal <- dftemp
        } else {
          #double the size of dftotal such that it easy to add new values by indexing
          #before returning potential extra rows with NA is removed
          dftotal<-rbind(dftotal,dftotal)
          dftotal[counter:nrow(dftotal),] <- NA
        }
      }
      #The next line commented out would be the normal assignment but replaced wiht the set function due to time performance
      #dftotal[counter,] <- dftemp[1,]
      data.table::set(dftotal,counter,1:ncol(dftotal),as.list(dftemp))
      counter <- counter +1L

    }
  }
  dftotal <- dftotal[1:(counter-1L),]
  return(dftotal)
}

WriteToRedCapFormat <- function(df,filename){
  ChangeFomat1decimalplace <- c('hgb','leu','neu','k','hgb_rt','leu_rt','neu_rt','k_rt')
  ChangeFomat2decimalplace <- c('fev1','fvc','dlco','gy_t_mean','gy_n_mean','gy_lung_mean','vol_gtv_t','vol_gtv_n','fev1_fu','fvc_fu')
  for (i in ChangeFomat1decimalplace){
    if (!is.null(df[[i]])){
      index <- is.na(df[[i]])
      df[[i]]<-format(df[[i]],nsmall=1)
      df[[i]][index] <- as.character(NA)
    }
  }
  for (i in ChangeFomat2decimalplace){
    if (!is.null(df[[i]])){
      index <- is.na(df[[i]])
      df[[i]]<-format(df[[i]],nsmall=2)
      df[[i]][index] <- as.character(NA)
    }
  }
  #Remove age from data since it is a calculated field which RedCap will calcluate internally. Supplying it will result in a warning
  df$age <- NULL
  utils::write.table(df,filename, row.names = FALSE,na='',quote=FALSE,sep=',')
}
#DemoData(nrpts=2000,uselabels=FALSE,seed=42,filepath='c:/home/cab/temp/Narlal_2000_pts.csv')

