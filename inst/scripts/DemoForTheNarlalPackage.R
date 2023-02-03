#Due to the use of ChangeText (see documentation of the individual functions), the code below might need to be run several times with slightly different ChangeText as required for the specific plots, or ChangeText needs to be updated between calls as done in the current example

library(Narlal2)
file <- system.file('extdata','DemoData.csv',package="Narlal2")
NarlalData <- LoadAndPrepareData(filename=file)
PtChar <- ExtractPatientCharacteristic(NarlalData)
PtTox <- ExtractToxicityData(NarlalData)
PtSurv <- ExtractSurvivalData(NarlalData,12*5)

ChangeText<-c()
ChangeText$ChangeLabels <- c(
  y='Cumulative distribution',
  count='Differential distribution',
  `Time [months]`='Time since randomisation [months]',
  `Time [Months]`='Time since randomisation [months]'
)
ChangeText$ChangeVar <-c(
  arm='Treatment Arm',
  age='Age [years]',
  gender='Sex',
  Perform='Performance status',
  weight='Weight [kg]',
  height='Height [cm]',
  histology='Histology',
  Stadium='Stage',
  MeanDose_T='Mean dose to T [Gy]',
  MeanDose_N='Mean dose to N [Gy]',
  MeanLung='Mean lung dose [Gy]',
  Vol_T='Volume of T',
  Vol_N='Volume of N',
  fatigue='Fatigue',
  dysphagia='Dysphagia',
  ps='Performance status',
  n_nav_rt='No. Navelbine',
  n_cis_rt='No. Cisplatin',
  n_carbo_rt='No. Carboplatin',
  n_platin_rt='No. platin during RT',
  cough='Cough',
  infection='Infection',
  FEV1_percent='FEV1 % of expected',
  DLCO_percent='DLCO % of expected',
  fx_factor='No. RT fractions',
  smoking_baseline='Smoking at start RT',
  daysRT='No. days during RT'
)
ChangeText$ChangeLevels <- c(
  Standard='Standard',
  Eskaleret='Escalated',
  T1A='T1',
  T1B='T1',
  T2='T2',
  T2A='T2',
  T2B='T2',
  Mand='Male',
  Kvinde='Female',
  mors='Death',
  `local+met`='Simultaneous local and distant failure',
  local='Locale failure',
  met='Distant failure',
  Planocellulær='Squamous carcinoma',
  Adenocarcinom='Adeno carcinoma',
  Adenosquamøs='Adenosquamous carcinoma',
  Udifferentieret='NOS',
  `NSCLC NOS`='NOS',
  Andet='NOS',
  Recidiv='Recurrence',
  Nej='No',
  Ja='Yes'
)

VariablesInclInTox<-c()
VariablesInclInTox$During<-c('dysphagia','fatigue','cough','infection')
VariablesInclInTox$Early<-c('dysphagia','fatigue','cough','infection')
VariablesInclInTox$DuringAndEarly<-c('dysphagia','fatigue','cough','infection')
VariablesInclInTox$Late<-c('dysphagia','fatigue','cough','infection')

filepath<-tempdir()
#filepath='c:/home/cab/temp'
print(paste('Files are stored in: ',filepath,sep=''))
PlotPatientCharacteristic(PtChar,filepath=filepath,ChangeText=ChangeText)
PlotSurvivalData(PtSurv,filepath=filepath,nboot=2000,conf.int=.95,seed=42,ChangeText=ChangeText)

ChangeText$ChangeLevels <- c(
  Standard='Standard',
  Eskaleret='Escalated',
  `0`='0-2',
  `1`='0-2',
  `2`='0-2'
)
PlotToxicityData(PtTox,filepath=filepath,ChangeText=ChangeText,VariablesInclInTox=VariablesInclInTox)

