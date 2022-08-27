#' Load csv file from the clinical trial Narlal2
#' @description 'LoadAndPrepareData'reads the data exported from the Narlal database (e.g. the included demo data)
#' @param filename path to the cvs file that contains all the data from the NARLAL datbase
#'
#' @return data.frame that contain all thhe data from the NARLAL database
#' @export LoadAndPrepareData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
LoadAndPrepareData <- function(filename='ExternalData/DemoData.csv'){
  data <- readr::read_csv(filename)

  Hmisc::label(data$patient_id)="Patient ID"
  Hmisc::label(data$redcap_event_name)="Event Name"
  Hmisc::label(data$cpr_nummer)="CPR nummer "
  Hmisc::label(data$initials)="Patientens Initialer"
  Hmisc::label(data$d_registrering)="Registreringsdato"
  Hmisc::label(data$center)="Center"
  Hmisc::label(data$birthday)="Fødselsdag"
  Hmisc::label(data$gender)="Køn"
  Hmisc::label(data$age)="Alder (år)"
  Hmisc::label(data$registrering_complete)="Complete?"
  Hmisc::label(data$stadium)="NSCLC Stadium"
  Hmisc::label(data$histology_squamous)="Histologi (Squamous/non-squamous)"
  Hmisc::label(data$age18)="1. Alder >= 18"
  Hmisc::label(data$stadium_inkl)="2. NSCLC stadie IIB til IIIB"
  Hmisc::label(data$perform)="3. Performance status 0 eller 1"
  Hmisc::label(data$able_curative_rt)="4. I stand til at gennemføre kurativ strålebehandling"
  Hmisc::label(data$able_followup)="5. I stand til at følge Follow Up"
  Hmisc::label(data$antikonception)="6. Antikonception"
  Hmisc::label(data$informed_consent)="7. Informeret samtykke"
  Hmisc::label(data$mulig_plan)="8. Plan mulig til 66 Gy"
  Hmisc::label(data$system_lidelse)="9. Systemisk lidelse"
  Hmisc::label(data$nasal_ilt)="10. Nasal ilt"
  Hmisc::label(data$thorakal_rt)="11.Tidligere thorakal strålebehandling"
  Hmisc::label(data$active_cancer)="12. Aktiv anden cancer"
  Hmisc::label(data$oral_medicin)="13. Oral medicin ikke mulig"
  Hmisc::label(data$active_ulcus)="14. Aktivt ulcus"
  Hmisc::label(data$lactation)="15. Ammende kvinde"
  Hmisc::label(data$randomisering)="Randomisering"
  Hmisc::label(data$inkl_doctor)="Patienten er inkluderet af (lægens navn)"
  Hmisc::label(data$inklusion_complete)="Complete?"
  Hmisc::label(data$d_diag)="Diagnose dato"
  Hmisc::label(data$hist)="Histologi"
  Hmisc::label(data$t)="TNM -T "
  Hmisc::label(data$n)="TNM - N"
  Hmisc::label(data$std)="Klinisk stadie"
  Hmisc::label(data$slung)="Syge side"
  Hmisc::label(data$weight)="Weight (kilograms)"
  Hmisc::label(data$height)="Height (cm)"
  Hmisc::label(data$ps)="Performance status"
  Hmisc::label(data$fev1)="Lungefunktion: FEV1"
  Hmisc::label(data$fvc)="Lungefunktion: FVC "
  Hmisc::label(data$dlcoref)="Lungefunktion: DLCO%"
  Hmisc::label(data$dlco)="Lungefunktion DLCO "
  Hmisc::label(data$n_neocis)="Neoadjuverende Cisplatin - antal serier"
  Hmisc::label(data$n_neocis_aarsag)="Årsag til mere end 1 serie"
  Hmisc::label(data$mg_neocis)="Neoadjuverende Cisplatin - total mg"
  Hmisc::label(data$d_neocis)="Neoadjuverende Cisplatin - dato start"
  Hmisc::label(data$n_neocarbo)="Neadjuverende Carboplatin - antal serier"
  Hmisc::label(data$n_neocarbo_aarsag)="Årsag til mere end 1 serie"
  Hmisc::label(data$mg_neocarbo)="Neoadjuverende Carboplatin - total mg"
  Hmisc::label(data$d_neocarbo)="Neoadjuverende Carboplatin - dato start"
  Hmisc::label(data$carbo_aarsag)="Årsag til valg af carboplatin"
  Hmisc::label(data$n_neonavelbine)="Neoadjuverende Navelbine oral - antal serier"
  Hmisc::label(data$mg_neonavel)="Neoadjuverende Navelbine oral - total mg"
  Hmisc::label(data$d_neonavel)="Neoadjuverende Navelbine oral - dato start"
  Hmisc::label(data$n_neovino)="Neoadjuverende Vinorelbine i.v. - antal serier"
  Hmisc::label(data$mg_neovino)="Neoadjuverende Vinorelbine i.v. - total mg"
  Hmisc::label(data$d_neovino)="Neoadjuverende Vinorelbine i.v. - dato start"
  Hmisc::label(data$hgb)="Hæmoglobin"
  Hmisc::label(data$leu)="Leukocytter"
  Hmisc::label(data$neu)="Neutrofile"
  Hmisc::label(data$tro)="Trombocyter"
  Hmisc::label(data$na)="Natrium"
  Hmisc::label(data$k)="Kalium"
  Hmisc::label(data$alb)="Albumin"
  Hmisc::label(data$creat_b)="Creatinine (mg/dL eller umol/L)"
  Hmisc::label(data$basp)="Basisk phosphatase"
  Hmisc::label(data$alat)="ALAT"
  Hmisc::label(data$ldh)="LDH"
  Hmisc::label(data$bili)="Bilirubin"
  Hmisc::label(data$onstudy_skema_complete)="Complete?"
  Hmisc::label(data$smoking)="Ryger patienten aktuelt"
  Hmisc::label(data$n_cigarettes)="Cigaretforbrug per dag"
  Hmisc::label(data$ps_rt)="Performance status"
  Hmisc::label(data$fatigue_rt)="Træthed"
  Hmisc::label(data$cough_rt)="Hoste"
  Hmisc::label(data$dyspnoe_rt)="Dyspnø"
  Hmisc::label(data$constipation_rt)="Optipation"
  Hmisc::label(data$nausea_rt)="Kvalme"
  Hmisc::label(data$vomiting_rt)="Opkastning"
  Hmisc::label(data$dysphagia_rt)="Dysfagi"
  Hmisc::label(data$pain_rt)="Smerter"
  Hmisc::label(data$sens_neuropati_rt)="Sensorisk neuropati"
  Hmisc::label(data$infection_rt)="Infektion"
  Hmisc::label(data$diaria_rt)="Diare"
  Hmisc::label(data$skinreaction_rt)="Hudreaktion i strålefelt"
  Hmisc::label(data$ototox_rt)="Ototoksicitet"
  Hmisc::label(data$other_tox1_rt)="Anden toksicitet 1"
  Hmisc::label(data$other_tox1_rt_type)="Anden toksitetstype"
  Hmisc::label(data$other_tox2_rt)="Anden toksicitet 2"
  Hmisc::label(data$other_tox2_rt_type)="Anden toksicitetstype 2"
  Hmisc::label(data$other_tox3_rt)="Anden toksicitet 3"
  Hmisc::label(data$other_tox3_rt_type)="Anden toksicitetstype 3"
  Hmisc::label(data$bivirkningsskema_strlebehandling_complete)="Complete?"
  Hmisc::label(data$hgb_rt)="Hæmoglobin under strålebehandling"
  Hmisc::label(data$leu_rt)="Leukocytter under strålebehandling"
  Hmisc::label(data$neu_rt)="Neutrofile under strålebehandling"
  Hmisc::label(data$tro_rt)="Trombocyter under strålebehandling"
  Hmisc::label(data$na_rt)="Natrium under strålebehandling"
  Hmisc::label(data$k_rt)="Kalium under strålebehandling"
  Hmisc::label(data$alb_rt)="Albumin under strålebehandling"
  Hmisc::label(data$creat_rt)="Creatinine (mikromol/L) under strålebehandling"
  Hmisc::label(data$basp_rt)="Basisk phosphatase under strålebehandling"
  Hmisc::label(data$alat_rt)="ALAT under strålebehandling"
  Hmisc::label(data$ldh_rt)="LDH under strålebehandling"
  Hmisc::label(data$bili_rt)="Bilirubin under strålebehandling"
  Hmisc::label(data$blodprver_under_strlebehandling_complete)="Complete?"
  Hmisc::label(data$n_cis_rt)="Cisplatin antal gange"
  Hmisc::label(data$cis_rt_mg)="Cisplatin under RT  mg"
  Hmisc::label(data$cis_rt_dosis)="Cisplatin dosisreduktion (inkl. ændring til Carboplatin)"
  Hmisc::label(data$d_cis_rt)="Cisplatin under RT start"
  Hmisc::label(data$d_cis_rtsl)="Cisplatin under RT slut"
  Hmisc::label(data$carbo_rt_mg)="Carboplatin under RT mg"
  Hmisc::label(data$n_carbo_rt)="Carboplatin antal gange"
  Hmisc::label(data$carbo_rt_dosis)="Carboplatin dosisreduktion"
  Hmisc::label(data$d_carbo_rt)="Carboplatin under RT start"
  Hmisc::label(data$d_carbo_rtsl)="Carboplatin under RT slut"
  Hmisc::label(data$n_nav_rt)="Navelbine under RT antal gange"
  Hmisc::label(data$nav_rt_mg)="Navelbine under RT mg"
  Hmisc::label(data$nav_rt_dosis)="Navelbine dosisreduktion"
  Hmisc::label(data$d_nav_rt)="Navelbine under RT start"
  Hmisc::label(data$d_nav_rtsl)="Navelbine under RT slut"
  Hmisc::label(data$n_and_rt_type)="Anden kemoterapi under RT type * PROTOKOL VIOLATION *"
  Hmisc::label(data$n_and_rt)="Anden kemoterapi under RT antal gange"
  Hmisc::label(data$and_rt_mg)="Anden kemoterapi under RT mg"
  Hmisc::label(data$d_and_rt)="Anden kemoterapi under RT start"
  Hmisc::label(data$d_and_rtsl)="Anden kemoterapi under RT slut"
  Hmisc::label(data$n_and2_rt_type)="Anden kemoterapi 2 under RT type * PROTOKOL VIOLATION *"
  Hmisc::label(data$n_and2_rt2)="Anden kemoterapi 2 under RT antal gange"
  Hmisc::label(data$and2_rt_mg)="Anden kemoterapi 2 under RT mg"
  Hmisc::label(data$d_and2_rt)="Anden kemoterapi 2 under RT start"
  Hmisc::label(data$d_and2_rtsl)="Anden kemoterapi 2 under RT slut"
  Hmisc::label(data$n_and3_rt_type)="Anden kemoterapi 3 under RT type 3 * PROTOKOL VIOLATION *"
  Hmisc::label(data$n_and3_rt)="Anden kemoterapi 3 under RT antal gange"
  Hmisc::label(data$and3_rt_mg)="Anden kemoterapi 3 under RT mg"
  Hmisc::label(data$d_and3_rt)="Anden kemoterapi 3 under RT start"
  Hmisc::label(data$d_and3_rtsl)="Anden kemoterapi 3 under RT slut"
  Hmisc::label(data$no_kemo_rt_hemotol)="Cisplatin ændring: Hæmatologisk årsag"
  Hmisc::label(data$no_kemo_rt_kvl)="Cisplatin ændring: Kvalme/opkastninger årsag"
  Hmisc::label(data$no_kemo_rt_ear)="Cisplatin ændring: Hørelse/tinitus årsag"
  Hmisc::label(data$no_kemo_rt_neuro)="Cisplatin ændring: Neuropati årsag"
  Hmisc::label(data$no_kemo_rt_esofagit)="Cisplatin ændring: Esofagitis årsag"
  Hmisc::label(data$no_kemo_rt_lunge2_8f7)="Cisplatin ændring: Lungetox årsag"
  Hmisc::label(data$no_kemo_rt_and_rt_tox)="Cisplatin ændring: Anden RT tox årsag"
  Hmisc::label(data$no_kemo_rt_pt)="Cisplatin ændring: Patientønske årsag"
  Hmisc::label(data$no_kemo_rt_nefro)="Cisplatin ændring: Nefrotox årsag"
  Hmisc::label(data$no_kemo_rt_at)="Cisplatin ændring: Almentilstand årsag"
  Hmisc::label(data$no_kemo_rt_and)="Cisplatin ændring: Anden årsag 1"
  Hmisc::label(data$no_kemo_rt_and_type)="Cisplatin ændring: Anden årsag 1 (beskriv)"
  Hmisc::label(data$no_kemo_rt_and2)="Cisplatin ændring: Anden årsag 2"
  Hmisc::label(data$no_kemo_rt_and_type2)="Cisplatin ændring: Anden årsag 2 (beskriv)"
  Hmisc::label(data$no_carbo_rt_hemotol)="Carboplatin ændring: Hæmatologisk årsag"
  Hmisc::label(data$no_carbo_rt_kvl)="Carboplatin ændring: Kvalme/opkastninger årsag"
  Hmisc::label(data$no_carbo_rt_ear)="Carboplatin ændring: Hørelse/tinitus årsag"
  Hmisc::label(data$no_carbo_rt_neuro)="Carboplatin ændring: Neuropati årsag"
  Hmisc::label(data$no_carbo_rt_esofagit)="Carboplatin ændring: Esofagitis årsag"
  Hmisc::label(data$no_carbo_rt_lunge2_8f7)="Carboplatin ændring: Lungetox årsag"
  Hmisc::label(data$no_carbo_rt_and_rt_tox)="Carboplatin ændring: Anden RT tox årsag"
  Hmisc::label(data$no_carbo_rt_pt)="Carboplatin ændring: Patientønske årsag"
  Hmisc::label(data$no_carbo_rt_at)="Carboplatin ændring: Almentilstand årsag"
  Hmisc::label(data$no_carbo_rt_and)="Carboplatin ændring: Anden årsag 1"
  Hmisc::label(data$no_carbo_rt_and_type)="Carboplatin ændring: Anden årsag 1 (beskriv)"
  Hmisc::label(data$no_carbo_rt_and2)="Carboplatin ændring: Anden årsag 2"
  Hmisc::label(data$no_carbo_rt_and_type2)="Carboplatin ændring: Anden årsag 2 (beskriv)"
  Hmisc::label(data$no_nav_rt_hemotol)="Navelbine ændring: Hæmatologisk årsag"
  Hmisc::label(data$no_navel_rt_kvl)="Navelbine ændring: Kvalme/opkastninger årsag"
  Hmisc::label(data$no_nav_rt_neuro)="Navelbine ændring: Neuropati årsag"
  Hmisc::label(data$no_nav_rt_esofagit)="Navelbine ændring: Esofagitis årsag"
  Hmisc::label(data$no_nav_rt_lunge2_8f7)="Navelbine ændring: Lungetox årsag"
  Hmisc::label(data$no_nav_rt_and_rt_tox)="Navelbine ændring: Anden RT tox årsag"
  Hmisc::label(data$no_nav_rt_pt)="Navelbine ændring: Patientønske årsag"
  Hmisc::label(data$no_nav_rt_at)="Navelbine ændring: Almentilstand årsag"
  Hmisc::label(data$no_nav_rt_and)="Navelbine ændring: Anden årsag 1"
  Hmisc::label(data$no_nav_rt_and_type)="Navelbine ændring: Anden årsag 1 (beskriv)"
  Hmisc::label(data$no_nav_rt_and2)="Navelbine ændring: Anden årsag 2"
  Hmisc::label(data$no_nav_rt_and_type2)="Navelbine ændring: Anden årsag 2 (beskriv)"
  Hmisc::label(data$kemoterapi_under_rt_complete)="Complete?"
  Hmisc::label(data$d_rt)="Start på strålebehandling"
  Hmisc::label(data$d_rtsl)="Slut på strålebehandling"
  Hmisc::label(data$gy_t_mean)="Middeldosis GTV-T"
  Hmisc::label(data$gy_n_mean)="Middeldosis GTV-N"
  Hmisc::label(data$fx)="Fraktioner"
  Hmisc::label(data$gy_lung_mean)="Middeldosis lunge"
  Hmisc::label(data$d_rt_plan2)="Start på plan 2"
  Hmisc::label(data$plan2_fx)="Fraktion plan 2 start"
  Hmisc::label(data$replan_2_aarsag)="Replan 2 årsag"
  Hmisc::label(data$d_rt_plan3)="Start på plan 3"
  Hmisc::label(data$plan3_fx3)="Fraktion plan 3 start"
  Hmisc::label(data$replan_3_aarsag)="Replan 3 årsag"
  Hmisc::label(data$d_rt_plan4)="Start på plan 4"
  Hmisc::label(data$plan4_fx)="Fraktion plan 4 start"
  Hmisc::label(data$replan_4_aarsag)="Replan 4 årsag"
  Hmisc::label(data$gtv_dfn)="GTV definition"
  Hmisc::label(data$vol_gtv_t)="Volumen for GTV-T"
  Hmisc::label(data$vol_gtv_n)="Volumen for GTV-N"
  Hmisc::label(data$gtv_t1_pet_suv)="GTV-T1 PET SUV peak"
  Hmisc::label(data$gtv_t2_pet_suv)="GTV-T2 PET SUV peak"
  Hmisc::label(data$gtv_t3_pet_suv)="GTV-T3 PET SUV peak"
  Hmisc::label(data$gtv_n1_pet_suv)="GTV-N1 PET SUV peak"
  Hmisc::label(data$gtv_n2_pet_suv)="GTV-N2 PET SUV peak"
  Hmisc::label(data$gtv_n3_pet_suv)="GTV-N3 PET SUV peak"
  Hmisc::label(data$gtv_n4_pet_suv)="GTV-N4 PET SUV peak"
  Hmisc::label(data$gtv_n5_pet_suv)="GTV-N5 PET SUV peak"
  Hmisc::label(data$gtv_n6_pet_suv)="GTV-N6 PET SUV peak"
  Hmisc::label(data$gtv_kongl_pet_suv)="GTV-Konglomerat PET SUV peak"
  Hmisc::label(data$comments_rt_planning)="Komentarer til planlægning og udførelse af RT "
  Hmisc::label(data$cirro_export)="Planer eksport til CIRRO"
  Hmisc::label(data$rt_slut_complete)="Complete?"
  Hmisc::label(data$durvalumab)="Durvalumab after study RT"
  Hmisc::label(data$pdl1)="PD-L1"
  Hmisc::label(data$d_durvalumab)="Durvalumab start"
  Hmisc::label(data$d_durvalumab_end)="Date of ending durvalumab"
  Hmisc::label(data$durvalumab_stop)="Durvalumab discontinued"
  Hmisc::label(data$durvalumab_stop_reason)="Reason for discontinueing"
  Hmisc::label(data$durvalumab_complete)="Complete?"
  Hmisc::label(data$fu_mdr)="Follow Up Måned"
  Hmisc::label(data$fev1_fu)="Lungefunktion: FEV1 i FU"
  Hmisc::label(data$fvc_fu)="Lungefunktion: FVC i FU"
  Hmisc::label(data$dlcoref_fu)="Lungefunktion: DLCO%  i FU"
  Hmisc::label(data$recidiv)="Recidiv"
  Hmisc::label(data$follow_up_complete)="Complete?"
  Hmisc::label(data$smoking_fu)="Ryger patienten aktuelt i FU"
  Hmisc::label(data$n_cigarettes_fu)="Cigaretforbrug per dag i FU"
  Hmisc::label(data$ps_fu)="Performance status i FU"
  Hmisc::label(data$fatigue_fu)="Træthed i FU"
  Hmisc::label(data$cough_fu)="Hoste i FU"
  Hmisc::label(data$dyspnoe_fu)="Dyspnø i FU"
  Hmisc::label(data$diare_fu)="Diare i FU"
  Hmisc::label(data$nausea_fu)="Kvalme i FU"
  Hmisc::label(data$vomiting_fu)="Opkastning i FU"
  Hmisc::label(data$dysphagia_fu)="Dysfagi i FU"
  Hmisc::label(data$pain_fu)="Smerter i FU"
  Hmisc::label(data$sens_neuropati_fu)="Sensorisk neuropati i FU"
  Hmisc::label(data$infection_fu)="Infektion i FU"
  Hmisc::label(data$other_tox1_fu)="Anden toksicitet 1 i FU"
  Hmisc::label(data$other_tox1_fu_type)="Anden toksitetstype 1 i FU"
  Hmisc::label(data$other_tox2_fu)="Anden toksicitet 2 i FU"
  Hmisc::label(data$other_tox2_fu_type)="Anden toksicitetstype 2 i FU"
  Hmisc::label(data$other_tox3_fu)="Anden toksicitet 3 i FU"
  Hmisc::label(data$other_tox3_fu_type)="Anden toksicitetstype 3 i FU"
  Hmisc::label(data$bivirkning_fu_complete)="Complete?"
  Hmisc::label(data$pd_chemo1)="Kemoterapi 1. linje"
  Hmisc::label(data$d_pd_chemo1)="Dato for kemoterapi 1. linje"
  Hmisc::label(data$pd_chemo1_type)="Kemoterapi 1. linje type"
  Hmisc::label(data$pd_chemo2)="Kemoterapi 2. linje"
  Hmisc::label(data$d_pd_chemo2)="Dato for kemoterapi 2. linje"
  Hmisc::label(data$pd_chemo2_type)="Kemoterapi 2. linje type"
  Hmisc::label(data$pd_chemo3)="Kemoterapi 3. linje"
  Hmisc::label(data$d_pd_chemo3)="Dato for kemoterapi 3. linje"
  Hmisc::label(data$pd_chemo3_type)="Kemoterapi 3. linje type"
  Hmisc::label(data$pd_biol)="Biologisk behandling for recidiv"
  Hmisc::label(data$d_pd_biol)="Dato for biologisk behandling for recidiv"
  Hmisc::label(data$pd_biol_type)="Biologisk behandling for recidiv type"
  Hmisc::label(data$pd_surg)="Kirurgisk behandling for recidiv"
  Hmisc::label(data$d_pd_surg)="Dato for kirurgisk behandling for recidiv"
  Hmisc::label(data$pd_surg_lok)="Kirurgisk behandling for recidiv - lokalisation"
  Hmisc::label(data$pd_rt1)="Strålebehandling for recidiv"
  Hmisc::label(data$pd_rt1_type)="Strålebehandling for recidiv- lokalisation"
  Hmisc::label(data$d_pd_rt1)="Dato for strålebehandling for recidiv"
  Hmisc::label(data$pd_rt2)="Strålebehandling for recidiv"
  Hmisc::label(data$d_pd_rt2)="Dato for strålebehandling for recidiv"
  Hmisc::label(data$pd_rt2_type)="Strålebehandling for recidiv- lokalisation"
  Hmisc::label(data$recidiv_behandling_complete)="Complete?"
  Hmisc::label(data$d_pd)="Recidiv /PD dato"
  Hmisc::label(data$pdskan)="Recidiv/PD skaningstype "
  Hmisc::label(data$pd_klin)="Recidiv/PD klinisk vurdering alene "
  Hmisc::label(data$pd_pul)="PD i lunge"
  Hmisc::label(data$d_pd_pul)="Dato for PD i lunge"
  Hmisc::label(data$pd_pul_biopsi)="Biopsi af PD i lunge"
  Hmisc::label(data$d_pd_pul_biopsi)="Dato for biopsi af PD i lunge"
  Hmisc::label(data$pd_med)="PD i mediastinum"
  Hmisc::label(data$d_pd_med)="Dato for PD i mediastinum"
  Hmisc::label(data$pd_med_biopsi)="Biopsi af PD i mediastinum"
  Hmisc::label(data$d_pd_med_biopsi)="Dato for biopsi af PD i mediastinum"
  Hmisc::label(data$pd_scl)="PD i supraklavt"
  Hmisc::label(data$d_pd_scl)="Dato for PD i supraklavt"
  Hmisc::label(data$pd_scl_biopsi)="Biopsi af PD i supraklavt"
  Hmisc::label(data$d_pd_scl_biopsi)="Dato for biopsi af PD i supraklavt"
  Hmisc::label(data$pd_cns)="Recidiv i hjerne"
  Hmisc::label(data$d_pd_cns)="Dato for recidiv i hjerne"
  Hmisc::label(data$pd_cns2_biopsi)="Biopsi af recidiv i hjerne"
  Hmisc::label(data$d_pd_cns_biopsi)="Dato for biopsi af recidiv i hjerne"
  Hmisc::label(data$pd_hep)="Recidiv i lever"
  Hmisc::label(data$d_pd_hep)="Dato for recidiv i lever"
  Hmisc::label(data$pd_hep_biopsi)="Biopsi af recidiv i lever"
  Hmisc::label(data$d_pd_hep_biopsi)="Dato for biopsi af recidiv i lever"
  Hmisc::label(data$pd_bon)="Recidiv i knogle"
  Hmisc::label(data$d_pd_bon)="Dato for recidiv i knogle"
  Hmisc::label(data$pd_bon_biopsi)="Biopsi af recidiv i knogle"
  Hmisc::label(data$d_pd_bon_biopsi)="Dato for biopsi af recidiv i knogle"
  Hmisc::label(data$pd_skin)="Recidiv i hud"
  Hmisc::label(data$d_pd_skin)="Dato for recidiv i hud"
  Hmisc::label(data$pd_skin_biopsi)="Biopsi af recidiv i hud"
  Hmisc::label(data$d_pd_skin_biopsi)="Dato for biopsi af recidiv i hud"
  Hmisc::label(data$pd_and)="Recidiv andetsteds"
  Hmisc::label(data$pd_and_note)="Recidiv andetsteds lokalisation "
  Hmisc::label(data$d_pd_and)="Dato for recidiv i andetsteds"
  Hmisc::label(data$pd_and2_a09)="Biopsi af recidiv i andetsteds"
  Hmisc::label(data$d_pd_and_biopsi)="Dato for biopsi af recidiv i andetsteds"
  Hmisc::label(data$d_mors)="Dato for død"
  Hmisc::label(data$mors)="Dødsårsag"
  Hmisc::label(data$mors_and)="Dødsårsag anden/specificer"
  Hmisc::label(data$recidivmors_complete)="Complete?"
  #Setting Units


  #Setting Factors(will create new variable for factors)
  data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("registration_arm_1","uge_1_arm_1","uge_2_arm_1","uge_3_arm_1","uge_4_arm_1","uge_5_arm_1","uge_6_arm_1","uge_7_arm_1","uge_8_arm_1","1_followup_arm_1","haendelser_arm_1","3_mdr_followup_arm_1","6_mdr_followup_arm_1","9_mdr_followup_arm_1","12_mdr_followup_arm_1","15_mdr_followup_arm_1","18_mdr_followup_arm_1","21_mdr_followup_arm_1","24_mdr_followup_arm_1","30_mdr_followup_arm_1","36_mdr_followup_arm_1","42_mdr_followup_arm_1","48_mdr_followup_arm_1","54_mdr_followup_arm_1","60_mdr_followup_arm_1"))
  data$center.factor = factor(data$center,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
  data$gender.factor = factor(data$gender,levels=c("1","2"))
  data$registrering_complete.factor = factor(data$registrering_complete,levels=c("0","1","2"))
  data$stadium.factor = factor(data$stadium,levels=c("0","1","2","3","4","5","6","7","8","9"))
  data$histology_squamous.factor = factor(data$histology_squamous,levels=c("1","2"))
  data$age18.factor = factor(data$age18,levels=c("1","0"))
  data$stadium_inkl.factor = factor(data$stadium_inkl,levels=c("1","0"))
  data$perform.factor = factor(data$perform,levels=c("1","0"))
  data$able_curative_rt.factor = factor(data$able_curative_rt,levels=c("1","0"))
  data$able_followup.factor = factor(data$able_followup,levels=c("1","0"))
  data$antikonception.factor = factor(data$antikonception,levels=c("1","0"))
  data$informed_consent.factor = factor(data$informed_consent,levels=c("1","0"))
  data$mulig_plan.factor = factor(data$mulig_plan,levels=c("1","0"))
  data$system_lidelse.factor = factor(data$system_lidelse,levels=c("1","0"))
  data$nasal_ilt.factor = factor(data$nasal_ilt,levels=c("1","0"))
  data$thorakal_rt.factor = factor(data$thorakal_rt,levels=c("1","0"))
  data$active_cancer.factor = factor(data$active_cancer,levels=c("1","0"))
  data$oral_medicin.factor = factor(data$oral_medicin,levels=c("1","0"))
  data$active_ulcus.factor = factor(data$active_ulcus,levels=c("1","0"))
  data$lactation.factor = factor(data$lactation,levels=c("1","0"))
  data$randomisering.factor = factor(data$randomisering,levels=c("1","2"))
  data$inklusion_complete.factor = factor(data$inklusion_complete,levels=c("0","1","2"))
  data$hist.factor = factor(data$hist,levels=c("1","2","3","4","5","6","7","8"))
  data$t.factor = factor(data$t,levels=c("1","2","3","4","5","6","7","8","9"))
  data$n.factor = factor(data$n,levels=c("0","1","2","3"))
  data$std.factor = factor(data$std,levels=c("0","1","2","3","4","5","6","7","8","9"))
  data$slung.factor = factor(data$slung,levels=c("1","2","3","4"))
  data$ps.factor = factor(data$ps,levels=c("0","1","2","3","4","5","6","7","8","9"))
  data$onstudy_skema_complete.factor = factor(data$onstudy_skema_complete,levels=c("0","1","2"))
  data$smoking.factor = factor(data$smoking,levels=c("0","1"))
  data$ps_rt.factor = factor(data$ps_rt,levels=c("0","1","2","3","4"))
  data$fatigue_rt.factor = factor(data$fatigue_rt,levels=c("0","1","2","3","4","5"))
  data$cough_rt.factor = factor(data$cough_rt,levels=c("0","1","2","3","4","5"))
  data$dyspnoe_rt.factor = factor(data$dyspnoe_rt,levels=c("0","1","2","3","4","5"))
  data$constipation_rt.factor = factor(data$constipation_rt,levels=c("0","1","2","3","4","5"))
  data$nausea_rt.factor = factor(data$nausea_rt,levels=c("0","1","2","3","4","5"))
  data$vomiting_rt.factor = factor(data$vomiting_rt,levels=c("0","1","2","3","4","5"))
  data$dysphagia_rt.factor = factor(data$dysphagia_rt,levels=c("0","1","2","3","4","5"))
  data$pain_rt.factor = factor(data$pain_rt,levels=c("0","1","2","3","4","5"))
  data$sens_neuropati_rt.factor = factor(data$sens_neuropati_rt,levels=c("0","1","2","3","4","5"))
  data$infection_rt.factor = factor(data$infection_rt,levels=c("0","1","2","3","4","5"))
  data$diaria_rt.factor = factor(data$diaria_rt,levels=c("0","1","2","3","4","5"))
  data$skinreaction_rt.factor = factor(data$skinreaction_rt,levels=c("0","1","2","3","4","5"))
  data$ototox_rt.factor = factor(data$ototox_rt,levels=c("0","1","2","3","4"))
  data$other_tox1_rt.factor = factor(data$other_tox1_rt,levels=c("0","1","2","3","4","5"))
  data$other_tox2_rt.factor = factor(data$other_tox2_rt,levels=c("0","1","2","3","4","5"))
  data$other_tox3_rt.factor = factor(data$other_tox3_rt,levels=c("0","1","2","3","4","5"))
  data$bivirkningsskema_strlebehandling_complete.factor = factor(data$bivirkningsskema_strlebehandling_complete,levels=c("0","1","2"))
  data$blodprver_under_strlebehandling_complete.factor = factor(data$blodprver_under_strlebehandling_complete,levels=c("0","1","2"))
  data$cis_rt_dosis.factor = factor(data$cis_rt_dosis,levels=c("1","2","3"))
  data$carbo_rt_dosis.factor = factor(data$carbo_rt_dosis,levels=c("1","2","3"))
  data$nav_rt_dosis.factor = factor(data$nav_rt_dosis,levels=c("1","2","3"))
  data$no_kemo_rt_hemotol.factor = factor(data$no_kemo_rt_hemotol,levels=c("0","1"))
  data$no_kemo_rt_kvl.factor = factor(data$no_kemo_rt_kvl,levels=c("0","1"))
  data$no_kemo_rt_ear.factor = factor(data$no_kemo_rt_ear,levels=c("0","1"))
  data$no_kemo_rt_neuro.factor = factor(data$no_kemo_rt_neuro,levels=c("0","1"))
  data$no_kemo_rt_esofagit.factor = factor(data$no_kemo_rt_esofagit,levels=c("0","1"))
  data$no_kemo_rt_lunge2_8f7.factor = factor(data$no_kemo_rt_lunge2_8f7,levels=c("0","1"))
  data$no_kemo_rt_and_rt_tox.factor = factor(data$no_kemo_rt_and_rt_tox,levels=c("0","1"))
  data$no_kemo_rt_pt.factor = factor(data$no_kemo_rt_pt,levels=c("0","1"))
  data$no_kemo_rt_nefro.factor = factor(data$no_kemo_rt_nefro,levels=c("0","1"))
  data$no_kemo_rt_at.factor = factor(data$no_kemo_rt_at,levels=c("0","1"))
  data$no_kemo_rt_and.factor = factor(data$no_kemo_rt_and,levels=c("0","1"))
  data$no_kemo_rt_and2.factor = factor(data$no_kemo_rt_and2,levels=c("0","1"))
  data$no_carbo_rt_hemotol.factor = factor(data$no_carbo_rt_hemotol,levels=c("0","1"))
  data$no_carbo_rt_kvl.factor = factor(data$no_carbo_rt_kvl,levels=c("0","1"))
  data$no_carbo_rt_ear.factor = factor(data$no_carbo_rt_ear,levels=c("0","1"))
  data$no_carbo_rt_neuro.factor = factor(data$no_carbo_rt_neuro,levels=c("0","1"))
  data$no_carbo_rt_esofagit.factor = factor(data$no_carbo_rt_esofagit,levels=c("0","1"))
  data$no_carbo_rt_lunge2_8f7.factor = factor(data$no_carbo_rt_lunge2_8f7,levels=c("0","1"))
  data$no_carbo_rt_and_rt_tox.factor = factor(data$no_carbo_rt_and_rt_tox,levels=c("0","1"))
  data$no_carbo_rt_pt.factor = factor(data$no_carbo_rt_pt,levels=c("0","1"))
  data$no_carbo_rt_at.factor = factor(data$no_carbo_rt_at,levels=c("0","1"))
  data$no_carbo_rt_and.factor = factor(data$no_carbo_rt_and,levels=c("0","1"))
  data$no_carbo_rt_and2.factor = factor(data$no_carbo_rt_and2,levels=c("0","1"))
  data$no_nav_rt_hemotol.factor = factor(data$no_nav_rt_hemotol,levels=c("0","1"))
  data$no_navel_rt_kvl.factor = factor(data$no_navel_rt_kvl,levels=c("0","1"))
  data$no_nav_rt_neuro.factor = factor(data$no_nav_rt_neuro,levels=c("0","1"))
  data$no_nav_rt_esofagit.factor = factor(data$no_nav_rt_esofagit,levels=c("0","1"))
  data$no_nav_rt_lunge2_8f7.factor = factor(data$no_nav_rt_lunge2_8f7,levels=c("0","1"))
  data$no_nav_rt_and_rt_tox.factor = factor(data$no_nav_rt_and_rt_tox,levels=c("0","1"))
  data$no_nav_rt_pt.factor = factor(data$no_nav_rt_pt,levels=c("0","1"))
  data$no_nav_rt_at.factor = factor(data$no_nav_rt_at,levels=c("0","1"))
  data$no_nav_rt_and.factor = factor(data$no_nav_rt_and,levels=c("0","1"))
  data$no_nav_rt_and2.factor = factor(data$no_nav_rt_and2,levels=c("0","1"))
  data$kemoterapi_under_rt_complete.factor = factor(data$kemoterapi_under_rt_complete,levels=c("0","1","2"))
  data$replan_2_aarsag.factor = factor(data$replan_2_aarsag,levels=c("1","2","3","4","5","6","7"))
  data$replan_3_aarsag.factor = factor(data$replan_3_aarsag,levels=c("1","2","3","4","5","6","7"))
  data$replan_4_aarsag.factor = factor(data$replan_4_aarsag,levels=c("1","2","3","4","5","6","7"))
  data$gtv_dfn.factor = factor(data$gtv_dfn,levels=c("1","2"))
  data$gtv_t1_pet_suv.factor = factor(data$gtv_t1_pet_suv,levels=c("1","2","3","4"))
  data$gtv_t2_pet_suv.factor = factor(data$gtv_t2_pet_suv,levels=c("1","2","3","4"))
  data$gtv_t3_pet_suv.factor = factor(data$gtv_t3_pet_suv,levels=c("1","2","3","4"))
  data$gtv_n1_pet_suv.factor = factor(data$gtv_n1_pet_suv,levels=c("1","2","3","4"))
  data$gtv_n2_pet_suv.factor = factor(data$gtv_n2_pet_suv,levels=c("1","2","3","4"))
  data$gtv_n3_pet_suv.factor = factor(data$gtv_n3_pet_suv,levels=c("1","2","3","4"))
  data$gtv_n4_pet_suv.factor = factor(data$gtv_n4_pet_suv,levels=c("1","2","3","4"))
  data$gtv_n5_pet_suv.factor = factor(data$gtv_n5_pet_suv,levels=c("1","2","3","4"))
  data$gtv_n6_pet_suv.factor = factor(data$gtv_n6_pet_suv,levels=c("1","2","3","4"))
  data$gtv_kongl_pet_suv.factor = factor(data$gtv_kongl_pet_suv,levels=c("1","2","3","4"))
  data$cirro_export.factor = factor(data$cirro_export,levels=c("0","1"))
  data$rt_slut_complete.factor = factor(data$rt_slut_complete,levels=c("0","1","2"))
  data$durvalumab.factor = factor(data$durvalumab,levels=c("1","0"))
  data$durvalumab_stop_reason.factor = factor(data$durvalumab_stop_reason,levels=c("1","2","3"))
  data$durvalumab_complete.factor = factor(data$durvalumab_complete,levels=c("0","1","2"))
  data$recidiv.factor = factor(data$recidiv,levels=c("0","1"))
  data$follow_up_complete.factor = factor(data$follow_up_complete,levels=c("0","1","2"))
  data$smoking_fu.factor = factor(data$smoking_fu,levels=c("0","1"))
  data$ps_fu.factor = factor(data$ps_fu,levels=c("0","1","2","3","4"))
  data$fatigue_fu.factor = factor(data$fatigue_fu,levels=c("0","1","2","3","4","5"))
  data$cough_fu.factor = factor(data$cough_fu,levels=c("0","1","2","3","4","5"))
  data$dyspnoe_fu.factor = factor(data$dyspnoe_fu,levels=c("0","1","2","3","4","5"))
  data$diare_fu.factor = factor(data$diare_fu,levels=c("0","1","2","3","4","5"))
  data$nausea_fu.factor = factor(data$nausea_fu,levels=c("0","1","2","3","4","5"))
  data$vomiting_fu.factor = factor(data$vomiting_fu,levels=c("0","1","2","3","4","5"))
  data$dysphagia_fu.factor = factor(data$dysphagia_fu,levels=c("0","1","2","3","4","5"))
  data$pain_fu.factor = factor(data$pain_fu,levels=c("0","1","2","3","4","5"))
  data$sens_neuropati_fu.factor = factor(data$sens_neuropati_fu,levels=c("0","1","2","3","4","5"))
  data$infection_fu.factor = factor(data$infection_fu,levels=c("0","1","2","3","4","5"))
  data$other_tox1_fu.factor = factor(data$other_tox1_fu,levels=c("0","1","2","3","4","5"))
  data$other_tox2_fu.factor = factor(data$other_tox2_fu,levels=c("0","1","2","3","4","5"))
  data$other_tox3_fu.factor = factor(data$other_tox3_fu,levels=c("0","1","2","3","4","5"))
  data$bivirkning_fu_complete.factor = factor(data$bivirkning_fu_complete,levels=c("0","1","2"))
  data$pd_chemo1.factor = factor(data$pd_chemo1,levels=c("0","1"))
  data$pd_chemo2.factor = factor(data$pd_chemo2,levels=c("0","1"))
  data$pd_chemo3.factor = factor(data$pd_chemo3,levels=c("0","1"))
  data$pd_biol.factor = factor(data$pd_biol,levels=c("0","1"))
  data$pd_surg.factor = factor(data$pd_surg,levels=c("0","1"))
  data$pd_rt1.factor = factor(data$pd_rt1,levels=c("0","1"))
  data$pd_rt2.factor = factor(data$pd_rt2,levels=c("0","1"))
  data$recidiv_behandling_complete.factor = factor(data$recidiv_behandling_complete,levels=c("0","1","2"))
  data$pdskan.factor = factor(data$pdskan,levels=c("1","2","3","4"))
  data$pd_klin.factor = factor(data$pd_klin,levels=c("0","1"))
  data$pd_pul.factor = factor(data$pd_pul,levels=c("0","1","2","3","9"))
  data$pd_pul_biopsi.factor = factor(data$pd_pul_biopsi,levels=c("0","1"))
  data$pd_med.factor = factor(data$pd_med,levels=c("0","1","2","3","9"))
  data$pd_med_biopsi.factor = factor(data$pd_med_biopsi,levels=c("0","1"))
  data$d_pd_med_biopsi.factor = factor(data$d_pd_med_biopsi,levels=c("0","1"))
  data$pd_scl.factor = factor(data$pd_scl,levels=c("0","1","2","3","9"))
  data$pd_scl_biopsi.factor = factor(data$pd_scl_biopsi,levels=c("0","1"))
  data$pd_cns.factor = factor(data$pd_cns,levels=c("0","1"))
  data$pd_cns2_biopsi.factor = factor(data$pd_cns2_biopsi,levels=c("0","1"))
  data$pd_hep.factor = factor(data$pd_hep,levels=c("0","1"))
  data$pd_hep_biopsi.factor = factor(data$pd_hep_biopsi,levels=c("0","1"))
  data$pd_bon.factor = factor(data$pd_bon,levels=c("0","1"))
  data$pd_bon_biopsi.factor = factor(data$pd_bon_biopsi,levels=c("0","1"))
  data$pd_skin.factor = factor(data$pd_skin,levels=c("0","1"))
  data$pd_skin_biopsi.factor = factor(data$pd_skin_biopsi,levels=c("0","1"))
  data$pd_and.factor = factor(data$pd_and,levels=c("0","1"))
  data$pd_and2_a09.factor = factor(data$pd_and2_a09,levels=c("0","1"))
  data$mors.factor = factor(data$mors,levels=c("1","2","3","4","5","6","7","9"))
  data$recidivmors_complete.factor = factor(data$recidivmors_complete,levels=c("0","1","2"))

  levels(data$redcap_event_name.factor)=c("Registration","Uge 1","Uge 2","Uge 3","Uge 4","Uge 5","Uge 6","Uge 7","Uge 8","1. Followup","Haendelser","3 mdr followup","6 mdr followup","9 mdr followup","12 mdr followup","15 mdr followup","18 mdr followup","21 mdr followup","24 mdr followup","30 mdr followup","36 mdr followup","42 mdr followup","48 mdr followup","54 mdr followup","60 mdr followup")
  levels(data$center.factor)=c("RH,","Herlev","Næstved","OUH","Aarhus","Vejle","Aalborg","Radiumhospitalet","Trondheim","Tromsoe","Ullevål","12")
  levels(data$gender.factor)=c("Mand","Kvinde")
  levels(data$registrering_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$stadium.factor)=c("0","IA","IB","IIA","IIB","IIIA","IIIB","IV","Recidiv","Uoplyst")
  levels(data$histology_squamous.factor)=c("Squamous","Non-squamous")
  levels(data$age18.factor)=c("Ja","Nej")
  levels(data$stadium_inkl.factor)=c("Ja","Nej")
  levels(data$perform.factor)=c("Ja","Nej")
  levels(data$able_curative_rt.factor)=c("Ja","Nej")
  levels(data$able_followup.factor)=c("Ja","Nej")
  levels(data$antikonception.factor)=c("Ja","Nej")
  levels(data$informed_consent.factor)=c("Ja","Nej")
  levels(data$mulig_plan.factor)=c("Ja","Nej")
  levels(data$system_lidelse.factor)=c("Ja","Nej")
  levels(data$nasal_ilt.factor)=c("Ja","Nej")
  levels(data$thorakal_rt.factor)=c("Ja","Nej")
  levels(data$active_cancer.factor)=c("Ja","Nej")
  levels(data$oral_medicin.factor)=c("Ja","Nej")
  levels(data$active_ulcus.factor)=c("Ja","Nej")
  levels(data$lactation.factor)=c("Ja","Nej")
  levels(data$randomisering.factor)=c("Standard","Eskaleret")
  levels(data$inklusion_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$hist.factor)=c("Planocellulær","Adenocarcinom","Adenosquamøs","Udifferentieret","-","NSCLC NOS","-","Andet")
  levels(data$t.factor)=c("T0","T1","T1A","T1B","T2","T2A","T2B","T3","T4")
  levels(data$n.factor)=c("0","1","2","3")
  levels(data$std.factor)=c("0","IA","IB","IIA","IIB","IIIA","IIIB","IV","Recidiv","Uoplyst")
  levels(data$slung.factor)=c("Højre","Venstre","Undifferentieret","Mediastinalt")
  levels(data$ps.factor)=c("0","1","2","3","4","-","-","-","-","Uoplyst")
  levels(data$onstudy_skema_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$smoking.factor)=c("Nej","Ja")
  levels(data$ps_rt.factor)=c("0","1","2","3","4")
  levels(data$fatigue_rt.factor)=c("0","1","2","3","4","5")
  levels(data$cough_rt.factor)=c("0","1","2","3","4","5")
  levels(data$dyspnoe_rt.factor)=c("0","1","2","3","4","5")
  levels(data$constipation_rt.factor)=c("0","1","2","3","4","5")
  levels(data$nausea_rt.factor)=c("0","1","2","3","4","5")
  levels(data$vomiting_rt.factor)=c("0","1","2","3","4","5")
  levels(data$dysphagia_rt.factor)=c("0","1","2","3","4","5")
  levels(data$pain_rt.factor)=c("0","1","2","3","4","5")
  levels(data$sens_neuropati_rt.factor)=c("0","1","2","3","4","5")
  levels(data$infection_rt.factor)=c("0","1","2","3","4","5")
  levels(data$diaria_rt.factor)=c("0","1","2","3","4","5")
  levels(data$skinreaction_rt.factor)=c("0","1","2","3","4","5")
  levels(data$ototox_rt.factor)=c("0","1","2","3","4")
  levels(data$other_tox1_rt.factor)=c("0","1","2","3","4","5")
  levels(data$other_tox2_rt.factor)=c("0","1","2","3","4","5")
  levels(data$other_tox3_rt.factor)=c("0","1","2","3","4","5")
  levels(data$bivirkningsskema_strlebehandling_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$blodprver_under_strlebehandling_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$cis_rt_dosis.factor)=c("Ingen dosisreduktion","75% dosis","Seponeret")
  levels(data$carbo_rt_dosis.factor)=c("Ingen dosisreduktion (AUC4)","AUC3","Seponeret")
  levels(data$nav_rt_dosis.factor)=c("Ingen dosisreduktion (100%)","80%","Seponeret")
  levels(data$no_kemo_rt_hemotol.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_kvl.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_ear.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_neuro.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_esofagit.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_lunge2_8f7.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_and_rt_tox.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_pt.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_nefro.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_at.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_and.factor)=c("Nej","Ja")
  levels(data$no_kemo_rt_and2.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_hemotol.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_kvl.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_ear.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_neuro.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_esofagit.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_lunge2_8f7.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_and_rt_tox.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_pt.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_at.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_and.factor)=c("Nej","Ja")
  levels(data$no_carbo_rt_and2.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_hemotol.factor)=c("Nej","Ja")
  levels(data$no_navel_rt_kvl.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_neuro.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_esofagit.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_lunge2_8f7.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_and_rt_tox.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_pt.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_at.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_and.factor)=c("Nej","Ja")
  levels(data$no_nav_rt_and2.factor)=c("Nej","Ja")
  levels(data$kemoterapi_under_rt_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$replan_2_aarsag.factor)=c("Atelektase","Pleuraeffusion","Pneumonitis","Normalvævs tox","Targetflytning","Beh.teknik/lejring","Andet")
  levels(data$replan_3_aarsag.factor)=c("Atelektase","Pleuraeffusion","Pneumonitis","Normalvævs tox","Targetflytning","Beh.teknik/lejring","Andet")
  levels(data$replan_4_aarsag.factor)=c("Atelektase","Pleuraeffusion","Pneumonitis","Normalvævs tox","Targetflytning","Beh.teknik/lejring","Andet")
  levels(data$gtv_dfn.factor)=c("Midt ventilation","MIP")
  levels(data$gtv_t1_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_t2_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_t3_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_n1_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_n2_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_n3_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_n4_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_n5_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_n6_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$gtv_kongl_pet_suv.factor)=c("30%","40%","50%","Udvidet")
  levels(data$cirro_export.factor)=c("Nej","Ja")
  levels(data$rt_slut_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$durvalumab.factor)=c("Yes","No")
  levels(data$durvalumab_stop_reason.factor)=c("Adverse event","Relapse","Other")
  levels(data$durvalumab_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$recidiv.factor)=c("Nej","Ja")
  levels(data$follow_up_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$smoking_fu.factor)=c("Nej","Ja")
  levels(data$ps_fu.factor)=c("0","1","2","3","4")
  levels(data$fatigue_fu.factor)=c("0","1","2","3","4","5")
  levels(data$cough_fu.factor)=c("0","1","2","3","4","5")
  levels(data$dyspnoe_fu.factor)=c("0","1","2","3","4","5")
  levels(data$diare_fu.factor)=c("0","1","2","3","4","5")
  levels(data$nausea_fu.factor)=c("0","1","2","3","4","5")
  levels(data$vomiting_fu.factor)=c("0","1","2","3","4","5")
  levels(data$dysphagia_fu.factor)=c("0","1","2","3","4","5")
  levels(data$pain_fu.factor)=c("0","1","2","3","4","5")
  levels(data$sens_neuropati_fu.factor)=c("0","1","2","3","4","5")
  levels(data$infection_fu.factor)=c("0","1","2","3","4","5")
  levels(data$other_tox1_fu.factor)=c("0","1","2","3","4","5")
  levels(data$other_tox2_fu.factor)=c("0","1","2","3","4","5")
  levels(data$other_tox3_fu.factor)=c("0","1","2","3","4","5")
  levels(data$bivirkning_fu_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$pd_chemo1.factor)=c("Nej","Ja")
  levels(data$pd_chemo2.factor)=c("Nej","Ja")
  levels(data$pd_chemo3.factor)=c("Nej","Ja")
  levels(data$pd_biol.factor)=c("Nej","Ja")
  levels(data$pd_surg.factor)=c("Nej","Ja")
  levels(data$pd_rt1.factor)=c("Nej","Ja")
  levels(data$pd_rt2.factor)=c("Nej","Ja")
  levels(data$recidiv_behandling_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$pdskan.factor)=c("CT","PET-CT","MR","Klinik alene")
  levels(data$pd_klin.factor)=c("Nej","Ja")
  levels(data$pd_pul.factor)=c("Nej","I felt","Udenfor felt","Begge","Kan ikke afgøres")
  levels(data$pd_pul_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_med.factor)=c("Nej","I felt","Udenfor felt","Begge","Kan ikke afgøres")
  levels(data$pd_med_biopsi.factor)=c("Nej","Ja")
  levels(data$d_pd_med_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_scl.factor)=c("Nej","I felt","Udenfor felt","Begge","Kan ikke afgøres")
  levels(data$pd_scl_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_cns.factor)=c("Nej","Ja")
  levels(data$pd_cns2_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_hep.factor)=c("Nej","Ja")
  levels(data$pd_hep_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_bon.factor)=c("Nej","Ja")
  levels(data$pd_bon_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_skin.factor)=c("Nej","Ja")
  levels(data$pd_skin_biopsi.factor)=c("Nej","Ja")
  levels(data$pd_and.factor)=c("Nej","Ja")
  levels(data$pd_and2_a09.factor)=c("Nej","Ja")
  levels(data$mors.factor)=c("PD,","Akut toksicitet","Sen toksicitet","PD+ toksicitet","Hjertesygdom","Anden cancer","Anden årsag","Uoplyst")
  levels(data$recidivmors_complete.factor)=c("Incomplete","Unverified","Complete")
  return(data)
}
