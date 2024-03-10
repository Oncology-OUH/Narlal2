#' Load csv file from the clinical trial Narlal2
#' @description 'LoadAndPrepareData' reads the data exported from the Narlal database (e.g. the included demo data)
#' @param filename path to the cvs file that contains all the data from the NARLAL database
#'
#' @return data.frame that contain all the data from the NARLAL database
#' @export LoadAndPrepareData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)

#Problem with danish letters in variables should be possible to be adresed
#using the method described in:
#https://stackoverflow.com/questions/43850229/is-it-possible-to-write-package-documentation-using-non-ascii-characters-with-ro
#This has not been implemented yet

LoadAndPrepareData <- function(filename='ExternalData/DemoData.csv'){
  #browser()
  data <- readr::read_csv(filename,show_col_types = FALSE,
                          col_types = list( "n_neocarbo_aarsag" = readr::col_character(),
                                            "othertox1_description"=readr::col_character(),
                                            "othertox2_description"=readr::col_character(),
                                            "othertox3_description"=readr::col_character(),
                                            "heart_late_retro_notes"=readr::col_character(),
                                            "lung_late_retro_notes"=readr::col_character(),
                                            "esoph_late_retro_notes"=readr::col_character(),
                                            "pain_late_retro_notes"=readr::col_character(),
                                            "bron_late_retro_notes"=readr::col_character(),
                                            "trachea_late_retro_notes"=readr::col_character(),
                                            "rtmyelo_late_retro_notes"=readr::col_character(),
                                            "cutis_late_retro_notes"=readr::col_character(),
                                            "heart_late_pro_notes"=readr::col_character(),
                                            "lung_late_pro_notes"=readr::col_character(),
                                            "esoph_late_pro_notes"=readr::col_character(),
                                            "pain_late_pro_notes"=readr::col_character(),
                                            "bron_late_pro_notes"=readr::col_character(),
                                            "trachea_late_pro_notes"=readr::col_character(),
                                            "rtmyelo_late_pro_notes"=readr::col_character(),
                                            "cutis_late_pro_notes"=readr::col_character(),
                                            "n_and_rt_type"=readr::col_character(),
                                            "other_imaging_modality"=readr::col_character(),
                                            "further_imaging_other_1"=readr::col_character(),
                                            "further_imaging_other_2"=readr::col_character(),
                                            "further_imaging_other_3"=readr::col_character(),
                                            "relapse_n_location_other"=readr::col_character(),
                                            "no_kemo_rt_and_type"=readr::col_character(),
                                            "no_kemo_rt_and_type2"=readr::col_character(),
                                            "no_carbo_rt_and_type"=readr::col_character(),
                                            "no_carbo_rt_and_type2"=readr::col_character(),
                                            "no_nav_rt_and_type"=readr::col_character(),
                                            "no_nav_rt_and_type2"=readr::col_character(),
                                            "replan_2_aarsag"=readr::col_character(),
                                            "replan_3_aarsag"=readr::col_character(),
                                            "replan_4_aarsag"=readr::col_character(),
                                            "pd_chemo1_type"=readr::col_character(),
                                            "pd_chemo2_type"=readr::col_character(),
                                            "pd_chemo3_type"=readr::col_character(),
                                            "other_tox1_rt_type"=readr::col_character(),
                                            "other_tox2_rt_type"=readr::col_character(),
                                            "other_tox3_rt_type"=readr::col_character(),
                                            "d_and2_rt"=readr::col_date(),
                                            "d_and2_rtsl"=readr::col_date(),
                                            "d_and3_rt"=readr::col_date(),
                                            "d_and3_rtsl"=readr::col_date(),
                                            "d_carbo_rt"=readr::col_date(),
                                            "d_carbo_rtsl"=readr::col_date(),
                                            "d_cis_rt"=readr::col_date(),
                                            "d_cis_rtsl"=readr::col_date(),
                                            "d_diag"=readr::col_date(),
                                            #"d_durvalumab"=readr::col_date(),
                                            #"d_durvalumab_end"=readr::col_date(),
                                            "d_mors"=readr::col_date(),
                                            "d_nav_rt"=readr::col_date(),
                                            "d_nav_rtsl"=readr::col_date(),
                                            "d_neocarbo"=readr::col_date(),
                                            "d_neocis"=readr::col_date(),
                                            "d_neonavel"=readr::col_date(),
                                            "d_neovino"=readr::col_date(),
                                            "d_pd"=readr::col_date(),
                                            "d_pd_and"=readr::col_date(),
                                            "d_pd_and_biopsi"=readr::col_date(),
                                            "d_pd_biol"=readr::col_date(),
                                            "d_pd_bon"=readr::col_date(),
                                            "d_pd_bon_biopsi"=readr::col_date(),
                                            "d_pd_chemo1"=readr::col_date(),
                                            "d_pd_chemo2"=readr::col_date(),
                                            "d_pd_chemo3"=readr::col_date(),
                                            "d_pd_cns"=readr::col_date(),
                                            "d_pd_cns_biopsi"=readr::col_date(),
                                            "d_pd_hep"=readr::col_date(),
                                            "d_pd_hep_biopsi"=readr::col_date(),
                                            "d_pd_med"=readr::col_date(),
                                            #"d_pd_med_biopsi"=readr::col_date(),
                                            "d_pd_pul"=readr::col_date(),
                                            "d_pd_pul_biopsi"=readr::col_date(),
                                            "d_pd_rt1"=readr::col_date(),
                                            "d_pd_rt2"=readr::col_date(),
                                            "d_pd_scl"=readr::col_date(),
                                            "d_pd_scl_biopsi"=readr::col_date(),
                                            "d_pd_skin"=readr::col_date(),
                                            "d_pd_skin_biopsi"=readr::col_date(),
                                            "d_pd_surg"=readr::col_date(),
                                            "d_registrering"=readr::col_date(),
                                            "d_rt"=readr::col_date(),
                                            "d_rt_plan2"=readr::col_date(),
                                            "d_rt_plan3"=readr::col_date(),
                                            "d_rt_plan4"=readr::col_date(),
                                            "d_rtsl"=readr::col_date(),
                                            "andrenal_biopsy_date"=readr::col_date(),
                                            "bone_biopsy_date"=readr::col_date(),
                                            "brain_biopsy_date"=readr::col_date(),
                                            "bron_late_retro_date"=readr::col_date(),
                                            "clinical_suspicion_date"=readr::col_date(),
                                            "contra_hilus_biopsy_date"=readr::col_date(),
                                            "contra_lung_biopsy_date"=readr::col_date(),
                                            "contra_supra_biopsy_date"=readr::col_date(),
                                            "cutis_biopsy_date"=readr::col_date(),
                                            "cutis_late_retro_date"=readr::col_date(),
                                            "date_of_imaging"=readr::col_date(),
                                            "diffhist_biopsy_date"=readr::col_date(),
                                            "emboli1_date"=readr::col_date(),
                                            "emboli2_date"=readr::col_date(),
                                            "emboli3_date"=readr::col_date(),
                                            "esoph_late_retro_date"=readr::col_date(),
                                            "followup_late_date"=readr::col_date(),
                                            "further_images_date_1"=readr::col_date(),
                                            "further_images_date_2"=readr::col_date(),
                                            "further_images_date_3"=readr::col_date(),
                                            "heart_late_retro_date"=readr::col_date(),
                                            "heart1_date"=readr::col_date(),
                                            "heart2_date"=readr::col_date(),
                                            "heart3_date"=readr::col_date(),
                                            "hemoptysis1_date"=readr::col_date(),
                                            "hemoptysis2_date"=readr::col_date(),
                                            "hemoptysis3_date"=readr::col_date(),
                                            "infection1_date"=readr::col_date(),
                                            "infection2_date"=readr::col_date(),
                                            "infection3_date"=readr::col_date(),
                                            "ipsi_hilus_biopsy_date"=readr::col_date(),
                                            "ipsi_supraclav_biopsy_date"=readr::col_date(),
                                            "liver_biopsy_date"=readr::col_date(),
                                            "lung_late_retro_date"=readr::col_date(),
                                            "mediastinal_biopsy_date"=readr::col_date(),
                                            "mult_lungmet_biopsy_date"=readr::col_date(),
                                            "neutropeni1_date"=readr::col_date(),
                                            "neutropeni2_date"=readr::col_date(),
                                            "neutropeni3_date"=readr::col_date(),
                                            "other_biopsy_date"=readr::col_date(),
                                            "otherlobe_biopsy_date"=readr::col_date(),
                                            "othertox1_date"=readr::col_date(),
                                            "othertox2_date"=readr::col_date(),
                                            "othertox3_date"=readr::col_date(),
                                            "pain_late_retro_date"=readr::col_date(),
                                            "pleura_efus_biopsy_date"=readr::col_date(),
                                            "pneumonitis_w_date"=readr::col_date(),
                                            "rtmyelo_late_retro_date"=readr::col_date(),
                                            "same_lobe_biopsy_date"=readr::col_date(),
                                            "trachea_late_retro_date"=readr::col_date(),
                                            "d_rt_plan2"=readr::col_date(),
                                            "d_rt_plan3"=readr::col_date(),
                                            "d_rt_plan4"=readr::col_date(),
                                            "bivirkning_fu_complete"=readr::col_integer(),
                                            "bivirkningsskema_strlebehandling_complete"=readr::col_integer(),
                                            "blodprver_under_strlebehandling_complete"=readr::col_integer(),
                                            "durvalumab_complete"=readr::col_integer(),
                                            "first_relapse_complete"=readr::col_integer(),
                                            "follow_up_complete"=readr::col_integer(),
                                            "image_data_clinical_suspicion_of_recurrence_complete"=readr::col_integer(),
                                            "inklusion_complete"=readr::col_integer(),
                                            "kemoterapi_under_rt_complete"=readr::col_integer(),
                                            "late_toxicity_complete"=readr::col_integer(),
                                            "onstudy_skema_complete"=readr::col_integer(),
                                            "recidiv_behandling_complete"=readr::col_integer(),
                                            "recidivmors_complete"=readr::col_integer(),
                                            "registrering_complete"=readr::col_integer(),
                                            "retrospective_toxicity_collection_complete"=readr::col_integer(),
                                            "rt_slut_complete"=readr::col_integer(),
                                            "sae_form_complete"=readr::col_integer(),
                                            "gtv_kongl_pet_suv"=readr::col_integer(),
                                            "gtv_n1_pet_suv"=readr::col_integer(),
                                            "gtv_n2_pet_suv"=readr::col_integer(),
                                            "gtv_n3_pet_suv"=readr::col_integer(),
                                            "gtv_n4_pet_suv"=readr::col_integer(),
                                            "gtv_n5_pet_suv"=readr::col_integer(),
                                            "gtv_n6_pet_suv"=readr::col_integer(),
                                            "gtv_t1_pet_suv"=readr::col_integer(),
                                            "gtv_t2_pet_suv"=readr::col_integer(),
                                            "gtv_t3_pet_suv"=readr::col_integer(),
                                            "bron_late_pro_degree"=readr::col_integer(),
                                            "bron_late_retro_degree"=readr::col_integer(),
                                            "cutis_late_pro_degree"=readr::col_integer(),
                                            "cutis_late_retro_degree"=readr::col_integer(),
                                            "emboli1_degree"=readr::col_integer(),
                                            "emboli2_degree"=readr::col_integer(),
                                            "emboli3_degree"=readr::col_integer(),
                                            "esoph_late_pro_degree"=readr::col_integer(),
                                            "esoph_late_retro_degree"=readr::col_integer(),
                                            "heart_late_pro_degree"=readr::col_integer(),
                                            "heart_late_retro_degree"=readr::col_integer(),
                                            "heart1_degree"=readr::col_integer(),
                                            "heart2_degree"=readr::col_integer(),
                                            "heart3_degree"=readr::col_integer(),
                                            "hemoptysis1_degree"=readr::col_integer(),
                                            "hemoptysis2_degree"=readr::col_integer(),
                                            "hemoptysis3_degree"=readr::col_integer(),
                                            "infection1_degree"=readr::col_integer(),
                                            "infection2_degree"=readr::col_integer(),
                                            "infection3_degree"=readr::col_integer(),
                                            "lung_late_pro_degree"=readr::col_integer(),
                                            "lung_late_retro_degree"=readr::col_integer(),
                                            "neutropeni1_degree"=readr::col_integer(),
                                            "neutropeni2_degree"=readr::col_integer(),
                                            "neutropeni3_degree"=readr::col_integer(),
                                            "othertox1_degree"=readr::col_integer(),
                                            "othertox2_degree"=readr::col_integer(),
                                            "othertox3_degree"=readr::col_integer(),
                                            "pain_late_pro_degree"=readr::col_integer(),
                                            "pain_late_retro_degree"=readr::col_integer(),
                                            "pneumonitis_w_degree"=readr::col_integer(),
                                            "rtmyelo_late_pro_degree"=readr::col_integer(),
                                            "rtmyelo_late_retro_degree"=readr::col_integer(),
                                            "trachea_late_pro_degree"=readr::col_integer(),
                                            "trachea_late_retro_degree"=readr::col_integer(),
                                            "bron_late_pro_related"=readr::col_integer(),
                                            "bron_late_retro_related"=readr::col_integer(),
                                            "cutis_late_pro_related"=readr::col_integer(),
                                            "cutis_late_retro_related"=readr::col_integer(),
                                            "emboli1_related"=readr::col_integer(),
                                            "emboli2_related"=readr::col_integer(),
                                            "emboli3_related"=readr::col_integer(),
                                            "esoph_late_pro_related"=readr::col_integer(),
                                            "esoph_late_retro_related"=readr::col_integer(),
                                            "heart_late_pro_related"=readr::col_integer(),
                                            "heart_late_retro_related"=readr::col_integer(),
                                            "heart1_related"=readr::col_integer(),
                                            "heart2_related"=readr::col_integer(),
                                            "heart3_related"=readr::col_integer(),
                                            "hemoptysis1_related"=readr::col_integer(),
                                            "hemoptysis2_related"=readr::col_integer(),
                                            "hemoptysis3_related"=readr::col_integer(),
                                            "infection1_related"=readr::col_integer(),
                                            "infection2_related"=readr::col_integer(),
                                            "infection3_related"=readr::col_integer(),
                                            "lung_late_pro_related"=readr::col_integer(),
                                            "lung_late_retro_related"=readr::col_integer(),
                                            "neutropeni1_related"=readr::col_integer(),
                                            "neutropeni2_related"=readr::col_integer(),
                                            "neutropeni3_related"=readr::col_integer(),
                                            "othertox1_related"=readr::col_integer(),
                                            "othertox2_related"=readr::col_integer(),
                                            "othertox3_related"=readr::col_integer(),
                                            "pain_late_pro_related"=readr::col_integer(),
                                            "pain_late_retro_related"=readr::col_integer(),
                                            "rtmyelo_late_pro_related"=readr::col_integer(),
                                            "rtmyelo_late_retro_related"=readr::col_integer(),
                                            "trachea_late_pro_related"=readr::col_integer(),
                                            "trachea_late_retro_related"=readr::col_integer(),
                                            "furter_images_modal_1"=readr::col_integer(),
                                            "furter_images_modal_2"=readr::col_integer(),
                                            "furter_images_modal_3"=readr::col_integer(),
                                            "reason_for_imaging"=readr::col_integer(),
                                            "same_lobe_biopsy_val"=readr::col_integer(),
                                            "mediastinal_biopsy_val"=readr::col_integer(),
                                            "ipsi_hilus_biopsy_val"=readr::col_integer(),
                                            "contra_hilus_biopsy_val"=readr::col_integer(),
                                            "ipsi_supraclav_biopsy_val"=readr::col_integer(),
                                            "diffhist_biopsy_val"=readr::col_integer(),
                                            "otherlobe_biopsy_val"=readr::col_integer(),
                                            "contra_lung_biopsy_val"=readr::col_integer(),
                                            "contra_supra_biopsy_val"=readr::col_integer(),
                                            "mult_lungmet_biopsy_val"=readr::col_integer(),
                                            "pleura_efus_biopsy_val"=readr::col_integer(),
                                            "brain_biopsy_val"=readr::col_integer(),
                                            "liver_biopsy_val"=readr::col_integer(),
                                            "andrenal_biopsy_val"=readr::col_integer(),
                                            "bone_biopsy_val"=readr::col_integer(),
                                            "cutis_biopsy_val"=readr::col_integer(),
                                            "other_biopsy_val"=readr::col_integer(),
                                            "plan2_fx"=readr::col_integer(),
                                            "plan3_fx3"=readr::col_integer(),
                                            "plan4_fx"=readr::col_integer()))
  #data <- utils::read.csv(filename)
  #browser()
  #problems(data)
  Hmisc::label(data$patient_id)="Patient ID"
  Hmisc::label(data$redcap_event_name)="Event Name"
  Hmisc::label(data$redcap_repeat_instrument)="Repeat Instrument"
  Hmisc::label(data$redcap_repeat_instance)="Repeat Instance"
  Hmisc::label(data$redcap_data_access_group)="Data Access Group"
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
  Hmisc::label(data$date_of_randomization)="Dato for randomisering"
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
  Hmisc::label(data$pack_years)="Pakkeår"
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
  Hmisc::label(data$fx_standard)="Number of fractions treated as the standard arm"
  Hmisc::label(data$fx_escalated)="Number of fractions treated as the escalated arm"
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
  #Hmisc::label(data$d_durvalumab)="Durvalumab start"
  Hmisc::label(data$date_durvalumab)="Durvalumab start"
  #Hmisc::label(data$d_durvalumab_end)="Date of ending durvalumab"
  Hmisc::label(data$date_durvalumab_end)="Date of ending durvalumab"
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
  Hmisc::label(data$reason_for_imaging)="Are this report based on:"
  Hmisc::label(data$clinical_suspicion_date)="Date of the clinical recurrence suspicion that requested futher investigation (thus likely different from date of imaging)"
  Hmisc::label(data$suspicion_images)="Was the clinical suspicion followed by medical imaging"
  Hmisc::label(data$date_of_imaging)="Date of imaging"
  Hmisc::label(data$imaging_rand_months)="Number of months from randomisation to imaging:"
  Hmisc::label(data$imaging_modality)="Modality of imaging"
  Hmisc::label(data$other_imaging_modality)="Specify other imaging modality"
  Hmisc::label(data$further_images)="Did the images result in further imaging to identify the possible recurrence"
  Hmisc::label(data$further_images_date_1)="Date of 1. additional image modality"
  Hmisc::label(data$furter_images_modal_1)="Modality of 1. additional image data"
  Hmisc::label(data$further_imaging_other_1)="Specify other imaging modality"
  Hmisc::label(data$further_images_date_2)="Date of 2. additional image modality"
  Hmisc::label(data$furter_images_modal_2)="Modality of 2. additional image data"
  Hmisc::label(data$further_imaging_other_2)="Specify other imaging modality"
  Hmisc::label(data$further_images_date_3)="Date of 3. additional image modality"
  Hmisc::label(data$furter_images_modal_3)="Modality of 3. additional image data"
  Hmisc::label(data$further_imaging_other_3)="Specify other imaging modality"
  Hmisc::label(data$any_recurrence)="Did the images identify a recurrence (both local and distant)"
  Hmisc::label(data$recurence_type___1)="What type of recurrence  was observed in the imaging (choice=Locoregional)"
  Hmisc::label(data$recurence_type___2)="What type of recurrence  was observed in the imaging (choice=Distant)"
  Hmisc::label(data$local_recurrence_location___1)="Location of the local recurrence (If recurrence is of clearly different histology than the primary tumour it is not scored as a local recurrence) (choice=Recurrence in the same lung lobe as the primary tumour)"
  Hmisc::label(data$local_recurrence_location___2)="Location of the local recurrence (If recurrence is of clearly different histology than the primary tumour it is not scored as a local recurrence) (choice=Recurrence within mediastinum)"
  Hmisc::label(data$local_recurrence_location___3)="Location of the local recurrence (If recurrence is of clearly different histology than the primary tumour it is not scored as a local recurrence) (choice=Recurrence in the ipsilateral hilus (relative to the primary tumour))"
  Hmisc::label(data$local_recurrence_location___4)="Location of the local recurrence (If recurrence is of clearly different histology than the primary tumour it is not scored as a local recurrence) (choice=Recurrence in the contralateral hilus (relative to the primary tumour))"
  Hmisc::label(data$local_recurrence_location___5)="Location of the local recurrence (If recurrence is of clearly different histology than the primary tumour it is not scored as a local recurrence) (choice=Recurrence in the ipsilateral supraclavicular region (relative to the primary tumour))"
  Hmisc::label(data$same_lobe_biopsy_val)="Was the recurrence in the same lung lobe as the primary tumour biopsy validated subsequently"
  Hmisc::label(data$same_lobe_biopsy_date)="Date of biopsy validation of same lung lobe as the primary tumour"
  Hmisc::label(data$mediastinal_biopsy_val)="Was the recurrence within mediastinum biopsy validated subsequently"
  Hmisc::label(data$mediastinal_biopsy_date)="Date of biopsy validation of mediastinum"
  Hmisc::label(data$ipsi_hilus_biopsy_val)="Was the recurrence in the ipsilateral hilus biopsy validated subsequently"
  Hmisc::label(data$ipsi_hilus_biopsy_date)="Date of biopsy validation of recurrence in the ipsilateral hilus"
  Hmisc::label(data$contra_hilus_biopsy_val)="Was the recurrence in the contralateral hilus biopsy validated subsequently"
  Hmisc::label(data$contra_hilus_biopsy_date)="Date of biopsy validation of recurrence in the contralateral hilus"
  Hmisc::label(data$ipsi_supraclav_biopsy_val)="Was the recurrence in the ipsilateral supraclavicular region biopsy validated subsequently"
  Hmisc::label(data$ipsi_supraclav_biopsy_date)="Date of biopsy validation of recurrence in ipsilateral supraclavicular region"
  Hmisc::label(data$distant_recurrence_loc___1)="Location of distant recurrence (choice=Recurrence in locale site but of clearly different histology than the primary tumour)"
  Hmisc::label(data$distant_recurrence_loc___2)="Location of distant recurrence (choice=Recurrence in a lobe without primary tumour involvement, but same lung as the primary tumour)"
  Hmisc::label(data$distant_recurrence_loc___3)="Location of distant recurrence (choice=Recurrence in the opposite lung than that of the primary tumour)"
  Hmisc::label(data$distant_recurrence_loc___4)="Location of distant recurrence (choice=Recurrence in the opposite supraclavicular region)"
  Hmisc::label(data$distant_recurrence_loc___5)="Location of distant recurrence (choice=Multiple lung metastases)"
  Hmisc::label(data$distant_recurrence_loc___6)="Location of distant recurrence (choice=Malignant pleural effusion)"
  Hmisc::label(data$distant_recurrence_loc___7)="Location of distant recurrence (choice=Brain)"
  Hmisc::label(data$distant_recurrence_loc___8)="Location of distant recurrence (choice=Liver)"
  Hmisc::label(data$distant_recurrence_loc___9)="Location of distant recurrence (choice=Adrenal gland)"
  Hmisc::label(data$distant_recurrence_loc___10)="Location of distant recurrence (choice=Bone)"
  Hmisc::label(data$distant_recurrence_loc___11)="Location of distant recurrence (choice=Cutis)"
  Hmisc::label(data$distant_recurrence_loc___12)="Location of distant recurrence (choice=Other)"
  Hmisc::label(data$diffhist_biopsy_val)="Was the recurrence in locale site but of clearly different histology than the primary tumour biopsy validated subsequently"
  Hmisc::label(data$diffhist_biopsy_date)="Date of biopsy validation of recurrence in locale site but of clearly different histology than the primary tumour"
  Hmisc::label(data$otherlobe_biopsy_val)="Was the recurrence in a lobe without primary tumour involvement, but same lung as the primary tumour, biopsy validated subsequently"
  Hmisc::label(data$otherlobe_biopsy_date)="Date of biopsy validation of recurrence in a lobe without primary tumour involvement, but same lung as the primary tumour"
  Hmisc::label(data$contra_lung_biopsy_val)="Was the recurrence in the opposite lung than that of the primary tumour biopsy validated subsequently"
  Hmisc::label(data$contra_lung_biopsy_date)="Date of biopsy validation of recurrence in the opposite lung than that of the primary tumour"
  Hmisc::label(data$contra_supra_biopsy_val)="Was the recurrence in the opposite supraclavicular region biopsy validated subsequently"
  Hmisc::label(data$contra_supra_biopsy_date)="Date of biopsy validation of Recurrence in the opposite supraclavicular region"
  Hmisc::label(data$mult_lungmet_biopsy_val)="Was the multiple lung metastases biopsy validated subsequently"
  Hmisc::label(data$mult_lungmet_biopsy_date)="Date of biopsy validation of multiple lung metastases"
  Hmisc::label(data$pleura_efus_biopsy_val)="Was the malignant pleural effusion biopsy validated subsequently"
  Hmisc::label(data$pleura_efus_biopsy_date)="Date of biopsy validation of malignant pleural effusion"
  Hmisc::label(data$brain_biopsy_val)="Was the brain recurrence biopsy validated subsequently"
  Hmisc::label(data$brain_biopsy_date)="Date of biopsy validation of brain recurrence"
  Hmisc::label(data$liver_biopsy_val)="Was the liver recurrence biopsy validated subsequently"
  Hmisc::label(data$liver_biopsy_date)="Date of biopsy validation of liver recurrence"
  Hmisc::label(data$andrenal_biopsy_val)="Was the adrenal gland recurrence biopsy validated subsequently"
  Hmisc::label(data$andrenal_biopsy_date)="Date of biopsy validation of adrenal gland recurrence"
  Hmisc::label(data$bone_biopsy_val)="Was the bone recurrence biopsy validated subsequently"
  Hmisc::label(data$bone_biopsy_date)="Date of biopsy validation of bone recurrence"
  Hmisc::label(data$cutis_biopsy_val)="Was the cutis recurrence biopsy validated subsequently"
  Hmisc::label(data$cutis_biopsy_date)="Date of biopsy validation of cutis recurrence"
  Hmisc::label(data$other_recurrence_type)="Specify the type of other recurrence"
  Hmisc::label(data$other_biopsy_val)="Was the other recurrence biopsy validated subsequently"
  Hmisc::label(data$other_biopsy_date)="Date of biopsy validation of other recurrence"
  Hmisc::label(data$image_data_clinical_suspicion_of_recurrence_complete)="Complete?"
  Hmisc::label(data$pneumonitis_w_degree)="Degree of worst pneumonitis"
  Hmisc::label(data$pneumonitis_w_date)="Date of worst degree of pneumonitis"
  Hmisc::label(data$heart_late_retro_degree)="Worst degree of heart"
  Hmisc::label(data$heart_late_retro_date)="Start time of the worst degree of heart"
  Hmisc::label(data$heart_late_retro_related)="Was the toxicity of heart expected to be related to RT/Chemo"
  Hmisc::label(data$heart_late_retro_notes)="Potential comments"
  Hmisc::label(data$lung_late_retro_degree)="Worst degree of lung"
  Hmisc::label(data$lung_late_retro_date)="Start time of the worst degree of lung"
  Hmisc::label(data$lung_late_retro_related)="Was the toxicity of lung expected to be related to RT/Chemo"
  Hmisc::label(data$lung_late_retro_notes)="Potential comments"
  Hmisc::label(data$esoph_late_retro_degree)="Worst degree of esophagus"
  Hmisc::label(data$esoph_late_retro_date)="Start time of the worst degree of esophagus"
  Hmisc::label(data$esoph_late_retro_related)="Was the toxicity of esophagus expected to be related to RT/Chemo"
  Hmisc::label(data$esoph_late_retro_notes)="Potential comments"
  Hmisc::label(data$pain_late_retro_degree)="Worst degree of pain"
  Hmisc::label(data$pain_late_retro_date)="Start time of the worst degree of pain"
  Hmisc::label(data$pain_late_retro_related)="Was the toxicity of pain expected to be related to RT/Chemo"
  Hmisc::label(data$pain_late_retro_notes)="Potential comments"
  Hmisc::label(data$bron_late_retro_degree)="Worst degree of bronchie"
  Hmisc::label(data$bron_late_retro_date)="Start time of the worst degree of bronchie"
  Hmisc::label(data$bron_late_retro_related)="Was the toxicity of bronchie expected to be related to RT/Chemo"
  Hmisc::label(data$bron_late_retro_notes)="Potential comments"
  Hmisc::label(data$trachea_late_retro_degree)="Worst degree of trachea"
  Hmisc::label(data$trachea_late_retro_date)="Start time of the worst degree of trachea"
  Hmisc::label(data$trachea_late_retro_related)="Was the toxicity of trachea expected to be related to RT/Chemo"
  Hmisc::label(data$trachea_late_retro_notes)="Potential comments"
  Hmisc::label(data$rtmyelo_late_retro_degree)="Worst degree of RT-myelopathy"
  Hmisc::label(data$rtmyelo_late_retro_date)="Start time of the worst degree of RT-myelopathy"
  Hmisc::label(data$rtmyelo_late_retro_related)="Was the toxicity of rtmyelopathy expected to be related to RT/Chemo"
  Hmisc::label(data$rtmyelo_late_retro_notes)="Potential comments"
  Hmisc::label(data$cutis_late_retro_degree)="Worst degree of cutis"
  Hmisc::label(data$cutis_late_retro_date)="Start time of the worst degree of cutis"
  Hmisc::label(data$cutis_late_retro_related)="Was the toxicity of cutis expected to be related to RT/Chemo"
  Hmisc::label(data$cutis_late_retro_notes)="Potential comments"
  Hmisc::label(data$retrospective_toxicity_collection_complete)="Complete?"
  Hmisc::label(data$followup_late_date)="Date of followup visit"
  Hmisc::label(data$heart_late_pro_degree)="Degree of heart"
  Hmisc::label(data$heart_late_pro_related)="Was the toxicity of heart expected to be related to RT/Chemo"
  Hmisc::label(data$heart_late_pro_notes)="Potential comments"
  Hmisc::label(data$lung_late_pro_degree)="Degree of lung"
  Hmisc::label(data$lung_late_pro_related)="Was the toxicity of lung expected to be related to RT/Chemo"
  Hmisc::label(data$lung_late_pro_notes)="Potential comments"
  Hmisc::label(data$esoph_late_pro_degree)="Degree of esophagus"
  Hmisc::label(data$esoph_late_pro_related)="Was the toxicity of esophagus expected to be related to RT/Chemo"
  Hmisc::label(data$esoph_late_pro_notes)="Potential comments"
  Hmisc::label(data$pain_late_pro_degree)="Degree of pain"
  Hmisc::label(data$pain_late_pro_related)="Was the toxicity of pain expected to be related to RT/Chemo"
  Hmisc::label(data$pain_late_pro_notes)="Potential comments"
  Hmisc::label(data$bron_late_pro_degree)="Degree of bronchie"
  Hmisc::label(data$bron_late_pro_related)="Was the toxicity of bronchie expected to be related to RT/Chemo"
  Hmisc::label(data$bron_late_pro_notes)="Potential comments"
  Hmisc::label(data$trachea_late_pro_degree)="Degree of trachea"
  Hmisc::label(data$trachea_late_pro_related)="Was the toxicity of trachea expected to be related to RT/Chemo"
  Hmisc::label(data$trachea_late_pro_notes)="Potential comments"
  Hmisc::label(data$rtmyelo_late_pro_degree)="Degree of RT-myelopathy"
  Hmisc::label(data$rtmyelo_late_pro_related)="Was the toxicity of rtmyelopathy expected to be related to RT/Chemo"
  Hmisc::label(data$rtmyelo_late_pro_notes)="Potential comments"
  Hmisc::label(data$cutis_late_pro_degree)="Degree of cutis"
  Hmisc::label(data$cutis_late_pro_related)="Was the toxicity of cutis expected to be related to RT/Chemo"
  Hmisc::label(data$cutis_late_pro_notes)="Potential comments"
  Hmisc::label(data$late_toxicity_complete)="Complete?"
  Hmisc::label(data$neutropeni1_degree)="Worst degree of febril neutropeni1"
  Hmisc::label(data$neutropeni1_date)="Start time of the worst degree of febril neutropeni1"
  Hmisc::label(data$neutropeni1_related)="Was the SAE of febril neutropeni1 expected to be related to RT/Chemo"
  Hmisc::label(data$hemoptysis1_degree)="Worst degree of hemoptysis1"
  Hmisc::label(data$hemoptysis1_date)="Start time of the worst degree of hemoptysis1"
  Hmisc::label(data$hemoptysis1_related)="Was the SAE of hemoptysis1 expected to be related to RT/Chemo"
  Hmisc::label(data$heart1_degree)="Worst degree of heart1"
  Hmisc::label(data$heart1_date)="Start time of the worst degree of heart1"
  Hmisc::label(data$heart1_related)="Was the SAE of heart1 expected to be related to RT/Chemo"
  Hmisc::label(data$emboli1_degree)="Worst degree of emboli1"
  Hmisc::label(data$emboli1_date)="Start time of the worst degree of emboli1"
  Hmisc::label(data$emboli1_related)="Was the SAE of emboli1 expected to be related to RT/Chemo"
  Hmisc::label(data$infection1_degree)="Worst degree of infection1"
  Hmisc::label(data$infection1_date)="Start time of the worst degree of infection1"
  Hmisc::label(data$infection1_related)="Was the SAE of infection1 expected to be related to RT/Chemo"
  Hmisc::label(data$othertox1_degree)="Worst degree of other toxicity"
  Hmisc::label(data$othertox1_date)="Start time of the worst degree of other toxicity"
  Hmisc::label(data$othertox1_related)="Was the SAE of other toxicity expected to be related to RT/Chemo"
  Hmisc::label(data$othertox1_description)="Describe type of other toxicity"
  Hmisc::label(data$neutropeni2_degree)="Worst degree of febril neutropeni2"
  Hmisc::label(data$neutropeni2_date)="Start time of the worst degree of febril neutropeni2"
  Hmisc::label(data$neutropeni2_related)="Was the SAE of febril neutropeni2 expected to be related to RT/Chemo"
  Hmisc::label(data$hemoptysis2_degree)="Worst degree of hemoptysis2"
  Hmisc::label(data$hemoptysis2_date)="Start time of the worst degree of hemoptysis2"
  Hmisc::label(data$hemoptysis2_related)="Was the SAE of hemoptysis2 expected to be related to RT/Chemo"
  Hmisc::label(data$heart2_degree)="Worst degree of heart2"
  Hmisc::label(data$heart2_date)="Start time of the worst degree of heart2"
  Hmisc::label(data$heart2_related)="Was the SAE of heart2 expected to be related to RT/Chemo"
  Hmisc::label(data$emboli2_degree)="Worst degree of emboli2"
  Hmisc::label(data$emboli2_date)="Start time of the worst degree of emboli2"
  Hmisc::label(data$emboli2_related)="Was the SAE of emboli2 expected to be related to RT/Chemo"
  Hmisc::label(data$infection2_degree)="Worst degree of infection2"
  Hmisc::label(data$infection2_date)="Start time of the worst degree of infection2"
  Hmisc::label(data$infection2_related)="Was the SAE of infection2 expected to be related to RT/Chemo"
  Hmisc::label(data$othertox2_degree)="Worst degree of other toxicity"
  Hmisc::label(data$othertox2_date)="Start time of the worst degree of other toxicity"
  Hmisc::label(data$othertox2_related)="Was the SAE of other toxicity expected to be related to RT/Chemo"
  Hmisc::label(data$othertox2_description)="Describe type of other toxicity"
  Hmisc::label(data$neutropeni3_degree)="Worst degree of febril neutropeni3"
  Hmisc::label(data$neutropeni3_date)="Start time of the worst degree of febril neutropeni3"
  Hmisc::label(data$neutropeni3_related)="Was the SAE of febril neutropeni3 expected to be related to RT/Chemo"
  Hmisc::label(data$hemoptysis3_degree)="Worst degree of hemoptysis3"
  Hmisc::label(data$hemoptysis3_date)="Start time of the worst degree of hemoptysis3"
  Hmisc::label(data$hemoptysis3_related)="Was the SAE of hemoptysis3 expected to be related to RT/Chemo"
  Hmisc::label(data$heart3_degree)="Worst degree of heart3"
  Hmisc::label(data$heart3_date)="Start time of the worst degree of heart3"
  Hmisc::label(data$heart3_related)="Was the SAE of heart3 expected to be related to RT/Chemo"
  Hmisc::label(data$emboli3_degree)="Worst degree of emboli3"
  Hmisc::label(data$emboli3_date)="Start time of the worst degree of emboli3"
  Hmisc::label(data$emboli3_related)="Was the SAE of emboli3 expected to be related to RT/Chemo"
  Hmisc::label(data$infection3_degree)="Worst degree of infection3"
  Hmisc::label(data$infection3_date)="Start time of the worst degree of infection3"
  Hmisc::label(data$infection3_related)="Was the SAE of infection3 expected to be related to RT/Chemo"
  Hmisc::label(data$othertox3_degree)="Worst degree of other toxicity"
  Hmisc::label(data$othertox3_date)="Start time of the worst degree of other toxicity"
  Hmisc::label(data$othertox3_related)="Was the SAE of other toxicity expected to be related to RT/Chemo"
  Hmisc::label(data$othertox3_description)="Describe type of other toxicity"
  Hmisc::label(data$sae_form_complete)="Complete?"
  Hmisc::label(data$target_t___1)="T of intial tumour (choice=T/T1)"
  Hmisc::label(data$target_t___2)="T of intial tumour (choice=T2)"
  Hmisc::label(data$target_t___3)="T of intial tumour (choice=T3)"
  Hmisc::label(data$target_t___4)="T of intial tumour (choice=No T)"
  Hmisc::label(data$target_station_n___1)="N stations included in target (choice=1R)"
  Hmisc::label(data$target_station_n___2)="N stations included in target (choice=1L)"
  Hmisc::label(data$target_station_n___3)="N stations included in target (choice=2R)"
  Hmisc::label(data$target_station_n___4)="N stations included in target (choice=2L)"
  Hmisc::label(data$target_station_n___5)="N stations included in target (choice=3A)"
  Hmisc::label(data$target_station_n___6)="N stations included in target (choice=3P)"
  Hmisc::label(data$target_station_n___7)="N stations included in target (choice=4R)"
  Hmisc::label(data$target_station_n___8)="N stations included in target (choice=4L)"
  Hmisc::label(data$target_station_n___9)="N stations included in target (choice=5)"
  Hmisc::label(data$target_station_n___10)="N stations included in target (choice=6)"
  Hmisc::label(data$target_station_n___11)="N stations included in target (choice=7)"
  Hmisc::label(data$target_station_n___12)="N stations included in target (choice=10 R)"
  Hmisc::label(data$target_station_n___13)="N stations included in target (choice=10 L)"
  Hmisc::label(data$target_station_n___14)="N stations included in target (choice=11 R)"
  Hmisc::label(data$target_station_n___15)="N stations included in target (choice=11 L)"
  Hmisc::label(data$target_station_n___16)="N stations included in target (choice=12 R)"
  Hmisc::label(data$target_station_n___17)="N stations included in target (choice=12 L)"
  Hmisc::label(data$target_station_n___18)="N stations included in target (choice=supraclav R)"
  Hmisc::label(data$target_station_n___19)="N stations included in target (choice=Supraclav L)"
  Hmisc::label(data$target_station_n___20)="N stations included in target (choice=No n-stations in target)"
  Hmisc::label(data$target_station_n___21)="N stations included in target (choice=other)"
  Hmisc::label(data$relapse_t)="Relapse T"
  Hmisc::label(data$relapse_t_location___1)="Location of T relapse (choice=Inside PTV)"
  Hmisc::label(data$relapse_t_location___2)="Location of T relapse (choice=Outside PTV)"
  Hmisc::label(data$relapse_t_location___3)="Location of T relapse (choice=not evaluable)"
  Hmisc::label(data$target_t_2___1)="Relapse T, mark where (choice=T/T1)"
  Hmisc::label(data$target_t_2___2)="Relapse T, mark where (choice=T2)"
  Hmisc::label(data$target_t_2___3)="Relapse T, mark where (choice=T3)"
  Hmisc::label(data$target_t_2___4)="Relapse T, mark where (choice=New T)"
  Hmisc::label(data$target_t_2___5)="Relapse T, mark where (choice=T relapse, but location not evaluable)"
  Hmisc::label(data$relapse_n)="Relapse N"
  Hmisc::label(data$n_relapse_loc___1)="Location of N relapse (choice=Inside PTV)"
  Hmisc::label(data$n_relapse_loc___2)="Location of N relapse (choice=Outside PTV)"
  Hmisc::label(data$n_relapse_loc___3)="Location of N relapse (choice=not evaluable)"
  Hmisc::label(data$relapse_station_n_2___1)="Relapse station N (choice=1R)"
  Hmisc::label(data$relapse_station_n_2___2)="Relapse station N (choice=1L)"
  Hmisc::label(data$relapse_station_n_2___3)="Relapse station N (choice=2R)"
  Hmisc::label(data$relapse_station_n_2___4)="Relapse station N (choice=2L)"
  Hmisc::label(data$relapse_station_n_2___5)="Relapse station N (choice=3A)"
  Hmisc::label(data$relapse_station_n_2___6)="Relapse station N (choice=3P)"
  Hmisc::label(data$relapse_station_n_2___7)="Relapse station N (choice=4R)"
  Hmisc::label(data$relapse_station_n_2___8)="Relapse station N (choice=4L)"
  Hmisc::label(data$relapse_station_n_2___9)="Relapse station N (choice=5)"
  Hmisc::label(data$relapse_station_n_2___10)="Relapse station N (choice=6)"
  Hmisc::label(data$relapse_station_n_2___11)="Relapse station N (choice=7)"
  Hmisc::label(data$relapse_station_n_2___12)="Relapse station N (choice=10 R)"
  Hmisc::label(data$relapse_station_n_2___13)="Relapse station N (choice=10 L)"
  Hmisc::label(data$relapse_station_n_2___14)="Relapse station N (choice=11 R)"
  Hmisc::label(data$relapse_station_n_2___15)="Relapse station N (choice=11 L)"
  Hmisc::label(data$relapse_station_n_2___16)="Relapse station N (choice=12 R)"
  Hmisc::label(data$relapse_station_n_2___17)="Relapse station N (choice=12 L)"
  Hmisc::label(data$relapse_station_n_2___18)="Relapse station N (choice=supraclav R)"
  Hmisc::label(data$relapse_station_n_2___19)="Relapse station N (choice=Supraclav L)"
  Hmisc::label(data$relapse_station_n_2___20)="Relapse station N (choice=other)"
  Hmisc::label(data$relapse_station_n_2___21)="Relapse station N (choice=No information on n stations involved)"
  Hmisc::label(data$relapse_n_location_other)="Specify, if other"
  Hmisc::label(data$first_relapse_complete)="Complete?"
  Hmisc::label(data$image_update_date)="Medical images:  All relevant Medical imaging information (Image data/Clinical suspicion of recurrence) until the provided date has been included in the clinical Redcap database."
  Hmisc::label(data$clinical_update_date)="Clinical information:  All clinical information (i.e. follow-up information during and after RT, Chemotherapy during RT, Durvalumab, Recidiv/Mors,  Retro tox, and SAE reports but excluding imaging information and rt_slut - the latter is partly retrieved from DcmCollab) until the provided date has been included in the clinical Redcap database."
  Hmisc::label(data$clinical_update_dates_complete)="Complete?"

  #Setting Units


  #Setting Factors(will create new variable for factors)
  data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("registration_arm_1","uge_1_arm_1","uge_2_arm_1","uge_3_arm_1","uge_4_arm_1","uge_5_arm_1","uge_6_arm_1","uge_7_arm_1","uge_8_arm_1","1_followup_arm_1","haendelser_arm_1","3_mdr_followup_arm_1","6_mdr_followup_arm_1","9_mdr_followup_arm_1","12_mdr_followup_arm_1","15_mdr_followup_arm_1","18_mdr_followup_arm_1","21_mdr_followup_arm_1","24_mdr_followup_arm_1","30_mdr_followup_arm_1","36_mdr_followup_arm_1","42_mdr_followup_arm_1","48_mdr_followup_arm_1","54_mdr_followup_arm_1","60_mdr_followup_arm_1"))
  data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("image_data_clinical_suspicion_of_recurrence"))
  data$redcap_data_access_group.factor = factor(data$redcap_data_access_group,levels=c("01_rh","02_herlev","03_naestved","04_ouh","05_aarhus","06_vejle","07_aalborg","08_radiumhospitale","09_trondheim","10_tromsoe","11_ullevaal","12"))
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
  data$reason_for_imaging.factor = factor(data$reason_for_imaging,levels=c("1","2"))
  data$suspicion_images.factor = factor(data$suspicion_images,levels=c("1","2"))
  data$imaging_modality.factor = factor(data$imaging_modality,levels=c("1","2","3","4","5","6"))
  data$further_images.factor = factor(data$further_images,levels=c("1","2"))
  data$furter_images_modal_1.factor = factor(data$furter_images_modal_1,levels=c("1","2","3","4","5","6"))
  data$furter_images_modal_2.factor = factor(data$furter_images_modal_2,levels=c("1","2","3","4","5","6"))
  data$furter_images_modal_3.factor = factor(data$furter_images_modal_3,levels=c("1","2","3","4","5","6"))
  data$any_recurrence.factor = factor(data$any_recurrence,levels=c("1","2"))
  data$recurence_type___1.factor = factor(data$recurence_type___1,levels=c("0","1"))
  data$recurence_type___2.factor = factor(data$recurence_type___2,levels=c("0","1"))
  data$local_recurrence_location___1.factor = factor(data$local_recurrence_location___1,levels=c("0","1"))
  data$local_recurrence_location___2.factor = factor(data$local_recurrence_location___2,levels=c("0","1"))
  data$local_recurrence_location___3.factor = factor(data$local_recurrence_location___3,levels=c("0","1"))
  data$local_recurrence_location___4.factor = factor(data$local_recurrence_location___4,levels=c("0","1"))
  data$local_recurrence_location___5.factor = factor(data$local_recurrence_location___5,levels=c("0","1"))
  data$same_lobe_biopsy_val.factor = factor(data$same_lobe_biopsy_val,levels=c("1","2","3","4"))
  data$mediastinal_biopsy_val.factor = factor(data$mediastinal_biopsy_val,levels=c("1","2","3","4"))
  data$ipsi_hilus_biopsy_val.factor = factor(data$ipsi_hilus_biopsy_val,levels=c("1","2","3","4"))
  data$contra_hilus_biopsy_val.factor = factor(data$contra_hilus_biopsy_val,levels=c("1","2","3","4"))
  data$ipsi_supraclav_biopsy_val.factor = factor(data$ipsi_supraclav_biopsy_val,levels=c("1","2","3","4"))
  data$distant_recurrence_loc___1.factor = factor(data$distant_recurrence_loc___1,levels=c("0","1"))
  data$distant_recurrence_loc___2.factor = factor(data$distant_recurrence_loc___2,levels=c("0","1"))
  data$distant_recurrence_loc___3.factor = factor(data$distant_recurrence_loc___3,levels=c("0","1"))
  data$distant_recurrence_loc___4.factor = factor(data$distant_recurrence_loc___4,levels=c("0","1"))
  data$distant_recurrence_loc___5.factor = factor(data$distant_recurrence_loc___5,levels=c("0","1"))
  data$distant_recurrence_loc___6.factor = factor(data$distant_recurrence_loc___6,levels=c("0","1"))
  data$distant_recurrence_loc___7.factor = factor(data$distant_recurrence_loc___7,levels=c("0","1"))
  data$distant_recurrence_loc___8.factor = factor(data$distant_recurrence_loc___8,levels=c("0","1"))
  data$distant_recurrence_loc___9.factor = factor(data$distant_recurrence_loc___9,levels=c("0","1"))
  data$distant_recurrence_loc___10.factor = factor(data$distant_recurrence_loc___10,levels=c("0","1"))
  data$distant_recurrence_loc___11.factor = factor(data$distant_recurrence_loc___11,levels=c("0","1"))
  data$distant_recurrence_loc___12.factor = factor(data$distant_recurrence_loc___12,levels=c("0","1"))
  data$diffhist_biopsy_val.factor = factor(data$diffhist_biopsy_val,levels=c("1","2","3","4"))
  data$otherlobe_biopsy_val.factor = factor(data$otherlobe_biopsy_val,levels=c("1","2","3","4"))
  data$contra_lung_biopsy_val.factor = factor(data$contra_lung_biopsy_val,levels=c("1","2","3","4"))
  data$contra_supra_biopsy_val.factor = factor(data$contra_supra_biopsy_val,levels=c("1","2","3","4"))
  data$mult_lungmet_biopsy_val.factor = factor(data$mult_lungmet_biopsy_val,levels=c("1","2","3","4"))
  data$pleura_efus_biopsy_val.factor = factor(data$pleura_efus_biopsy_val,levels=c("1","2","3","4"))
  data$brain_biopsy_val.factor = factor(data$brain_biopsy_val,levels=c("1","2","3","4"))
  data$liver_biopsy_val.factor = factor(data$liver_biopsy_val,levels=c("1","2","3","4"))
  data$andrenal_biopsy_val.factor = factor(data$andrenal_biopsy_val,levels=c("1","2","3","4"))
  data$bone_biopsy_val.factor = factor(data$bone_biopsy_val,levels=c("1","2","3","4"))
  data$cutis_biopsy_val.factor = factor(data$cutis_biopsy_val,levels=c("1","2","3","4"))
  data$other_biopsy_val.factor = factor(data$other_biopsy_val,levels=c("1","2","3","4"))
  data$image_data_clinical_suspicion_of_recurrence_complete.factor = factor(data$image_data_clinical_suspicion_of_recurrence_complete,levels=c("0","1","2"))
  data$pneumonitis_w_degree.factor = factor(data$pneumonitis_w_degree,levels=c("0","1","2","3","4","5"))
  data$heart_late_retro_degree.factor = factor(data$heart_late_retro_degree,levels=c("0","3","4","5"))
  data$heart_late_retro_related.factor = factor(data$heart_late_retro_related,levels=c("0","1","2"))
  data$lung_late_retro_degree.factor = factor(data$lung_late_retro_degree,levels=c("0","3","4","5"))
  data$lung_late_retro_related.factor = factor(data$lung_late_retro_related,levels=c("0","1","2"))
  data$esoph_late_retro_degree.factor = factor(data$esoph_late_retro_degree,levels=c("0","3","4","5"))
  data$esoph_late_retro_related.factor = factor(data$esoph_late_retro_related,levels=c("0","1","2"))
  data$pain_late_retro_degree.factor = factor(data$pain_late_retro_degree,levels=c("0","3","4","5"))
  data$pain_late_retro_related.factor = factor(data$pain_late_retro_related,levels=c("0","1","2"))
  data$bron_late_retro_degree.factor = factor(data$bron_late_retro_degree,levels=c("0","3","4","5"))
  data$bron_late_retro_related.factor = factor(data$bron_late_retro_related,levels=c("0","1","2"))
  data$trachea_late_retro_degree.factor = factor(data$trachea_late_retro_degree,levels=c("0","3","4","5"))
  data$trachea_late_retro_related.factor = factor(data$trachea_late_retro_related,levels=c("0","1","2"))
  data$rtmyelo_late_retro_degree.factor = factor(data$rtmyelo_late_retro_degree,levels=c("0","3","4","5"))
  data$rtmyelo_late_retro_related.factor = factor(data$rtmyelo_late_retro_related,levels=c("0","1","2"))
  data$cutis_late_retro_degree.factor = factor(data$cutis_late_retro_degree,levels=c("0","3","4","5"))
  data$cutis_late_retro_related.factor = factor(data$cutis_late_retro_related,levels=c("0","1","2"))
  data$retrospective_toxicity_collection_complete.factor = factor(data$retrospective_toxicity_collection_complete,levels=c("0","1","2"))
  data$heart_late_pro_degree.factor = factor(data$heart_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$heart_late_pro_related.factor = factor(data$heart_late_pro_related,levels=c("0","1","2"))
  data$lung_late_pro_degree.factor = factor(data$lung_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$lung_late_pro_related.factor = factor(data$lung_late_pro_related,levels=c("0","1","2"))
  data$esoph_late_pro_degree.factor = factor(data$esoph_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$esoph_late_pro_related.factor = factor(data$esoph_late_pro_related,levels=c("0","1","2"))
  data$pain_late_pro_degree.factor = factor(data$pain_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$pain_late_pro_related.factor = factor(data$pain_late_pro_related,levels=c("0","1","2"))
  data$bron_late_pro_degree.factor = factor(data$bron_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$bron_late_pro_related.factor = factor(data$bron_late_pro_related,levels=c("0","1","2"))
  data$trachea_late_pro_degree.factor = factor(data$trachea_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$trachea_late_pro_related.factor = factor(data$trachea_late_pro_related,levels=c("0","1","2"))
  data$rtmyelo_late_pro_degree.factor = factor(data$rtmyelo_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$rtmyelo_late_pro_related.factor = factor(data$rtmyelo_late_pro_related,levels=c("0","1","2"))
  data$cutis_late_pro_degree.factor = factor(data$cutis_late_pro_degree,levels=c("0","1","2","3","4","5"))
  data$cutis_late_pro_related.factor = factor(data$cutis_late_pro_related,levels=c("0","1","2"))
  data$late_toxicity_complete.factor = factor(data$late_toxicity_complete,levels=c("0","1","2"))
  data$neutropeni1_degree.factor = factor(data$neutropeni1_degree,levels=c("0","1","2","3","4","5"))
  data$neutropeni1_related.factor = factor(data$neutropeni1_related,levels=c("0","1","2"))
  data$hemoptysis1_degree.factor = factor(data$hemoptysis1_degree,levels=c("0","1","2","3","4","5"))
  data$hemoptysis1_related.factor = factor(data$hemoptysis1_related,levels=c("0","1","2"))
  data$heart1_degree.factor = factor(data$heart1_degree,levels=c("0","1","2","3","4","5"))
  data$heart1_related.factor = factor(data$heart1_related,levels=c("0","1","2"))
  data$emboli1_degree.factor = factor(data$emboli1_degree,levels=c("0","1","2","3","4","5"))
  data$emboli1_related.factor = factor(data$emboli1_related,levels=c("0","1","2"))
  data$infection1_degree.factor = factor(data$infection1_degree,levels=c("0","1","2","3","4","5"))
  data$infection1_related.factor = factor(data$infection1_related,levels=c("0","1","2"))
  data$othertox1_degree.factor = factor(data$othertox1_degree,levels=c("0","1","2","3","4","5"))
  data$othertox1_related.factor = factor(data$othertox1_related,levels=c("0","1","2"))
  data$neutropeni2_degree.factor = factor(data$neutropeni2_degree,levels=c("0","1","2","3","4","5"))
  data$neutropeni2_related.factor = factor(data$neutropeni2_related,levels=c("0","1","2"))
  data$hemoptysis2_degree.factor = factor(data$hemoptysis2_degree,levels=c("0","1","2","3","4","5"))
  data$hemoptysis2_related.factor = factor(data$hemoptysis2_related,levels=c("0","1","2"))
  data$heart2_degree.factor = factor(data$heart2_degree,levels=c("0","1","2","3","4","5"))
  data$heart2_related.factor = factor(data$heart2_related,levels=c("0","1","2"))
  data$emboli2_degree.factor = factor(data$emboli2_degree,levels=c("0","1","2","3","4","5"))
  data$emboli2_related.factor = factor(data$emboli2_related,levels=c("0","1","2"))
  data$infection2_degree.factor = factor(data$infection2_degree,levels=c("0","1","2","3","4","5"))
  data$infection2_related.factor = factor(data$infection2_related,levels=c("0","1","2"))
  data$othertox2_degree.factor = factor(data$othertox2_degree,levels=c("0","1","2","3","4","5"))
  data$othertox2_related.factor = factor(data$othertox2_related,levels=c("0","1","2"))
  data$neutropeni3_degree.factor = factor(data$neutropeni3_degree,levels=c("0","1","2","3","4","5"))
  data$neutropeni3_related.factor = factor(data$neutropeni3_related,levels=c("0","1","2"))
  data$hemoptysis3_degree.factor = factor(data$hemoptysis3_degree,levels=c("0","1","2","3","4","5"))
  data$hemoptysis3_related.factor = factor(data$hemoptysis3_related,levels=c("0","1","2"))
  data$heart3_degree.factor = factor(data$heart3_degree,levels=c("0","1","2","3","4","5"))
  data$heart3_related.factor = factor(data$heart3_related,levels=c("0","1","2"))
  data$emboli3_degree.factor = factor(data$emboli3_degree,levels=c("0","1","2","3","4","5"))
  data$emboli3_related.factor = factor(data$emboli3_related,levels=c("0","1","2"))
  data$infection3_degree.factor = factor(data$infection3_degree,levels=c("0","1","2","3","4","5"))
  data$infection3_related.factor = factor(data$infection3_related,levels=c("0","1","2"))
  data$othertox3_degree.factor = factor(data$othertox3_degree,levels=c("0","1","2","3","4","5"))
  data$othertox3_related.factor = factor(data$othertox3_related,levels=c("0","1","2"))
  data$sae_form_complete.factor = factor(data$sae_form_complete,levels=c("0","1","2"))
  data$target_t___1.factor = factor(data$target_t___1,levels=c("0","1"))
  data$target_t___2.factor = factor(data$target_t___2,levels=c("0","1"))
  data$target_t___3.factor = factor(data$target_t___3,levels=c("0","1"))
  data$target_t___4.factor = factor(data$target_t___4,levels=c("0","1"))
  data$target_station_n___1.factor = factor(data$target_station_n___1,levels=c("0","1"))
  data$target_station_n___2.factor = factor(data$target_station_n___2,levels=c("0","1"))
  data$target_station_n___3.factor = factor(data$target_station_n___3,levels=c("0","1"))
  data$target_station_n___4.factor = factor(data$target_station_n___4,levels=c("0","1"))
  data$target_station_n___5.factor = factor(data$target_station_n___5,levels=c("0","1"))
  data$target_station_n___6.factor = factor(data$target_station_n___6,levels=c("0","1"))
  data$target_station_n___7.factor = factor(data$target_station_n___7,levels=c("0","1"))
  data$target_station_n___8.factor = factor(data$target_station_n___8,levels=c("0","1"))
  data$target_station_n___9.factor = factor(data$target_station_n___9,levels=c("0","1"))
  data$target_station_n___10.factor = factor(data$target_station_n___10,levels=c("0","1"))
  data$target_station_n___11.factor = factor(data$target_station_n___11,levels=c("0","1"))
  data$target_station_n___12.factor = factor(data$target_station_n___12,levels=c("0","1"))
  data$target_station_n___13.factor = factor(data$target_station_n___13,levels=c("0","1"))
  data$target_station_n___14.factor = factor(data$target_station_n___14,levels=c("0","1"))
  data$target_station_n___15.factor = factor(data$target_station_n___15,levels=c("0","1"))
  data$target_station_n___16.factor = factor(data$target_station_n___16,levels=c("0","1"))
  data$target_station_n___17.factor = factor(data$target_station_n___17,levels=c("0","1"))
  data$target_station_n___18.factor = factor(data$target_station_n___18,levels=c("0","1"))
  data$target_station_n___19.factor = factor(data$target_station_n___19,levels=c("0","1"))
  data$target_station_n___20.factor = factor(data$target_station_n___20,levels=c("0","1"))
  data$target_station_n___21.factor = factor(data$target_station_n___21,levels=c("0","1"))
  data$relapse_t.factor = factor(data$relapse_t,levels=c("1","0"))
  data$relapse_t_location___1.factor = factor(data$relapse_t_location___1,levels=c("0","1"))
  data$relapse_t_location___2.factor = factor(data$relapse_t_location___2,levels=c("0","1"))
  data$relapse_t_location___3.factor = factor(data$relapse_t_location___3,levels=c("0","1"))
  data$target_t_2___1.factor = factor(data$target_t_2___1,levels=c("0","1"))
  data$target_t_2___2.factor = factor(data$target_t_2___2,levels=c("0","1"))
  data$target_t_2___3.factor = factor(data$target_t_2___3,levels=c("0","1"))
  data$target_t_2___4.factor = factor(data$target_t_2___4,levels=c("0","1"))
  data$target_t_2___5.factor = factor(data$target_t_2___5,levels=c("0","1"))
  data$relapse_n.factor = factor(data$relapse_n,levels=c("1","0"))
  data$n_relapse_loc___1.factor = factor(data$n_relapse_loc___1,levels=c("0","1"))
  data$n_relapse_loc___2.factor = factor(data$n_relapse_loc___2,levels=c("0","1"))
  data$n_relapse_loc___3.factor = factor(data$n_relapse_loc___3,levels=c("0","1"))
  data$relapse_station_n_2___1.factor = factor(data$relapse_station_n_2___1,levels=c("0","1"))
  data$relapse_station_n_2___2.factor = factor(data$relapse_station_n_2___2,levels=c("0","1"))
  data$relapse_station_n_2___3.factor = factor(data$relapse_station_n_2___3,levels=c("0","1"))
  data$relapse_station_n_2___4.factor = factor(data$relapse_station_n_2___4,levels=c("0","1"))
  data$relapse_station_n_2___5.factor = factor(data$relapse_station_n_2___5,levels=c("0","1"))
  data$relapse_station_n_2___6.factor = factor(data$relapse_station_n_2___6,levels=c("0","1"))
  data$relapse_station_n_2___7.factor = factor(data$relapse_station_n_2___7,levels=c("0","1"))
  data$relapse_station_n_2___8.factor = factor(data$relapse_station_n_2___8,levels=c("0","1"))
  data$relapse_station_n_2___9.factor = factor(data$relapse_station_n_2___9,levels=c("0","1"))
  data$relapse_station_n_2___10.factor = factor(data$relapse_station_n_2___10,levels=c("0","1"))
  data$relapse_station_n_2___11.factor = factor(data$relapse_station_n_2___11,levels=c("0","1"))
  data$relapse_station_n_2___12.factor = factor(data$relapse_station_n_2___12,levels=c("0","1"))
  data$relapse_station_n_2___13.factor = factor(data$relapse_station_n_2___13,levels=c("0","1"))
  data$relapse_station_n_2___14.factor = factor(data$relapse_station_n_2___14,levels=c("0","1"))
  data$relapse_station_n_2___15.factor = factor(data$relapse_station_n_2___15,levels=c("0","1"))
  data$relapse_station_n_2___16.factor = factor(data$relapse_station_n_2___16,levels=c("0","1"))
  data$relapse_station_n_2___17.factor = factor(data$relapse_station_n_2___17,levels=c("0","1"))
  data$relapse_station_n_2___18.factor = factor(data$relapse_station_n_2___18,levels=c("0","1"))
  data$relapse_station_n_2___19.factor = factor(data$relapse_station_n_2___19,levels=c("0","1"))
  data$relapse_station_n_2___20.factor = factor(data$relapse_station_n_2___20,levels=c("0","1"))
  data$relapse_station_n_2___21.factor = factor(data$relapse_station_n_2___21,levels=c("0","1"))
  data$first_relapse_complete.factor = factor(data$first_relapse_complete,levels=c("0","1","2"))
  data$clinical_update_dates_complete.factor = factor(data$clinical_update_dates_complete,levels=c("0","1","2"))

  levels(data$redcap_event_name.factor)=c("Registration","Uge 1","Uge 2","Uge 3","Uge 4","Uge 5","Uge 6","Uge 7","Uge 8","1. Followup","Haendelser","3 mdr followup","6 mdr followup","9 mdr followup","12 mdr followup","15 mdr followup","18 mdr followup","21 mdr followup","24 mdr followup","30 mdr followup","36 mdr followup","42 mdr followup","48 mdr followup","54 mdr followup","60 mdr followup")
  levels(data$redcap_repeat_instrument.factor)=c("Image Data Clinical Suspicion Of Recurrence")
  levels(data$redcap_data_access_group.factor)=c("01. RH","02. Herlev","03. Naestved","04. OUH","05. Aarhus","06. Vejle","07. Aalborg","08. Radiumhospitalet","09. Trondheim","10. Tromsoe","11. Ullevaal","12.")
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
  levels(data$reason_for_imaging.factor)=c("Clinical suspicion of recurrence","Standard planed images as part of the trial (no previous clinical suspicion of recurrence)")
  levels(data$suspicion_images.factor)=c("yes","no")
  levels(data$imaging_modality.factor)=c("CT","PET / PET-CT","MR","PET-MR","X-ray","Other")
  levels(data$further_images.factor)=c("No","Yes")
  levels(data$furter_images_modal_1.factor)=c("CT","PET / PET-CT","MR","PET-MR","X-ray","Other")
  levels(data$furter_images_modal_2.factor)=c("CT","PET / PET-CT","MR","PET-MR","X-ray","Other")
  levels(data$furter_images_modal_3.factor)=c("CT","PET / PET-CT","MR","PET-MR","X-ray","Other")
  levels(data$any_recurrence.factor)=c("No","Yes")
  levels(data$recurence_type___1.factor)=c("Unchecked","Checked")
  levels(data$recurence_type___2.factor)=c("Unchecked","Checked")
  levels(data$local_recurrence_location___1.factor)=c("Unchecked","Checked")
  levels(data$local_recurrence_location___2.factor)=c("Unchecked","Checked")
  levels(data$local_recurrence_location___3.factor)=c("Unchecked","Checked")
  levels(data$local_recurrence_location___4.factor)=c("Unchecked","Checked")
  levels(data$local_recurrence_location___5.factor)=c("Unchecked","Checked")
  levels(data$same_lobe_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$mediastinal_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$ipsi_hilus_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$contra_hilus_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$ipsi_supraclav_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$distant_recurrence_loc___1.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___2.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___3.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___4.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___5.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___6.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___7.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___8.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___9.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___10.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___11.factor)=c("Unchecked","Checked")
  levels(data$distant_recurrence_loc___12.factor)=c("Unchecked","Checked")
  levels(data$diffhist_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$otherlobe_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$contra_lung_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$contra_supra_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$mult_lungmet_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$pleura_efus_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$brain_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$liver_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$andrenal_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$bone_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$cutis_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$other_biopsy_val.factor)=c("No biopsy","Confirmed imaging result","Rejected imaging result","Inconclusive biopsy")
  levels(data$image_data_clinical_suspicion_of_recurrence_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$pneumonitis_w_degree.factor)=c("0","1","2","3","4","5")
  levels(data$heart_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$heart_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$lung_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$lung_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$esoph_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$esoph_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$pain_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$pain_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$bron_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$bron_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$trachea_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$trachea_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$rtmyelo_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$rtmyelo_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$cutis_late_retro_degree.factor)=c("0-2","3","4","5")
  levels(data$cutis_late_retro_related.factor)=c("No","Yes","Potentially")
  levels(data$retrospective_toxicity_collection_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$heart_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$heart_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$lung_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$lung_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$esoph_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$esoph_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$pain_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$pain_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$bron_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$bron_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$trachea_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$trachea_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$rtmyelo_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$rtmyelo_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$cutis_late_pro_degree.factor)=c("0","1","2","3","4","5")
  levels(data$cutis_late_pro_related.factor)=c("No","Yes","Potentially")
  levels(data$late_toxicity_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$neutropeni1_degree.factor)=c("0","1","2","3","4","5")
  levels(data$neutropeni1_related.factor)=c("No","Yes","Potentially")
  levels(data$hemoptysis1_degree.factor)=c("0","1","2","3","4","5")
  levels(data$hemoptysis1_related.factor)=c("No","Yes","Potentially")
  levels(data$heart1_degree.factor)=c("0","1","2","3","4","5")
  levels(data$heart1_related.factor)=c("No","Yes","Potentially")
  levels(data$emboli1_degree.factor)=c("0","1","2","3","4","5")
  levels(data$emboli1_related.factor)=c("No","Yes","Potentially")
  levels(data$infection1_degree.factor)=c("0","1","2","3","4","5")
  levels(data$infection1_related.factor)=c("No","Yes","Potentially")
  levels(data$othertox1_degree.factor)=c("0","1","2","3","4","5")
  levels(data$othertox1_related.factor)=c("No","Yes","Potentially")
  levels(data$neutropeni2_degree.factor)=c("0","1","2","3","4","5")
  levels(data$neutropeni2_related.factor)=c("No","Yes","Potentially")
  levels(data$hemoptysis2_degree.factor)=c("0","1","2","3","4","5")
  levels(data$hemoptysis2_related.factor)=c("No","Yes","Potentially")
  levels(data$heart2_degree.factor)=c("0","1","2","3","4","5")
  levels(data$heart2_related.factor)=c("No","Yes","Potentially")
  levels(data$emboli2_degree.factor)=c("0","1","2","3","4","5")
  levels(data$emboli2_related.factor)=c("No","Yes","Potentially")
  levels(data$infection2_degree.factor)=c("0","1","2","3","4","5")
  levels(data$infection2_related.factor)=c("No","Yes","Potentially")
  levels(data$othertox2_degree.factor)=c("0","1","2","3","4","5")
  levels(data$othertox2_related.factor)=c("No","Yes","Potentially")
  levels(data$neutropeni3_degree.factor)=c("0","1","2","3","4","5")
  levels(data$neutropeni3_related.factor)=c("No","Yes","Potentially")
  levels(data$hemoptysis3_degree.factor)=c("0","1","2","3","4","5")
  levels(data$hemoptysis3_related.factor)=c("No","Yes","Potentially")
  levels(data$heart3_degree.factor)=c("0","1","2","3","4","5")
  levels(data$heart3_related.factor)=c("No","Yes","Potentially")
  levels(data$emboli3_degree.factor)=c("0","1","2","3","4","5")
  levels(data$emboli3_related.factor)=c("No","Yes","Potentially")
  levels(data$infection3_degree.factor)=c("0","1","2","3","4","5")
  levels(data$infection3_related.factor)=c("No","Yes","Potentially")
  levels(data$othertox3_degree.factor)=c("0","1","2","3","4","5")
  levels(data$othertox3_related.factor)=c("No","Yes","Potentially")
  levels(data$sae_form_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$target_t___1.factor)=c("Unchecked","Checked")
  levels(data$target_t___2.factor)=c("Unchecked","Checked")
  levels(data$target_t___3.factor)=c("Unchecked","Checked")
  levels(data$target_t___4.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___1.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___2.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___3.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___4.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___5.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___6.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___7.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___8.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___9.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___10.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___11.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___12.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___13.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___14.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___15.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___16.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___17.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___18.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___19.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___20.factor)=c("Unchecked","Checked")
  levels(data$target_station_n___21.factor)=c("Unchecked","Checked")
  levels(data$relapse_t.factor)=c("Yes","No")
  levels(data$relapse_t_location___1.factor)=c("Unchecked","Checked")
  levels(data$relapse_t_location___2.factor)=c("Unchecked","Checked")
  levels(data$relapse_t_location___3.factor)=c("Unchecked","Checked")
  levels(data$target_t_2___1.factor)=c("Unchecked","Checked")
  levels(data$target_t_2___2.factor)=c("Unchecked","Checked")
  levels(data$target_t_2___3.factor)=c("Unchecked","Checked")
  levels(data$target_t_2___4.factor)=c("Unchecked","Checked")
  levels(data$target_t_2___5.factor)=c("Unchecked","Checked")
  levels(data$relapse_n.factor)=c("Yes","No")
  levels(data$n_relapse_loc___1.factor)=c("Unchecked","Checked")
  levels(data$n_relapse_loc___2.factor)=c("Unchecked","Checked")
  levels(data$n_relapse_loc___3.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___1.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___2.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___3.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___4.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___5.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___6.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___7.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___8.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___9.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___10.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___11.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___12.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___13.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___14.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___15.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___16.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___17.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___18.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___19.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___20.factor)=c("Unchecked","Checked")
  levels(data$relapse_station_n_2___21.factor)=c("Unchecked","Checked")
  levels(data$first_relapse_complete.factor)=c("Incomplete","Unverified","Complete")
  levels(data$clinical_update_dates_complete.factor)=c("Incomplete","Unverified","Complete")


  #Diaries have been spelt in Danish for the months's measurements and in English for the weeks. Correct to English.
  names(data)[names(data) == "diare_fu.factor"] <- "diaria_fu.factor"
  names(data)[names(data) == "diare_fu"] <- "diaria_fu"
  # Smoking is missing an _rt in the weekly scores. Correct
  names(data)[names(data) == "smoking.factor"] <- "smoking_rt.factor"
  names(data)[names(data) == "smoking"] <- "smoking_rt"

  return(data)
}
