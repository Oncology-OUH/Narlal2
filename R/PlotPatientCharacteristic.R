#' Write word files containing patient characteristics
#'
#' @param df the output from the the function ExtracPatientCharacteristic
#' @param filepath the filepath to the directory where the patient characteristics shall be stored
#'
#' @return Is returning word files with tables of patient characteristics data
#' @export PlotPatientCharacteristic
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtChar <- ExtractPatientCharacteristic(df)
#' PlotPatientCharacteristic(df=PtChar,filepath='c:/home/cab/temp')
PlotPatientCharacteristic <- function(df,filepath){

  listVars <- c("age", "gender","histology","T","N","Stadium","Perform","FEV1","FVC","weight","height","MeanDose_T","MeanDose_N","MeanLung","Vol_T","Vol_N")
  catVars <- c("gender","histology","T","N","Stadium","Perform")
  nonNormalVars <- c("age","FEV1","FVC","weight","height","MeanDose_T","MeanDose_N","MeanLung","Vol_T","Vol_N")


  #All data
  table1 <- tableone::CreateTableOne(vars = listVars, data = df, factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Overall patient characteristic"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"PatientCharacteristics_AllPatients.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  #Patients without durvalumab
  index <- df$durvalumab == 'No'
  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Patient characteristic for patients that did not receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"PatientCharacteristics_WithoutDurvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  #Patients with durvalumab
  index <- !index
  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Patient characteristic for patients that did receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"PatientCharacteristics_WithDurvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))
}
