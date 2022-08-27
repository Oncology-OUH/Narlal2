#' Write word files containing patient toxicity data
#'
#' @param df the output from the the function ExtractToxicityData
#' @param filepath the filepath to the directory where the patient toxicity shall be stored
#'
#' @return Is returning word files with tables of patient toxicity data
#' @export PlotToxicityData
#'
#' @examples file <- system.file('extdata','DemoData.csv',package="Narlal2")
#' df <- LoadAndPrepareData(filename=file)
#' PtTox <- ExtractToxicityData(df)
#' PlotToxicityData(df=PtTox,filepath='c:/home/cab/temp')
PlotToxicityData <- function(df,filepath){

  #All data
  listVars <- names(df)[grep("^During*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()
  table1 <- tableone::CreateTableOne(vars = listVars, data = df, factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Toxicity during RT for all patients"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityDuring_AllPatients.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  listVars <- names(df)[grep("^Early*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()

  table1 <- tableone::CreateTableOne(vars = listVars, data = df, factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Early toxicity for all patients"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityEarly_AllPatients.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  listVars <- names(df)[grep("^Late*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()

  table1 <- tableone::CreateTableOne(vars = listVars, data = df, factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Late toxicity for all patients"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityLate_AllPatients.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))



  #Patients without durvalumab
  listVars <- names(df)[grep("^During*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()
  index <- df$durvalumab == 'No'
  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Toxicity during RT for patients that did not receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityDuring_NoDurvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  listVars <- names(df)[grep("^Early*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()

  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Early toxicity for patients that did not receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityEarly_NoDurvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  listVars <- names(df)[grep("^Late*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()

  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Late toxicity for patients that did not receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityLate_NoDurvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  #Patients with durvalumab
  listVars <- names(df)[grep("^During*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()
  index <- !index
  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Toxicity during RT for patients that did receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityDuring_Durvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  listVars <- names(df)[grep("^Early*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()

  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Early toxicity for patients that did receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityEarly_Durvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))

  listVars <- names(df)[grep("^Late*",names(df))]
  catVars <- listVars
  nonNormalVars <- c()

  table1 <- tableone::CreateTableOne(vars = listVars, data = df[index,], factorVars = catVars,strata = c("arm"),includeNA = FALSE,test=TRUE,addOverall=TRUE)
  tab1_word <- print(table1, quote = F, noSpaces = F,cramVars = catVars,test = T, contDigits = 1, printToggle = F,
                     dropEqual = F,explain = T)
  tab1_word <- cbind(rownames(tab1_word),tab1_word)
  colnames(tab1_word)[1] <- "Variable"
  tab1_df <- as.data.frame(tab1_word)
  tab1_df <- tab1_df[, !names(tab1_df) %in% c("test")]
  customtab_defaults_Narlal()
  header <- "Late toxicity for patients that did receive durvalumab"
  footer <- ""
  flextable_1 <- custom_tab_Narlal(tab1_df, header, footer)
  filename <- file.path(filepath,"ToxicityLate_Durvalumab.docx")
  flextable::save_as_docx(flextable_1, path = filename, pr_section =  officer::prop_section(page_size = officer::page_size(orient = "portrait"), type = "continuous"))
}
