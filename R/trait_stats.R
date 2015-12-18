#'Function to calculate summary statistics for fieldbook data from excel files
#'@param fieldbook A data frame of the fieldbook
#'@param trait The name or position of the measured variable that will be summarized.
#'@param groupfactors A vector containing names of columns that contain grouping variables
#'@param na.rm A boolean that indicates whether to ignore NA's
#'@param trait_dict The data frame of the data dictionary for potato and sweetpotato
#'@return A data frame with the count, mean and standard desviation
#'@author Omar Benites
#'@details This function is capable of divide the information in categorial or quantitative data based on a data dictionary for potato and sweepotato.
#'If it is categorical, returns the count #'and mode. And if it is quantitative, returns the count, media and standart desviation. 
#'@references Progress in developing a potato ontology for breeders. Reinhard Simon, Vilma Hualla, E. Salas, Rene Gomez, Raul Cordova and Stef de Haan.
#'Crop Ontology 2014.
#'@keywords stats, summary
#'@family stats,summary
#'@export 

trait_summary <- function(fieldbook = NULL,trait, groupfactors=NULL, na.rm=FALSE, trait_dict = NULL) {
 
  if(missing(fieldbook)){
    stop("Please enter your data")
  }
  if(missing(trait)){
    stop("Please enter the name or position of the trait")
  }
  if(missing(groupfactors)){
    stop("Please enter the name of columns that contain grouping variables")
  }
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  datos <- fieldbook
  vvv <- datos[,trait]
  lbl <- names(datos[trait])   #extract the trait's label name
  measurevar <- lbl    #the trait's name
  tp <- get_trait_type(trait = trait, trait_dict = trait_dict )    #the type of variable
  
  if(tp=="Continuous" || tp=="Discrete"){
    # filter continuous and discrete data
    formula <- as.formula(paste(measurevar, paste(groupfactors, collapse=" + "), sep=" ~ "))
    datac <- doBy::summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- paste(measurevar,"_Mean",sep="")  
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- paste(measurevar,"_sd",sep = "")
  }  #Cuantitativa
  
  if(tp=="Categorical"){
    #filter categorical data
    formula <- as.formula(paste(measurevar, paste(groupfactors, collapse=" + "), sep=" ~ "))
    datac <- doBy::summaryBy(formula, data=data, FUN=c(length2,themode)) #quit the na.rm
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
    names(datac)[ names(datac) == paste(measurevar, ".themode", sep="") ] <- paste(measurevar,"_Mode",sep="")                               
  }  #Cualitativa
  
  return(datac)
}


#' ####################################################################################################
#' Conditional Trait Format in Fieldbook Spreadseet (Excel)
#' @param file The name of the file which contains your data fieldbook data. It must be an .xlsx file
#' @param fbsheet The name of the fieldbook sheet into the excel file.
#' @param trait The abbreviation(s) of the trait(s) used in fieldbooks.  
#' @param trait_dict The trait dictionary on crop ontology format.
#' @description This function highlight all the values which are out of range in a fieldbook spreadsheet 
#' (paint with colours) the trait column to identify out of range values.
#' @return An excel file with conditional format according trait conditions 
#' @export
#' 



