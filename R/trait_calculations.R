#' Calculation of trait variables
#' @description A generalized function to calculate all the posibles derivated trait from Fieldbook. Use as a 
#' backend sbformulas.
#' @param fb Data frame with the fieldbook
#' @param plot_size plot size
#' @param plant_den plant density
#' @param mgt Crop Managment Data
#' @param mtl Material List Data
#' @importFrom sbformula sbcalculate
#' @param trial_type the type of trial
#' @export
#'   
calculate_trait_variables <- function(fb, plot_size=NA, plant_den=NA, mgt, mtl, trial_type=NA){

  fb <- as.data.frame(fb, stringsAsFactors=FALSE)   
  fieldbook_names <- names(fb)
  fieldbook <- sbformula::sbcalculate(fb = fb, plot.size=plot_size, plant.den=plant_den)
  
  #if(trial_type =="Late blight"){  #begin late blight
  date_logic <- !all(is.na(mgt$Date))
  saudpc_logic <- !all(is.na(mtl$Scale_audpc))    
  isRepCol <- is.element("REP", fieldbook_names)
  
  if(date_logic){
  
    #mgt1 <- transform_dates(mgt = mgt) 
    mgt <- transform_dates(mgt = mgt) 
    print(mgt)
    
    #print(mgt)
    
    rel.days <- get.rel.days(mgt)
    print(rel.days)
    
    if(any(is.na(rel.days))){ 
      rel.days <- get.rel.days(mgt) 
      print(rel.days)
      mgt <- transform_dates(mgt = mgt)
      print(mgt)
      rel.days <- get.rel.days(mgt) 
      print(rel.days)
    }
    
    print("despues")
    print(rel.days)
    print(mgt)
    
    lb.control <- get.lb.control(mtl)
    
    lbf = c("LB1","LB2","LB3","LB4","LB5","LB6","LB7","LB8","LB9","LB10","LB11","LB12")
    yy = names(fieldbook)
    
    if("AUDPC" %in% names(fieldbook)){
      fieldbook=within(fieldbook,{
        AUDPC  = xaudpc(
          eval = fieldbook[,yy[yy %in% lbf]] ,
          #eval = cbind(LB1,LB2,LB3,LB4,LB5,LB6,LB7) ,
          dates= rel.days,
          type = "absolute")
      })
    }
    
    
    
      if("rAUDPC" %in% names(fieldbook)){
        fieldbook=within(fieldbook,{
          rAUDPC	= xaudpc(
            eval = fieldbook[,yy[yy %in% lbf]] ,
            #eval = cbind(LB1,LB2,LB3,LB4,LB5,LB6,LB7) ,
            dates= rel.days,
            type = "relative")
        })
      }
    if(saudpc_logic && isRepCol){ #begin saudpc_logic for SAUDPC AND rAUDPC
      if("SAUDPC" %in% names(fieldbook)){
        fieldbook=within(fieldbook,{
          SAUDPC	= saudpc(INSTN,AUDPC,REP, lb.control)
        })
      }
   }  #end saudpc_logic for SAUDPC AND rAUDPC
    
  } #end late blight
  
  fieldbook <- fieldbook[,fieldbook_names]
  
  return(fieldbook)
  
}
