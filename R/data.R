#' @name sweetpt
#' @title Sample data of sweet potato 
#' @docType data
#' @aliases spg
#' @description This dataset contains sweet potato data
#' @references This data is related to HiDAP crop template
#' @usage spg
#' @format data frame
#' @source International Potato Center, sweet potato experimental data.
NULL
#save(spg,file = "data/spg.rda")


#' @name subsample
#' @title trial data using sub sampling for each observation
#' @docType data
#' @aliases subsample
#' @description This dataset contains data comec from experiment using sub sampling for each genotype.
#' @references This data is related to HiDAP fbcheck module.
#' @usage subsample
#' @format data frame
#' @source International Potato Center, potato experimental data.
NULL
# datos <- datos %>% dplyr::mutate(yield= rnorm(n= 500,mean =  12, sd = 1))
# subsample <- datos
# save(subsample, file = "data/subsample.rda")



#' @name fctsubsample data with factor
#' @title trial data using factor with sub sampling at the same time.
#' @docType data
#' @aliases fctsubsample
#' @description This dataset contains data which comes from experiment using factors and sub sampling.
#' @references This data is related to HiDAP fbcheck module.
#' @usage fctsubsample
#' @format data frame
#' @source International Potato Center, potato and sweet potato experimental data.
NULL


#' @name augbd augmendted block design data using factors and subsamples
#' @title trial data of an augmented block design experiment using factor with three levels. Beside it has subsample per genotype
#' @docType data
#' @aliases augbd
#' @description This dataset contains data which comes from experiment using factors and sub sampling.
#' @references This data is related to HiDAP fbcheck module.
#' @usage augbd
#' @format data frame
#' @source International Potato Center, potato and sweet potato experimental data.
NULL


# datos <- read.delim2("clipboard")
# datos <- datos %>% dplyr::mutate(yield= rnorm(n= nrow(datos),mean =  12, sd = 1))
# fctsubsample <- datos
# save(fctsubsample, file = "data/fctsubsample.rda")