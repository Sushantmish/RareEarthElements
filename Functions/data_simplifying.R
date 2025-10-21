#this is a function for refining data from GeoRoc
data_simplifying <- function(input_data)
{
  REE_list <- c("LA.PPM.","CE.PPM.","PR.PPM.","ND.PPM.","SM.PPM.","EU.PPM.","GD.PPM.","TB.PPM.","DY.PPM.","HO.PPM.","ER.PPM.","TM.PPM.","YB.PPM.","LU.PPM.")
  complete_row <- complete.cases(input_data[, REE_list])
  refined_REE_data <- input_data[complete_row,REE_list]
  
} 