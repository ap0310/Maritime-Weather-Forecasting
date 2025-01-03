IR_BC_plot <- function(model){
  #Load needed libraries
  library(MASS)
  library(car)
  
  #Return 1 by 2 Figure:
  boxcox(model)
  invResPlot(model)
}
