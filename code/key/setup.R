#2/22/24
#Creation of common libraries and useful function loader
#


print('begin lib loading')

#regular
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(viridis)
  library(scales)
  library(tidyverse)
  #library(ggcorrplot)
  library(melt)
  library(reshape2)
  
  #options
  options(bitmapType = "cairo")
  options(error = function() traceback(3))
  
  #seed
  set.seed(123)
  
  #ggplot holder list
  gg <- vector(mode='list', length=12)



print('lib loading complete')