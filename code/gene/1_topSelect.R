

library(data.table)
library(argparse)


#options
options(bitmapType = 'cairo')
options(error = function() traceback(3))

#seed
set.seed(123)


topSelect <- function(input, centile, output){

  core <- readRDS(input)
  data <- core[order(-cor), term]
  cutoff <- as.integer(length(data) * centile / 100)
  final <- data[1:cutoff]
  subbed <- sub(pattern='.', replacement=':', x=final, fixed=TRUE) # replace . with : for all terms

  write(subbed, file=output)
}


#args----
parser <- ArgumentParser(description= 'snakemake transfer')

parser$add_argument('--input')
parser$add_argument('--centile', type="integer")
parser$add_argument('--output')

snake <- parser$parse_args()
print(str(snake))

#call----
topSelect(snake$input,
          snake$centile,
          snake$output)
