library(tidyverse)

df <- list.files(path="data/snake/2_cor",
                 pattern = "*.Rds",
                 full.names=TRUE) %>% map_dfr(readRDS)

saveRDS(df, "data/3_final/allCor.Rds")

gSink <- rep(9,9)
saveRDS(gSink, snakemake@output[[1]])
