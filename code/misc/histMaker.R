#!/usr/bin/env Rscript


addNewData <- 1

if(1){
  library(dplyr)
  library(data.table)
  library(tidyverse)
  
  #options
  options(bitmapType = "cairo")
  options(error = function() traceback(3))
  
  #seed
  set.seed(123)
  
  #loop count and data limit
  iter <- 50
  #ggplot holder list
  gg <- vector(mode='list', length=12)
  setwd('/data2/morgante_lab/nklimko/rep/dgrp-starve/')
}

ggTidy <- function(data){
  
  for(i in 1:dim(data)[2]){
    
    name <- colnames(data)[i]
    temp <- cbind(rep(name, dim(data)[1]), data[,i, with=FALSE])
    
    if(i==1){
      hold <- temp
    } else{
      hold <- rbind(hold, temp, use.names=FALSE)
    }
  }
  colnames(hold) <- c("method", "cor")
  
  return(hold)
}

filt <- function(data, method){
  
  if(method=="sr"){
    col <- 1
    return(unlist(data[col, ]))
  } else if(method=="time"){
    col <- 2
    start <- 3
    skip <-5
    
    raw <- unlist(data[col, ])
    trim <- raw[seq(start, length(raw), by=skip)]
    
    return(trim)
    
    }else if(method=="nested"){
    hold <- vector(length=50)
    for(i in 1:length(data)){
      hold[i] <- data[[i]]$cor 
    }
    return(hold)
  } else if(method=='top3'){
    col <- 1
    start <- 1
    skip <- 16
    
    raw <- unlist(data[col, ])
    trim <- raw[seq(start, length(raw), by=skip)]
    
    return(trim)
  } else{
    print('Method not recognized.')
    return(NA)
  }
}

snakemake rules:
  
  
  if(1){
  strip data to file

concatenate all files
  }

if(2){
  strip time to files
  concatenate all files
}
\



RMD:
  
  pull all data

change graph subsets from all data






#female----
if(addNewData){
  #READ IN
  raw_sr_rf <- readRDS("snake/data/sr/30_summary/rf_f.Rds")
  raw_sr_pcr <- readRDS("snake/data/sr/30_summary/pcr_f.Rds")
  raw_sr_varbvs <- readRDS("snake/data/20_cor/varbvs_f_starvation.Rds")
  
  raw_sr_bayesC <- readRDS("snake/data/sr/30_summary/bayesC_0.9_f.Rds")
  raw_top3_bayesC <- readRDS('snake/data/top3/30_summary/multibayesC_f_top3.Rds')
  #raw_top3_gblup <- readRDS('snake/data/top3/30_summary/multibayesC_f.Rds')
  
  raw_sr_gblup <- readRDS("snake/data/20_cor/gblup_f_starvation.Rds")
  raw_top3_gblup <- readRDS('snake/data/top3/30_summary/multigblup_f_top3.Rds')
  #raw_top3_gblup <- readRDS('snake/data/top3/30_summary/multigblup_f.Rds')
  
  raw_sr_rr <- readRDS("snake/data/20_cor/rr_f_starvation.Rds")
  raw_sr_lasso <- readRDS("snake/data/20_cor/lasso_f_starvation.Rds")
  raw_top3_lasso <- readRDS("snake/data/top3/30_summary/mlasso_f.Rds")
  
  #raw_top3_mash <- readRDS('snake/data/top3/30_summary/mash_f.Rds')
  raw_sr_mash <- readRDS("snake/data/sr/30_summary/mash_f.Rds")
  raw_top3_mash <- readRDS('snake/data/top3/30_summary/mr.mash_f_top3.Rds')
  raw_top3_datamash <- readRDS('snake/data/top3/30_summary/datadrive_f.Rds')
  
  raw_sr_nn <-readRDS('snake/data/sr/30_summary/nn_f.Rds')
  raw_sr_pcr <- readRDS('snake/data/sr/30_summary/pcr_f.Rds')
  
  
  raw_sr_pls_kernel <- readRDS('snake/data/sr/30_summary/pls_kernelpls_f.Rds')
  raw_sr_pls_wide <- readRDS('snake/data/sr/30_summary/pls_widekernelpls_f.Rds')
  raw_sr_pls_oscore <- readRDS('snake/data/sr/30_summary/pls_oscorespls_f.Rds')
  raw_sr_pls_simpl <- readRDS('snake/data/sr/30_summary/pls_simpls_f.Rds')
  
  #FILTER
  sr_rf <- filt(raw_sr_rf, "sr")
  sr_pcr <- filt(raw_sr_pcr, "sr")
  sr_nn <- filt(raw_sr_nn, "sr")
  sr_varbvs <- filt(raw_sr_varbvs, "nested")
  
  sr_bayesC <- filt(raw_sr_bayesC, "sr")
  top3_bayesC <- filt(raw_top3_bayesC, "top3")
  
  sr_gblup <- filt(raw_sr_gblup, "nested")
  top3_gblup <- filt(raw_top3_gblup, "top3")
  
  sr_rr <- filt(raw_sr_rr, "nested")
  sr_lasso <- filt(raw_sr_lasso, "nested")
  top3_lasso <- filt(raw_top3_lasso, "top3")
  
  sr_mash <- filt(raw_sr_mash, "sr")
  top3_mash <- filt(raw_top3_mash, "top3")
  top3_datamash <- filt(raw_top3_datamash, "top3")
  
  sr_rf <- filt(raw_sr_rf, "sr")
  sr_pcr <- filt(raw_sr_pcr, "sr")
  sr_nn <- filt(raw_sr_nn, "sr")
  
  
  sr_pls_kernel <- filt(raw_sr_pls_kernel, "sr")
  sr_pls_wide <- filt(raw_sr_pls_wide, "sr")
  sr_pls_oscore <- filt(raw_sr_pls_oscore, "sr")
  sr_pls_simpl <- filt(raw_sr_pls_simpl, "sr")
  
  allData <- data.table(sr_rf, sr_pcr, sr_nn, sr_varbvs,
                        sr_bayesC, top3_bayesC, sr_gblup, top3_gblup,
                        sr_rr, sr_lasso, top3_lasso,
                        sr_mash, top3_mash, top3_datamash)
  
  saveRDS(allData, "data/all_hist_data_f.Rds")
  
  srData <- data.table(sr_rf, sr_pcr, sr_nn, sr_varbvs, sr_bayesC,
                       sr_gblup, sr_rr, sr_lasso, sr_mash)
  colnames(srData) <- c('rf', 'pcr', 'nn', 'varbvs', 'bayesC',
                        'gblup', 'rr', 'lasso', 'mash')
  
  saveRDS(srData, "data/sr_hist_data_f.Rds")
  
  plsData <- data.table(sr_pls_kernel, sr_pls_wide, sr_pls_oscore, sr_pls_simpl)
  
  saveRDS(plsData, "data/pls_hist_data_f.Rds")
  
  data <- ggTidy(plsData)
  data$method <- factor(data$method, levels=unique(data$method))
  
  if(game){
    gg[[3]] <- ggplot(data, aes(x=method, y=cor, fill=method)) +
      geom_violin(color = NA, width = 0.65) +
      geom_boxplot(color='#440154FF', width = 0.15) +
      theme_minimal() +
      stat_summary(fun=mean, color='#440154FF', geom='point', 
                   shape=18, size=3, show.legend=FALSE) +
      labs(x=NULL,y='Correlation',tag='F', title='Univariate Female') +
      theme(legend.position='none',
            axis.text.x = element_text(angle = -45, size=10),
            text=element_text(size=10),
            plot.tag = element_text(size=15)) +
      scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
  }
  
  time_pls_kernel <- filt(raw_sr_pls_kernel, "time")
  time_pls_wide <- filt(raw_sr_pls_wide, "time")
  time_pls_oscore <- filt(raw_sr_pls_oscore, "time")
  time_pls_simpl <- filt(raw_sr_pls_simpl, "time")
  
  time_pls_data <- data.table(time_pls_kernel, time_pls_wide, time_pls_oscore, time_pls_simpl)
  
  data <- ggTidy(time_pls_data)
  data$method <- factor(data$method, levels=unique(data$method))
  
  if(game){
    gg[[4]] <- ggplot(data, aes(x=method, y=cor, fill=method)) +
      geom_violin(color = NA, width = 0.65) +
      geom_boxplot(color='#440154FF', width = 0.15) +
      theme_minimal() +
      stat_summary(fun=mean, color='#440154FF', geom='point', 
                   shape=18, size=3, show.legend=FALSE) +
      labs(x=NULL,y='Runtime',tag='F', title='Univariate Female') +
      theme(legend.position='none',
            axis.text.x = element_text(angle = -45, size=10),
            text=element_text(size=10),
            plot.tag = element_text(size=15)) +
      scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
  }
  
}

#male----
if(addNewData){
  #READ IN
  raw_sr_rf <- readRDS("snake/data/sr/30_summary/rf_m.Rds")
  raw_sr_pcr <- readRDS("snake/data/sr/30_summary/pcr_m.Rds")
  raw_sr_varbvs <- readRDS("snake/data/20_cor/varbvs_m_starvation.Rds")
  
  raw_sr_bayesC <- readRDS("snake/data/sr/30_summary/bayesC_0.9_m.Rds")
  raw_top3_bayesC <- readRDS('snake/data/top3/30_summary/multibayesC_m_top3.Rds')
  #raw_top3_gblup <- readRDS('snake/data/top3/30_summary/multibayesC_m.Rds')
  
  raw_sr_gblup <- readRDS("snake/data/20_cor/gblup_m_starvation.Rds")
  raw_top3_gblup <- readRDS('snake/data/top3/30_summary/multigblup_m_top3.Rds')
  #raw_top3_gblup <- readRDS('snake/data/top3/30_summary/multigblup_m.Rds')
  
  raw_sr_rr <- readRDS("snake/data/20_cor/rr_m_starvation.Rds")
  raw_sr_lasso <- readRDS("snake/data/20_cor/lasso_m_starvation.Rds")
  raw_top3_lasso <- readRDS("snake/data/top3/30_summary/mlasso_m.Rds")
  
  #raw_top3_mash <- readRDS('snake/data/top3/30_summary/mash_m.Rds')
  raw_sr_mash <- readRDS("snake/data/sr/30_summary/mash_m.Rds")
  raw_top3_mash <- readRDS('snake/data/top3/30_summary/mr.mash_m_top3.Rds')
  raw_top3_datamash <- readRDS('snake/data/top3/30_summary/datadrive_m.Rds')
  
  raw_sr_nn <-readRDS('snake/data/sr/30_summary/nn_m.Rds')
  raw_sr_pcr <- readRDS('snake/data/sr/30_summary/pcr_m.Rds')
  
  #FILTER
  sr_rf <- filt(raw_sr_rf, "sr")
  sr_pcr <- filt(raw_sr_pcr, "sr")
  sr_nn <- filt(raw_sr_nn, "sr")
  sr_varbvs <- filt(raw_sr_varbvs, "nested")
  
  sr_bayesC <- filt(raw_sr_bayesC, "sr")
  top3_bayesC <- filt(raw_top3_bayesC, "top3")
  
  sr_gblup <- filt(raw_sr_gblup, "nested")
  top3_gblup <- filt(raw_top3_gblup, "top3")
  
  sr_rr <- filt(raw_sr_rr, "nested")
  sr_lasso <- filt(raw_sr_lasso, "nested")
  top3_lasso <- filt(raw_top3_lasso, "top3")
  
  sr_mash <- filt(raw_sr_mash, "sr")
  top3_mash <- filt(raw_top3_mash, "top3")
  top3_datamash <- filt(raw_top3_datamash, "top3")
  
  
  allData <- data.table(sr_rf, sr_pcr, sr_nn, sr_varbvs,
                        sr_bayesC, top3_bayesC, sr_gblup, top3_gblup,
                        sr_rr, sr_lasso, top3_lasso,
                        sr_mash, top3_mash, top3_datamash)
  
  saveRDS(allData, "data/all_hist_data_m.Rds")
  
  srData <- data.table(sr_rf, sr_pcr, sr_nn, sr_varbvs, sr_bayesC,
                       sr_gblup, sr_rr, sr_lasso, sr_mash)
  colnames(srData) <- c('rf', 'pcr', 'nn', 'varbvs', 'bayesC',
                        'gblup', 'rr', 'lasso', 'mr.ash')
  
  saveRDS(srData, "data/sr_hist_data_m.Rds")
  
}




