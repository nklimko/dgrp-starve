  if(0){
    ### FEMALE
    
    gg[[1]] <- ggplot(data, aes(x=method, y=cor, fill=method)) +
      geom_violin(color = NA, width = 0.65) +
      geom_boxplot(color='#440154FF', width = 0.15) +
      theme_minimal() +
      stat_summary(fun=mean, color='#440154FF', geom='point', 
                   shape=18, size=3, show.legend=FALSE) +
      labs(x=NULL,y='Correlation between True and Predicted Phenotype',tag='A') +
      theme(legend.position='none',
            axis.text.x = element_text(angle = -45, size=25),
            text=element_text(size=25),
            plot.tag = element_text(size=50)) +
      scale_fill_viridis(begin = 0.4, end=0.9,discrete=TRUE)
    
    gg[[1]]
    
    virPal <- c('#440154FF', '#472D7BFF', '#3B528BFF', '#2C728EFF', '#21908CFF', '#27AD81FF','#5DC863FF', '#AADC32FF', '#FDE725FF')
    virPal <- viridis(9)
    
  }