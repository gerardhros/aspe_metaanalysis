# own function to plot multiple histograms
# all these plot functions have been defined for a single paper and axis titles or plot titles are specific for this purpose.
ggplot_hist <- function(...) {
  
  obj <- list(...)
  
  # make empty list
  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                           residual = obj[[i]]$`_residuals_`)
    
  }
  out <- rbindlist(out)
  out <- out[order(label)]
  
  # make a residual plot
  if(length(obj) > 1){ 
    
    p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) + 
      geom_histogram(position='identity',fill = '#482173')  +
      geom_density(aes(x=residual,y=..density..),fill = '#482173')+
      facet_wrap(~label,scales = "free_y")
    
  } else {
    
    p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) + 
      geom_histogram(position='identity',fill = '#482173')  +
      geom_density(aes(x=residual,y=..density..),fill = '#482173')  
    
  }

  p <- p + theme_bw() + theme(legend.position = 'none',
                              axis.text = element_text(size = 20),
                              axis.title = element_text(size=20),
                              plot.title = element_text(size=22)) 
  
  
  return(p)
}

# plot 1 to 1 plots

# plot multiple histograms
ggplot_onetoone <- function(...) {
  
  obj <- list(...)
  
  # make empty list
  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                           obs = obj[[i]]$`_y_`,
                           pred = obj[[i]]$`_y_hat_`)
    
  }
  out <- rbindlist(out)
  out <- out[order(label)]
  out[,label := gsub('_lm','_glm',label)]
  
  # make a residual plot
  if(length(obj) > 1){ 
    
    p <- ggplot(out,aes(x=obs,y=pred,col='#482173')) + 
      geom_point(fill = '#482173',col='#482173')  + geom_abline(intercept = 0,slope = 1,lty=2)+ 
      facet_wrap(~label)
    
  } else {
    
    p <- ggplot(out,aes(x=obs,y=pred,col='#482173')) + 
      geom_point(fill = '#482173',col='#482173')  + geom_abline(intercept = 0,slope = 1,lty=2)
    
  }
  
  p <- p + theme_bw() + theme(legend.position = 'none',
                              axis.text = element_text(size = 20),
                              axis.title = element_text(size=20),
                              plot.title = element_text(size=22)) + 
    ylab('Predicted EF') + 
    xlab('Observed EF')
  
  return(p)
}

ggplot_ale2 <- function(...,type='numeric'){
  
  obj <- list(...)
  
  # make empty list
  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    out[[i]] <- data.table(parm = unique(obj[[i]]$`_vname_`),
                           label = unique(obj[[i]]$`_label_`),
                           y = obj[[i]]$`_yhat_`,
                           x = obj[[i]]$`_x_`)
    
  }
  out <- rbindlist(out)
  out <- out[order(label)]
  
  # remove log from the label
  out[,parm:= gsub('ln_','',parm)]
  
  # set axes
  xaxmax <- min(3,max(out$x))
  xaxmin <- max(-3,min(out$x))
  
  # make a ALE plot
  if(length(obj) > 1){ 
    
    p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
      geom_point()  + geom_smooth()+
      facet_wrap(~label,scales='free')
    
  } else {
    
    p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
      geom_point()  + geom_smooth()
    
  }
  
  p <- p + theme_bw() + theme(legend.position = 'bottom') + 
    ylab('Change in average predicted EF') + 
    xlab('Change in explanatory variable (scaled to unit variance)') +
    xlim(xaxmin,xaxmax) +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size=10))
  
  return(p)
}


ggplot_ale <- function(...,ftitle = NULL,pncol = NULL,tsc = 1){
  
  obj <- list(...)
  
  # make empty list
  out = out.cat = out.num = pout.cat = pout.num = list()
  count.cat = count.num = 0
  
  # convert all residual objects into one data.table
  for(i in 1:length(obj)){
      
      # plot title
      pt = list()
    
      # overwrite object name
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='fert_cat'] <- 'fertilizer type'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='crop_cat'] <- 'crop type'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='nsplitc'] <- 'number of doses'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='appc'] <- 'application mode'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='temp'] <- 'temperature'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='prec'] <- 'precipitation'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='ph'] <- 'soil pH'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='ndose'] <- 'N dose'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='clay'] <- 'clay content'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='cec'] <- 'CEC'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='soc'] <- 'SOC'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='crop_cat'] <- 'crop type'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='GEnZ'] <- 'climate zone'
      obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='phtype'] <- 'pH type'
      
      if(is.null(ftitle)){
        pt[[i]] <- paste0('Impact of ',unique(obj[[i]]$`_vname_`),"on EF")
      
      } else {
        
        fname <- paste0("impact of ",unique(obj[[i]]$`_vname_`)," ")
        if(ftitle[i]=='nh3') pt[[i]] <- substitute(paste(a,"on EF for N-NH"[3]),list(a=fname))
        if(ftitle[i]=='n2o') pt[[i]] <- substitute(paste(a,"on EF for N-N"[2],"O"),list(a=fname))
        if(ftitle[i]=='no') pt[[i]] <- substitute(paste(a,"on EF for N-NO"),list(a=fname))
        if(ftitle[i]=='no3') pt[[i]] <- substitute(paste(a,"on EF for N-NO"[3]),list(a=fname))
      }
      
      if(class(obj[[i]]$`_x_`)=='factor'){
        
        count.cat <- count.cat + 1
        out <- data.table(parm = unique(obj[[i]]$`_vname_`),
                                   label = unique(obj[[i]]$`_label_`),
                                   y = obj[[i]]$`_yhat_`,
                                   x = obj[[i]]$`_x_`,
                                   pname = ftitle[i])
        
        p <- ggplot(out,aes(x = x,y = y,color=parm,fill=parm)) + 
             geom_bar(stat='identity',show.legend = FALSE)  +
             facet_wrap(~label,scales='free')
        
        p <- p + theme_bw() + 
            scale_color_viridis_d(name ='driver')+
            scale_fill_viridis_d(name ='driver')+
            ylab(expression(delta*"EF")) + 
            xlab('') +
            theme(axis.text.x = element_text(size = 20),
                  axis.text.y = element_text(size = 20),
                  axis.title = element_text(size=20),
                  plot.title = element_text(size=22),
                  strip.background = element_blank(),
                  strip.text = element_blank()) +
            ggtitle(label=pt[[i]])
        
        pout.cat[[count.cat]] <- p
        
      } else {
        
        out.num[[i]] <- data.table(parm = unique(obj[[i]]$`_vname_`),
                                   label = unique(obj[[i]]$`_label_`),
                                   y = obj[[i]]$`_yhat_`,
                                   x = obj[[i]]$`_x_`,
                                   pname = ftitle[i])
        
      }
    }
      
  if(length(out.num)>0){
    
    # combine numeric ones per pname
    out <- rbindlist(out.num)
    
    # set factor to ensure order of plots
    glevel <- c('nh3','n2o','no','no3')
    glevel <- glevel[glevel %in% unique(out$pname)]
    out[,pname := factor(pname,levels = glevel)]
    
    # split over the emission route
    out2 <- split(out,out$pname)
    
    count.num <- 0
    
    xaxmax <- min(3,max(out$x))
    xaxmin <- max(-3,min(out$x))
    
    for(j in 1:length(out2)){
      
      count.num <- count.num + 1
      
      out3 <- out2[[j]]
      
      #set title
      opn <- out3$pname[1]
      if(length(unique(out3$parm))==1){
        fname <- paste0("Impact of ",unique(out3$parm)," ")
      } else {
        fname <- paste0("Impact of site properties ")
      }
      
      if(opn=='nh3') pt <- substitute(paste(a,"on EF for N-NH"[3]),list(a=fname))
      if(opn=='n2o') pt <- substitute(paste(a,"on EF for N-N"[2],"O"),list(a=fname))
      if(opn=='no') pt <- substitute(paste(a,"on EF for N-NO"),list(a=fname))
      if(opn=='no3') pt <- substitute(paste(a,"on EF for N-NO"[3]),list(a=fname))
      
      if(j==1){
        p <- ggplot(out3,aes(x=x,y=y,color=parm,group=parm)) + 
          geom_point(show.legend = TRUE)+ geom_smooth(show.legend = FALSE) 
      } else {
        p <- ggplot(out3,aes(x=x,y=y,color=parm,group=parm)) + 
          geom_point(show.legend = FALSE)  + geom_smooth(show.legend = FALSE)
      }
      
      
      p <- p + theme_bw() + 
        theme(legend.position ='inside',
              legend.position.inside = c(0.2,0.8)) + 
        #scale_color_viridis_d(name ='driver')+
        scale_color_manual(name = 'driver',
                           values = c('soil pH'='red3','N dose'='black','clay content'='green3',
                                      'precipitation'='skyblue','SOC'='gray75','temperature'='orange'))+
        ylab(expression(delta*"EF")) + 
        xlab('Change in variable') +
        xlim(xaxmin,xaxmax) +
        theme(axis.text.x = element_text(size = 20*tsc),
              axis.text.y = element_text(size = 20*tsc),
              axis.title = element_text(size=20*tsc),
              plot.title = element_text(size=22*tsc),
              legend.text = element_text(size=20*tsc),
              legend.title = element_text(size=20*tsc),
              strip.background = element_blank(),
              strip.text = element_blank()
        ) +
        ggtitle(pt) 
      
        if (j==1 & length(unique(out3$parm))>6){p <- p + guides(colour=guide_legend(ncol=2))}
      
      pout.num[[count.num]] <- p
      rm(p)
    }
  }
     
   
     ptout <- as.list(c(pout.cat,pout.num))
     
     if(is.null(pncol)){pncol =  min(length(ptout),4)} else {pncol = pncol}
     p <- patchwork::wrap_plots(ptout, ncol=pncol,axis_titles = 'collect')  
    
  
  return(p)
}


ggplot_pdp <- function(obj, x) {
  
  
  out <- as.data.table(obj$agr_profiles)
  out[,label]
  p <- 
    as_tibble() %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              size = 0.5, alpha = 0.05, color = "gray50")
  
  num_colors <- n_distinct(obj$agr_profiles$`_label_`)
  
  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
  }
  
  p
}

ggplot_vip <- function(...,vexclude = NULL,ftitle = NULL,plotrow = 1){
  
  obj <- list(...)

  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    obj2 <- obj[[i]]
    
    if(!is.null(vexclude)){
      obj2 <- obj2[!obj2$variable %in% c(vexclude),]
    } 
    
    # plot
    if(is.null(ftitle)){
      pt = pst = list()
      pst[[i]] <- 'created for meta-regression model predicting EF'
      pt[[i]] <- 'Feature importance plot'
      } else {
      
      pt = pst = list()
      if(ftitle[i]=='nh3') pst[[i]] <- expression(paste("created for meta-regression model for EF-NH"[3]))
      if(ftitle[i]=='n2o') pst[[i]] <- expression(paste("created for meta-regression model for EF-N"[2],"O"))
      if(ftitle[i]=='no') pst[[i]] <- expression(paste("created for meta-regression model for EF-NO"))
      if(ftitle[i]=='no3') pst[[i]] <- expression(paste("created for meta-regression model for EF-NO"[3]))
      
      if(ftitle[i]=='nh3') pt[[i]] <- expression(paste("Feature importance for N-NH"[3]))
      if(ftitle[i]=='n2o') pt[[i]] <- expression(paste("Feature importance for N-N"[2],"O"))
      if(ftitle[i]=='no') pt[[i]] <- expression(paste("Feature importance for N-NO"))
      if(ftitle[i]=='no3') pt[[i]] <- expression(paste("Feature importance for N-NO"[3]))
    }
    
    # adjust the names of the variables
    obj2[obj2$variable=='temp','variable'] <- 'temperature'
    obj2[obj2$variable=='prec','variable'] <- 'precipitation'
    obj2[obj2$variable=='ph','variable'] <- 'soil pH'
    obj2[obj2$variable=='ndose','variable'] <- 'N dose'
    obj2[obj2$variable=='clay','variable'] <- 'clay content'
    obj2[obj2$variable=='cec','variable'] <- 'CEC'
    obj2[obj2$variable=='soc','variable'] <- 'SOC'
    obj2[obj2$variable=='crop_cat','variable'] <- 'crop type'
    obj2[obj2$variable=='fert_cat','variable'] <- 'fertilizer type'
    obj2[obj2$variable=='nsplitc','variable'] <- 'number of doses'
    obj2[obj2$variable=='appc','variable'] <- 'application mode'
    obj2[obj2$variable=='GEnZ','variable'] <- 'climate zone'
    obj2[obj2$variable=='phtype','variable'] <- 'pH type'
    obj2[obj2$variable=='measmethod','variable'] <- 'measurement method'
    
    out[[i]] <- plot(obj2) + theme_bw() + theme(legend.position = 'none') + 
                ggtitle(label=pt[[i]],
                        subtitle=pst[[i]])+
                ylab('') +
                theme(axis.text = element_text(size = 20),
                      axis.title = element_text(size=18),
                      plot.title = element_text(size=20),
                      plot.subtitle = element_text(size=18),
                      strip.background = element_blank(),
                      strip.text = element_blank()) 
    
  } 
  names(out) <- paste0('p',1:length(obj))
  print(names(out))
  pout <- patchwork::wrap_plots(out, nrow=plotrow)
  
  return(pout)}


  
  
