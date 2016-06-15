require(blockrand)
require(adsim)
require(ggplot2)
require(plotly)
require(DT)
require(RColorBrewer)
require(knitr)
require(nlme)
require(contrast)
require(shiny)
require(shinydashboard)
require(webshot)

screenshot_plotly = function(x, file = 'webshot.png', ...) { 
  file2 = normalizePath(file, mustWork = FALSE) 
  d = tempfile() 
  dir.create(d) 
  owd = setwd(d) 
  on.exit({ 
    setwd(owd) 
    unlink(d, recursive = TRUE) 
  }, add = TRUE) 
  htmlwidgets::saveWidget(plotly::as.widget(x), 'index.html', FALSE) 
  file.copy(webshot::webshot('index.html', ...), file2) 
  file 
} 

data(posterior)
postMed <- apply(posterior, 2, median)

parallel_one <- function(n.per.arm, lbMmse.use=14, ubMmse.use=26,emax1 = 0,emax2 = 0.101,
                         et50 = 1, et50wash = 1, edm = c(0,0.4),duration = 84,
                         dropout = "Yes",drug = "Both"){
  n.enrolled <- n.per.arm * 2
  treatments <- c("Placebo","Treatment")
  if (drug == "Both"){
    emax = c(emax1,emax2)
  }
  if (drug == "Symptomatic"){
    emax = c(emax1,emax2)
    edm = c(0,0)
  }
  if (drug == "Disease Modifying"){
    emax = c(0,0)
  }
  TrtSeqTable <- data.frame(Arm = LETTERS[1:2],
                            DrugDose = treatments,
                            DoseBegin = c(0, 0),
                            DoseEnd = c(duration,duration)
  )
  TrtParTable <- data.frame(
    DrugDose = treatments,
    emaxSx = emax,
    et50Sx = et50,
    et50SxWash = et50wash, 
    eDm = edm 
  )
  patientTable <- acRecruit(n.enrolled, postMed,lbMmse=lbMmse.use, ubMmse=ubMmse.use)
  baseTable <- acRandomize(patientTable, TrtSeqTable,TrtParTable)
  t.global <- seq(0,duration,length.out = 8)
  if (dropout == "Yes"){
    baseTable$dropT <- dropTime(baseTable, postMed)
    xOverDat <- acRun(postMed, baseTable, t.global)
    xOverDat <- xOverDat[ xOverDat$Time < xOverDat$dropT,]
  }
  else {xOverDat <- acRun(postMed, baseTable, t.global)}
  epresult <- pAnalyzeParallel(xOverDat,ep.time = duration)
  
  pp <- ggplot(xOverDat, aes(x = jitter(Time), y = AdasCog)) 

  plot <- pp + theme(legend.position="none") + 
    geom_smooth(aes(colour=DrugDose,fill = DrugDose, se=FALSE)) + 
    facet_wrap(~ DrugDose, ncol=2) + xlab("Week") + ylab("AdasCog")
  
  plot1 <- ggplot(xOverDat,aes(x=Time, y = AdasCog, group = Patient)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + facet_wrap(~ DrugDose) + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  
  baseTable$Arm <- baseTable$dropT <- NULL
  list("test" = epresult, "stat" = baseTable, "plot" = ggplotly(plot), 
       "plot1" = ggplotly(plot1),"stat1" = xOverDat)
}
 
#cross-over, Both, 1 sim
xover_one <- function(n.per.arm, lbMmse.use=14, ubMmse.use=26,emax1 = -0.1,
                           et50 = 1, et50wash = 1, edm = 0.4, firstduration = 6,
                           frequency = 3,washout = 3, dropout = "Yes",drug = "Both"){
  
  n.enrolled <- n.per.arm * 2
  treatments <- c("Treatment","Placebo")
  if (drug == "Both"){
    emax = emax1
  }
  if (drug == "Symptomatic"){
    emax = emax1
    edm = 0
  }
  if (drug == "Disease Modifying"){
    emax = c(0,0)
  }
  TrtParTable <- data.frame(
      DrugDose = treatments,
      emaxSx = emax,
      et50Sx = et50,
      et50SxWash = et50wash, 
      eDm = edm 
    )
  TrtSeqTable <- data.frame(Arm = LETTERS[1:2],
                            DrugDose = treatments,
                            DoseBegin = c(0, firstduration+washout),
                            DoseEnd = c(firstduration, 2*firstduration+washout)
  )
  patientTable <- acRecruit(n.enrolled, postMed,lbMmse=lbMmse.use, ubMmse=ubMmse.use)
  baseTable <- acRandomize(patientTable, TrtSeqTable,TrtParTable)
  t.global <- seq(0,2*firstduration+washout,frequency)
  if (dropout == "Yes"){
    baseTable$dropT <- dropTime(baseTable, postMed)
    xOverDat <- acRun(postMed, baseTable, t.global)
    xOverDat <- xOverDat[ xOverDat$Time < xOverDat$dropT,]
  }
  else {xOverDat <- acRun(postMed, baseTable, t.global)}
  
  xOverDat$Period <- 1 + with(xOverDat, Time >= firstduration+washout)
  xOverDat$Stratum <- factor(ifelse(xOverDat$Bmmse < 20, 'mild', '
moderate'))
  xOverDat$TimeWithin <- with(xOverDat, Time %% firstduration+washout)
  nSites <- 6 
  site <- sample(1:nSites, length(unique(xOverDat$Patient)), replace
  =TRUE)
  xOverDat$Site <- factor(site[xOverDat$Patient])
  epresult <- pAnalyzeXover(xOverDat,ep.time = firstduration)
  
  pp <- ggplot(xOverDat, aes(x = jitter(Time), y = AdasCog)) 

  plot <- pp + theme(legend.position="none") + 
    geom_smooth(aes(colour=DrugDose,fill = DrugDose, se=FALSE))+
    facet_wrap(~ DrugDose, ncol=2) + xlab("Week") + ylab("AdasCog") + 
    geom_vline(xintercept = c(firstduration, firstduration + washout),
               linetype = "longdash")
  
  plot1 <- ggplot(xOverDat,aes(x=Time, y = AdasCog, group = Patient)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
         shape = 17, size = 3,color = "dark blue") + facet_wrap(~ DrugDose) + 
     theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),legend.position="none")
  
  baseTable$Arm <- baseTable$dropT <- NULL
  list("test" = epresult, "stat" = baseTable, "plot" = ggplotly(plot),
       "plot1"=ggplotly(plot1),"stat1" = xOverDat)
} 
  
delay_one <- function(n.per.arm, lbMmse.use=14, ubMmse.use=26,emax1 = 0,emax2 = 0.101,
                         et50 = 1, et50wash = 1, edm = c(0.4,0.4),start = 52,
                         period = 91,dropout = "Yes",drug = "Both"){
  n.enrolled <- n.per.arm * 2
  treatments <- c("Treatment","Placebo")
  t.global <- c(0,start/2,start,period - start/2,period)
  if (drug == "Both"){
    emax = c(emax1,emax2)
  }
  if (drug == "Symptomatic"){
    emax = c(emax1,emax2)
    edm = c(0,0)
  }
  if (drug == "Disease Modifying"){
    emax = c(0,0)
  }
  TrtSeqTable <- data.frame(Arm = LETTERS[1:2],
                            DrugDose = treatments,
                            DoseBegin = c(0, start),
                            DoseEnd = period
  )
  TrtParTable <- data.frame(
    DrugDose = treatments,
    emaxSx = emax,
    et50Sx = et50,
    et50SxWash = et50wash, 
    eDm = edm 
  )
  patientTable <- acRecruit(n.enrolled, postMed,lbMmse=lbMmse.use, ubMmse=ubMmse.use)
  baseTable <- acRandomize(patientTable, TrtSeqTable,TrtParTable)
  
  if (dropout == "Yes"){
    baseTable$dropT <- dropTime(baseTable, postMed)
    xOverDat <- acRun(postMed, baseTable, t.global)
    xOverDat <- xOverDat[ xOverDat$Time < xOverDat$dropT,]
  }
  else {xOverDat <- acRun(postMed, baseTable, t.global)}

  epresult <- pAnalyzeDelayedStart(xOverDat,ep.time = c(start,max(t.global)),
                                   stab.time = c(period-start/2,max(t.global)))
  
  pp <- ggplot(xOverDat, aes(x = jitter(Time), y = AdasCog)) 
 
  plot <- pp + theme(legend.position="none") + 
    geom_smooth(aes(colour=DrugDose,fill = DrugDose, se=FALSE)) + 
    facet_wrap(~ DrugDose, ncol=2) + xlab("Week") + ylab("AdasCog") +
    geom_vline(xintercept = start,linetype = "longdash")
  
  plot1 <- ggplot(xOverDat,aes(x=Time, y = AdasCog, group = Patient)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + facet_wrap(~ DrugDose) + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  
  baseTable$Arm <- baseTable$dropT <- NULL
  list("test" = epresult, "stat" = baseTable, "plot" = ggplotly(plot), 
       "plot1" = ggplotly(plot1),"stat1" = xOverDat)
}






xover_sim <- function(nSim,n.per.arm, lbMmse.use=14, ubMmse.use=26,emax1 = -0.1,
                      et50 = 1, et50wash = 1, edm = 0.4, firstduration = 6,
                      frequency = 3,washout = 3, dropout = "Yes",drug = "Both"){
  datList <- lapply(1:nSim,function(simNum){
    xover <- xover_one(n.per.arm = n.per.arm, lbMmse.use=lbMmse.use, ubMmse.use=ubMmse.use,
                       emax1 = emax1,
              et50 = et50, et50wash = et50wash, edm = edm, firstduration = firstduration,
              frequency = frequency,washout = washout, dropout = dropout,drug = drug)
    xover$stat1
  })
  resList <- lapply(datList, pAnalyzeXover, ep.time = firstduration)
  res <- do.call('rbind',resList)
  mns <- lapply(1:length(datList),function(i){with(datList[[i]], 
                tapply(AdasCog, list(DrugDose, Time), mean))})
  mns <- t(do.call('rbind',mns))
  pla <- mns[,colnames(mns) == 'Placebo']
  trt <- mns[,colnames(mns) == 'Treatment']
  colnames(pla) <- colnames(trt) <- NULL
  spla <- stack(as.data.frame(pla))
  strt <- stack(as.data.frame(trt))
  spla$x <- strt$x <- as.numeric(rep(rownames(pla), ncol(pla)))
  plot1 <- ggplot(spla,aes(x=x, y = values, group = ind)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  plot2 <- ggplot(strt,aes(x=x, y = values, group = ind)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  list("data" = datList, "res" = res,"plot1" = ggplotly(plot1),"plot2" = ggplotly(plot2))
}

parallel_sim <- function(nSim,n.per.arm, lbMmse.use=14, ubMmse.use=26,emax1 = 0,emax2 = 0.101,
                         et50 = 1, et50wash = 1, edm = c(0,0.4),duration = 84,
                         dropout = "Yes",drug = "Both"){
  datList <- lapply(1:nSim,function(simNum){
    parallel <- parallel_one(n.per.arm = n.per.arm, lbMmse.use=lbMmse.use, ubMmse.use=ubMmse.use,
                             emax1 = emax1,emax2 = emax2,
                             et50 = et50, et50wash = et50wash, edm = edm,duration = duration,
                       dropout = dropout,drug = drug)
    parallel$stat1
  })
  resList <- lapply(datList, pAnalyzeParallel, ep.time = duration)
  res <- do.call('rbind',resList)
  mns <- lapply(1:length(datList),function(i){with(datList[[i]], 
                                                   tapply(AdasCog, list(DrugDose, Time), mean))})
  mns <- t(do.call('rbind',mns))
  pla <- mns[,colnames(mns) == 'Placebo']
  trt <- mns[,colnames(mns) == 'Treatment']
  colnames(pla) <- colnames(trt) <- NULL
  spla <- stack(as.data.frame(pla))
  strt <- stack(as.data.frame(trt))
  spla$x <- strt$x <- as.numeric(rep(rownames(pla), ncol(pla)))
  plot1 <- ggplot(spla,aes(x=x, y = values, group = ind)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  plot2 <- ggplot(strt,aes(x=x, y = values, group = ind)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  list("data" = datList, "res" = res,"plot1" = ggplotly(plot1),"plot2" = ggplotly(plot2))
}


delay_sim <- function(nSim,n.per.arm, lbMmse.use=14, ubMmse.use=26,emax1 = 0,emax2 = 0.101,
                      et50 = 1, et50wash = 1, edm = c(0.4,0.4),start = 52,
                      period = 91, dropout = "Yes",drug = "Both"){
  datList <- lapply(1:nSim,function(simNum){
    delay <- delay_one(n.per.arm = n.per.arm, lbMmse.use=lbMmse.use, ubMmse.use=ubMmse.use,
                             emax1 = emax1,emax2 = emax2,
                             et50 = et50, et50wash = et50wash, edm = edm,start = start,
                             period = period,dropout = dropout,drug = drug)
    delay$stat1
  })
  t.global <- c(0,start/2,start,period-start/2,period)
  resList <- lapply(datList, pAnalyzeDelayedStart, ep.time = c(start,max(t.global)),
                    stab.time = c(period-start/2,max(t.global)))
  res <- do.call('rbind',resList)
  mns <- lapply(1:length(datList),function(i){with(datList[[i]], 
                      tapply(AdasCog, list(DrugDose, Time), mean))})
  mns <- t(do.call('rbind',mns))
  pla <- mns[,colnames(mns) == 'Placebo']
  trt <- mns[,colnames(mns) == 'Treatment']
  colnames(pla) <- colnames(trt) <- NULL
  spla <- stack(as.data.frame(pla))
  strt <- stack(as.data.frame(trt))
  spla$x <- strt$x <- as.numeric(rep(rownames(pla), ncol(pla)))
  plot1 <- ggplot(spla,aes(x=x, y = values, group = ind)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  plot2 <- ggplot(strt,aes(x=x, y = values, group = ind)) + 
    geom_line(color='light blue') + geom_point(shape=18,color='light blue') + 
    stat_summary(aes(group=1),geom = "point", fun.y = mean,
                 shape = 17, size = 3,color = "dark blue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),legend.position="none")
  list("data" = datList, "res" = res,"plot1" = ggplotly(plot1),"plot2" = ggplotly(plot2))
}



png('nihao.png')
a$"webshot.png"
dev.off()

