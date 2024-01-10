library(lubridate)

rootwd <- here()
setwd(rootwd)
setwd("./data")

datafold <- getwd()
foldlist <- list.files()
nfolds <- length(foldlist)
for(curfold in 1:2)
{
  setwd(datafold)
  setwd(foldlist[curfold])
  flist <- list.files()
  datafile <- grep("csv", flist)
  ndat <- length(flist[datafile])
  datanames <- flist[datafile]
  for(curdat in 1:ndat)
  {
    setwd(datafold)
    setwd(foldlist[curfold])
    
    datamat <- read.table(datanames[curdat], skip = 23, header= F, sep = ",")
    datatimes <- as_datetime(datamat[,1])
    dt <- diff(datatimes)
    freq <- as.numeric(dt[1])
    tcdat <- datamat[,3:8]
    for(curtc in 1:dim(tcdat)[2])
    {
      tcvec <- tcdat[,curtc]
      timevec <- freq*(1:length(tcvec ))
      tempspline <- smooth.spline(timevec, tcvec, df = 50)
      tempfilt <- filter(tcvec, filter = rep(1/60, 60), method = "convolution")
      tempfilt[is.na(tempfilt)]<- 0
      indseq <- 1:length(tcvec)
      mint <- freq * min(indseq[tempspline$y > 50])
      maxt <- freq * length(tcvec)
      fout <- gsub(".csv", ".png", datanames[curdat])
      
      setwd(rootwd)
      
      setwd("./output")
      png(fout)
      plot(timevec, tcvec, type = 'l', xlim = c(mint, maxt))
      #lines(tempspline, col = "red")
      lines(timevec, tempfilt, col = "red")
      
      dev.off()
      minsptime <- freq*min(indseq[tempfilt > 500])
      maxtemptime <- freq*which.max(tempfilt)
      deptime <- freq*max(indseq[tempfilt > 500])
      outdf <- data.frame(rbind(minsptime, maxtemptime, deptime))
      fout <- gsub(".csv", "-times.txt", datanames[curdat])
      write.table(outdf, fout)
    }
    
  }
  
  
  
}

for(curfold in 3:nfolds)
{
  setwd(datafold)
  setwd(foldlist[curfold])
  flist <- list.files()
  datafile <- grep("csv", flist)
  ndat <- length(flist[datafile])
  datanames <- flist[datafile]
  for(curdat in 1:ndat)
  {
    setwd(datafold)
    setwd(foldlist[curfold])
    
    datamat <- read.table(datanames[curdat], skip = 23, header= F)
    datatimes <- as_datetime(datamat[,1])
    dt <- diff(datatimes)
    freq <- as.numeric(dt[1])
    tcdat <- datamat[,3:8]
    for(curtc in 1:dim(tcdat)[2])
    {
      tcvec <- tcdat[,curtc]
      timevec <- freq*(1:length(tcvec ))
      tempspline <- smooth.spline(timevec, tcvec, df = 50)
      tempfilt <- filter(tcvec, filter = rep(1/60, 60), method = "convolution")
      tempfilt[is.na(tempfilt)]<- 0
      indseq <- 1:length(tcvec)
      mint <- freq * min(indseq[tempspline$y > 50])
      maxt <- freq * length(tcvec)
      fout <- gsub(".csv", ".png", datanames[curdat])
      
      setwd(rootwd)
      
      setwd("./output")
      png(fout)
      plot(timevec, tcvec, type = 'l', xlim = c(mint, maxt))
      #lines(tempspline, col = "red")
      lines(timevec, tempfilt, col = "red")
      
      dev.off()
      minsptime <- freq*min(indseq[tempfilt > 500])
      maxtemptime <- freq*which.max(tempfilt)
      deptime <- freq*max(indseq[tempfilt > 500])
      outdf <- data.frame(rbind(minsptime, maxtemptime, deptime))
      fout <- gsub(".csv", "-times.txt", datanames[curdat])
      write.table(outdf, fout)
    }
    
  }
  
  
  
}

