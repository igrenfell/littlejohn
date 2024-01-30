library(lubridate)
library(here)
library(readxl)
rootwd <- here()
setwd(rootwd)
setwd("./data")


datafold <- getwd()
flist <- list.files()
nfiles <- length(flist)

xflist <- Sys.glob(("*.xlsx"))
nxl <- length(xflist)
datalist <- vector("list", nxl)
###Convert all excel to csv
for(i in 1:nxl)
{
  curfile <- xflist[i]
  curmat <- read_xlsx(xflist[i])
  datalist[[i]] <- curmat
  print(i)
}

outlist <- vector("list", nxl)

for(i in 1:nxl)
{
    datamat <- datalist[[i]]
    datamatsub <- datamat[,1:10]
    #names(datamatsub) <- c("Time", "TC1", "TC2", "TC3", "TC3", "TC4", "TC5", "TC6", "Particle Surface", "Rad1", "Rad2")
    
    dfsub <- data.frame(datamatsub)
    #datatimes <- as_datetime(dmat[,1])
    dt <- diff(dfsub[,1])
    freq <- as.numeric(dt[1])
    tcdat <- dfsub[,2:7]
    
    outlistsub <- vector("list", 6)
    for(curtc in 1:dim(tcdat)[2])
    {
      tcvec <- tcdat[,curtc]
      radvec <- datamat$RAD1
      timevec <- freq*(1:length(tcvec ))
      tempspline <- smooth.spline(timevec, tcvec, df = 50)
      tempfilt <- filter(tcvec, filter = rep(1/60, 60), method = "convolution")
      tempfilt[is.na(tempfilt)]<- 0
      indseq <- 1:length(tcvec)
      arrind <- min(indseq[tempspline$y > 500])
      arrtime <- freq * arrind
      startind <- arrind - round(1/freq  )
      endind <- arrind + round(1/freq  )
      
      rad_at_arr_time <- radvec[arrind]
      rad_avg <- mean(radvec[startind:endind])
      arr_time <- freq * arrind
      maxt <- freq * length(tcvec)
      # fout <- gsub(".csv", ".png", datanames[curdat])
      
      # setwd(rootwd)
      
      # setwd("./output")
      # png(fout)
      # plot(timevec, tcvec, type = 'l', xlim = c(mint, maxt))
      # #lines(tempspline, col = "red")
      # lines(timevec, tempfilt, col = "red")
      # 
      # dev.off()
      # minsptime <- freq*min(indseq[tempfilt > 500])
      # maxtemptime <- freq*which.max(tempfilt)
      # deptime <- freq*max(indseq[tempfilt > 500])
      # outdf <- data.frame(rbind(minsptime, maxtemptime, deptime))
      # fout <- gsub(".csv", "-times.txt", datanames[curdat])
      # write.table(outdf, fout)
      outvec <- c(arr_time, rad_at_arr_time, rad_avg)
      outlistsub[[curtc]] <- outvec
      
    }
    
    outmatsub <- do.call(rbind, outlistsub)
    outnames <- paste(xflist[i], "-", 1:6)
    rownames(outmatsub) <- outnames
    colnames(outmatsub) <- c("arr_time", "rad_at_arr_time", "rad_avg")
    
    fnameout <- gsub(".xlsx",  "", xflist[i])
    fnameout <- paste(fnameout, ".txt", sep = "")
    write.table(outmatsub, fnameout, row.names = F, quote = F)
    
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

