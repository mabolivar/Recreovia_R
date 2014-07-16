#==============================================================================
# Working directory
#==============================================================================

setwd("C:\\Users\\ma.bolivar643\\Dropbox\\Accelerometria\\RECREOVIA")

#==============================================================================
# Required libraries
#==============================================================================

library("chron")
library("scales")
source(".\\R\\ADPP.R")

#==============================================================================
# Functions
#==============================================================================

# Identifies the bouts of physical activity (moderate to vigorous) using a maximum total break duration criteria
boutsMaxTotalBreakDuration <- function(PAvector, boutlen = 10, bouttol = 2){
  # Identifies the bouts of physical activity (moderate to vigorous) 
  # using a maximum total break duration criteria
  #
  # Args:
  #   PAVector: Character vector with the physical intensity level for each epoch. 
  #   boutlen: Minimum number of epochs of a bouth duration.
  #   bouttol: Maximum number of minutes for break within a bout (tolerance).
  # Returns:
  #   A data frame with all the bouts information. Each row contains:
  #     - Start and end index of a bout.
  #     - Duration (in minutes)
  #     - Minutes of break
  #     - Minutes of moderate physical activity intensity level
  #     - Minutes of vigorous physical activity intensity level
  #
  
  start <- vector()              #Vector with the start of the bout
  end <- vector()                #Vector with the end of the bout
  duration <- vector()            #Vector with the bouts duration (minutes)
  breaks <- vector()              #Vector with the break minutes per bout
  moderate <- vector()            #Vector with the moderate PA minutes per bour
  vigorous <- vector()            #Vector with the vigorous PA minutes per bour
  
  boutdef <- c("moderate","vigorous", "very vigorous")
  
  boutcount <- 0 #Counter
  idx <- 0 #index
  i <- 1 #actual row
  
  #Column to identify non wear periods (ActiLife uses mv)
  
  while(i <= length(PAvector)){
    
    if(PAvector[i]%in% boutdef ){
      idx<-i
      c <- 0    #counts of violations
      bk <- 0   #Number of violations within the bout
      mod <- 0  #Moderate PA minutes within the bout
      vig <- 0  #Vigorous PA minutes within the bout
      boutcount<-0
      lstz <- 0 #lastzero
      newi <- 0 #new index i
      j<-i
      #While exists at least 120 sec of activity (tolerance) -> PAvector
      #non-wear valid periods must be more than 3600sec (60min)
      while(c<=bouttol && j <= length(PAvector)){
        if(PAvector[j] %in% boutdef){
          boutcount<- boutcount+1
          bk <- c
          mod <- mod + (PAvector[j]==boutdef[1])
          vig <- vig + (PAvector[j]==boutdef[2])
          lstz <- j
        }else{
          boutcount<- boutcount+1
          c <- c+1
          newi <- newi + j*(c==1)
        }
        j <- j+1
      }
      if(lstz-i+1>=boutlen){
        start <- c(start,idx)
        end <- c(end,lstz)
        duration <- c(duration, lstz-idx+1)
        breaks <- c(breaks, bk)
        moderate <- c(moderate, mod)
        vigorous <- c(vigorous, vig)
        i <- lstz
      }
      else{
        if(newi != 0){
          i <- newi
        }
      }
      
    }
    i <- i+1
  }
  
  dta <- data.frame(start,end,duration,breaks,moderate,vigorous)
  return(dta)
  
  
}

boutsPercActiv <- function(PAvector, boutlen = 10, bouttol = 2, ratio = 0.2){
  # Identifies the bouts of physical activity (moderate to vigorous) 
  # using a maximum total break duration criteria
  #
  # Args:
  #   PAVector: Character vector with the physical intensity level for each epoch. 
  #   boutlen: Minimum number of epochs of a bouth duration.
  #   bouttol: Maximum number of epochs for break within a bout (tolerance) before 
  #            the bout duration is greater than boutlen.
  #   ratio: Allowed ratio between the epochs of moderate to vigorous physical activity
  #          and the total epochs of bout duration (including breaks). After the 
  #          bout duration is greater than boutlen, the maximum break length (epochs) 
  #          is defined as: [total bout duration]*[ratio]
  # Returns:
  #   A data frame with all the bouts information. Each row contains:
  #     - Start and end index of a bout.
  #     - Duration (in minutes)
  #     - Minutes of break
  #     - Minutes of moderate physical activity intensity level
  #     - Minutes of vigorous physical activity intensity level
  #
  
  start <- vector()              #Vector with the start of the bout
  end <- vector()                #Vector with the end of the bout
  duration <- vector()            #Vector with the bouts duration (minutes)
  breaks <- vector()              #Vector with the break minutes per bout
  moderate <- vector()            #Vector with the moderate PA minutes per bour
  vigorous <- vector()            #Vector with the vigorous PA minutes per bour
  boutdef <- c("moderate","vigorous")
  
  boutcount <- 0 #Counter
  idx <- 0 #index
  i <- 1 #actual row
  
  #Column to identify non wear periods (ActiLife uses mv)
  
  while(i <= length(PAvector)){
    
    if(PAvector[i]%in% boutdef ){
      idx<-i
      c <- 0    #counts of violations
      bk <- 0   #Number of violations within the bout
      mod <- 0  #Moderate PA minutes within the bout
      vig <- 0  #Vigorous PA minutes within the bout
      boutcount<-0
      lstz <- 0 #lastzero
      newi <- 0 #new index i
      j<-i
      while(c<=ifelse(boutcount>boutlen,boutcount*ratio,bouttol) && j <= length(PAvector)){
        if(PAvector[j] %in% boutdef){
          boutcount<- boutcount+1
          bk <- c
          mod <- mod + (PAvector[j]==boutdef[1])
          vig <- vig + (PAvector[j]==boutdef[2])
          lstz <- j
        }else{
          boutcount<- boutcount+1
          c <- c+1
          newi <- newi + j*(c==1)
        }
        j <- j+1
      }
      if(lstz-i+1>=boutlen){
        start <- c(start,idx)
        end <- c(end,lstz)
        duration <- c(duration, lstz-idx+1)
        breaks <- c(breaks, bk)
        moderate <- c(moderate, mod)
        vigorous <- c(vigorous, vig)
        i <- lstz
      }
      else{
        if(newi != 0){
          i <- newi
        }
      }
      
    }
    i <- i+1
  }
  
  dta <- data.frame(start,end,duration,breaks,moderate,vigorous)
  return(dta)
  
}

#Labels the epochs within the intervals parameter (intv) in the vector x
setValues <- function(x, v, intv){
  # Labels the epochs within the intervals parameter (intv) in the vector x
  #
  # Args:
  #   x: Activity vector
  #   v: Label to classify the epochs
  #   intv: 2-column matrix in which each rows is an interval to be labeled 
  #
  # Returns:
  #   The updated activity column.
  
  if(nrow(intv)>=1){
    for(r in seq(1,nrow(intv))){
      x[intv[r,1]:intv[r,2]] <- rep(v,intv[r,2]-intv[r,1]+1)
    }
  }
  return(x)
}

#Get the last n elements of the string n
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#==============================================================================
# Main
#==============================================================================


inputdir <- ".\\data"
outputdir <- ".\\output"
outputfile <- "COL PA_20140305.csv"

d <- "A1_0003_01.agd"
adata <- data.frame()

main <-function(){
  
  tnow <- Sys.time()
  
  #Get datafiles names
  dataFiles <- dir(inputdir)
  
  #Read the PACK
  #pack <- readPack("_________.xlsx",sheets)  
  
  #Agreggate data
  
  
  for(d in dataFiles){
    cat(d)
    dbDir <- paste(inputdir,"\\",d,sep="")
    
    #Check file size
    if(file.info(dbDir)$size/1000<100){
      cat("...........Wrong file size\n")
    }else{
      
      #1. Read database      
      db <- readDatabase(dbDir)
      data <- db$data
      settings <- db$settings
      
      #2. Identify non wear periods
      nw <- nonWearPeriods(data,innactivity=60,tolerance=0,scanParam="axis1")
      data$activity <- setValues(rep("wear",nrow(data)),"non-wear", nw)
      
      #4. PA identification
      data$PA <- mapply(FUN=cut_points_freedsonAdults1998,data$axis1,60)
      
      #5. Identify bouts
      bouts <- boutsPercActiv(PAvector=data$PA,boutlen=10,bouttol=2, ratio = 0.2)
      #bouts2 <- boutsMaxTotalBreakDuration(PAvector=data$PA,boutlen=10,bouttol=2)  
      data$bouted <- setValues(rep("unbout",nrow(data)),"bouted", bouts)
      
      #6. Remove invalid data
      sample_days <- sort(unique(as.Date(data$datetime)))
      first_day <- min(sample_days)
      last_day <- max(sample_days)
      toremove <- sample_days[sample_days>first_day+6]
      data <- removeData(data,toremove=c(toremove,last_day),units="days")
      
      #7. Consolidation
      mindays <- 4 #minimum number of days for a participant to be valid
      minminutes <- 600 #minimum number of minutes of a valid day
      
      #General information
      wearperday <- as.data.frame.matrix(table(as.Date(data$datetime),data$activity))[["wear"]]
      names(wearperday) <- row.names(as.data.frame.matrix(table(as.Date(data$datetime),data$activity)))
      valid_days <- as.Date(names(wearperday)[wearperday>=minminutes])
      bouts$day <- as.Date(data$datetime[bouts[,1]])
      bouts$weekday <- weekdays(bouts$day)
      bouts$valid <- (bouts$day%in%valid_days)
      data$day <- as.Date(data$datetime)
      data$weekday <- weekdays(data$day)
      
      #Week days definition
      saturday <- weekdays(as.Date("2013-07-13"))
      sunday <- weekdays(as.Date("2013-07-14"))
      weekend <- c(saturday,sunday) #Saturday and sunday
      midweek <- subset(unique(data$weekday), subset= !(unique(data$weekday)%in%weekend) )
      
      
      #Alltime bout derived variables
      valid_bouts <-bouts[bouts$valid==T,]
      btMVPA_num <- nrow(valid_bouts)
      btVPA_num <- sum(valid_bouts$vigorous>=1)
      bt_min <- sum(valid_bouts$duration)
      bt_rest <- sum(valid_bouts$breaks)
      bt_MPA <- sum(valid_bouts$moderate)
      bt_VPA <- sum(valid_bouts$vigorous)
      bt_MVPA <- bt_MPA+bt_VPA
      
      #Midweek bout derived variables
      valid_bouts <-bouts[(bouts$valid==T & bouts$weekday%in%midweek),]
      btMVPA_num_WK <- nrow(valid_bouts)
      btVPA_num_WK <- sum(valid_bouts$vigorous>=1)
      bt_min_WK <- sum(valid_bouts$duration)
      bt_rest_WK <- sum(valid_bouts$breaks)
      bt_MPA_WK <- sum(valid_bouts$moderate)
      bt_VPA_WK <- sum(valid_bouts$vigorous)  
      bt_MVPA_WK <- bt_MPA_WK+bt_VPA_WK
      
      #Weekend bout derived variables
      valid_bouts <-bouts[(bouts$valid==T & bouts$weekday%in%weekend),]
      btMVPA_num_WD <- nrow(valid_bouts)
      btVPA_num_WD <- sum(valid_bouts$vigorous>=1)
      bt_min_WD <- sum(valid_bouts$duration)
      bt_rest_WD <- sum(valid_bouts$breaks)
      bt_MPA_WD <- sum(valid_bouts$moderate)
      bt_VPA_WD <- sum(valid_bouts$vigorous) 
      bt_MVPA_WD <- bt_MPA_WD+bt_VPA_WD
      
      #Sunday bout derived variables
      valid_bouts <-bouts[(bouts$valid==T & bouts$weekday%in%sunday),]
      btMVPA_num_S <- nrow(valid_bouts)
      btVPA_num_S <- sum(valid_bouts$vigorous>=1)
      bt_min_S <- sum(valid_bouts$duration)
      bt_rest_S <- sum(valid_bouts$breaks)
      bt_MPA_S <- sum(valid_bouts$moderate)
      bt_VPA_S <- sum(valid_bouts$vigorous) 
      bt_MVPA_S <- bt_MPA_S+bt_VPA_S
      
      
      #Extract the participant id
      m<-regexec("\\\\[[:print:]]+\\\\[A-Z0-9]+[[:punct:]]([[:digit:]]+)",dbDir)
      PID <- regmatches(dbDir, m)[[1]][2]
      PID <- as.data.frame(PID)
      
      #Total PA derived variables
      VldDays <- length(valid_days)
      VldDays_WK <-sum(weekdays(valid_days)%in%midweek == T)
      VldDays_WD <-sum(weekdays(valid_days)%in%weekend == T)
      Valid <- ifelse(weekdays(as.Date("2013-07-14")) %in% weekdays(valid_days) && VldDays>=mindays,1,0)
      
      valid_data <- data[data$day%in%valid_days,]
      Tot_MinMVPA <- sum((valid_data$PA=="moderate" | valid_data$PA=="vigorous")==T)
      Tot_MinMPA <- sum((valid_data$PA=="moderate")==T)
      Tot_MinVPA <- sum((valid_data$PA=="vigorous")==T) 
      
      valid_data <- data[(data$day%in%valid_days & weekdays(data$day)%in%midweek),]
      Tot_MinMVPA_WK <- sum((valid_data$PA=="moderate" | valid_data$PA=="vigorous")==T)
      Tot_MinMPA_WK <- sum((valid_data$PA=="moderate")==T)
      Tot_MinVPA_WK <- sum((valid_data$PA=="vigorous")==T) 
      
      valid_data <- data[(data$day%in%valid_days & weekdays(data$day)%in%weekend),]
      Tot_MinMVPA_WD <- sum((valid_data$PA=="moderate" | valid_data$PA=="vigorous")==T)
      Tot_MinMPA_WD <- sum((valid_data$PA=="moderate")==T)
      Tot_MinVPA_WD <- sum((valid_data$PA=="vigorous")==T) 
      
      
      aa <- data.frame(PID, Valid, VldDays,	VldDays_WK,	VldDays_WD,	Tot_MinMVPA,	Tot_MinMPA,	Tot_MinVPA,	
                       Tot_MinMVPA_WK,	Tot_MinMPA_WK,	Tot_MinVPA_WK,
                       Tot_MinMVPA_WD, Tot_MinMPA_WD,	Tot_MinVPA_WD, 
                       btMVPA_num,btVPA_num,bt_min,bt_rest,bt_MVPA,bt_MPA,bt_VPA,
                       btMVPA_num_WK,btVPA_num_WK,bt_min_WK,bt_rest_WK,bt_MVPA_WK,bt_MPA_WK,bt_VPA_WK,
                       btMVPA_num_WD,btVPA_num_WD,bt_min_WD,bt_rest_WD,bt_MVPA_WD,bt_MPA_WD,bt_VPA_WD,
                       btMVPA_num_S,btVPA_num_S,bt_min_S,bt_rest_S,bt_MVPA_S,bt_MPA_S,bt_VPA_S)
      adata <- rbind(adata,aa)
      write.csv(adata,file=paste(outputdir,"\\",outputfile, sep="",collapse=""), row.names=F)
      
      cat("...........OK\n")
    }
    
  }
  
  print(paste("Total time:", as.numeric(Sys.time()-tnow,units="mins"),"mins"))
  write.csv(adata,file=paste(outputdir,"\\",outputfile, sep="",collapse=""), row.names=F)
  
}

#=============================================================================

tryCatch(main(),finally=sink())

#=============================================================================
