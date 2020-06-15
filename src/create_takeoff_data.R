######################################################################
#  HM @ Jun 2020
#  Program reads a flight data file and extracts the data from the 
#  take-off to write a CSV file

# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

# Capture working directory
work_dir <- getwd()

timech1_0 <- proc.time()

library(data.table)


########################## START OF MAIN PROCEDURE ##############################
#
# file list capture
setwd("data/sample")
fileList <- list.files(path=getwd(), pattern=".csv")

## Escolher o ficheiro pelo n? "s"
#s = 10
#s=25

# alternativa - fazer o enable deste ciclo FOR para todos os ficheiros do folder
for (s in 1:NROW(fileList)) {

  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                      colClasses = c(ACT='character', AC_TYPE='character', ORIGIN='character', 
                      RUNWAY_TO='character', RUNWAY_LD='character', DESTINATION='character', 
                      DATE='Date', FLTNUM1='character', FLTNUM2='character', FLTNUM3='character',
                      FLTNUM4='character', FLT_NUMBER1='character', FLT_NUMBER2='character',
                      FLT_NUMBER3='character', FLT_NUMBER4='character', FLT_NUMBER5='character',
                      FLT_NUMBER6='character', FLT_NUMBER7='character', FLT_NUMBER8='character',
                      CITY_FROM_R='character', CITY_FROM_TO_R='character', CITY_TO_R='character',
                      FROM='character', FROM_TO='character',TO='character', UTC_HOUR='character' )) 

# Take-Off Start and End Points determination
t0 <- min(which(flightdata$FM_FWC==3)) - 80
t1 <- max(which(flightdata$FM_FWC==4)) + 50
delta_time <- (t1-t0)+1

# data colected and calculated for take-off analysis
data_takeoff <- data.frame()

flight_phase <- vector(mode="character", length=delta_time)
flight_phase[1:delta_time] <- "TAKE OFF"

flapl_0 <- vector(mode="character", length=delta_time)
flapl_1 <- vector(mode="character", length=delta_time)
flapl_2 <- vector(mode="character", length=delta_time)
flapl_3 <- vector(mode="character", length=delta_time)
flapl_3[1:delta_time] <- "POS 3"
flapl_f <- vector(mode="character", length=delta_time)

lgl <- min(which(flightdata$LDG_ON_1[t0:t1] == 0 ))
ldg_lh <- vector(mode="character", length=delta_time)
ldg_lh[1:(lgl-1)] <- "ON GROUND"
ldg_lh[lgl:delta_time] <- "IN FLIGHT"

lgr <- min(which(flightdata$LDG_ON_2[t0:t1] == 0 ))
ldg_rh <- vector(mode="character", length=delta_time)
ldg_rh[1:(lgl-1)] <- "ON GROUND"
ldg_rh[lgl:delta_time] <- "IN FLIGHT"

lgn <- min(which(flightdata$LDGN_ON[t0:t1] == 0 ))
ldg_nose <- vector(mode="character", length=delta_time)
ldg_nose[1:(lgl-1)] <- "ON GROUND"
ldg_nose[lgl:delta_time] <- "IN FLIGHT"

data_takeoff <- as.data.frame(cbind(flightdata$RALTD1[t0:t1],flightdata$RALTD2[t0:t1],flightdata$PITCH[t0:t1],
                flightdata$GSC[t0:t1], flightdata$IASC[t0:t1], flightdata$ELEV_1[t0:t1], flightdata$ELEV_2[t0:t1],
                flightdata$CG[t0:t1], flightdata$GW1KG[t0:t1], flightdata$LONG[t0:t1], flightdata$VRTG[t0:t1],
                flightdata$PTCR[t0:t1], flightdata$PITCH_CPT[t0:t1], flightdata$PITCH_FO[t0:t1],
                flightdata$TAT[t0:t1], flightdata$N11C[t0:t1], flightdata$N11C[t0:t1], flightdata$N12C[t0:t1],
                flightdata$N12C[t0:t1], flightdata$N21C[t0:t1], flightdata$N21C[t0:t1], flightdata$N22C[t0:t1],
                flightdata$N22C[t0:t1],flight_phase, flapl_0, flapl_1, flapl_2, flapl_3, flapl_f, ldg_lh, 
                ldg_rh, ldg_nose ))

names(data_takeoff) <- c("RALT1", "RALT2", "PITCH", "GS",
                         "CAS", "ELEV_L", "ELEV_R", "CG", "GW", "LONG",  "VERTG",
                         "PITCH_RATE","PITCH_CPT","PITCH_FO", "SAT",
                         "N1_1", "N1_2", "N1_3", "N1_4", "N2_1", "N2_2", "N2_3","N2_4","FLIGHT_PHASE",
                         "FLAPL_0", "FLAPL_1", "FLAPL_2", "FLAPL_3", "FLAPL_F", "LDG_LH", "LDG_RH","LDG_NOSE")

setwd(work_dir)
setwd("data")
name_file <- paste0("file",s,".csv")

write.csv(data_takeoff, name_file, row.names = FALSE)

setwd(work_dir)
setwd("data/sample")

} #END FOR Cycle


timech1 <- proc.time() - timech1_0
tsec <- timech1[3]
tmin <- timech1[3]/60


cat("-------------------------------------------------", "
    Processing Time [seconds]", "    ", tsec," 
    Processing Time [minutes]", "    ", tmin," 
    -------------------------------------------------")
