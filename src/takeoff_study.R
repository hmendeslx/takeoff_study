######################################################################
# Takeoff Performance study
# HM @May2020
#
#

# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

# Capture working directory
# reset workind directory manually
# setwd("/home/hmendes/Projects/R/flightRtools/takeoff_study")
#
work_dir <- getwd()

timech1_0 <- proc.time()

#library(ggplot2)
library(brew)
library(data.table)

#library(grid)
#library(tools)
#library(psych)
#library(lubridate)
#library(xtable)

# 
# #########################################################################
# # finding parameters - Trouble-shooting
# # use:
# # > testpar("EXAMPLE")
# testpar <- function(str){
#   name <- vector(mode='character')
#   pars <- grep(str, names(flightdata))
#   for (i in 1:nrow(as.matrix(pars))){
#     name[i] <- names(flightdata)[pars[i]]  
#     #print(name)
#   }
#   return(name)
# }
# 
# #########################################################################
# panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
# {
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- abs(cor(x, y))
#   txt <- format(c(r, 0.123456789), digits=digits)[1]
#   txt <- paste(prefix, txt, sep="")
#   if(missing(cex.cor)) cex.cor <- 1.5/strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor )#* r)
# }
# 
# 
# #########################################################################
# # derivative of a vector
# derivative <- function(vector,time_interval) {
#   #f_i1 is the parameter value at instant i
#   #f_i2 is the parameter value at instant i+1
#   #time_interval is the total row number of the parameter for the derivative
#   f_derivative=mat.or.vec(NROW(time_interval),1)
#   f_i1=0
#   f_i2=0
#   instant=seq(from=0,to=time_interval,by=1)
#   
#   for (k in 1:(time_interval-1)) {
#     f_i1[k]=vector[k]
#     f_i2[k]=vector[k+1]
#     f_derivative[k]=(f_i2[k]-f_i1[k])/((instant[k+1])-instant[k])
#   }
#   
#   f_derivative[time_interval]=f_i2[k]
#   
#   return(f_derivative)
# }

#########################################################################
to_plots <- function(){

  # par.print <- as.data.frame(cbind(par.names, par.descr, par.origin))
  # names(par.print) <- c("Menemonic", "Description", "Origin")
  # 
  # par.table <- print(xtable(par.print, format="latex"), include.rownames=FALSE, 
  #              size="scriptsize") 
  #          
  # # Lift-Off detection
  # tlg <- min(which(LG_left[t0:t1]==0))
  # trg <- min(which(LG_right[t0:t1]==0))
#  
#   loff <- max(tlg,trg) # Lift Off according to MLG criteria
#   
#   r1 <- raltd1[t0:t1]
#   r2 <- raltd2[t0:t1]
#   
#   # loff radio alt correction
#   if ( r1[loff] > 1  || r2[loff] > 1) {
#     
#     min <- loff - 20
#     max <- loff + 10
#     
#     r1_positive <- min(which(r1[min:max]>0))
#     lo_r1 <- min + r1_positive
#     
#     r2_positive <- min(which(r2[min:max]>0))
#     lo_r2 <- min + r2_positive
#     
#     #lo_ralt <- min(lo_r1,lo_r2)
#     loff <- max(lo_r1,lo_r2)    
# }
#   
  # loff_sec <- loff/8 # in seconds

  # Rotation point
  # rot_sample <- min(which(ptcr[t0:t1]>1))
  # rotation_time <- (loff - rot_sample)/8 
  # rot <- rot_sample/8 # seconds

  # # Values to include in the report 
  # # integrar mais tarde em "flight_measurements"
  # flxtemp1 <- round(data_takeoff$FLX1_TEMP[rot_sample],1)
  # flxtemp2 <- round(data_takeoff$FLX2_TEMP[rot_sample],1)
  # tla1_pos <- data_takeoff$TLA1C[rot_sample]
  # tla2_pos <- data_takeoff$TLA2C[rot_sample]
  # gw_ini <- data_takeoff$GW1KG[1]
  # gw_final <- data_takeoff$GW1KG[(t1-t0)+1]
  # fuel_gw <- gw_ini - gw_final
  time_considered <- nrows
  flaps_takeoff <- flightdata$FLAPL_3[10] # correct with IF condition in case other position is used  
  nulltext <- "TBC"
  # # calculo de consumo atrav?s do FF - comparar com o de cima
  
  # Graphics
  # Fast Testing Command:
  # > with(data_takeoff, plot(FF1, N21, type="l", col="blue")); grid(,,col="dark red")
  setwd(work_dir)
  setwd("output")

  png("GS.png")
  plot(flightdata$GS,type = "l",col="blue", ylab = "Ground Speed [Kts]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("VERTG.png")
  plot(flightdata$VERTG,type = "l",col="blue", ylab = "Vertical Acceleration [g]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("CAS.png")
  plot(flightdata$CAS,type = "l",col="blue", ylab = "Calibrated Airspeed [Kts]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  flightdata$RALT1[which(flightdata$RALT1==4095)] <- -1
  flightdata$RALT1[which(flightdata$RALT1==4094)] <- -2
  flightdata$RALT1[which(flightdata$RALT1==4092)] <- -4
  png("RALT1.png")
  plot(flightdata$RALT1,type = "l",col="blue", ylab = "Radio Altemeter #1 [ft]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  flightdata$RALT2[which(flightdata$RALT2==4095)] <- -1
  flightdata$RALT2[which(flightdata$RALT2==4094)] <- -2
  flightdata$RALT2[which(flightdata$RALT2==4093)] <- -3
  png("RALT2.png")
  plot(flightdata$RALT2,type = "l",col="blue", ylab = "Radio Altemeter #2 [ft]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("PITCH.png")
  plot(flightdata$PITCH,type = "l",col="blue", ylab = "Pitch [deg]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("PITCH_RATE.png")
  plot(flightdata$PITCH_RATE,type = "l",col="blue", ylab = "Pitch Rate [deg/s]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("PITCH_CAPT.png")
  plot(flightdata$PITCH_CAPT,type = "l",col="blue", ylab = "Pitch Command Capt", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("PITCH_FO.png")
  plot(flightdata$PITCH_FO,type = "l",col="blue", ylab = "Pitch Command FO", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("LONG.png")
  plot(flightdata$LONG,type = "l",col="blue", ylab = "Longitudinal Acceleration [g]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("SAT.png")
  plot(flightdata$SAT,type = "l",col="blue", ylab = "Static Air Temperature [deg C]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N1_1.png")
  plot(flightdata$N1_1,type = "l",col="blue", ylab = "N1 Eng #1 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N1_2.png")
  plot(flightdata$N1_2,type = "l",col="blue", ylab = "N1 Eng #2 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N1_3.png")
  plot(flightdata$N1_3,type = "l",col="blue", ylab = "N1 Eng #3 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N1_4.png")
  plot(flightdata$N1_4,type = "l",col="blue", ylab = "N1 Eng #4 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N2_1.png")
  plot(flightdata$N2_1,type = "l",col="blue", ylab = "N2 Eng #1 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N2_2.png")
  plot(flightdata$N2_2,type = "l",col="blue", ylab = "N2 Eng #2 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N2_3.png")
  plot(flightdata$N2_3,type = "l",col="blue", ylab = "N2 Eng #3 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("N2_4.png")
  plot(flightdata$N2_4,type = "l",col="blue", ylab = "N2 Eng #4 [%]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("ELEV_L.png")
  plot(flightdata$ELEV_L,type = "l",col="blue", ylab = "Elevator Left [deg]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  png("ELEV_R.png")
  plot(flightdata$ELEV_R_A,type = "l",col="blue", ylab = "Elevator Right [deg]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  LDG_LH <- as.numeric(!as.numeric(as.factor(flightdata$LDG_LH))-1)
  png("LDG_LH.png")
  plot(LDG_LH,type = "l",col="blue", ylab = "Main Landing Gear Left", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  LDG_RH <- as.numeric(!as.numeric(as.factor(flightdata$LDG_RH))-1)
  png("LDG_RH.png")
  plot(LDG_RH,type = "l",col="blue", ylab = "Main Landing Gear Right", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  LDG_NOSE <- as.numeric(!as.numeric(as.factor(flightdata$LDG_NOSE))-1)
  png("LDG_NOSE.png")
  plot(LDG_NOSE,type = "l",col="blue", ylab = "Nose Landing Gear", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  CG <- approx(seq(1:nrows), flightdata$CG,xout=c(1:nrows), method="linear",n=nrows)$y
  png("CG.png")
  plot(CG,type = "l",col="blue", ylab = "Centre of Gravity [%MAC]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
  GW <- approx(seq(1:nrows), flightdata$GW,xout=c(1:nrows), method="linear",n=nrows)$y
  png("GW.png")
  plot(GW,type = "l",col="blue", ylab = "Gross Weight [Kg]", xlab = "Time [sec]")
  grid(,,col="red")
  dev.off()
  
png("landing_gear.png")  
plot(LDG_LH, type="l",xlab="Time [sec]",col="blue",lwd=2)
lines(LDG_NOSE, type="l", col="black",lwd=2)
lines(LDG_RH, type="l", col="brown",lwd=2)
# abline(v=rot,col="green", lwd=2)
# abline(v=loff_sec,col="red", lwd=2)
grid(,,col="dark red")
legend("topleft",legend=c("Landing Gear Left","Landing Gear Nose","Landing Gear Right")
       ,col=c("blue","black","brown"),bg="light blue",lwd=1, cex=1)
dev.off()


# #################################################
# # Graphics from Correlation of several parameters
# 
#   png("tla_vs_ff_eng1.png") #
#   with(data_takeoff, plot( TLA1, FF1, type="l", col="blue", lwd=1.5))
#   regress <- lm(FF1 ~ TLA1, data_takeoff)
#   sum_reg <- summary(regress)
#   abline(regress, col="red")
#   legend("topleft", paste("R^2 = ", round(sum_reg$r.squared,3)))
#   grid(,,"dark red")
#   dev.off()
# 
# #  Capture information about the regression for the final report
#   line1 <- paste("\\","begin{verbatim}",sep="")
#   write(line1, file="tla_vs_ff_eng1.tex")
#   capture.output(sum_reg, file="tla_vs_ff_eng1.tex", append=TRUE)
#   line2 <- paste("\\","end{verbatim}",sep="")
#   write(line2, file="tla_vs_ff_eng1.tex", append=TRUE)
# 
#   png("tla_vs_ff_eng2.png")
#   with(data_takeoff, plot( TLA2, FF2, type="l", col="blue", lwd=1.5))
#   regress <- lm(FF2 ~ TLA2, data_takeoff)
#   sum_reg <- summary(regress)
#   abline(regress, col="red")
#   legend("topleft", paste("R^2 = ", round(sum_reg$r.squared,3)))
#   grid(,,"dark red")
#   dev.off()

 
  ##$## PDF reporting commented on 9 May 2020 to recover in a later stage
  
  # PDF Reporting
  #setwd(binpath)
  
  brew("takeoff_study.brew", paste0('takeoff_study',s,".tex"))
  texi2dvi(paste0('takeoff_study',s,".tex"), pdf = TRUE)

  setwd(work_dir)
  setwd("data")
}

########################## START OF MAIN PROCEDURE ##############################
#################################################################################
### Choose the type of analysis:
###  * set = 1 at the report desired (both =1 allowed)
#################################################################################
# 
# ## Constants
# c_knot_ms <- 1852/3600
# c_ms_kmh <- 3.6
# # areaA320 <- 138 # [m^2] - surface of the wing with flaps for Airbus A320

# file list capture
setwd("data")
fileList <- list.files(path=getwd(), pattern=".csv")

## Escolher o ficheiro pelo n? "s"
#s = 2
#s=25

# alternativa - fazer o enable deste ciclo FOR para todos os ficheiros do folder
for (s in 1:NROW(fileList)) {

# Variable and Vector Initialization
#
#flight_measurements <- mat.or.vec(NROW(fileList),20) # Matrix of dimensions: [fligths x Measurements]
#flight_measurements <- data.frame()
#mean_phase_store <- mat.or.vec(NROW(fileList),1)

  
  ## LOAD Flight From List
  #setwd("data")
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                      colClasses = c(LDG_LH='character', LDG_NOSE='character', LDG_RH='character', 
                      FLAPL_0='character', FLAPL_1='character', FLAPL_2='character', 
                      FLAPL_3='character', FLAPL_F='character', FLIGHT_PHASE='character' )) 
    
    nrows <- NROW(flightdata)
        
# ## Interpolations
#     pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
#     ptcr <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
#     pitch_cpt <- approx(seq(1:nrows), flightdata$PITCH_CPT,xout=c(1:nrows), method="linear",n=nrows)$y
#     pitch_fo <- approx(seq(1:nrows), flightdata$PITCH_FO,xout=c(1:nrows), method="linear",n=nrows)$y
#     alt_std <- approx(seq(1:nrows), flightdata$ALT_STDC,xout=c(1:nrows), method="linear",n=nrows)$y
#     raltd1 <- approx(seq(1:nrows), flightdata$RALTD1,xout=c(1:nrows), method="linear",n=nrows)$y
#     raltd2 <- approx(seq(1:nrows), flightdata$RALTD2,xout=c(1:nrows), method="linear",n=nrows)$y
#     head_mag <- approx(seq(1:nrows), flightdata$HEAD_MAG,xout=c(1:nrows), method="linear",n=nrows)$y
#     ivv <- approx(seq(1:nrows), flightdata$IVV,xout=c(1:nrows), method="linear",n=nrows)$y
#     long <- approx(seq(1:nrows), flightdata$LONG,xout=c(1:nrows), method="linear",n=nrows)$y
#     latg <- approx(seq(1:nrows), flightdata$LATG,xout=c(1:nrows), method="linear",n=nrows)$y
#     roll <- approx(seq(1:nrows), flightdata$ROLL,xout=c(1:nrows), method="linear",n=nrows)$y
#     ias <- approx(seq(1:nrows), flightdata$IASC,xout=c(1:nrows), method="linear",n=nrows)$y
#     mach <- approx(seq(1:nrows), flightdata$MACH,xout=c(1:nrows), method="linear",n=nrows)$y
#     gs <- approx(seq(1:nrows), flightdata$GSC,xout=c(1:nrows), method="linear",n=nrows)$y
#     gw1kg <- approx(seq(1:nrows), flightdata$GW1KG,xout=c(1:nrows), method="linear",n=nrows)$y
#     fpa <- approx(seq(1:nrows), flightdata$FPA,xout=c(1:nrows), method="linear",n=nrows)$y
#     fpac <- approx(seq(1:nrows), flightdata$FPAC,xout=c(1:nrows), method="linear",n=nrows)$y
#     LG_left <- approx(seq(1:nrows), flightdata$LDG_ON_1,xout=c(1:nrows), method="linear",n=nrows)$y
#     LG_nose <- approx(seq(1:nrows), flightdata$LDGN_ON,xout=c(1:nrows), method="linear",n=nrows)$y
#     LG_right <- approx(seq(1:nrows), flightdata$LDG_ON_2,xout=c(1:nrows), method="linear",n=nrows)$y
#     aoal <- approx(seq(1:nrows), flightdata$AOAL,xout=c(1:nrows), method="linear",n=nrows)$y
#     aoar <- approx(seq(1:nrows), flightdata$AOAR,xout=c(1:nrows), method="linear",n=nrows)$y
#     n11 <- approx(seq(1:nrows), flightdata$N11C,xout=c(1:nrows), method="linear",n=nrows)$y
#     n12 <- approx(seq(1:nrows), flightdata$N12C,xout=c(1:nrows), method="linear",n=nrows)$y
#     n21 <- approx(seq(1:nrows), flightdata$N21C,xout=c(1:nrows), method="linear",n=nrows)$y
#     n22 <- approx(seq(1:nrows), flightdata$N22C,xout=c(1:nrows), method="linear",n=nrows)$y
#     ff1 <- approx(seq(1:nrows), flightdata$FF1C,xout=c(1:nrows), method="linear",n=nrows)$y
#     ff2 <- approx(seq(1:nrows), flightdata$FF2C,xout=c(1:nrows), method="linear",n=nrows)$y
#     egt1 <- approx(seq(1:nrows), flightdata$EGT1C,xout=c(1:nrows), method="linear",n=nrows)$y
#     egt2 <- approx(seq(1:nrows), flightdata$EGT2C,xout=c(1:nrows), method="linear",n=nrows)$y
#     flx1_temp <- approx(seq(1:nrows), flightdata$FLX1_TEMP,xout=c(1:nrows), method="linear",n=nrows)$y
#     flx2_temp <- approx(seq(1:nrows), flightdata$FLX2_TEMP,xout=c(1:nrows), method="linear",n=nrows)$y
#     at_flx <- approx(seq(1:nrows), flightdata$AT_FLX,xout=c(1:nrows), method="linear",n=nrows)$y
#     q1 <- approx(seq(1:nrows), flightdata$Q1,xout=c(1:nrows), method="linear",n=nrows)$y
#     q2 <- approx(seq(1:nrows), flightdata$Q2,xout=c(1:nrows), method="linear",n=nrows)$y
#     pt1 <- approx(seq(1:nrows), flightdata$PT1,xout=c(1:nrows), method="linear",n=nrows)$y
#     pt2 <- approx(seq(1:nrows), flightdata$PT2,xout=c(1:nrows), method="linear",n=nrows)$y
#     tla1 <- approx(seq(1:nrows), flightdata$TLA1,xout=c(1:nrows), method="linear",n=nrows)$y
#     tla1c <- approx(seq(1:nrows), flightdata$TLA1C,xout=c(1:nrows), method="linear",n=nrows)$y
#     tla2 <- approx(seq(1:nrows), flightdata$TLA2,xout=c(1:nrows), method="linear",n=nrows)$y
#     tla2c <- approx(seq(1:nrows), flightdata$TLA2C,xout=c(1:nrows), method="linear",n=nrows)$y
#     sat <- approx(seq(1:nrows), flightdata$SAT,xout=c(1:nrows), method="linear",n=nrows)$y
#     tat <- approx(seq(1:nrows), flightdata$TAT,xout=c(1:nrows), method="linear",n=nrows)$y
#     p0_1 <- approx(seq(1:nrows), flightdata$P0_1,xout=c(1:nrows), method="linear",n=nrows)$y
#     p0_2 <- approx(seq(1:nrows), flightdata$P0_2,xout=c(1:nrows), method="linear",n=nrows)$y
#     p2_1 <- approx(seq(1:nrows), flightdata$P2_1,xout=c(1:nrows), method="linear",n=nrows)$y
#     p2_2 <- approx(seq(1:nrows), flightdata$P2_2,xout=c(1:nrows), method="linear",n=nrows)$y
#     p31 <- approx(seq(1:nrows), flightdata$P31,xout=c(1:nrows), method="linear",n=nrows)$y
#     p32 <- approx(seq(1:nrows), flightdata$P32,xout=c(1:nrows), method="linear",n=nrows)$y
#     t12_1 <- approx(seq(1:nrows), flightdata$T12_1,xout=c(1:nrows), method="linear",n=nrows)$y
#     t12_2 <- approx(seq(1:nrows), flightdata$T12_2,xout=c(1:nrows), method="linear",n=nrows)$y
#     t25_1 <- approx(seq(1:nrows), flightdata$T25_1,xout=c(1:nrows), method="linear",n=nrows)$y
#     t25_2 <- approx(seq(1:nrows), flightdata$T25_2,xout=c(1:nrows), method="linear",n=nrows)$y
#     fm_fwc <- approx(seq(1:nrows), flightdata$FM_FWC,xout=c(1:nrows), method="linear",n=nrows)$y
#     winspd <- approx(seq(1:nrows), flightdata$WIN_SPDR,xout=c(1:nrows), method="linear",n=nrows)$y
#     windir <- approx(seq(1:nrows), flightdata$WIN_DIRR,xout=c(1:nrows), method="linear",n=nrows)$y
#     vibn1fnt1 <- approx(seq(1:nrows), flightdata$VIB_N1FNT1,xout=c(1:nrows), method="linear",n=nrows)$y
#     vibn1fnt2 <- approx(seq(1:nrows), flightdata$VIB_N1FNT2,xout=c(1:nrows), method="linear",n=nrows)$y
#     vibn2fnt1 <- approx(seq(1:nrows), flightdata$VIB_N2FNT1,xout=c(1:nrows), method="linear",n=nrows)$y
#     vibn2fnt2 <- approx(seq(1:nrows), flightdata$VIB_N2FNT2,xout=c(1:nrows), method="linear",n=nrows)$y
#     
    
#   vrtg <- flightdata$VRTG


    to_plots()
    #setwd(work_dir)
    

}
#} END FOR Cycle


timech1 <- proc.time() - timech1_0
tsec <- timech1[3]
tmin <- timech1[3]/60


cat("-------------------------------------------------", "
    Processing Time [seconds]", "    ", tsec," 
    Processing Time [minutes]", "    ", tmin," 
    -------------------------------------------------")
