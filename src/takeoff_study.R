    ######################################################################
    # Takeoff Performance study
    # HM @May2020
    #
    # Never delete from the output folder:
    # - easa.png
    # - takeoff_study.brew 
    #
    ########################
    # 2DO 
    # - include N1 and N2 into the same graphics
    # - flaps @ take-off tem Bug
    # - RA1 e RA2 sem valores (s=2)
    
    # Clean workspace (& environment data)
    rm(list=ls(all=TRUE))
    
    # Capture working directory
    # reset workind directory manually
    # setwd("/home/hmendes/Projects/R/flightRtools/takeoff_study")
    #
    work_dir <- getwd()
    
    timech1_0 <- proc.time()
    
    library(ggplot2)
    library(tools)
    library(brew)
    library(data.table)
    
    
    #library(grid)
    #library(tools)
    #library(psych)
    #library(lubridate)
    #library(xtable)
    
  
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
  

    #########################################################################
    to_plots <- function(){
    
      # par.print <- as.data.frame(cbind(par.names, par.descr, par.origin))
      # names(par.print) <- c("Menemonic", "Description", "Origin")
      # 
      # par.table <- print(xtable(par.print, format="latex"), include.rownames=FALSE, 
      #              size="scriptsize") 
      #   

      # Graphics
      # Fast Testing Command:
      # > with(data_takeoff, plot(FF1, N21, type="l", col="blue")); grid(,,col="dark red")
      setwd(work_dir)
      setwd("output")
    
      png("GS.png")
      plot(gs,type = "l",col="blue", ylab = "Ground Speed [Kts]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("CAS.png")
      plot(cas,type = "l",col="blue", ylab = "Calibrated Airspeed [Kts]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("RALT1.png")
      plot(ralt1,type = "l",col="blue", ylab = "Radio Altemeter #1 [ft]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("RALT2.png")
      plot(ralt2,type = "l",col="blue", ylab = "Radio Altemeter #2 [ft]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("ELEV_L.png")
      plot(elev_l,type = "l",col="blue", ylab = "Elevator Left [deg]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("ELEV_R.png")
      plot(elev_r,type = "l",col="blue", ylab = "Elevator Right [deg]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("CG.png")
      plot(cg,type = "l",col="blue", ylab = "Centre of Gravity [%MAC]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("GW.png")
      plot(gw,type = "l",col="blue", ylab = "Gross Weight [Kg]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("LONG.png")
      plot(long,type = "l",col="blue", ylab = "Longitudinal Acceleration [g]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("VERTG.png")
      plot(vertg,type = "l",col="blue", ylab = "Vertical Acceleration [g]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("PITCH.png")
      plot(pitch,type = "l",col="blue", ylab = "Pitch [deg]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("PITCH_RATE.png")
      plot(pitch_rate,type = "l",col="blue", ylab = "Pitch Rate [deg/s]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("PITCH_CAPT.png")
      plot(pitch_capt,type = "l",col="blue", ylab = "Pitch Command Capt", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("PITCH_FO.png")
      plot(pitch_fo,type = "l",col="blue", ylab = "Pitch Command FO", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("SAT.png")
      plot(sat,type = "l",col="blue", ylab = "Static Air Temperature [deg C]", xlab = "Time [1/8 sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
    
      png("N1_1.png")
      plot(n1_1,type = "l",col="blue", ylab = "N1 Eng #1 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N1_2.png")
      plot(n1_2,type = "l",col="blue", ylab = "N1 Eng #2 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N1_3.png")
      plot(n1_3,type = "l",col="blue", ylab = "N1 Eng #3 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N1_4.png")
      plot(n1_4,type = "l",col="blue", ylab = "N1 Eng #4 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N2_1.png")
      plot(n2_1,type = "l",col="blue", ylab = "N2 Eng #1 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N2_2.png")
      plot(n2_2,type = "l",col="blue", ylab = "N2 Eng #2 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N2_3.png")
      plot(n2_3,type = "l",col="blue", ylab = "N2 Eng #3 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("N2_4.png")
      plot(n2_4,type = "l",col="blue", ylab = "N2 Eng #4 [%]", xlab = "Time [sec]")
      abline(v=rotate,col="red",lty=3, lwd=2)
      abline(v=liftoff,col="green",lty=3, lwd=2)
      legend("topleft",legend=c("rotate", "liftoff")
             ,col=c("red","green"), lty=c(3,3),
             bg="white",lwd=2.5, cex=1.2)
      grid(,,col="red")
      dev.off()
      
      png("LDG_LH.png")
      plot(ldg_lh,type = "l",col="blue", ylab = "Main Landing Gear Left", xlab = "Time [1/8 sec]")
      grid(,,col="red")
      dev.off()
      
      png("LDG_RH.png")
      plot(ldg_rh,type = "l",col="blue", ylab = "Main Landing Gear Right", xlab = "Time [1/8 sec]")
      grid(,,col="red")
      dev.off()
      
      png("LDG_NOSE.png")
      plot(ldg_nose,type = "l",col="blue", ylab = "Nose Landing Gear", xlab = "Time [1/8 sec]")
      grid(,,col="red")
      dev.off()
    
    png("landing_gear.png")  
    plot(ldg_lh, type="l",xlab="Time [sec]",col="blue",lwd=2)
    lines(ldg_nose, type="l", col="black",lwd=2)
    lines(ldg_rh, type="l", col="brown",lwd=2)
    abline(v=rotate,col="red",lty=3, lwd=2)
    abline(v=liftoff,col="green",lty=3, lwd=2)
    grid(,,col="dark red")
    legend("topleft",legend=c("Landing Gear Left","Landing Gear Nose","Landing Gear Right", "liftoff", "rotate")
           ,col=c("blue","black","brown","green","red"), lty=c(1, 1, 1, 3,3),
           bg="light grey",lwd=1, cex=1)
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
    
    
      # PDF Reporting creation
      #brew("takeoff_study.brew", paste0('takeoff_study',s,".tex"))
      brew("takeoff_study.brew", "takeoff_study.tex")
      system("pdflatex takeoff_study.tex")
      
      nome_do_ficheiro <- paste0('takeoff_study',s,".pdf")
      init_command <- "mv takeoff_study.pdf"
      rename_file  <- paste(init_command,nome_do_ficheiro)
      system(rename_file)
      
      # This command stoped working - replaced by the commands above!!!
      #texi2dvi(paste0('takeoff_study',s,".tex"), pdf = TRUE)
    
      setwd(work_dir)
      setwd("data")
    }
    
    ########################## START OF MAIN PROCEDURE ##############################
    #################################################################################
    ### 
    ###  
    ###
    #################################################################################
    
    
    # init flight measurements
    flight_measurements <- data.frame()
    
    
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
        
        fo_flying <- 0
        capt_flying <- 0
         
        ### Langing Gear parameters #####
        # Way to obtain the LG parameter @ 1Hz (discontinued @ 8Hz) 
        # LDG_LH <- as.numeric(!as.numeric(as.factor(flightdata$LDG_LH))-1)
        # LDG_RH <- as.numeric(!as.numeric(as.factor(flightdata$LDG_RH))-1)
        # LDG_NOSE <- as.numeric(!as.numeric(as.factor(flightdata$LDG_NOSE))-1)
        # LG Left
        lgl <- min(which(flightdata$LDG_LH == "IN FLIGHT"))
        ldg_lh <- c(1:nrows)
        ldg_lh[1:(lgl-1)] <- 0
        ldg_lh[lgl:nrows] <- 1
        # LG Right
        lgr <- min(which(flightdata$LDG_RH == "IN FLIGHT"))
        ldg_rh <- c(1:nrows)
        ldg_rh[1:(lgr-1)] <- 0
        ldg_rh[lgr:nrows] <- 1
        # LG Nose
        lgn <- min(which(flightdata$LDG_NOSE == "IN FLIGHT"))
        ldg_nose <- c(1:nrows)
        ldg_nose[1:(lgn-1)] <- 0
        ldg_nose[lgn:nrows] <- 1
        
        # Radio Altitude corrections
        # RALT1
        flightdata$RALT1[which(flightdata$RALT1==4095)] <- -1
        flightdata$RALT1[which(flightdata$RALT1==4094)] <- -2
        flightdata$RALT1[which(flightdata$RALT1==4093)] <- -2
        flightdata$RALT1[which(flightdata$RALT1==4092)] <- -4
        # RALT2
        flightdata$RALT2[which(flightdata$RALT2==4095)] <- -1
        flightdata$RALT2[which(flightdata$RALT2==4094)] <- -2
        flightdata$RALT2[which(flightdata$RALT2==4093)] <- -2
        flightdata$RALT2[which(flightdata$RALT2==4092)] <- -4
        
        # Vertical acceleration
        vertg <- flightdata$VERTG
        
    ### Interpolations
        gs <- approx(seq(1:nrows), flightdata$GS,xout=c(1:nrows), method="linear",n=nrows)$y
        cas <- approx(seq(1:nrows), flightdata$CAS,xout=c(1:nrows), method="linear",n=nrows)$y
        ralt1 <- approx(seq(1:nrows), flightdata$RALT1,xout=c(1:nrows), method="linear",n=nrows)$y
        ralt2 <- approx(seq(1:nrows), flightdata$RALT2,xout=c(1:nrows), method="linear",n=nrows)$y
        elev_l <- approx(seq(1:nrows), flightdata$ELEV_L,xout=c(1:nrows), method="linear",n=nrows)$y
        elev_r <- approx(seq(1:nrows), flightdata$ELEV_R,xout=c(1:nrows), method="linear",n=nrows)$y
        cg <- approx(seq(1:nrows), flightdata$CG,xout=c(1:nrows), method="linear",n=nrows)$y
        gw <- approx(seq(1:nrows), flightdata$GW,xout=c(1:nrows), method="linear",n=nrows)$y
        long <- approx(seq(1:nrows), flightdata$LONG,xout=c(1:nrows), method="linear",n=nrows)$y
        pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
        pitch_rate <- approx(seq(1:nrows), flightdata$PITCH_RATE,xout=c(1:nrows), method="linear",n=nrows)$y
        pitch_capt <- approx(seq(1:nrows), flightdata$PITCH_CAPT,xout=c(1:nrows), method="linear",n=nrows)$y
        pitch_fo <- approx(seq(1:nrows), flightdata$PITCH_FO,xout=c(1:nrows), method="linear",n=nrows)$y
        sat <- approx(seq(1:nrows), flightdata$SAT,xout=c(1:nrows), method="linear",n=nrows)$y
        n1_1 <- approx(seq(1:nrows), flightdata$N1_1,xout=c(1:nrows), method="linear",n=nrows)$y
        n1_2 <- approx(seq(1:nrows), flightdata$N1_2,xout=c(1:nrows), method="linear",n=nrows)$y
        n1_3 <- approx(seq(1:nrows), flightdata$N1_3,xout=c(1:nrows), method="linear",n=nrows)$y
        n1_4 <- approx(seq(1:nrows), flightdata$N1_4,xout=c(1:nrows), method="linear",n=nrows)$y
        n2_1 <- approx(seq(1:nrows), flightdata$N2_1,xout=c(1:nrows), method="linear",n=nrows)$y
        n2_2 <- approx(seq(1:nrows), flightdata$N2_2,xout=c(1:nrows), method="linear",n=nrows)$y
        n2_3 <- approx(seq(1:nrows), flightdata$N2_3,xout=c(1:nrows), method="linear",n=nrows)$y
        n2_4 <- approx(seq(1:nrows), flightdata$N2_4,xout=c(1:nrows), method="linear",n=nrows)$y
    
        ## Lift-Off detection
        tlg <- min(which(ldg_lh==1))
        trg <- min(which(ldg_rh==1))
        
        liftoff <- max(tlg,trg) # Lift Off according to MLG criteria
        
        # Rotation point
        if( max(na.omit(pitch_fo)) > max(na.omit(pitch_capt))) {
          # FO is the pilot flying
          fo_flying <- 1
          pilot_flying <- "First officer"
          rotate <- min(which(pitch_fo < -2))  
        } else {
          # CAPT is the pilot flying
          capt_flying <- 1
          pilot_flying <- "Captain"
          rotate <- min(which(pitch_capt < -2))
        }
        
        # Measurements to include in the final report 
        time_considered <- round(nrows/8,1)
        rotation_time <- round(((liftoff - rotate)/8),1)
        pitch_rotate <- pitch[rotate]
        pitch_liftoff <- pitch[liftoff]
        pitch_rate_rotate <- pitch_rate[rotate]
        mean_pitch_rate <- round(mean(pitch_rate[rotate:liftoff]),1)
        v_rotate <- round(gs[rotate],1)
        if (capt_flying == 1){
          mean_pitch_cmd <- round(mean(pitch_capt[rotate:liftoff]),1)
        } else {
          mean_pitch_cmd <- round(mean(pitch_fo[rotate:liftoff]),1)
        }
        
        flaps_takeoff <- flightdata$FLAPL_3[10] # correct with IF condition in case other position is used  
        nulltext <- "TBC"
        
        #### Produce plot report for each flight (all paranmeters)
        to_plots()
        
        #### Create flight measurements data frame
        flight_measurements[s,1] <- time_considered
        flight_measurements[s,2] <- rotation_time
        flight_measurements[s,3] <- pitch_rotate
        flight_measurements[s,4] <- pitch_liftoff
        flight_measurements[s,5] <- pitch_rate_rotate
        flight_measurements[s,6] <- mean_pitch_rate
        flight_measurements[s,7] <- v_rotate
        flight_measurements[s,8] <- mean_pitch_cmd
        
        # Name each of the columns
        
        names(flight_measurements) <- c("TIME", "ROT_TIME","PITCH_ROT","PITCH_LO", "PITCHRATE_ROT",
                                        "MEAN_PITCHRATE", "V_ROT", "MEAN_PITCHCMD" )
    
    }
    #} END FOR Cycle
    
    
    setwd(work_dir)
    setwd("output")
    
    ### Histograms from the measurements
    png("TIME_HIST.png")
    print(ggplot(flight_measurements, aes(x=TIME)) + geom_histogram(binwidth=1, colour="black", fill="green"))
    dev.off()
    png("ROT_TIME_HIST.png")
    print(ggplot(flight_measurements, aes(x=ROT_TIME)) + geom_histogram(binwidth=.2, colour="black", fill="green"))
    dev.off()
    png("PITCH_ROT_HIST.png")
    print(ggplot(flight_measurements, aes(x=PITCH_ROT)) + geom_histogram(binwidth=.2, colour="black", fill="green"))
    dev.off()
    png("PITCH_LO_HIST.png")
    print(ggplot(flight_measurements, aes(x=PITCH_LO)) + geom_histogram(binwidth=.5, colour="black", fill="green"))
    dev.off()
    png("PITCHRATE_ROT_HIST.png")
    print(ggplot(flight_measurements, aes(x=PITCHRATE_ROT)) + geom_histogram(binwidth=.2, colour="black", fill="green"))
    dev.off()
    png("MEAN_PITCHRATE_HIST.png")
    print(ggplot(flight_measurements, aes(x=MEAN_PITCHRATE)) + geom_histogram(binwidth=.1, colour="black", fill="green"))
    dev.off()
    png("V_ROT_HIST.png")
    print(ggplot(flight_measurements, aes(x=V_ROT)) + geom_histogram(binwidth=.5, colour="black", fill="green"))
    dev.off()
    png("MEAN_PITCMD_HIST.png")
    print(ggplot(flight_measurements, aes(x=MEAN_PITCHCMD)) + geom_histogram(binwidth=.5, colour="black", fill="green"))
    dev.off()
    
    ### Density plots from the measurements
    png("TIME_DENS.png")
    print(ggplot(flight_measurements, aes(x=TIME)) + geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(TIME, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("ROT_TIME_DENS.png")
    print(ggplot(flight_measurements, aes(x=ROT_TIME)) + geom_histogram(aes(y=..density..), binwidth=.2, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(ROT_TIME, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("PITCH_ROT_DENS.png")
    print(ggplot(flight_measurements, aes(x=PITCH_ROT)) + geom_histogram(aes(y=..density..), binwidth=.2, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(PITCH_ROT, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("PITCH_LO_DENS.png")
    print(ggplot(flight_measurements, aes(x=PITCH_LO)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(PITCH_LO, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("PITCHRATE_ROT_DENS.png")
    print(ggplot(flight_measurements, aes(x=PITCHRATE_ROT)) + geom_histogram(aes(y=..density..), binwidth=.2, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(PITCHRATE_ROT, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("MEAN_PITCHRATE_DENS.png")
    print(ggplot(flight_measurements, aes(x=MEAN_PITCHRATE)) + geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(MEAN_PITCHRATE, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("V_ROT_DENS.png")
    print(ggplot(flight_measurements, aes(x=V_ROT)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(V_ROT, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    png("MEAN_PITCHCMD_DENS.png")
    print(ggplot(flight_measurements, aes(x=MEAN_PITCHCMD)) + geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="green") + 
            geom_density(colour="blue", size=1) + geom_vline(aes(xintercept=mean(MEAN_PITCHCMD, na.rm=T)), colour="red", size=1.4, linetype=2))
    dev.off()
    
    
    setwd(work_dir)
    
    timech1 <- proc.time() - timech1_0
    tsec <- timech1[3]
    tmin <- timech1[3]/60
    
    
    print("-------------------------------------------------")
    print(paste("Total Flights Processed: ",s))
    print("-------------------------------------------------")
    
    cat("-------------------------------------------------", "
        Processing Time [seconds]", "    ", tsec," 
        Processing Time [minutes]", "    ", tmin," 
        -------------------------------------------------")
