program ffwi
    !     PROGRAM NO.: F-4
    !     FORTRAN IV PROGRAM TO CALCULATE CANADIAN FOREST
    !     FIRE WEATHER INDEX FOR A DEC PDP 11 AT P.F.E.S.
    !     READS DATA AND PRINTS OUT IN METRIC UNITS.
    
    implicit none
    
    integer :: j, i, l, startMonth, startNumDays, indexDays, totalDays, &
         &humid, wind, workWind, workHumid, FWI, printFwi, printFfmc, &
         &printDmc, printDc, printIsi, printBui
    real :: startFfmc, startDmc, startDc, temp, rain, emcWet, emcDry, &
         &startMoist, finalMoist, todayFfmc, rainFfmc, moistCont, &
         &interLdr, ldr, dcDryFactor, moistDc, rainDc, &
         &rainFunc, correctRain, rainEffFunc, logFwi, interFwi, &
         &effecRain, dryFactor, printTemp, printRain, &
         &workRain, workMoist, ffmFunc, ratioFunc, isi, workFfmc, &
         &DC, DMC, BUI, CC, dmcRain
    
    integer, dimension(12) :: monthLength
    real, dimension(12) :: dmcFactors, dcFactors
    character(len = *), parameter :: format1 = "(F4.1,I4,I4,F4.1)"
    character(len = *), parameter :: format2 = "  DATE  TEMP  RH   WIND  &
         &RAIN   FFMC   DMC   DC   ISI   BUI   FWI"
    character(len = 50) :: filename
    
    
    write(*, '(a)') 'Please input a file name: '
    read(*, *) fileName
    
    open(1, file = filename)
    
    !
    !     READS LENGTH OF MONTHS, AND DAY LENGTH FACTORS
    !
    do j = 1, 12
      read(1,'(i2, f4.1, f4.1)') monthLength(j), dmcFactors(j), &
                                  dcFactors(j)
    end do
    
    !
    !     READS INITIAL VALUES OF FFMC, DMC, DC, STARTING MONTH AND NUMBER
    !     OF DAYS OF DATA STARTING MONTH.
    !
    read(1,'(f4.1, f4.1, f5.1, i2, i2)') startFfmc, startDmc, startDc, &
                                        startMonth, startNumDays
    
    !loops through each month and then each day to read/write output
    do j = startMonth, 12         ! label is 25
      totalDays = monthLength(j)
    
      if (j.eq.startMonth) then
        indexDays = monthLength(j) - startNumDays + 1
      else 
        indexDays = 1
      end if
    
    
    !
    !     READS DAILY WEATHER DATA
    !
    
    !Loops through days in the month to read and output each day
      l = 0
      do i = indexDays, totalDays     !
        l = l + 1
        read(1,format1,end=2000) temp, humid, wind, rain
    
        if(l.eq.1) then
          write(*,*)format2
        end if
        printTemp = temp
        workHumid = humid
        workWind = wind
        printRain = rain
    
    !
    !     FINE FUEL MOISTURE CODE
    !
        if(rain.gt.0.5) then
    
          workRain = rain
          
          if(workRain.le.1.45) then
            rainFunc = 123.85 - (55.6*alog(workRain + 1.016))
    
          else
    
            if((workRain - 5.75).le.0.0) then !simplified with else if block
              rainFunc = 57.87 - (18.2 * alog(workRain - 1.016))
    
            else 
              rainFunc = 40.69 - (8.25 * alog(workRain - 1.905))
    
            end if
    
          end if
    
          correctRain = 8.73 * exp(-0.1117 * startFfmc)
          rainFfmc = (startFfmc / 100.0) * rainFunc + (1.0 - correctRain)
    
          if(rainFfmc.lt.0.0) then
            rainFfmc = 0.0
    
          end if
    
        else
          rain = 0.0
          rainFfmc = startFfmc
        end if
    
    
        startMoist = 101.0 - rainFfmc
        emcDry = 0.942 * (workHumid**0.679) + (11.0 * EXP((workHumid - 100.0) / 10.0)) &
          &+ 0.18 * (21.1 - temp) * (1.0 - 1.0 / EXP(0.115 * workHumid))
    
        if((startMoist - emcDry).lt.0.0) then !26,27,28
    
          emcWet = 0.618 * (workHumid**0.753) + (10.*EXP((workHumid - 100.0) / 10.0)) + 0.18 &
                &* (21.1 - temp) * (1.0 - 1.0 / EXP(0.115 * workHumid))
          if(startMoist.lt.emcWet) then
            finalMoist = emcWet - (emcWet - startMoist) / 1.9953
          end if
    
        else if((startMoist - emcDry).eq.0.0) then
          finalMoist = startMoist
    
        else
            
          interLdr = 0.424 * (1.0 - (workHumid / 100.0)**1.7) + (0.0694 * (workWind**0.5)) * &
                &(1.0 - (workHumid / 100.0)**8)
          ldr = interLdr * (0.463 * (EXP(0.0365 * temp)))
          finalMoist = emcDry + (startMoist - emcDry) / 10.0**ldr
        end if
    
    
        todayFfmc = 101.0 - finalMoist
    
        if(todayFfmc.gt.101.0) then
          todayFfmc = 101.0
    
        else
    
          if(todayFfmc.le.0.0) then
            todayFfmc = 0.0
    
          end if
        end if
    
    
    !
    !     DUFF MOISTURE CODE
    !
        if((temp + 1.1).lt.0.0) then !GO TO 41
          temp=-1.1
        end if
    
        dryFactor = 1.894 * (temp + 1.1) * (100.0 - workHumid) * (dmcFactors(j) * 0.0001)
    
        if(rain.gt.1.5) then
    
          workRain = rain
          effecRain = 0.92 * workRain - 1.27
          moistCont = 20.0 + 280.0 / EXP(0.023 * startDmc)
    
          if(startDmc.le.33.0) then !GO TO 50
    
            rainEffFunc = 100.0/(0.5 + 0.3 *startDmc)
    
          else 
            if((startDmc - 65.0).le.0.0) then ! 52,52,53
    
              rainEffFunc = 14.0 - 1.3 * alog(startDmc)
            else
              rainEffFunc = 6.2 * alog(startDmc) - 17.2
            end if
          end if
    
          workMoist = moistCont + (1000.0 * effecRain) / (48.77 + rainEffFunc * effecRain)
          dmcRain = 43.43 * (5.6348 - alog(workMoist - 20.0))
          
        else
          dmcRain = startDmc
        end if
    
        if(dmcRain.ge.0.0) then !GO TO 61
          DMC = dmcRain + dryFactor
        else
          dmcRain = 0.0
        end if
    
    !
    !     DROUGHT CODE
    !
        if((temp + 2.8).lt.0.0) then
          temp = -2.8
        end if
    
        dcDryFactor = (0.36 * (temp + 2.8) + dcFactors(j)) / 2.0
    
        if(rain.gt.2.8) then !GO TO 300
          workRain = rain
          effecRain = 0.83 * workRain - 1.27
          moistDc = 800.0 * EXP(-startDc / 400.0)
          rainDc = startDc - 400.0 * alog(1.0 + ((3.937 * effecRain) / moistDc))
    
          if(rainDc.le.0.0) then
            rainDc=0.0
          end if
    
        else
          rainDc = startDc  
        end if
    
        DC = rainDc + dcDryFactor
    
        if(DC.lt.0.0) then
          DC = 0.0
        end if
    
    !
    !     INITIAL SPREAD INDEX, BUILDUP INDEX, FIRE WEATHER INDEX
    !
        workFfmc = 101.0 - todayFfmc
        ffmFunc = 19.1152 * EXP(-0.1386 * workFfmc) * (1.0 + workFfmc**4.65/7950000.0)
        isi = ffmFunc * EXP(0.05039 * workWind)
        BUI = (0.8 * DC * DMC) / (DMC + 0.4 * DC)
    
        if(BUI.ge.DMC) then !GO TO 95
    
        else 
    
          ratioFunc = (DMC - BUI) / DMC
          CC = 0.92 + (0.0114 * DMC)**1.7
          BUI = DMC - (CC * ratioFunc)
          if(BUI.lt.0.0) then
            BUI = 0.0
          end if
    
        end if
    
    
        if(BUI.gt.80.0) then !GO TO 60
          interFwi = 0.1 * isi * (1000.0 / (25.0 + 108.64 / EXP(0.023 * BUI)))
        else
          interFwi = 0.1 * isi * (0.626 * BUI**0.809 + 2.0)
        end if
    
    
        if((interFwi - 1.0).le.0.0) then !GO TO 98
          FWI = int(interFwi)
        else 
          logFwi = 2.72 * (0.43 * alog(interFwi))**0.647
          FWI = int(EXP(logFwi))
        end if
    
        printDc = int(DC + 0.5)
        printFfmc = int(todayFfmc + 0.5)
        printDmc = int(DMC + 0.5)
        printIsi = int(isi + 0.5)
        printBui = int(BUI + 0.5)
        printFwi = int(FWI + 0.5)
    
        write(*,"(1X,2I3,F6.1,I4,I6,F7.1,6I6)") j, i, printTemp, humid, &
              &wind, printRain, printFfmc, printDmc, printDc, printIsi, printBui, printFwi
        startFfmc = todayFfmc
        startDmc = DMC
        startDc = DC
    
      end do
    end do
    
    2000 end program ffwi
          
    