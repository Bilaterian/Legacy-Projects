      PROGRAM FIRE
!     Variable declarations
      implicit none
      real :: dry, wet, isnow, precip, wind, buo, iherb, df, ffm, adfm, grass, timber, fload
!     Input of variables
      print *, "INPUT: "
      print *, " Dry bulb temperature   ="
      read *, dry
      print *, " Wet bulb temperature   ="
      read *, wet
      print *, " Is there snow?         ="
      read *, isnow
      print *, " Precipitation          ="
      read *, precip
      print *, " Wind speed mph         ="
      read *, wind
      print *, " Build up index         ="
      read *, buo
      print *, " Herb state             ="
      read *, iherb

      CALL DANGER(dry,wet,isnow,precip,wind,buo,iherb,df,ffm,adfm,grass,timber,fload)

!     Output of calculated info from DANGER
      print *, "OUTPUT: "
      print *, " Drying Factor          =", df
      print *, " Fine Fuel Moisture     =", ffm
      print *, " Adjusted Fuel Moisture =", adfm
      print *, " Grass Spread Index     =", grass
      print *, " Timber Spread Index    =", timber
      print *, " Fire Load Index        =", fload
      print *, " Build Up Index         =", buo
      
      END PROGRAM FIRE


      subroutine danger(DRY,WET,ISNOW,PRECIP,WIND,BUO,IHERB,DF,FFM,ADFM,GRASS,TIMBER,FLOAD)
!     subroutine parameters
      real, intent(inout) :: DRY,WET,ISNOW,PRECIP,WIND,BUO,IHERB,DF,FFM,ADFM,GRASS,TIMBER,FLOAD
!     iterative integer for do loops
      integer :: I
!     call to buildup index function to calculate build up
      real :: calculateBuildUp
!     danger ratings
      real, dimension(4) :: A, B
      real, dimension(3) :: C
      real, dimension(6) :: D

!     some variables set to specific values
      FFM= 99.0
      ADFM= 99.0
      DF=0.0
      FLOAD=0.0

!     setting the danger ratings
      A = (/ -0.185900, -0.85900, -0.059660, -0.077373 /)
      B = (/ 30.0, 19.2, 13.8, 22.5 /)
      C = (/ 4.5, 12.5, 27.0 /)
      D = (/ 16.0, 10.0, 7.0, 5.0, 4.0, 3.0 /)

!     chek if there is snow on the ground
      if (ISNOW > 0) then
         GRASS = 0
         TIMBER = 0

!        check if precipitation exceeds over 0.1 inches
         if (PRECIP > 0.1) then
            BUO= calculateBuildUp(BUO, PRECIP)
         else
            return
         end if

!        if buildup index is above zero, set it to zero
         if (BUO < 0) then
            BUO = 0
            return
         else
            return
         end if
      else
         DIF = DRY - WET
      end if

!     iterative loop that ultimate sets I to 3
      do I = 1, 3
         continue
      end do

!     if the wet and dry difference is greater than the danger rating then calculate FFM
      if (DIF <= C(I)) then
         FFM = B(I)*EXP(A(I)*DIF)
      else
         I = 4
         FFM = B(I)*EXP(A(I)*DIF)
      end if

!     iterative loop that ultimate sets I to 6
      do I = 1, 6
         continue
      end do

!     checks for fine fuel moisture rating
      if (FFM <= D(I)) then
         DF = 7
      else
         DF = I - 1
      end if

!     check if fine fuel moisture is less than 1, set it to 1
      if (FFM < 1.0) then
         FFM = 1.0
      end if
      FFM = FFM + (IHERB - 1) * 5.0

!     check if precipitation is less than  0.1
      if (PRECIP <= 0.1) then
         BUO = BUO + DF
      else
         BUO = calculateBuildUp(BUO, PRECIP)
         if (BUO < 0) then
            BUO = 0.0
         end if
         BUO = BUO + DF
      end if

      ADFM = 0.9 * FFM + 0.5 + 9.5 * EXP(-BUO/50.0)

!     big if case that splits the rest of the subroutine, entirely dependent on the adjusted fuel moisture being less than 30% or not
      if ( ADFM < 30.0 ) then
!        check if the wind is less than 14 mph
         if ( WIND < 14.0 )then
!           calculate timber and grass fuel moisture rating based on the wind value
            TIMBER = 0.01312*(WIND + 6.0) * (33.0 - ADFM)**1.65 - 3.0
            GRASS = 0.01312*(WIND + 6.0) * (33.0 - FFM)**1.65 - 3.0
!           give the fuel moistures a rating of 1 at the very least
            if ( TIMBER < 1.0 ) then
               TIMBER = 1.0
            end if
            if ( GRASS <= 1.0 ) then
               GRASS = 1.0
            end if

!           unsure why timber would be zero or less here given the previous assignment of timber
            if ( TIMBER <= 0) then
               return
            else
!              this should run regardless
               if ( BUO <= 0) then
                  return
               else
                  FLOAD = 1.75 * ALOG10( TIMBER ) + 0.32 * ALOG10( BUO ) - 1.640
!                 if FLOAD is less than zero set it to zero 
                  if ( FLOAD < 0) then
                     FLOAD = 0.0
                     return
!                 FLOAD is equal to or greater than zero
                  else
                     FLOAD = 10.0 ** FLOAD
                     return
                  end if
               end if
            end if
!        wind is >= 14 mph
         else
!           timber and grass are calculated using a different equation if wind is greater than 14 mph
            TIMBER = 0.00918*(WIND+14.0) * (33.0-ADFM)**1.65 - 3.0
            GRASS  = 0.00918*(WIND+14.0) * (33.0-FFM)**1.65 - 3.0

!           sets fuel max to 99            
            if ( GRASS > 99.0 ) then
               GRASS = 99.0
               if ( TIMBER > 99.0 ) then
                  TIMBER = 99.0
               end if
            end if

!           checks if timber is below 0
            if ( TIMBER <= 0) then
               return
            else
               if ( BUO <= 0) then
                  return
               end if
            end if
            FLOAD = 1.75*ALOG10( TIMBER ) + 0.32*ALOG10( BUO ) - 1.640
!           if FLOAD is less than zero set it to zero
            if ( FLOAD <= 0) then
               FLOAD = 0.0
               return
            end if
!           FLOAD is equal to or greater than zero
            FLOAD = 10.0 ** FLOAD
            return
         end if
!     ADFM >= 30
      else
!        fuel is too wet to work, set them both to 1
         if ( FFM >= 30.0 ) then
            GRASS = 1.0
            TIMBER = 1.0
            return
         end if        
         TIMBER = 1.0

!        if it's not windy out
         if ( WIND < 14.0 ) then
            GRASS = 0.01312*(WIND + 6.0) * (33.0-FFM)**1.65 - 3.0
!           set fuel minimum
            if ( TIMBER < 1.0 ) then
               TIMBER = 1.0
            end if
            if ( GRASS < 1.0 ) then
               GRASS = 1.0
            end if

!           shouldn't run given that the minimus were set
            if ( TIMBER <= 0) then
               return
            end if
            if ( BUO <= 0) then
               return
            end if

            FLOAD = 1.75*ALOG10( TIMBER ) + 0.32*ALOG10( BUO ) - 1.640

!           if FLOAD is less than zero set it to zero            
            if ( FLOAD <= 0) then
               FLOAD = 0.0
               return
            end if
!           FLOAD is equal to or greater than zero
            FLOAD = 10.0 ** FLOAD
            return
         else
            GRASS  = 0.00918*(WIND + 14.0) * (33.0 - FFM)**1.65 - 3.0
!           set fuel max
            if ( GRASS > 99.0 ) then
               GRASS = 99.0
               if ( TIMBER > 99.0 ) then
                  TIMBER = 99.0
               end if
            end if
!           checks if timber or grass is less than zero
            if ( TIMBER <= 0) then
               return
            end if
            if ( BUO <= 0) then
               return
            end if

            FLOAD = 1.75*ALOG10( TIMBER ) + 0.32*ALOG10( BUO ) - 1.640
!           if FLOAD is less than zero set it to zero  
            if ( FLOAD <= 0) then
               FLOAD = 0.0
               return
            end if
!           FLOAD is equal to or greater than zero
            FLOAD = 10.0 ** FLOAD
            return
         end if
      end if
      end subroutine danger

!     calculate buildup calculates the buildup index
      real function calculateBuildUp(BUO, PRECIP) result(output)
      real, intent(in) :: BUO, PRECIP
      output = -50.0 * ALOG(1.0-(1.0-EXP(-BUO/50.0)) * EXP( -1.175*(PRECIP-0.1)))   
      end function calculateBuildUp