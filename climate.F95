! Author: Simon Mason
Module climate
!
! Modules
  Use data_numbers,      Only: rp
  Use data_io_constants, Only: lprd
  Use time,              Only: pdate
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: calc_climate, check_climates, clim_per_v11_to_v12, climatology, close_climate, init_climate, init_climatep, &
            label_climate, print_climatology, proj_read_climate, proj_write_climate, reset_climate, set_climate, valid_climate
!
! Arrays
!
! Integer arrays
  Integer, Dimension(:), Allocatable, Public :: ncu ! - number of used climatological cases -
!
! Real arrays
  Real(Kind=rp), Dimension(:,:), Allocatable, Public :: ave   ! - climatological averages -
  Real(Kind=rp), Dimension(:,:), Allocatable, Public :: climx ! - model climatological (sorted) data -
  Real(Kind=rp), Dimension(:,:), Allocatable, Public :: sdev  ! - climatological standard deviations -
!
  Real(Kind=rp), Dimension(:,:,:), Allocatable, Public :: clim ! - climatological (sorted) response data -
!
! Scalars
!
! Integer scalars
  Integer, Public :: iclim     ! - setable climatological period flag -
  Integer, Public :: ieclim    ! - climatological period can extend beyond training period flag -
  Integer, Public :: ieclim_bk ! - climatological period can extend beyond training period backup flag -
!
! Logical scalars
  Logical, Public :: lcex = .false. ! - climatological period extends beyond the range of the training period? -
!
  Logical, Private :: lbefore ! - climatological period extends before start of training period? -
  Logical, Private :: lafter  ! - climatological period extends after start of training period? -
!
! Derived types
!
! Derived type definitions
! - climatological period -
  Public clim_per
  Type clim_per
     Sequence
     Type(pdate) :: d1        ! - first date of climatology -
     Type(pdate) :: d2        ! - last date of climatology -
!
     Integer :: ic1           ! - index of first date of climatological period -
     Integer :: ic2           ! - index of last date of climatological period -
     Integer :: nc            ! - length of climatological period -
     Integer :: il1           ! - index of first date of climatological period relative to independent training period -
     Integer :: il2           ! - index of last date of climatological period relative to independent training period -
     Integer :: lsn           ! - length of season -
!
     Character(lprd) :: clim1 ! - first date of climatology -
     Character(lprd) :: clim2 ! - last date of climatology -
  End Type clim_per
!
! - version 11 climatological period -
  Public clim_per_v11
  Type clim_per_v11
     Sequence
     Type(pdate) :: d1        ! - first date of climatology -
     Type(pdate) :: d2        ! - last date of climatology -
!
     Integer :: ic1           ! - index of first date of climatological period -
     Integer :: ic2           ! - index of last date of climatological period -
     Integer :: nc            ! - length of climatological period -
     Integer :: lsn           ! - length of season -
!
     Character(lprd) :: clim1 ! - first date of climatology -
     Character(lprd) :: clim2 ! - last date of climatology -
  End Type clim_per_v11
!
! Interface operators
  Private :: Operator(==)
!
  Public :: Operator(/=)
!
  Interface Operator(==)
     Module Procedure same_clim_per
  End Interface
  Interface Operator(/=)
     Module Procedure diff_clim_per
  End Interface
!
! Derived type scalars
  Type(clim_per), Public :: climate_per ! - climatological period -
  Type(clim_per), Public :: climate_bak ! - backup climatological period -
!
Contains
!
!
 Function same_clim_per(cp1, cp2)
!
! Are climate periods identical?
!
! Modules
  Use time, Only: Operator(==)
!
! Function type
  Logical same_clim_per
!
! Arguments
!
! Input scalars
  Type(clim_per), Intent(In) :: cp1 ! - first climatological period -
  Type(clim_per), Intent(In) :: cp2 ! - second climatological period -
!
! Executable Statements
!
! Test whether climate periods are identical
  If ((cp1%d1 == cp2%d1) .and. (cp1%d2 == cp2%d2)) Then
     same_clim_per = .true.
  Else
     same_clim_per = .false.
  End If
!
  Return
 End Function same_clim_per
!
!
!
 Function diff_clim_per(cp1, cp2)
!
! Are climate periods different?
!
! Modules
  Use time, Only: Operator(==)
!
! Function type
  Logical diff_clim_per
!
! Arguments
!
! Input scalars
  Type(clim_per), Intent(In) :: cp1 ! - first climatological period -
  Type(clim_per), Intent(In) :: cp2 ! - second climatological period -
!
! Executable Statements
!
! Test whether climate periods are different
  If ((cp1%d1 == cp2%d1) .and. (cp1%d2 == cp2%d2)) Then
     diff_clim_per = .false.
  Else
     diff_clim_per = .true.
  End If
!
  Return
 End Function diff_clim_per
!
!
!
 Subroutine reset_climate ()
!
! Resets climatological period
!
! Executable Statements
!
! Switch off climatological period
  iclim = 0
  Call init_climatep ()
!
  Return
 End Subroutine reset_climate
!
!
!
 Subroutine init_climatep ()
!
! Initialises climatological period
!
! Modules
  Use time, Only: Assignment(=)
!
! Executable Statements
!
! Initialise climatological period
  climate_per%d1 = 0      ! - first date of climatology -
  climate_per%d2 = 0      ! - last date of climatology -
  climate_per%ic1 = 0     ! - index of first date of climatological period -
  climate_per%ic2 = 0     ! - index of last date of climatological period -
  climate_per%nc = 0      ! - length of climatological period -
  climate_per%il1 = 0     ! - index of first date of climatological period relative to independent training period -
  climate_per%il2 = 0     ! - index of last date of climatological period relative to independent training period -
  climate_per%lsn = 0     ! - length of season -
  climate_per%clim1 = ' ' ! - first period of climatology -
  climate_per%clim2 = ' ' ! - last period of climatology -
!
  Return
 End Subroutine init_climatep
!
!
!
 Subroutine clim_per_v11_to_v12 (climate_v11, climate_v12)
!
! Converts version 11 climate to version 12
!
! Arguments
!
! Input scalars
  Type(clim_per_v11), Intent(In) :: climate_v11 ! - version 11 climate -
!
! Output scalars
  Type(clim_per), Intent(Out) :: climate_v12 ! - version 12 climate -
!
! Executable Statements
!
! Convert climatological period
  climate_v12%d1 = climate_v11%d1       ! - first date of climatology -
  climate_v12%d2 = climate_v11%d2       ! - last date of climatology -
  climate_v12%ic1 = climate_v11%ic1     ! - index of first date of climatological period -
  climate_v12%ic2 = climate_v11%ic2     ! - index of last date of climatological period -
  climate_v12%nc = climate_v11%nc       ! - length of climatological period -
  climate_v12%il1 = 0                   ! - index of first date of climatological period relative to independent training period -
  climate_v12%il2 = 0                   ! - index of last date of climatological period relative to independent training period -
  climate_v12%lsn = climate_v11%lsn     ! - length of season -
  climate_v12%clim1 = climate_v11%clim1 ! - first date of climatology -
  climate_v12%clim2 = climate_v11%clim2 ! - last date of climatology -
!
  Return
 End Subroutine clim_per_v11_to_v12
!
!
!
 Subroutine init_climate (i0, nsn, ifail)
!
! Initialises memory for climatologies
!
! Modules
  Use arrays,   Only: rwk
  Use settings, Only: igauss, mya, nu
  Use distribs, Only: alpha, beta, pcen
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: i0  ! - lower bound -
  Integer, Intent(In) :: nsn ! - number of seasons -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic routines
  Intrinsic Allocated
!
! Executable Statements
!
! Free existing memory
  If (Allocated(rwk)) Deallocate (rwk)
!
! Allocate memory
! - climatological data -
  If (.not.lcex) Then
     Allocate (clim(mya,nu,i0:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
  End If
! - number of used climatological cases -
  Allocate (ncu(i0:nsn), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - climatological averages -
  Allocate (ave(mya,i0:nsn), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - climatological standard deviations -
  Allocate (sdev(mya,i0:nsn), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - gamma parameters -
  If (igauss == 2) Then
     Allocate (alpha(mya,-1:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     Allocate (beta(mya,-1:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     Allocate (pcen(mya,-1:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
  End If
!
  Return
!
! Errors
1 ifail = 1
!
  Return
 End Subroutine init_climate
!
!
!
 Function climatology(icalc, lsn0, nsn, lprst)
!
! Redefines climatological period for forecasts
!
! Modules
  Use data_numbers,   Only: one
  Use progress_meter, Only: end_progress_meter, set_progress_increment, start_progress_meter
  Use arrays,         Only: y
  Use settings,       Only: igauss, mya, nu, &
                            record_change
  Use categories,     Only: ithr, tobs, tobst, &
                            set_percentiles, set_thresholds
!
! Function type
  Integer :: climatology
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: icalc ! - calculation completed flag -
  Integer, Intent(In) :: lsn0  ! - length of persistence seasons -
  Integer, Intent(In) :: nsn   ! - number of seasons -
!
  Logical, Intent(In) :: lprst ! - include persistence component? -
!
! Locals
!
! Local scalars
  Integer :: isn   ! - season index -
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
!
! Executable Statements
!
! Recalculate climatology
  Call set_climate (icalc, lsn0, nsn)
  If ((climate_per%ic1 < 1) .or. (climate_per%ic2 > nu)) Then
     climatology = -1
     Return
  End If
  Select Case (ithr)
   Case (1, 3)
     Select Case (igauss)
      Case (0)
        Call set_progress_increment (one/Real(3, Kind=rp), .false.)
      Case Default
        Call set_progress_increment (one/Real(4, Kind=rp), .false.)
     End Select
   Case (2)
     Select Case (igauss)
      Case (0)
        Call set_progress_increment (one/Real(2, Kind=rp), .false.)
      Case Default
        Call set_progress_increment (one/Real(3, Kind=rp), .false.)
     End Select
  End Select
  Call start_progress_meter ('Calculating climatologies', .false.)
  Do isn = 1, nsn
     Call calc_climate (.true., isn, mya, y(1:mya,1:nu,isn), climate_per, ave(:,isn), sdev(:,isn), ncu(isn), &
          clim(1:mya,1:climate_per%nc,isn), ifail)
  End Do
  If (lprst) ncu(0) = ncu(1)
!
! Calculate thresholds
  If (ifail == 0) Then
     Select Case (ithr)
      Case (1)
        Call set_thresholds (igauss, icalc, mya, nsn, ncu(:), clim(:,:,1:), tobs, tobst, ifail)
        climatology = ifail
      Case (2)
        climatology = set_percentiles(nsn, ncu(:), clim(:,:,1))
        Call set_thresholds (igauss, icalc, mya, nsn, ncu(:), clim(:,:,1:), tobs, tobst, ifail)
        climatology = ifail
      Case (3)
        climatology = set_percentiles(nsn, ncu(:), clim(:,:,1))
     End Select
     If (climatology /= 0) Then
        climate_per = climate_bak
        Call end_progress_meter (.false., .false.)
        climatology = 1
     End If
     climatology = record_change() ! - record change for project file -
     Call end_progress_meter (.false., .true.)
     climatology = 0
  Else
     climate_per = climate_bak
     Call end_progress_meter (.false., .false.)
     climatology = 1
  End If
!
  Return
 End Function climatology
!
!
!
 Function valid_climate()
!
! Checks that end of climatological period is later than beginning
!
! Modules
  Use time,     Only: date_diff, &
                      Operator(+)
  Use gui,      Only: window_update
  Use errors,   Only: cpt_error
  Use settings, Only: nt
  Use iofiles,  Only: yfile
!
! Function type
  Integer :: valid_climate
!
! Locals
!
! Local scalars
  Integer :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for valid settings
  climate_per%nc = date_diff(climate_per%d1, climate_per%d2, yfile%iseq) + 1
  If (climate_per%nc <= 0) Then
     climate_per%d2 = yfile%prd1%sdate + (yfile%it1 + nt - 2)
     Call window_update (climate_per%d2%iyr)
     ifail = 1
     Call cpt_error ('valid_climate', ifail, .false., &
          i_arg1=yfile%iseq)
     valid_climate = 2
  Else
     valid_climate = -1
  End If
!
  Return
 End Function valid_climate
!
!
!
 Subroutine check_climates (cfile, isq, lpfv, ls0, ls1, ls2, ls3, imd)
!
! Checks that climate period is within data range
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, nmn
  Use time,                Only: Operator(+), Operator(-), Operator(>), Operator(<), Operator(<=), Operator(==), &
                                 iseq, &
                                 date_diff, reset_iseq
  Use settings,            Only: ncm, nct, nt
  Use iofiles,             Only: ifile
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: isq ! - sequencing -
  Integer, Intent(In) :: ls0 ! - length of persistence component -
  Integer, Intent(In) :: ls1 ! - length of season -
  Integer, Intent(In) :: ls2 ! - length of all seasons -
  Integer, Intent(In) :: ls3 ! - length of last season -
  Integer, Intent(In) :: imd ! - difference in months between first and last seasons -
!
  Logical, Intent(In) :: lpfv ! - PFV? -
!
  Type(ifile), Intent(In) :: cfile ! - file containing climatological data -
!
! Locals
!
! Local scalars
  Integer :: dmx ! - maximum date difference -
  Integer :: idc ! - difference in months between first and last seasons of climatology -
!
  Type(pdate) :: dbdate ! - default beginning date -
  Type(pdate) :: dldate ! - default last date -
  Type(pdate) :: d1     ! - first date -
  Type(pdate) :: d2     ! - second date -
!
! Executable Statements
!
! Set date limits
  d1 = cfile%bdate
  d2 = cfile%bdate + (nt - 1)
  If (.not.lpfv .or. ieclim == 0) Then
     If (ieclim == 0) Then
        dbdate = d1
        dldate = d2
     Else
        dbdate = cfile%prd1%sdate
        If (iseq < 0) Then
           Call reset_iseq (isq_new=isq_mn)
        Else
           Call reset_iseq (isq_new=iseq)
        End If
        dldate = cfile%prdn%sdate - (ls1 - 1)
        Call reset_iseq ()
     End If
  Else
     dbdate = cfile%prd1%sdate
     dldate = cfile%bdate - 1
     If (dldate <= dbdate) dldate = cfile%prdn%sdate
  End If
!
! Check for valid settings
! - set to default if climatological period has not been previously set -
  If (climate_per%nc <= 1) Then
     climate_per%d1 = dbdate
     climate_per%d2 = dldate
! - check that limits are within range of data -
  Else
     If (climate_per%d1 < dbdate) climate_per%d1 = dbdate
     If (climate_per%d2 > dldate) climate_per%d2 = dldate
     If (climate_per%d1 > climate_per%d2) Then
        climate_per%d1 = dbdate
        climate_per%d2 = dldate
     End If
  End If
! - adjust end date to ensure equal number of each season -
  If (imd > 0) Then
     idc = climate_per%d2%imn - climate_per%d1%imn
     If (idc /= imd) climate_per%d2 = climate_per%d2 + (imd - nmn - idc)
  End If
!
! Check whether climatological period extends outside of training period
  lbefore = climate_per%d1 < d1
  lafter = climate_per%d2 > d2
  If (lbefore .or. lafter) Then
     lcex = .true.
     If (lbefore .or. climate_per%d1 == cfile%bdate) Then ! - climatological period starts before training period -
        climate_per%il1 = 1
        d1 = climate_per%d1
     Else
        climate_per%il1 = date_diff(cfile%bdate, climate_per%d1, cfile%iseq) + 1 + ls0
     End If
!
! Set combined length of climatological and training periods
     If (lafter) Then ! - climatological period ends after training period -
        d2 = climate_per%d2
        Select Case (isq)
         Case Default
           nct = date_diff(d1, d2, isq) + ls2
         Case (isq_sn)
           nct = date_diff(d1, d2, isq_mn) + ls3
        End Select
     Else ! - climatological period starts before training period -
        Select Case (isq)
         Case Default
           nct = date_diff(d1, d2, isq) + ls1
         Case (isq_sn)
           nct = date_diff(d1, d2, isq_mn) + ls3
        End Select
     End If
! - adjust if period is too long -
     Select Case (isq)
      Case Default
        dmx = date_diff(d1, cfile%prdn%edate, isq) + 1
      Case (isq_sn)
        dmx = date_diff(d1, cfile%prdn%edate, isq_mn) + 1
     End Select
     dmx = dmx + 1 - ls3
     Select Case (isq)
      Case Default
        If (nct > dmx) nct = dmx
      Case (isq_mn, isq_so)
        Do
           If (nct > dmx) Then
              nct = nct - nmn
              Cycle
           Else
              Exit
           End If
        End Do
     End Select
     climate_per%il2 = climate_per%il1 + date_diff(climate_per%d1, climate_per%d2, cfile%iseq)
     climate_per%nc = (climate_per%il2 - climate_per%il1) + 1
  Else
     lcex = .false.
     nct = nt
  End If
  ncm = nct
!
  Return
 End Subroutine check_climates
!
!
!
 Subroutine set_climate (icalc, lsn0, nsn)
!
! Sets climatological parameters given date ranges
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
  Use time,                Only: Operator(-), &
                                 pdate, &
                                 iseq, &
                                 date_diff, reset_iseq
  Use settings,            Only: ncm, nt
  Use iofiles,             Only: yfile
  Use missing,             Only: kcuse, kuse
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: icalc ! - calculation completed flag -
  Integer, Intent(In) :: lsn0 ! - length of persistence seasons -
  Integer, Intent(In) :: nsn  ! - number of seasons -
!
! Locals
!
! Local scalars
  Integer :: isn ! - season index -
  Integer :: k   ! - case index -
  Integer :: kk  ! - case index -
!
  Type(pdate) :: d1 ! - date -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Count, Cshift, Min
!
! Executable Statements
!
! Identify year indices
  If (lsn0 == 0) Then
     climate_per%ic1 = date_diff(yfile%fdate, climate_per%d1, yfile%iseq) + 1
     climate_per%ic2 = date_diff(yfile%fdate, climate_per%d2, yfile%iseq) + 1
  Else
     Call reset_iseq (isq_new=isq_mn)
     d1 = yfile%fdate - lsn0
     Call reset_iseq ()
     climate_per%ic1 = date_diff(d1, climate_per%d1, yfile%iseq) + 1
     climate_per%ic2 = date_diff(d1, climate_per%d2, yfile%iseq) + 1
  End If
  IF (icalc == 1) Then
     If (.not.lcex) Then
        If (climate_per%ic1 >= 1) Then
           climate_per%ic1 = climate_per%ic1 - Count(.not.kuse(1:climate_per%ic1))
           If (climate_per%ic1 == 0) climate_per%ic1=1
        End If
        climate_per%ic2 = climate_per%ic2 - Count(.not.kuse(1:Min(climate_per%ic2, nt)))
        climate_per%nc = climate_per%ic2 + 1 - climate_per%ic1
        ncu(:) = climate_per%nc
     Else
! - remove early data that are not part of the climatology -
        Select Case (iseq)
         Case Default
           climate_per%nc = Count(kcuse(climate_per%il1:climate_per%il2))
           If (.not.lbefore) Then
              clim(:,:,:) = Cshift(clim(:,:,:), climate_per%il1-1, Dim=2)
              kcuse(:) = Cshift(kcuse(:), climate_per%il1-1)
              climate_per%il2 = climate_per%il2 + 1 - climate_per%il1
              climate_per%il1 = 1
           Else
              climate_per%ic1 = climate_per%il1 - Count(.not.kcuse(1:climate_per%il1))
           End If
! - delete missing years -
           If (climate_per%ic1 == 0) Then
              Do
                 climate_per%ic1 = climate_per%ic1 + 1
                 If (kcuse(climate_per%ic1)) Exit
              End Do
           End If
           climate_per%ic2 = climate_per%il2 - Count(.not.kcuse(1:climate_per%il2))
           kk = 0
           climate_per%nc = 0
           Do k = climate_per%il1, climate_per%il2
              kk = kk + 1
              If (.not.kcuse(k)) Cycle
              climate_per%nc = climate_per%nc + 1
              If (climate_per%nc < kk) clim(:,climate_per%nc,:) = clim(:,k,:)
           End Do
           ncu(:) = climate_per%nc
         Case (isq_so, isq_sn)
           climate_per%nc = climate_per%d2%iyr + 1 - climate_per%d1%iyr
           If (lbefore) Then
              climate_per%il1 = 1
              climate_per%il2 = climate_per%nc
           Else
              climate_per%il1 = 1 + date_diff(yfile%fdate, climate_per%d1, isq_yr)
              climate_per%il2 = climate_per%il1 + climate_per%nc - 1
           End If
           Do isn = 1, nsn
              kk = 0
              Do k = climate_per%il1, climate_per%il2
                 If ((k - 1)*nsn + isn > ncm) Exit
                 If (.not.kcuse((k-1)*nsn + isn)) Cycle
                 kk = kk + 1
                 If (kk < k) clim(:,kk,isn) = clim(:,k,isn)
              End Do
              ncu(isn) = kk
           End Do
        End Select
     End If
  End If
!
! Calculate length of season
  Select Case (iseq)
   Case Default
     climate_per%lsn = date_diff(yfile%prd1%sdate, yfile%prd1%edate, isq_mn) + 1
   Case (isq_sn)
     climate_per%lsn = date_diff(yfile%prd1%sdate, yfile%prd1%edate, isq_mn) + 1
   Case (1:)
     climate_per%lsn = date_diff(yfile%prd1%sdate, yfile%prd1%edate, iseq) + 1
  End Select
!
! Construct climatological period labels
  Call label_climate (lsn0)
!
  Return
 End Subroutine set_climate
!
!
!
 Subroutine label_climate (lsn0)
!
! Constructs climatological period labels
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_sn, nmn
  Use time,                Only: iseq, &
                                 date_diff, get_cdate
  Use iofiles,             Only: yfile
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: lsn0 ! - length of persistence seasons -
!
! Locals
!
! Local scalars
  Integer :: ls  ! - length of season -
!
!
! Executable Statements
!
! Construct climatological period labels
  If (climate_per%lsn <= nmn) Then
     ls = climate_per%lsn + lsn0
  Else
     ls = climate_per%lsn + lsn0 + 1 - nmn
  End If
  climate_per%clim1 = get_cdate(climate_per%d1, ls, 2)
  If (iseq == isq_sn) Then
     climate_per%lsn = date_diff(yfile%prdn%sdate, yfile%prdn%edate, isq_mn) + 1
     If (climate_per%lsn <= nmn) Then
        ls = climate_per%lsn + lsn0
     Else
        ls = climate_per%lsn + lsn0 + 1 - nmn
     End If
  End If
  climate_per%clim2 = get_cdate(climate_per%d2, ls, 2)
!
  Return
 End Subroutine label_climate
!
!
!
 Subroutine calc_climate (lstd, isn, m, y, climate_per, ybar, ystd, ncu, clim, ifail)
!
! Calculates mean and variance for a climate period
!
! Modules
  Use data_numbers,   Only: zero, one
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: yhat, yret, yrpls, &
                            insertion_sort
  Use settings,       Only: igauss, iretro, istd, nu, nur
  Use distribs,       Only: alpha, beta, pcen, &
                            stdize, thom_gamma, ustdize
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: isn ! - season -
  Integer, Intent(In) :: m   ! - number of variables -
  Integer, Intent(In) :: ncu ! - number of used climatological cases -
!
  Logical, Intent(In) :: lstd ! - recalculate climatology? -
!
  Type(clim_per), Intent(In) :: climate_per ! - climatological period -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input/output arrays
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - data -
!
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: clim ! - climatological (sorted) data -
!
! Output arrays
  Real(Kind=rp), Dimension(:), Intent(Out) :: ybar ! - climatological mean -
  Real(Kind=rp), Dimension(:), Intent(Out) :: ystd ! - climatological standard deviation -
!
! Locals
!
! Local scalars
  Integer :: i ! - variable index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real, Sqrt, Sum
!
! Executable Statements
!
! Unstandardise, if requested
  ifail = -1
  If (.not.lcex) Then
     If ((lstd) .and. (istd /= 0)) Then
        Call ustdize (istd, igauss, isn, m, nu, ybar, ystd, y)
        Call ustdize (istd, igauss, isn, m, nu, ybar, ystd, yhat)
        If (iretro == 1) Then
           Call ustdize (istd, igauss, isn, m, nur, ybar, ystd, yret)
           Call ustdize (istd, igauss, isn, m, nur, ybar, ystd, yrpls(:,:)%lower)
           Call ustdize (istd, igauss, isn, m, nur, ybar, ystd, yrpls(:,:)%upper)
        End If
     End If
  End If
!
! Extract climatological data
  Do i = 1, m
     If (.not.lcex) clim(i,1:climate_per%nc) = y(i,climate_per%ic1:climate_per%ic2)
!
! Calculate climatological mean
     ybar(i) = Sum(clim(i,1:ncu))/Real(ncu, Kind=rp)
!
! Calculate climatological standard deviation
     If (climate_per%nc > 1) Then
        ystd(i) = Sum((clim(i,1:ncu) - ybar(i))**2)/Real(ncu-1, Kind=rp)
        If (ystd(i) > zero) Then
           ystd(i) = Sqrt(ystd(i))
        Else
           ystd(i) = one
        End If
     Else
        ystd(i) = one
     End If
!
! Create climatology
     Call insertion_sort (ncu, clim(i,:), 'a')
!
! Calculate gamma parameters
     If (igauss == 2) Call thom_gamma (ncu, clim(i,:), alpha(i,isn), beta(i,isn), pcen(i,isn))
!
! Update progress meter
     If (update_progress_meter(.false.) /= 0) Return
  End Do
!
! Restandardise, if requested
  If (.not.lcex) Then
     If ((lstd) .and. (istd /= 0)) Then
        Call stdize (istd, igauss, isn, m ,nu, ybar, ystd, y)
        Call stdize (istd, igauss, isn, m, ncu, ybar, ystd, clim)
        Call stdize (istd, igauss, isn, m, nu, ybar, ystd, yhat)
        If (iretro == 1) Then
           Call stdize (istd, igauss, isn, m, nur, ybar, ystd, yret)
           Call stdize (istd, igauss, isn, m, nur, ybar, ystd, yrpls(:,:)%lower)
           Call stdize (istd, igauss, isn, m, nur, ybar, ystd, yrpls(:,:)%upper)
        End If
     End If
  End If
!
! Update progress meter
  If (update_progress_meter(.false.) /= 0) Return
  ifail = 0
!
  Return
 End Subroutine calc_climate
!
!
!
 Subroutine print_climatology (lprmpt, &
            iout)
!
! Prints climatology
!
! Modules
  Use gui, Only: print_text
!
! Arguments
!
! Input scalars
  Logical, Intent(In), Optional :: lprmpt ! - print prompt? -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: iout ! - output unit number -
!
! Functions nad Subroutines
!
! Intrinsic functions
  Intrinsic Present, Trim
!
! Executable Statements
!
! Print climatology
  If (Present(iout)) Then
     If (lprmpt) Then
        Call print_text ('Climatology:', &
             iout=iout)
        Call print_text ('    '//Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2), &
             iout=iout)
     Else
        Call print_text (Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2), &
             iout=iout)
     End If
  Else
     If (lprmpt) Then
        Call print_text ('Climatology:', &
             lcolour=.true.)
        Call print_text ('    '//Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2))
     Else
        Call print_text (Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2))
     End If
  End If
!
  Return
 End Subroutine print_climatology
!
!
!
 Subroutine proj_read_climate (iin, nt, lsn0, lres, ifail)
!
! Reads climatology settings from project file
!
! Modules
  Use data_time_constants, Only: isq_mn
  Use time,                Only: Operator(+), Operator(<), Operator(>), &
                                 date_diff
  Use iofiles,             Only: yfile
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin  ! - input file unit number -
  Integer, Intent(In) :: nt   ! - number of cases in training period -
  Integer, Intent(In) :: lsn0 ! - length of persistence season -
!
  Logical, Intent(In) :: lres ! - read results? -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Read climatology settings
  If (.not.lres) Then
     Read (Unit=iin, Err=1, End=2) climate_per, ieclim
!
! Check whether climatological period extends beyond training period
     If (yfile%cfile%lset) Then
        If ((climate_per%d1 < yfile%fdate) .or. (climate_per%d2 > yfile%fdate+(nt-1))) Then
           If (lsn0 > 0) Then ! - adjust for persistence component -
              If (date_diff(climate_per%d1, yfile%fdate, isq_mn) > lsn0) Then
                 lcex = .true.
              Else
                 lcex = .false.
              End If
           Else
              lcex = .true.
           End If
        Else
           lcex = .false.
        End If
     Else
        lcex = .false.
     End If
!
! Read results
  Else
     Read (Unit=iin, Err=1, End=2) ave
     Read (Unit=iin, Err=1, End=2) clim
     Read (Unit=iin, Err=1, End=2) sdev
     Read (Unit=iin, Err=1, End=2) ncu
  End If
!
  ifail = 0
  Return
!
! Errors in project file
! - problem reading file -
1 ifail = 1
  Return
!
! - end of file -
2 ifail = 2
  Return
!
 End Subroutine proj_read_climate
!
!
!
 Subroutine proj_write_climate (iout, lres, ifail)
!
! Writes climatology settings to project file
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iout ! - output file unit number -
!
  Logical, Intent(In) :: lres ! - write results? -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write climatology settings
  If (.not.lres) Then
     Write (Unit=iout, Err=1) climate_per, ieclim
!
! Write results
  Else
     Write (Unit=iout, Err=1) ave
     Write (Unit=iout, Err=1) clim
     Write (Unit=iout, Err=1) sdev
     Write (Unit=iout, Err=1) ncu
  End If
!
  ifail = 0
  Return
!
! Errors in project file
! - problem writing file -
1 ifail = 1
  Return
!
 End Subroutine proj_write_climate
!
!
!
 Function close_climate()
!
! Frees memory allocated for climate
!
! Modules
  Use distribs, Only: alpha, beta, pcen
!
! Function type
  Integer :: close_climate
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated
!
! Executable Statements
!
! Free memory allocated for categories
  If (Allocated(pcen))  Deallocate (pcen)
  If (Allocated(beta))  Deallocate (beta)
  If (Allocated(alpha)) Deallocate (alpha)
  If (Allocated(sdev))  Deallocate (sdev)
  If (Allocated(ave))   Deallocate (ave)
  If (Allocated(ncu))   Deallocate (ncu)
  If (Allocated(clim))  Deallocate (clim)
  close_climate = 0
!
  Return
 End Function close_climate
End Module climate
