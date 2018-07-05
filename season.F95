! Author: Simon Mason
Module season
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: close_season, get_cssel, get_cssn, get_seasons, proj_read_season, proj_write_season, prompt_season, reset_season, &
            seasonal, seasonal_zyp, set_icsv, set_seasons
!
! Parameters
!
! Local parameters
  Integer, Parameter, Public :: lssel = 40 ! - length of current season selection -
!
! Arrays
!
! Integer arrays
  Integer, Dimension(:), Allocatable, Public :: lsn ! - length of season -
!
! Real arrays
  Real(Kind=rp), Dimension(:,:,:), Allocatable, Private :: vs ! - seasonal aggregates -
!
! Logical arrays
  Logical, Dimension(:), Allocatable, Private :: ldsn ! - distinct season? -
!
! Scalars
!
! Integer scalars
  Integer, Public :: iafter ! - A's date should be before (-1) equal to (0) or after (1) B's date? -
  Integer, Public :: icsv   ! - calculate seasonal values flag -
  Integer, Public :: ispi   ! - SPI period -
  Integer, Public :: lensn  ! - total length of season -
  Integer, Public :: lb     ! - lower-bound -
  Integer, Public :: nsn    ! - number of seasons -
  Integer, Public :: nssr   ! - number of seasons with shortened records -
!
  Integer, Private :: l0  ! - first month of persistence component -
  Integer, Private :: nfd ! - total number of fields -
!
! Character scalars
  Character(Len=lssel), Public :: cssel
!
! Logical scalars
  Logical, Public :: lprst ! - include persistence component? -
!
  Logical, Private :: lssr ! - seasons have shortened records? -
!
Contains
!
!
 Subroutine reset_season ()
!
! Resets season definitions
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated
!
! Executable Statements
!
! Reset season settings
  icsv = 0
  ispi = 0
  lb = 1
  lensn = 0
  nsn = 1
  lprst = .false.
!
! Reset memory
  If (Allocated(lsn)) Deallocate (lsn)
  Allocate (lsn(0:nsn))
  lsn(0) = 0
  lsn(1) = 1
!
  Return
 End Subroutine reset_season
!
!
!
 Subroutine get_seasons (ianal, xfield, yfield, ifail)
!
! Identifies (PFV and model predictors) or prompts (otherwise) for target season
!
! Modules
  Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, nmn
  Use time,                Only: Operator(==), &
                                 iseq, nss, &
                                 same_ssn
  Use errors,              Only: cpt_error
  Use settings,            Only: isynch
  Use iofiles,             Only: xfile, yfile
  Use fields,              Only: field, &
                                 mfield, nmfd, &
                                 init_field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ianal ! - analysis indicator -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Pointer arrays
  Type(field), Dimension(:), Pointer :: xfield ! - X field settings -
  Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
!
! Locals
!
! Local scalars
  Integer :: isn    ! - season/field index -
  Integer :: lsn_bk ! - back-up length of season -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated, Any, Size, Ubound
!
! Exeutable Statements
!
! Switch off persistence component if seasonal calculations are not required
  If ((ianal/=ia_pfv) .and. (isynch == 0)) Then
     If (xfield(1)%mdate == 0) Then
        iafter = 1
     Else If (.not.same_ssn(xfield(1)%tprd, yfield(1)%tprd)) Then ! - force a lag if Y season is incompatible with X -
        If (xfile%iseq == yfile%iseq) Then ! - seasons are necessarily incompatible if the sequencing is identical -
           iafter = 1
        Else If (yfile%iseq == isq_mn .and. xfile%iseq < 0) Then ! - no incompatibility if Y-file is monthly -
           iafter = 0
        Else
           iafter = 0
        End If
     Else
        iafter = 0
     End If
  Else
     iafter = 0
  End If
  If (icsv == 0) Then
     If (nsn == 1) Then
        xfile%cssn = get_cssn(xfile%prd1, xfile%iseq,nsn)
     Else
        xfile%cssn = get_cssn(yfile%prd1, xfile%iseq,nsn)
     End If
     lb = 1
     lensn = 0
     lprst = .false.
     ifail = 0
     Return
  End If
!
! Count number of different seasons
  nfd = Size(xfield)
  If (Allocated(ldsn)) Deallocate (ldsn)
  Allocate (ldsn(nfd))
  ldsn(1) = .true.
  Select Case (iseq)
   Case (isq_yr, isq_sn)
     nsn = 1
     If (nfd > 1) Then
        Do isn = 2, nfd
           If (Any(xfield(1:isn-1)%cssn == xfield(isn)%cssn)) Then
              ldsn(isn) = .false.
           Else
              ldsn(isn) = .true.
              nsn = nsn + 1
           End If
        End Do
     End If
   Case (isq_so)
     nsn = nmn
     nss = 1
  End Select
  If (iseq == isq_sn) nss = nsn
  Select Case (ianal)
    Case (ia_cca, ia_pcr, ia_mlr)
     If (xfield(1)%mdate == 0) nsn = 1
  End Select
!
! Allocate workspace
  If (Ubound(lsn, Dim=1) /= nsn) Then
     lsn_bk = lsn(1)
     Deallocate (lsn)
     Allocate (lsn(0:nsn))
     lsn(1) = lsn_bk
  End If
!
! Backup Y fields as monthly fields
  Nullify (mfield)
  Select Case (iseq)
   Case (isq_yr)
     nmfd = Size(yfield)
   Case Default
     nmfd = nsn
  End Select
  Call init_field (mfield, nmfd, yfield(:)%rmiss, ifail)
  If (ifail /= 0) Return
  Select Case (iseq)
   Case (isq_yr)
     mfield(:) = yfield(:)
   Case Default
     mfield(1) = yfield(1)
     mfield(2:) = mfield(1)
  End Select
!
! Identify season
  Select Case (ianal)
   Case (ia_cca, ia_pcr, ia_mlr)
     If ((xfield(1)%mdate == 0) .and. ((isynch == 0) .or. (lb == 0))) Then
        nsn = 1
        Call prompt_season (nsn, 1, yfield, ifail)
     Else
        Call identify_seasons (nsn, xfield, yfield, ifail)
     End If
   Case (ia_gcm, ia_pfv)
     Call identify_seasons (nsn, xfield, yfield, ifail)
  End Select
!
! Set seasons
  Select Case (ifail)
   Case (0)
     Call set_seasons (nsn, yfield)
   Case (-1) ! - cancelled -
     Return
   Case Default ! - problem allocating memory -
     ifail = 1
     Call cpt_error ('get_seasons', ifail, .false.)
  End Select
!
  Return
 End Subroutine get_seasons
!
!
!
 Subroutine prompt_season (nsn, lby, yfield, ifail)
!
! Prompts for season definitions
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_yr, nmn
  Use time,                Only: Operator(+), Operator(-), &
                                 adjust_pprd, reset_iseq
  Use labels,              Only: cg_cselection, &
                                 l_tseason
  Use gui,                 Only: iw, &
                                 box_close, box_open, init_win, print_cvalue, print_text, prompt_integer, win_prompt
#if GUI == 1
  Use gui,                 Only: print_advisory, prompt_boolean
#endif
  Use errors,              Only: cpt_error
#if GUI == 1
  Use settings,            Only: istd
#endif
  Use settings,            Only: record_change, set_options
  Use iofiles,             Only: mfile, xfile, yfile
  Use fields,              Only: field, &
                                 pfield, &
                                 init_field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nsn ! - number of distinct seasons -
  Integer, Intent(In) :: lby ! - lower bound for yfield -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Pointer arrays
  Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
!
! Locals
!
! Local scalars
  Integer :: iprst ! - use persistence component -
  Integer :: istdu ! - used standardization option -
!
  Character(Len=64) :: cse ! - error seasons -
!
! Local arrays
  Type(field), Dimension(:), Allocatable :: tfield ! - temporary field information -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Lbound, Trim, Ubound
!
! Executable Statements
!
! Backup season settings
  Call reset_iseq (isq_new=isq_mn)
  If (ispi == 0) ispi=1
  ifail = set_options(i1=yfield(1)%tprd%sdate%imn, i2=lsn(1), l1=lprst)
  If (lprst) Then
     iprst = 1
  Else
     iprst = 0
  End If
!
! Prompt for season
  cssel = get_cssel(0, yfield(1)%tprd%sdate%imn, lsn(1), 1, 0)
  pfield => yfield(1)
  Call init_win (title=Trim(l_tseason%c))
  Call box_open (.false., .false., &
       title='Set season')
  Call prompt_integer ('First month of season to forecast', yfield(1)%tprd%sdate%imn, &
       itab=60, ilow=1, ihgh=nmn, fcb1=record_change, fcb2=update_ctsel, lfocus = .false.)
!
! Prompt for season
  Call prompt_integer ('Length of season to forecast', lsn(1), &
       itab=60, ilow=1, ihgh=nmn, fcb1=record_change, fcb2=update_ctsel, lfocus = .false.)
!
! Prompt for persistence component
#if GUI == 1
  Call prompt_boolean ('Add persistence component?', iprst)
#endif
  Call print_text (' ')
  Call print_cvalue (Trim(cg_cselection), cssel, &
       iwid=lssel)
  Call print_text (' ')
  Call box_close (.false.)
  iw = win_prompt(.true.)
!
! Set option
  lprst = (iprst == 1)
  If (set_options(iw=iw, i1=yfield(1)%tprd%sdate%imn, i2=lsn(1), l1=lprst) >= 3) Then
     ifail = -1
     GoTo 2
  End If
!
! Prompt for persistence component
1 Continue
#if GUI == 1
  If (lprst) Then
     If (lsn(1) < nmn) Then
        istdu = istd
#else
        istdu = 3
#endif
        l0 = yfield(lby)%tprd%sdate%imn
        If (istdu /= 3) Then
           ispi = pfield%tprd%sdate%imn - l0
           If (ispi < 0) ispi = ispi + nmn
           ispi = ispi + lsn(1)
        End If
        cssel = get_cssel(0, pfield%tprd%sdate%imn, lsn(1), 1, ispi)
        ifail = set_options(i1=ispi, i2=l0)
        Call init_win (title=Trim(l_tseason%c))
        Call box_open (.false., .false., &
             title='Persistence component')
        Select Case (istdu)
         Case Default
           Call prompt_integer ('First month of persistence component', l0, &
                ilow=1, ihgh=nmn, fcb1=record_change, fcb2=update_cl0, lfocus = .false.)
         Case (3) ! - SPI standardization -
           Call prompt_integer ('Length of SPI', ispi, &
                ilow=lsn(1), ihgh=nmn, fcb1=record_change, fcb2=update_cspi, lfocus = .false.)
        End Select
        Call print_text (' ')
        Call print_cvalue (Trim(cg_cselection), cssel, &
             iwid=lssel)
        Call box_close (.false.)
        iw = win_prompt(.true.)
        If (istdu /= 3) Then
           ispi = pfield%tprd%sdate%imn - l0
           If (ispi < 0) ispi = ispi + nmn
           ispi = ispi + lsn(1)
        End If
        If (set_options(iw=iw, i1=ispi, i2=l0) >= 3) ispi = lsn(1)
#if GUI == 1
     Else
        Call print_advisory ('It is not possible to add a persistence component when the length of the season is one year')
        ispi = lsn(1)
     End If
  Else
     ispi = lsn(1)
  End If
#endif
! - check for invalid SPI period -
  Allocate (tfield(0:Ubound(yfield, Dim=1)))
  tfield(0) = yfield(1)
  If (ispi > nmn) Then
     ifail = 1
     tfield(1)%tprd = yfield(1)%tprd
     tfield(1)%tprd%edate = tfield(1)%tprd%sdate + (lsn(1) - 1)
     tfield(0)%tprd%sdate = tfield(1)%tprd%edate - (ispi - 1)
     tfield(0)%tprd%edate = tfield(1)%tprd%sdate - 1
     lsn(0) = ispi - lsn(1)
     cse='Persistence season: '//Trim(get_cssn(tfield(0)%tprd, isq_yr, 1))//&
        &'; Target season: '//Trim(get_cssn(tfield(1)%tprd, isq_yr, 1))
     Call cpt_error ('prompt_season', ifail, .false., &
          c_arg1=Trim(cse))
     ispi = lsn(1)
     Deallocate (tfield)
     GoTo 1
! - check for persistence component -
  Else If (ispi > lsn(1)) Then
     lsn(0) = ispi - lsn(1)
     lprst = .true.
     lb = 0
  Else
     lsn(0) = 0
     lprst = .false.
     lb = 1
  End If
!
! Adjust start year if month is unavailable
  yfield(1)%tprd%edate = yfield(1)%tprd%sdate + (lsn(1) - 1)
  Call adjust_pprd (yfield(1)%tprd, mfile%prd1%sdate, xfile%iseq, .true.)
!
! Reset Y fields
  If ((Lbound(yfield, Dim=1)/=lb) .or. (Ubound(yfield(:), Dim=1)/yfile%nfs /= nsn)) Then
     tfield(1:) = yfield(1:)
!     Call init_field (yfield, Ubound(tfield(:), Dim=1), (/tfield(1)%rmiss/), ifail, & ! For 64-bit code
     Call init_field (yfield, Ubound(tfield(:), Dim=1, Kind=Kind(0)), (/tfield(1)%rmiss/), ifail, &
          lb=lb)
     If (ifail /= 0) GoTo 2
     yfield(lb:) = tfield(lb:)
  End If
  If (lb == 0) yfield(0)%tprd = yfield(1)%tprd-lsn(0)
  ifail = 0
!
! Restore time sequencing
2 Call reset_iseq ()
!
  Return
 End Subroutine prompt_season
!
!
!
 Subroutine identify_seasons (nsn, xfield, yfield, ifail)
!
! Identifies season definitions from X field settings
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_so
  Use time,                Only: Operator(+), Operator(>), &
                                 iseq, &
                                 adjust_pprd, date_diff, reset_iseq
  Use iofiles,             Only: mfile, xfile
  Use fields,              Only: field, &
                                 mfield, &
                                 init_field
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nsn ! - number of distinct seasons -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Pointer arrays
  Type(field), Dimension(:), Pointer :: xfield ! - X field settings -
  Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
!
! Locals
!
! Local scalars
  Integer :: ifd    ! - field index -
  Integer :: isn    ! - season index -
  Integer :: isn2   ! - season index -
  Integer :: iyd    ! - year difference -
!
  Logical :: lso ! - seasonally-overlapping sequencing? -
!
! Executable Statements
!
! Re-initialise Y fields
  Call init_field (yfield, nsn, mfield(1:1)%rmiss, ifail)
  If (ifail /= 0) Return  
  yfield(:) = mfield(1)
!
! Identify season
  lso = (iseq == isq_so)
  Call reset_iseq (isq_new=isq_mn)
  isn = 0
  Do ifd = 1, nfd
     If (.not.ldsn(ifd)) Cycle
     isn = isn + 1
     lsn(isn) = date_diff(xfield(ifd)%tprd%sdate, xfield(ifd)%tprd%edate, isq_mn) + 1
     yfield(isn)%tprd%sdate%imn = xfield(isn)%tprd%sdate%imn
     yfield(isn)%tprd%edate = yfield(isn)%tprd%sdate + (lsn(isn) - 1)
     If (ifd == 1) Then
        If (mfile%prd1%sdate > xfield(ifd)%tprd%sdate) Then
           Call adjust_pprd (yfield(isn)%tprd, mfile%prd1%sdate, xfile%iseq, .true.)
        End If
     Else
        iyd = xfield(isn)%tprd%sdate%iyr - xfield(1)%tprd%sdate%iyr
        yfield(isn)%tprd%sdate%iyr = yfield(1)%tprd%sdate%iyr + iyd
        yfield(isn)%tprd%edate = yfield(isn)%tprd%sdate + (lsn(isn) - 1)
        If (iyd < 0) Then ! - adjust if season is earlier than first -
           Call adjust_pprd (yfield(isn)%tprd, mfile%prd1%sdate, xfile%iseq, .true.)
           iyd = xfield(1)%tprd%sdate%iyr - xfield(isn)%tprd%sdate%iyr
           Do isn2 = 1, isn-1
              yfield(isn2)%tprd%sdate%iyr = yfield(isn)%tprd%sdate%iyr + iyd
              yfield(isn2)%tprd%edate = yfield(isn2)%tprd%sdate + (lsn(isn2) - 1)
           End Do
        End If
     End If
  End Do
!
! Set overlapping seasons
  If (lso) Then
     lsn(2:) = lsn(1)
     Do isn = 2, nsn
        yfield(isn)%tprd = yfield(isn-1)%tprd + 1
     End Do
  End If
  Call reset_iseq ()
!
! Switch off persistence component
  ispi = lsn(1)
  lsn(0) = 0
  lprst = .false.
!
  Return
 End Subroutine identify_seasons
!
!
!
 Subroutine set_seasons (nsn, yfield)
!
! Sets season definitions
!
! Modules
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr
  Use time,                Only: Operator( == ), Operator(<), Operator(>), Operator(-), Operator(+), &
                                 iseq, &
                                 adjust_pdate, adjust_pprd, date_diff, get_cdate, reset_iseq
  Use gui,                 Only: window_update
  Use iofiles,             Only: mfile, xfile, yfile, &
                                 set_fdate
  Use fields,              Only: field
  Use climate,             Only: climate_per
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nsn ! - number of seasons -
!
! Pointer arrays
  Type(field), Dimension(:), Pointer :: yfield ! - Y field settings -
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
  Intrinsic Ubound
!
! Executable Statements
!
! Set Y-file season
  yfile%prd1%sdate = yfield(1)%tprd%sdate
  yfile%prd1%edate = yfield(1)%tprd%edate
  If (lprst) Then
     lensn = ispi
  Else
     If (nsn == 1) Then
        lensn = lsn(1)
     Else
        Select Case (iseq)
         Case Default
           Do isn = 2, nsn
              If (yfield(isn)%tprd%sdate < yfile%prd1%sdate) yfile%prd1%sdate = yfield(isn)%tprd%sdate
              If (yfield(isn)%tprd%edate > yfile%prd1%edate) yfile%prd1%edate = yfield(isn)%tprd%edate
           End Do
           lensn = date_diff(yfile%prd1%sdate, yfile%prd1%edate, isq_mn) + 1
         Case (isq_sn, isq_so)
           yfile%prdn = yfile%prd1
           Do
              If (yfile%prdn%edate < mfile%prdn%edate) Then
                 yfile%prdn = yfile%prdn + 1
              Else
                 Exit
              End If
           End Do
           lensn = date_diff(yfield(1)%tprd%sdate, yfield(nsn)%tprd%edate, isq_mn) + 1
        End Select
     End If
  End If
  If (nsn == 1 .and. Ubound(yfield(:), Dim=1) > 1) yfield(1:)%tprd = yfield(1)%tprd ! - apply to any additional fields -
!
! Determine last season available
  If ((iseq /= isq_sn) .and. (iseq /= isq_so)) Then
     Call reset_iseq (isq_new=isq_mn)
     yfile%prdn%edate%imn = yfile%prd1%edate%imn
     yfile%prdn%sdate = yfile%prdn%edate - (lensn - 1)
     Call reset_iseq ()
  End If
  Call adjust_pprd (yfile%prdn, mfile%prdn%edate, xfile%iseq, .false.)
!
! Set appropriate start date
  yfile%fdate%imn = yfile%prd1%sdate%imn
  yfile%bdate = yfile%fdate
  If (xfile%fdate%imn == 0 .and. iseq == isq_yr) Then
     xfile%fdate%imn = xfile%prd1%sdate%imn
     xfile%fdate%idy = xfile%prd1%sdate%idy
     xfile%bdate%imn = xfile%prd1%sdate%imn
     xfile%bdate%idy = xfile%prd1%sdate%idy
  End If
  If (lprst) Then ! - set appropriate lag if necessary -
     If (iseq < 0) Then
        Call reset_iseq (isq_new=isq_mn)
     Else
        Call reset_iseq (isq_new=iseq)
     End If
     yfile%bdate = yfile%bdate - lsn(0)
     yfield(0)%tprd%edate = yfield(0)%tprd%sdate + (lsn(0) - 1)
     Call reset_iseq ()
  End If
  Call set_fdate (iafter, xfile%iseq, xfile%fdate, mfile%prd1%sdate, yfile%bdate, yfile%fdate, ifail)
  If (ifail /= 0) Call set_fdate (-iafter, xfile%iseq, yfile%fdate, xfile%prd1%sdate, xfile%bdate, xfile%fdate, ifail)
  mfile%bdate = yfile%bdate
  mfile%fdate = yfile%fdate
  Call window_update (yfile%fdate%iyr)
  Call window_update (yfile%fdate%imn)
  Call window_update (xfile%fdate%iyr)
  Call window_update (xfile%fdate%imn)
!
! Set season labels
  Select Case (iseq)
   Case Default
     yfile%cprd1 = get_cdate(yfile%prd1, lsn(1), 2)
     yfile%cprdn = get_cdate(yfile%prdn, lsn(1), 2)
   Case (isq_sn, isq_so)
     yfile%cprd1 = get_cdate(yfile%prd1, 1, 2)
     yfile%cprdn = get_cdate(yfile%prdn, 1, 2)
  End Select
  If (nsn == 1) Then
     xfile%cssn = get_cssn(xfile%prd1, xfile%iseq, nsn)
  Else
     xfile%cssn = get_cssn(yfile%prd1, xfile%iseq, nsn)
  End If
  yfile%cssn = get_cssn(yfile%prd1, yfile%iseq, nsn)
  mfile%cssn = get_cssn(mfile%prd1, mfile%iseq, 1)
  Do isn = lb, nsn
     yfield(isn)%cssn = get_cssn(yfield(isn)%tprd, yfile%iseq, 1)
  End Do
!
! Apply to climatology
  If (.not.climate_per%d1 == 0) Then
     climate_per%d1%imn = yfile%prd1%sdate%imn
     If (lsn(0) > 0) Then ! - adjust climatology start date for persistence component -
        Call reset_iseq (isq_new=isq_mn)
        climate_per%d1 = climate_per%d1 - lsn(0)
        Call reset_iseq ()
     End If
     Call adjust_pdate (climate_per%d1, mfile%prd1%sdate, xfile%iseq, .true.)
  End If
  If (.not.climate_per%d2 == 0) Then
     climate_per%d2%imn = yfile%prdn%sdate%imn
     If (climate_per%d2 > mfile%prdn%edate) Call adjust_pdate (climate_per%d2, mfile%prdn%edate, xfile%iseq, .false.)
  End If
!
  Return
 End Subroutine set_seasons
!
!
!
 Function get_cssn(p, isq, nsn) &
          Result (cssn)
!
! Creates season as a character string
!
! Modules
  Use data_text,           Only: ilang
  Use data_time_constants, Only: cma, cmon, isq_mn, nmn
  Use time,                Only: Operator(==), &
                                 pprd, &
                                 date_diff
!
! Function type
  Character(Len=21) :: cssn
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: isq ! - sequence -
  Integer, Intent(In) :: nsn ! - number of seasons -
!
  Type(pprd), Intent(In) :: p ! - period -
!
! Locals
!
! Local scalars
  Integer :: ls ! - length of season -
!
! Executable Statements
!
! Create season
  If (isq<0 .and. p%sdate%imn /= 0 .and. p%edate%imn /= 0) Then
     If (p%edate == p%sdate) Then
        cssn = cmon(p%sdate%imn, ilang)
     Else If (nsn <= 1) Then
        If (p%edate%iyr == p%sdate%iyr) Then
           If (p%sdate%imn == p%edate%imn) Then
              cssn = cmon(p%sdate%imn,ilang)
           Else
              If (p%edate%idy == p%sdate%idy) Then
                 Write (Unit=cssn, Fmt='(A)') cma(ilang)(p%sdate%imn:p%edate%imn)
              Else
                 Write (Unit=cssn, Fmt='(I2.2,3A,I2.2,2A)') &
                    p%sdate%idy, ' ', cmon(p%sdate%imn,ilang), ' / ', p%edate%idy, ' ', cmon(p%edate%imn,ilang)
              End If
           End If
        Else
           If (p%sdate%idy == 0) Then
              ls = date_diff(p%sdate, p%edate, isq_mn)
              Select Case (ls)
               Case (:4)
                 Write (Unit=cssn, Fmt='(A)') cma(ilang)(p%sdate%imn:p%edate%imn+nmn)
               Case Default
                 Write (Unit=cssn, Fmt='(3A)') cmon(p%sdate%imn,ilang), ' - ', cmon(p%edate%imn,ilang)
              End Select
           Else
              Write (Unit=cssn, Fmt='(I2.2,3A,I2.2,2A)') &
                 p%sdate%idy, ' ', cmon(p%sdate%imn,ilang), ' / ', p%edate%idy, ' ', cmon(p%edate%imn,ilang)
           End If
        End If
     Else
        Write (Unit=cssn, Fmt='(3A)') &
           cma(ilang)(p%sdate%imn:p%sdate%imn+lsn(1)-1), ' - ', cma(ilang)(p%edate%imn+nmn-(lsn(nsn)-1):p%edate%imn+nmn)
     End If
  Else
     cssn = ' '
  End If
!
  Return
 End Function get_cssn
!
!
!
 Function get_cssel(imd0, imd1, ls, nld, ispi) &
          Result (cssel)
!
! Returns current season selection
!
! Modules
  Use data_text,           Only: ilang
  Use data_time_constants, Only: cma, cmon, nmn
  Use time,                Only: add_to_month
  Use labels,              Only: cg_initin_l
!
! Function type
  Character(Len=lssel) :: cssel
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: imd0 ! - month of forecast initialization -
  Integer, Intent(In) :: imd1 ! - first month of season -
  Integer, Intent(In) :: ls   ! - length of season -
  Integer, Intent(In) :: nld  ! - number of seasons -
  Integer, Intent(In) :: ispi ! - length of SPI -
!
! Locals
!
! Local scalars
  Integer :: l  ! - season index -
  Integer :: lp ! - length of persistence season -
  Integer :: m0 ! - zeroth month -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Current season(s)
  If (ls == 1) Then
     cssel = cmon(imd1, ilang)
  Else
     cssel = cma(ilang)(imd1:imd1+ls-1)
  End If
  Select Case (nld)
   Case (1)
     Continue
   Case (2:4)
     Do l = 2, nld
        If (ls == 1) Then
           If (imd1+l-1 <= nmn) Then
              cssel = Trim(cssel)//', '//cmon(imd1+l-1,ilang)
           Else
              cssel = Trim(cssel)//', '//cmon(imd1+l-(nmn+1),ilang)
           End If
        Else
           m0 = add_to_month(imd1, (l-1)*ls-1)
           cssel = Trim(cssel)//', '//cma(ilang)(m0+1:m0+ls)
        End If
     End Do
   Case Default
     If (ls == 1) Then
        If (imd1+l-1 <= nmn) Then
           cssel = Trim(cssel)//' - '//cmon(imd1+nld-1,ilang)
        Else
           cssel = Trim(cssel)//' - '//cmon(imd1+nld-(nmn+1),ilang)
        End If
     Else
        m0 = add_to_month(imd1, (nld-1)*ls-1)
        cssel = Trim(cssel)//' - '//cma(ilang)(m0+1:m0+ls)
     End If
  End Select
!
! Add start date
  If (imd0 > 0) cssel = Trim(cssel)//' '//Trim(cg_initin_l)//' '//cmon(imd0,ilang)
  If (ispi > 0) Then
     cssel = 'Forecast: '//Trim(cssel)
     lp = ispi - ls
     m0 = add_to_month(imd1, -(lp+1))
     Select Case (lp)
      Case (:0)
        cssel = 'Persistence: N/A; '//Trim(cssel)
      Case (1)
        cssel = 'Persistence: '//cmon(m0+1,ilang)//'; '//Trim(cssel)
      Case Default
        cssel = 'Persistence: '//cma(ilang)(m0+1:m0+lp)//'; '//Trim(cssel)
     End Select
  End If
!
  Return
 End Function get_cssel
!
!
!
 Function update_ctsel() &
          Result (update)
!
! Updates target season selection indicator
!
! Modules
  Use gui,    Only: window_update
  Use fields, Only: pfield
!
! Function type
  Integer :: update
!
! Executable Statements
!
! Update season selection indicator
  cssel = get_cssel(0, pfield%tprd%sdate%imn, lsn(1), 1, 0)
  Call window_update (cssel)
  update = 2
!
  Return
 End Function update_ctsel
!
!
!
 Function update_cl0() &
          Result (update)
!
! Updates target season selection indicator given first month of persistence
!
! Modules
  Use data_time_constants, Only: nmn
  Use fields,              Only: pfield
!
! Function type
  Integer :: update
!
! Executable Statements
!
! Update season selection indicator
  ispi = pfield%tprd%sdate%imn - l0
  If (ispi < 0) ispi = ispi + nmn
  ispi = ispi + lsn(1)
  update = update_cspi()
!
  Return
 End Function update_cl0
!
!
!
 Function update_cspi() &
          Result (update)
!
! Updates target season selection indicator given SPI length
!
! Modules
  Use gui,    Only: window_update
  Use fields, Only: pfield
!
! Function type
  Integer :: update
!
! Executable Statements
!
! Update season selection indicator
  cssel = get_cssel(0, pfield%tprd%sdate%imn, lsn(1), 1, ispi)
  Call window_update (cssel)
  update = 2
!
  Return
 End Function update_cspi
!
!
!
 Subroutine set_icsv (icsv,units)
!
! Determines whether to calculate seasonal averages or seasonal aggregations
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: units ! - field units -
!
! Input/output scalars
  Integer, Intent(InOut) :: icsv ! - calculate seasonal values flag -
!
! Executable Statements
!
! Determine best estimate of appropriate means of seasonal aggregation
  Select Case (Trim(units))
   Case ('K', 'Kelvin_scale', 'C', 'degree_Celsius', 'Celsius_scale', 'F', 'Farenheit') ! - temperatures -
     icsv = 1
   Case ('m/s', 'meters/second', 'm s-1', 'mm/day', 'cm/day') ! - rates -
     icsv = 1
   Case ('m', 'meters', 'cm', 'mm') ! - depths -
     icsv = 2
   Case Default
     If (icsv == 0) icsv = 1
  End Select
!
  Return
 End Subroutine set_icsv
!
!
!
 Subroutine seasonal ()
!
! Converts monthly to seasonal data
!
! Modules
  Use data_time_constants, Only: isq_sn, isq_so, isq_yr, nmn
  Use time,                Only: Operator(+), Operator(-), &
                                 iseq
  Use arrays,              Only: y
  Use settings,            Only: ncm, nct, nmt, nt, nur, ny, nyr
  Use iofiles,             Only: mfile
  Use fields,              Only: yfield
  Use climate,             Only: clim,climate_per, lcex
!
! Locals
!
! Local scalars
  Integer :: n     ! - number of cases -
  Integer :: ns    ! - number of seasons -
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Ceiling, Mod, Real
!
! Executable Statements
!
! Determine parameters
  If (lprst) Then
     ns = 2
  Else
     ns = nsn
  End If
!
! Calculate seasonal aggregates from climatological data
  If (lcex) Then
     nyr = Ceiling(Real(nct)/Real(nmn))
     Allocate (vs(ny,nyr,lb:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     Call calc_seasonal (icsv, lprst, ny, nyr, nct, ns, lsn(lb:), yfield(lb:)%tprd%sdate%imn, climate_per%d1, yfield(lb:)%rmiss, &
          clim(:,:,1), vs(:,:,lb:))
! - reassign memory -
     Select Case (iseq)
      Case (isq_so)
        n = Ceiling(Real(nt)/Real(nsn))
        nur = n
      Case (isq_sn)
        nur = nt
        n = Ceiling(Real(nt)/Real(nsn))
      Case Default
        n = nt
     End Select
     Deallocate (clim)
     Deallocate (y)
     Allocate (y(ny,n,lb:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     Allocate (clim(ny,nyr,lb:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     clim(:,:,:) = vs(:,:,:)
     nct = nyr
     nyr = n
!
! Calculate seasonal aggregates from training data
  Else
     Select Case (iseq)
      Case (isq_yr)
        nyr = nt
      Case (isq_sn, isq_so)
        nyr = Ceiling(Real(nt)/Real(nsn))
     End Select
     Allocate (vs(ny,nyr,lb:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     Select Case (iseq)
      Case (isq_yr)
        n = nmt
      Case (isq_sn, isq_so)
        n = nt
     End Select
     Call calc_seasonal (icsv, lprst, ny, nyr, n, ns, lsn(lb:), yfield(lb:)%tprd%sdate%imn, mfile%bdate, yfield(lb:)%rmiss, &
          y(:,:,1), vs(:,:,lb:))
! - reassign memory -
     Deallocate (y)
     Allocate (y(ny,nyr,lb:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 1
     y(:,:,:) = vs(:,:,:)
  End If
  Deallocate (vs)
!
! Reset numbers of cases once seasonal totals are calculated
  nssr = Mod(nt, nsn)
  lssr = (nssr == 0)
  Select Case (iseq)
   Case (isq_yr)
     ncm = nct
     If (lssr) Then
        If (lcex) Then
           nmt = nt
        Else
           nmt = nyr
        End If
     End If
   Case (isq_sn, isq_so)
     nmt = nt
  End Select
!
  Return
!
1 ifail = 1
  Return
 End Subroutine seasonal
!
!
!
 Subroutine seasonal_zyp ()
!
! Converts monthly to seasonal data for persistence components of forecasts
!
! Modules
  Use data_time_constants, Only: isq_so, isq_yr
  Use time,                Only: Operator(+), Operator(-), &
                                 iseq
  Use arrays,              Only: zyp
  Use settings,            Only: nmf, nf, ny
  Use iofiles,             Only: pfile
  Use fields,              Only: yfield
!
! Locals
!
! Local scalars
  Integer :: n     ! - number of cases -
  Integer :: nyr   ! - number of years -
  Integer :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Ceiling,Real
!
! Executable Statements
!
! Calculate seasonal aggregates from training data
  Select Case (iseq)
   Case (isq_yr)
     nyr = nf
   Case (isq_so)
     nyr = Ceiling(Real(nf)/Real(nsn))
  End Select
  Allocate (vs(ny,nyr,0:0), Stat=ifail)
  If (ifail /= 0) GoTo 1
  Select Case (iseq)
   Case (isq_yr)
     n = nmf
   Case (isq_so)
     n = nf
  End Select
  Call calc_seasonal (icsv, lprst, ny, nyr, n, 1, lsn(0:), yfield(0:)%tprd%sdate%imn, pfile%bdate, yfield(0:)%rmiss, zyp(:,:,1), &
       vs(:,:,0:))
! - reassign memory -
  Deallocate (zyp)
  Allocate (zyp(ny,nyr,0:0), Stat=ifail)
  If (ifail /= 0) GoTo 1
  zyp(:,:,:) = vs(:,:,:)
  nf = nyr
  Deallocate (vs)
  If (iseq == isq_yr) nmf = nyr
!
  Return
!
1 ifail = 1
  Return
 End Subroutine seasonal_zyp
!
!
!
 Subroutine calc_seasonal (icsv, lcs, m, nyr, nmx, nsn, lsn, imn1, d1, rmiss, vm, vs)
!
! Calculates seasonal averages/totals
!
! Modules
  Use data_numbers,        Only: zero, tol
  Use data_time_constants, Only: nmn
  Use time,                Only: pdate, &
                                 add_to_month, ndays
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: icsv ! - seasonal aggregation method -
  Integer, Intent(In) :: m    ! - number of locations -
  Integer, Intent(In) :: nyr  ! - number of years -
  Integer, Intent(In) :: nsn  ! - number of seasons -
  Integer, Intent(In) :: nmx  ! - maximum number of cases -
!
  Logical, Intent(In) :: lcs ! - consecutive seasons? -
!
  Type(pdate), Intent(In) :: d1 ! - first date available -
!
! Input arrays
  Integer, Dimension(:), Intent(In) :: lsn  ! - length of season -
  Integer, Dimension(:), Intent(In) :: imn1 ! - first month of season -
!
  Real(Kind=rp), Dimension(:), Intent(In) :: rmiss ! - missing values -
!
  Real(Kind=rp), Dimension(:,:), Intent(In) :: vm ! - monthly values (dimensions: m,nyr*nmn) -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:,:), Intent(Out) :: vs ! - seasonal values (dimensions: m,nyr,nsn) -
!
! Locals
!
! Local scalars
  Integer :: i   ! - location index -
  Integer :: k   ! - year index -
  Integer :: l   ! - month index -
  Integer :: kl  ! - year/month index -
  Integer :: isn ! - season index -
  Integer :: is1 ! - index of first month of season -
  Integer :: isl ! - index of last month of season -
  Integer :: imn ! - current month -
  Integer :: iyr ! - current year -
  Integer :: lmn ! - length of month -
  Integer :: lsd ! - length of season in days -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Real
!
! Executable Statements
!
! Initialise
  Do isn = 1, nsn
     vs(:,:,isn) = rmiss(isn)
     If (lcs .and. isn > 1) Then
        is1 = isl + 1
     Else
        is1 = add_to_month(imn1(isn), 1-d1%imn)
     End If
     isl = is1 + lsn(isn) - 1
!
! Calculate seasonal aggregate
     Do k = 1, nyr
        iyr = d1%iyr + k - 1
        lsd = 0
        imn = imn1(isn) - 1
        vs(:,k,isn) = zero
        Do l = is1, isl
           imn = add_to_month(imn, 1, &
                 iyr=iyr)
           Select Case (icsv)
            Case (1) ! - seasonal average -
              lmn = ndays(iyr, imn)
              lsd = lsd + lmn
            Case (2) ! - seasonal total -
              lmn = 1
              lsd = 1
           End Select
           kl = (k - 1)*nmn + l
           If (kl > nmx) Then
              vs(:,k:,isn) = rmiss(isn)
              Exit
           End If
           Do i = 1, m
              If ((vm(i,kl) > rmiss(isn)-tol) .and. (vm(i,kl) < rmiss(isn)+tol)) Then
                 vs(i,k,isn) = rmiss(isn)
              Else If (vs(i,k,isn) /= rmiss(isn)) Then
                 vs(i,k,isn) = vs(i,k,isn) + vm(i,kl)*Real(lmn, Kind=rp)
              End If
           End Do
        End Do
        Where (vs(:,k,isn) /= rmiss(isn)) vs(:,k,isn) = vs(:,k,isn)/Real(lsd, Kind=rp)
     End Do
  End Do
!
  Return
 End Subroutine calc_seasonal
!
!
!
 Subroutine proj_read_season (iin, pver, ifail)
!
! Reads program settings from project file
!
! Modules
  Use data_time_constants, Only: isq_so
  Use data_numbers,        Only: sp
  Use time,                Only: iseq, nss
  Use iofiles,             Only: xfile
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input file unit number -
!
  Real(Kind=sp), Intent(In) :: pver ! - project file version -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Clear existing settings
  Call close_season ()
!
! Read program settings
  If (pver > 14.06_sp) Then
     Read (Unit=iin, Err=1, End=2) icsv, ispi, lb, lensn, lprst, lensn, nsn, nss, iseq
  Else If (pver > 14.0_sp) Then
     Read (Unit=iin, Err=1, End=2) icsv, ispi, lb, lensn, lprst, lensn, nsn
  Else
     Call reset_season ()
  End If
!
! Read season settings if saved
  If (icsv > 0) Then
     If (pver < 14.07_sp) Then
        Select Case (iseq)
         Case Default
           nss = nsn
         Case (isq_so)
           nss = 1
        End Select
     End If
     nfd = xfile%nfs*nss
     Allocate (ldsn(nfd), Stat=ifail)
     If (ifail /= 0) GoTo 3
     Deallocate (lsn)
     Allocate (lsn(0:nsn), Stat=ifail)
     If (ifail /= 0) GoTo 3
     Read (Unit=iin, Err=1, End=2) ldsn, lsn
  Else
     If (pver < 14.07_sp) nss = 1
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
! - problem initializing memory -
3 ifail = 3
  Return
!
 End Subroutine proj_read_season
!
!
!
 Subroutine proj_write_season (iout, ifail)
!
! Writes program settings to project file
!
! Modules
  Use time, Only: iseq,nss
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iout ! - output file unit number -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write program settings
  Write (Unit=iout, Err=1) icsv, ispi, lb, lensn, lprst, lensn, nsn, nss, iseq
!
! Write season settings if saved
  If (icsv > 0) Write (Unit=iout, Err=1) ldsn, lsn
!
  ifail = 0
  Return
!
! Errors in project file
! - problem writing file -
1 ifail = 1
  Return
!
 End Subroutine proj_write_season
!
!
!
 Subroutine close_season ()
!
! Frees memory allocated for calculating seasonal aggregates
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated
!
! Executable Statements
!
! Free memory allocated for scoring
  Call reset_season ()
  If (Allocated(ldsn)) Deallocate (ldsn)
!
  Return
 End Subroutine close_season
End Module season
