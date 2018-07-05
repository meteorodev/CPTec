! Author: Simon Mason
Module cpt_actions
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: cpt_reset, reset, reset_mem, run_analysis
#if GUI == 1
  Public :: check_reset
#endif
!
Contains
!
!
 Function run_analysis() 
!
! Reads data and fits selected model
!
! Modules
#if GUI == 1
  Use clrwin$,             Only: clear_window$
#endif
  Use data_numbers,        Only: rp, zero, one, onem
  Use data_cpt_constants,  Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv, mnt, ng, nts
  Use data_time_constants, Only: isq_mn, isq_sn, isq_so, isq_yr, nmn
  Use time,                Only: Operator(+), &
                                 pprd, &
                                 iseq, &
                                 get_cdate, reset_iseq
  Use labels,              Only: ca_atypes_a, &
                                 l_climper
  Use gui,                 Only: ifvo, &
                                 set_cursor_waiting, window_update
#if GUI == 1
  Use gui,                 Only: ih_con
#endif
  Use progress_meter,      Only: lstop, prog, &
                                 end_progress_meter, set_progress_increment, start_progress_meter, stop_progress_meter, &
                                 update_progress_meter
  Use errors,              Only: cpt_error
  Use arrays,              Only: y, yhat, ypc, yret
  Use settings,            Only: igauss, igto, iretro, istd, istdo, izero, lcw, lzero, &
                                 mxa, mya, nct, nmt, nt, ntr, nt1, nu, nur, nyr, &
                                 check_n, get_cv, get_retro, record_results, set_nused
  Use distribs,            Only: set_gamma, stdize, ustdize
  Use iofiles,             Only: mfile, xfile, yfile, zfile, &
                                 get_maxnt, set_bdate
  Use space,               Only: cstndy
#if GUI == 1
  Use fields,              Only: iffx, iffy, ilimx0, ilimx1, ilimx2, ilimy0, ilimy1, ilimy2, &
                                 set_ilims
#endif
  Use fields,              Only: xfield, yfield
  Use pcs,                 Only: lcde, &
                                 check_pcs, get_cca_settings, get_eofx_settings, get_eofy_settings, set_scree
  Use categories,          Only: irobs, ithr, pthr, rfps, tobs, tobst, &
                                 check_refdates, init_thresholds, set_cv_categories, set_ra_categories, set_percentiles, &
                                 set_thresholds
  Use climate,             Only: ave, clim, climate_per, lcex, ncu, sdev, &
                                 calc_climate, check_climates, init_climate, set_climate
  Use season,              Only: icsv, lb, lensn, lprst, lsn, nsn, nssr, &
                                 get_cssn, seasonal
  Use missing,             Only: kuse, &
                                 compress, replace_missing_xy
  Use analysis,            Only: ianal, icalc, &
                                 analysis_flags, close_analysis, init_results
  Use cca,                 Only: perform_cca
  Use pcr,                 Only: perform_pcr
  Use gcm,                 Only: perform_gcm
  Use pfv,                 Only: perform_pfv
  Use cpt_input,           Only: get_data
  Use forecast,            Only: ifc, ifcast
  Use verif,               Only: irv
!
! Function type
  Integer :: run_analysis
!
! Locals
!
! Local scalars
  Integer :: i     ! - location index -
  Integer :: j     ! - location index -
  Integer :: k     ! - time index -
  Integer :: kk    ! - time index -
  Integer :: l     ! - field index -
  Integer :: isn   ! - season index -
  Integer :: is0   ! - zeroth season -
  Integer :: ierr  ! - error parameter -
  Integer :: nact  ! - number of actions -
  Integer :: nmx   ! - maximum length of training period -
  Integer :: nmtx  ! - maximum length of training period in months -
  Integer :: ifail ! - error indicator -
  Integer :: iz    ! - zero-bound option -
!
  Logical :: lpfv ! - PFV? -
!
  Real(Kind=rp) :: tol ! - missing value rounding tolerance -
!
  Type(pprd) :: error_prd ! - period of erroneous data -
!
! Local arrays
  Real(Kind=rp), Dimension(:,:,:), Allocatable :: wk3 ! - workspace -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Abs, Max, MaxVal, Real, Trim
!
! Executable Statements
!
! Check for invalid length of training period
! - check for too few -
  ifail = check_n(nt, lcw, .true., ianal, iretro, ierr)
  If (ifail /= 0) Then
     If ((ifail /= 2) .and. (ianal /= ia_pfv)) Then
        Call window_update (lcw)
        Call cpt_error ('check_n', ifail, .false., &
             i_arg1=ierr)
        run_analysis = 1
        Return
     End If
  End If
! - check for too many -
  Call set_bdate (lsn(0)) ! - set beginning dates in case start date was modified -
  mfile%fdate = yfile%fdate
  mfile%bdate = yfile%bdate
  Call get_maxnt (icsv, lensn, nmx, nmtx)
  If (.not.lprst) Then
     Call get_maxnt (icsv,lensn,nmx,nmt)
  Else If (iseq /= isq_so) Then
     Call get_maxnt (icsv, lensn-lsn(0), nmx, nmt)
  Else
     Call get_maxnt (icsv, lsn(nsn), nmx, nmt)
  End If
  If (nt > nmx) Then
     nt = nmx
     Call window_update (nt)
     ifail = 1
     Call cpt_error ('run_analysis', ifail, .false.)
     run_analysis = 1
     Return
  End If
!
! Check for cancelled retroactive calculations
  Select Case (iretro)
   Case (0) ! - cross-validation -
     run_analysis = get_cv()
   Case (1) ! - retroactive -
     If (ianal /= ia_pfv) Then
        run_analysis = get_retro()
        If (iretro == -1) Then
           iretro = 0
           run_analysis = 1
           Return
        End If
     End If
  End Select
  run_analysis = 1
!
! Initialise analysis
! - switch on hour glass -
  Call set_cursor_waiting (1)
! - initialise calculation flags -
  Call analysis_flags ('on')
  ifc = 0
!
! Check climatologies
  lpfv = (ianal == ia_pfv)
  If (lpfv) Then
     If (Sum(xfield(1)%prob(:)) /= zero) Then
        pthr(1) = xfield(1)%prob(1)
        Do j = 2, nts
           pthr(j) = pthr(j-1) + xfield(1)%prob(j)
        End Do
     Else
        xfield(1)%prob(1) = pthr(1)
        Do j = 2, nts
           xfield(1)%prob(j) = pthr(j) - pthr(j-1)
        End Do
        xfield(1)%prob(ng) = one - pthr(nts)
     End If
  End If
  If (icsv>0) Then
     Select Case (iseq)
      Case (isq_yr)
        Call check_climates (yfile, mfile%iseq, lpfv, lsn(0), lensn, lensn, 0, 0)
      Case (isq_sn)
        Call check_climates (yfile, yfile%iseq, lpfv, 0, 1, 1, lsn(nsn), 0)
      Case Default
        Call check_climates (mfile, mfile%iseq, lpfv, lsn(0), lsn(1), lensn+1, 0, &
             yfield(nsn)%tprd%sdate%imn-yfield(1)%tprd%sdate%imn)
     End Select
  Else
     Call check_climates (yfile, yfile%iseq, lpfv, 0, 1, 1, 0, 0)
  End If
!
! Read data
  Call start_progress_meter ('Reading data', .false.)
  ifail = get_data(icsv, lcex, .true.)
  If ((ifail /= 0) .or. (lstop)) GoTo 1
  Call end_progress_meter (.false., .false.)
!
! Check for zero-bound
  If ((igto == 2) .and. (igauss > 0)) Then
     iz = 2
  Else If (izero == 1) Then
     iz = 3
  Else
     iz = 0
  End If
! - search for negative values if zero bound is on -
  If (iz /= 0) Then
     ifail = 0
     i = 0
     search_neg: Do l = 1, yfile%nfl ! - search for negative values -
        tol = Abs(yfield(l)%rmiss)/onem
        Do j = 1, yfield(l)%nv
           i = i + 1
           If (lcex) Then
              Do k = 1, nct
                 If (clim(i,k,1) < zero .and. (clim(i,k,1) < yfield(l)%rmiss-tol .or. clim(i,k,1) > yfield(l)%rmiss+tol)) Then
                    ifail = iz
                    Exit search_neg
                 End If
              End Do
           Else
              Do k = 1, nmt
                 If (y(i,k,1) < zero .and. (y(i,k,1) < yfield(l)%rmiss-tol .or. y(i,k,1) > yfield(l)%rmiss+tol)) Then
                    ifail = iz
                    Exit search_neg
                 End If
              End Do
           End If
        End Do
     End Do search_neg
     If (ifail /= 0) Then ! - negative values found -
        If (icsv == 0) Then
           error_prd = yfile%prd1 + (yfile%it1 + k - 2)
        Else
           Call reset_iseq (isq_new=isq_mn)
           error_prd = mfile%prd1 + (mfile%it1 + k - 2)
           Call reset_iseq ()
        End If
        Select Case (yfile%idstr)
         Case (1)
           Call cpt_error ('run_analysis', ifail, .false., &
                c_arg1=Trim(yfile%cfile%ffile), c_arg2=get_cdate(error_prd,lsn(1),2))
         Case Default
           Call cpt_error ('run_analysis', ifail, .false., &
                c_arg1=Trim(yfile%cfile%ffile), c_arg2=get_cdate(error_prd,lsn(1),2), c_arg3=cstndy(j,l))
        End Select
        GoTo 1
     End If
  End If
!
! Calculate seasonal values
  If (icsv > 0) Then
     Print *, 'Calculating seasonal values ...'
     Call seasonal ()
  Else
     nyr = nt
     nssr = 0
  End If
!
! Replace missing values and check for missing variables
  Call start_progress_meter ('Checking for missing values', .false.)
  If (lcex) Then
     ifail = replace_missing_xy(ianal, nsn, nssr, lcex, lprst, lsn(1), climate_per%d1, &
           clim=clim)
  Else
     ifail = replace_missing_xy(ianal, nsn, nssr, lcex, lprst, lsn(1), climate_per%d1)
  End If
  If ((ifail /= 0) .or. (lstop)) GoTo 1
! - double check for invalid length of training period given missing values -
  If (nu < nt) Then
     ifail = check_n(nu, lcw, .false., ianal, iretro, ierr)
     Select Case (ifail)
      Case (0)
        CONTINUE
      Case (2)
        If (ianal /= ia_pfv) Then
           CONTINUE
        Else If (ifail /= 2) Then
           ifail = 5
           Call cpt_error ('check_n', ifail, .false., &
                i_arg1=ierr)
           GoTo 1
        End If
      Case Default
        ifail = 5
        Call cpt_error ('check_n', ifail, .false., &
             i_arg1=ierr)
        GoTo 1
     End Select
     If (icsv == 0) nyr = nu
  End If
  Select Case (iseq)
   Case Default
     ifail = set_nused(.true., kuse)
   Case (isq_sn, isq_so)
     ifail = set_nused(.false., kuse)
  End Select
  If (ifail /= 0) Then
     Call window_update (nt1)
     Call cpt_error ('set_nused', ifail, .false., &
          i_arg1=mnt-1)
     GoTo 1
  End If
  Call end_progress_meter (.false., .false.)
!
! Initialise progress meter
  Call start_progress_meter ('Calculating climatologies and thresholds', .false.)
  nact = 1              ! - initial checks -
  nact = nact + mya + 1 ! - calc_climate -
  nact = nact + 1       ! - standardize -
  nact = nact + mya     ! - set_percentiles -
  Select Case (igauss)  ! - set_thresholds -
   Case (0)
     nact = nact + mya
   Case Default
     nact = nact + mya + 1
  End Select
  Call set_progress_increment (one/Real(nact+1, Kind=rp), .false.)
!
! Set climatology
  If (.not.lprst) Then
     Call init_climate (1, nsn, ifail)
  Else
     Call init_climate (0, nsn, ifail)
  End If
  If (lstop) GoTo 1
  If (ifail /= 0) Then
     Call cpt_error ('init_climate', ifail, .false.)
     GoTo 1
  End If
  If (init_thresholds(nsn) /= 0) Then
     Call cpt_error ('init_thresholds', ifail, .false.)
     GoTo 1
  End If
  Call set_climate (1, lsn(0), nsn)
  If (lstop) GoTo 1
  Print *, Trim(l_climper%c)//': '//Trim(climate_per%clim1)//' to '//Trim(climate_per%clim2)
  If (ifail /= 0) GoTo 1
!
! Check reference years
  If (ithr == 3) ifail = check_refdates()
  If (update_progress_meter(.false.) /= 0) GoTo 1
!
! Calculate climatologies
! - add persistence and predicted components of season -
  If (lprst) Then
     Allocate (ypc(mya,nu,lb:nsn), Stat=ifail)
     If (ifail /= 0) Then
        ifail = 4
        Call cpt_error ('run_analysis', ifail, .false.)
        GoTo 1
     End If
     ypc(:,:,:) = y(1:mya,1:nu,:)
     y(1:mya,1:nu,0) = y(1:mya,1:nu,0) + y(1:mya,1:nu,1)
  End If
! - calculate climatologies -
  Do isn = lb, nsn
     Call calc_climate (.false., isn, mya, y(1:mya,1:nyr,isn), climate_per, ave(:,isn), sdev(:,isn), ncu(isn), &
          clim(1:mya,1:ncu(isn),isn), ifail)
     If (lstop) GoTo 1
     If (ifail /= 0) GoTo 1
  End Do
  If (lprst .and. lcex) clim(1:mya,1:climate_per%nc,0) = clim(1:mya,1:climate_per%nc,0) + clim(1:mya,1:climate_per%nc,1)
  If (igauss == 2) Call set_gamma (.true.)
!
! Standardize
  Do isn = 1, nsn ! - but leave combined season unstandardized -
     Call stdize (istd, igauss, isn, mya, nu, ave(:,isn), sdev(:,isn), y(1:mya,1:nyr,isn))
  End Do
  Do isn = lb, nsn
     Call stdize (istd, igauss, isn, mya, climate_per%nc, ave(:,isn), sdev(:,isn), clim(1:mya,1:ncu(isn),isn))
  End Do
  If (update_progress_meter(.false.) /= 0) GoTo 1
!
! Calculate thresholds
  Select Case (ithr)
   Case (1, 2)
     If (set_percentiles(nsn, ncu(:), clim(:,:,lb)) /= 0) GoTo 1 ! - clim is not used for PFV -
     If (lstop) GoTo 1
     Call set_thresholds(igauss, icalc, mya, nsn, ncu(:), clim(:,:,lb:), tobs, tobst, ifail)
     If (lstop) GoTo 1
   Case (3)
     Call set_thresholds(igauss, icalc, mya, nsn, ncu(:), clim(:,:,lb:), tobs, tobst, ifail)
     If (lstop) GoTo 1
     If (set_percentiles(nsn, ncu(:), clim(:,:,lb)) /= 0) GoTo 1
     If (lstop) GoTo 1
  End Select
  If (update_progress_meter(.false.) /= 0) GoTo 1
! - temporarily remove persistence component -
  If (lprst) Then
     y(1:mya,1:nu,:) = ypc(:,:,:)
     If (igauss /= 0) Call stdize (istd, igauss, 1, mya, nu, ave(:,1), sdev(:,1), y(1:mya,1:nu,1)) ! - restandardise -
  End If
  Select Case (iseq)
   Case (isq_sn, isq_so)
!     nu = nmt
  End Select
!
! Check validity of EOF and CCA settings
  Call start_progress_meter ('Beginning analysis',.false.)
  If (.not.lcde) Then
     ifail = check_pcs(ianal)
     If (ifail == -1) GoTo 1
  Else ! - calculate mode numbers -
     Select Case (ianal)
      Case (ia_cca) ! - CCA -
        ifail = get_eofx_settings()
        ifail = get_eofy_settings()
        ifail = get_cca_settings()
      Case (ia_pcr) ! - PCR -
        ifail = get_eofx_settings()
     End Select
     ifail = 0
  End If
!
! Perform analysis
  istdo=istd
  lzero=(igauss == 0) .and. (izero == 1)
  Select Case (ianal)
   Case (ia_cca) ! - CCA -
     ifail = perform_cca()
   Case (ia_pcr) ! - PCR -
     ifail = perform_pcr()
   Case (ia_mlr) ! - MLR -
     ifail = perform_pcr()
   Case (ia_gcm) ! - GCM -
     ifail = perform_gcm()
   Case (ia_pfv) ! - PFV -
     ifail = perform_pfv(ierr)
  End Select
  If (lstop) GoTo 1
  Select Case (ifail)
   Case (0)
     CONTINUE
   Case (-1)
     GoTo 1
   Case (11:)
     ifail = ifail-10
     Call cpt_error ('match_fields', ifail, .false., &
          c_arg1='PFV', c_arg2=yfield(ierr)%var, c_arg3=yfield(ierr)%cssn, i_arg1=ierr)
     GoTo 1
   Case Default
     Call cpt_error ('perform_'//ca_atypes_a(ianal), ifail, .false., &
          c_arg1=ca_atypes_a(ianal))
     GoTo 1
  End Select
!
! Add persistence component to cross-validated hindcasts
  If (lprst) Then
     If (igauss == 2) Call set_gamma (.false.)
     Select Case (igauss)
      Case (0)
        yhat(:,:) = yhat(:,:) + y(1:mya,1:nu,0)
        If (istd /= 0) Call stdize (istd, igauss, 0, mya, nu, ave(:,0), sdev(:,0), yhat(:,:))
        If (iretro == 1) Then
           yret(:,:) = yret(:,:) + y(1:mya,1:nur,0)
           If (istd /= 0) Call stdize (istd, igauss, 0, mya, nur, ave(:,0), sdev(:,0), yret(:,:))
        End If
        y(1:mya,1:nu,0) = y(1:mya,1:nu,0) + y(1:mya,1:nu,1) ! - replace persistence component -
        If (istd /= 0) Then
           Do isn = lb, nsn
              Call stdize (istd, igauss, isn, mya, nu, ave(:,isn), sdev(:,isn), y(1:mya,1:nu,isn))
           End Do
        End If
      Case (1, 2)
        y(1:mya,1:nu,:) = ypc(:,:,:)
        If (istd /= 0) Call ustdize (istd, igauss, 1, mya, nu, ave(:,1), sdev(:,1), yhat(:,:))
        yhat(:,:) = yhat(:,:) + y(1:mya,1:nu,0)
        If (istd /= 0) Call stdize (istd, igauss, 0, mya, nu, ave(:,0), sdev(:,0), yhat(:,:))
        y(1:mya,1:nu,0) = y(1:mya,1:nu,0) + y(1:mya,1:nu,1) ! - replace persistence component -
        If (istd /= 0) Then
           Do isn = lb, nsn
              Call stdize (istd, igauss, isn, mya, nu, ave(:,isn), sdev(:,isn), y(1:mya,1:nu,isn))
           End Do
        End If
     End Select
! - add predicted component to persistence season -
     yfield(0)%tprd%edate = yfield(1)%tprd%edate
     yfield(0)%cssn = get_cssn(yfield(0)%tprd,yfile%iseq,1)
  End If
!
! Allocate additional memory
  Call init_results (ianal, Max(MaxVal(xfield(:)%nlt), MaxVal(yfield(:)%nlt)), Max(MaxVal(xfield(:)%nlg), MaxVal(yfield(:)%nlg)), &
       ifail)
  If (ifail /= 0) Then
     ifail = 1
     Call cpt_error ('perform_'//ca_atypes_a(ianal), ifail, .false., &
          c_arg1=ca_atypes_a(ianal))
     GoTo 1
  End If
!
! Determine forecast categories
  Print *, 'Identifying categories ...'
  If (ianal /= ia_pfv) Then 
     If (iretro == 1) Then
        Call set_progress_increment ((one - prog)*Real(nu, Kind=rp)/Real(mya*(nu + nur), Kind=rp),.false.)
        is0=0
     Else
        Call set_progress_increment ((one - prog)/Real(mya, Kind=rp),.false.)
     End If
     If (set_cv_categories(lb) /= 0) GoTo 1
  Else
     If (nsn /= nmn) Then
        is0 = 0
     Else
        is0 = xfile%fdate%imn - 1
     End If
  End If
  If (iretro == 1) Then
     Call set_progress_increment ((one - prog)/Real(mya, Kind=rp),.false.)
     If (set_ra_categories(ianal, lb, nsn, is0, ncu(lb:), clim(:,:,lb:)) /= 0) GoTo 1
  End If
! - compress seasonal data -
  If (ianal == ia_pfv .and. nsn > 1) Then ! - seasonal data were not previously compressed -
     Call compress (nt,kuse(:), nur, &
          ia=irobs(1:mya,:), aa=rfps(1:mya,:,:))
! - restructure Y if data are seasonally sequenced -
     Select Case (iseq)
      Case (isq_sn, isq_so)
        Allocate (wk3(mya,nyr,nsn), Stat=ifail)
        If (ifail /= 0) Then
           ifail = 5
           Call cpt_error ('run_analysis', ifail, .false.)
           GoTo 1
        End If
        wk3(:,:,:) = y(1:mya,1:nyr,:)
        DEAllocate (y)
        Allocate (y(mya,nsn*nyr,1), Stat=ifail)
        kk = 0
        Do k = 1, nyr
           Do isn = 1, nsn
              kk = kk + 1
              y(:,kk,1) = wk3(:,k,isn)
           End Do
        End Do
        DEAllocate (wk3)
     End Select
  End If
!
! Permit forecasts if Z data file is set
  If (zfile%cfile%lset) Then
     ifcast = 1
  Else
     ifcast = 0
  End If
!
! Permit scree plots if number of modes is more than one
  ifail = set_scree()
!
#if GUI == 1
! Initialise variable limits
  Call set_ilims (xfield(:)%nv, xfield(:)%nva, iffx, ilimx0, ilimx1, ilimx2)
  Call set_ilims (yfield(1:)%nv, yfield(1:)%nva, iffy, ilimy0, ilimy1, ilimy2)
!
#endif
! End analysis
  irv = iretro
  Call window_update (irv)
! - set length of verification period -
  Select Case (iseq)
   Case (isq_sn, isq_so)
     ntr = nt
  End Select
! - switch off calculation flags -
  Call analysis_flags ('end')
  Call window_update (icalc)
  ifvo = iretro
! - reord change -
  ifail = record_results()
! - end progress meter -
  Call end_progress_meter (.true.,.true.)
  ifail = 0
  Return
!
! Terminate analysis
1 If (lstop) Then
     mxa = 0
     mya = 0
     Call window_update (mxa)
     Call window_update (mya)
  End If
  Call analysis_flags ('off')
  ifail = close_analysis() ! - clear memory -
#if GUI == 1
  Call clear_window$ (ih_con)
#endif
  Write (Unit=*, Fmt=*)
  Call stop_progress_meter (.true.)
!
  Return
 End Function run_analysis
!
!
!
 Function cpt_reset()
!
! Resets CPT ready for calculation
!
! Function type
  Integer :: cpt_reset
!
! Executable Statements
!
! Reset
  cpt_reset = reset('Resetting', .true.)
!
  Return
 End Function cpt_reset
!
!
!
 Function reset(msg,lflags)
!
! Clears memory for recalculation
!
! Modules
  Use data_cpt_constants, Only: ia_pfv
#if GUI == 1
  Use clrwin$,            Only: clear_window$
#endif
  Use data_numbers,       Only: zero
  Use gui,                Only: ifvo, &
                                prompt_yesno, window_update
#if GUI == 1
  Use gui,                Only: ih_con, &
                                close_windows
#endif
  Use progress_meter,     Only: set_progress_meter
  Use settings,           Only: isave, mxa, mya
  Use analysis,           Only: ianal, icalc, &
                                analysis_flags
!
! Function type
  Integer :: reset
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: msg ! - message -
!
  Logical, Intent(In) :: lflags ! - reset analysis flags? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Len_Trim
!
! Executable Statements
!
! Check for reset
  If (icalc == 1) Then
     reset = 1
     If (Len_Trim(msg) > 0) Then
        If (prompt_yesno(.true., .false., msg//' will clear the current results.', &
                         msg2='Do you wish to continue?') /= 1) Return
     End If
     isave = isave - 2
!
! Close existing windows and clear memory
#if GUI == 1
     reset = close_windows()
     Call clear_window$ (ih_con)
#endif
     reset = reset_mem()
     Select Case (ianal)
      Case Default
        ifvo = 0
      Case (ia_pfv)
        ifvo = 1
     End Select
!
! Reset numbers of used variables
     mxa = 0
     mya = 0
     Call window_update (mxa)
     Call window_update (mya)
!
! Reset progress meter
     Call set_progress_meter (zero)
     Print *, ' '
     If (lflags) Call analysis_flags ('off')
  End If
  reset = 2
!
  Return
 End Function reset
!
!
!
#if GUI == 1
 Function check_reset() &
          Result (check)
!
! Prompts for reset
!
! Modules
  Use gui,      Only: ic, ih_nt, ih_xfd, ih_yfd, &
                      window_update
  Use settings, Only: nt, nt_old, xfd_old, yfd_old, &
                      record_change
  Use iofiles,  Only: xfile, yfile
  Use analysis, Only: icalc
!
! Function type
  Integer :: check
!
! Executable Statements
!
! Check for reset
  check = 1
  If (icalc == 1) Then
     If (ic == ih_nt) Then ! - length of training period -
        check = reset('Modifying length of training period', .true.)
     Else If (ic == ih_xfd) Then ! - first X year of interest -
        check = reset('Modifying first X year of interest', .true.)
     Else If (ic == ih_yfd) Then ! - first Y year of interest -
        check = reset('Modifying first Y year of interest', .true.)
     End If
     If (check == 1) Then
!        If (ic == ih_nt) Then ! - length of training period -
           nt = nt_old
           Call window_update (nt)
!        Else If (ic == ih_xfd) Then ! - first X year of interest -
           xfile%fdate%iyr = xfd_old
           Call window_update (xfile%fdate%iyr)
!        Else If (ic == ih_yfd) Then ! - first Y year of interest -
           yfile%fdate%iyr = yfd_old
           Call window_update (yfile%fdate%iyr)
!        End If
        check = 2
        Return
     End If
  End If
!
! Update backups
!  If (ic == ih_nt) Then ! - length of training period -
     nt_old = nt
!  Else If (ic == ih_xfd) Then ! - first X year of interest -
     xfd_old = xfile%fdate%iyr
!  Else If (ic == ih_yfd) Then ! - first Y year of interest -
     yfd_old = yfile%fdate%iyr
!  End If
  check = record_change()
!
  Return
 End Function check_reset
#endif
!
!
!
 Function reset_mem()
!
! Clears memory
!
! Modules
  Use analysis,  Only: close_analysis
  Use bootstrap, Only: close_boot, close_pboot
  Use forecast,  Only: close_fcast
!
! Function type
  Integer :: reset_mem
!
! Executable Statements
!
! Clear memory
  reset_mem = close_pboot()
  reset_mem = close_boot()
  reset_mem = close_fcast()
  reset_mem = close_analysis()
!
  reset_mem = 2
!
  Return
 End Function reset_mem
End Module cpt_actions
