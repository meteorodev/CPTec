! Author: Simon Mason
Module analysis
!
! Declarations

! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: analysis_flags, close_analysis, init_analysis1, init_analysis2, init_analysis_pfv, &
            init_results, num_calcs, proj_read_analysis, proj_write_analysis, reset_analysis
#if GUI == 0
  Public :: get_nt
#endif
!
! Scalars
!
! Integer scalars
  Integer, Public :: ianal = 0 ! - analysis identifier (1=CCA, 2=PCR, 3=MLR, 4=GCM, 5=PFV) -
  Integer, Public :: ianaln    ! - intended new analysis identifier (1=CCA, 2=PCR, 3=MLR, 4=GCM, 5=PFV) -
  Integer, Public :: iaction   ! - ready for calculation flag -
  Integer, Public :: icalc     ! - calculation completed flag -
  Integer, Public :: jcalc     ! - calculation incomplete flag -
  Integer, Public :: nopt      ! - total number of optimization steps -
!
! Logical scalars
  Logical, Public :: lconvert ! - convert units? -
!
! Arrays
!
! Logical arrays
  Logical, Dimension(:,:), Allocatable, Public :: lopt ! - optimized? -
!
Contains
!
!
 Subroutine reset_analysis ()
!
! Resets analysis flags
!
! Executable Statements
!
! Initialise analysis settings
  Call analysis_flags ('off')
  iaction = 0
  lconvert = .false.
!
  Return
 End Subroutine reset_analysis
!
!
!
 Subroutine analysis_flags (onoff)
!
! Switches calculation flags on or off
!
! Modules
  Use data_numbers,       Only: zero, one
  Use data_cpt_constants, Only: ia_pfv
  Use progress_meter,     Only: lcalc, lstop, &
                                close_progress_meter, set_progress_meter
  Use settings,           Only: iretro
!
! Arguments
!
! Input scalars
  Character(Len=*), Intent(In) :: onoff
!
! Functions and Subroutines
!
! Intrinsic routines
  Intrinsic Random_Seed
!
! Executable Statements
!
! Set flags on
  Select Case (onoff)
   Case ('on', 'ON', 'On')
     icalc = 0                       ! - calculation completed flag -
     lcalc = .true.                  ! - calculation active flag -
     lstop = .false.                 ! - stop calculation flag -
     Call set_progress_meter (zero)  ! - progress meter -
!
! - initialise random number generator -
     Call Random_Seed ()
!
! Set flags to indicate end of calculation
   Case ('end', 'END', 'End')
     icalc = 1                       ! - calculation completed flag -
     jcalc = 0                       ! - calculation incomplete flag -
     lcalc = .false.                 ! - calculation active flag -
     Call set_progress_meter (one)   ! - progress meter -
!
! Set all flags off
   Case ('off', 'OFF', 'Off')
     icalc = 0                       ! - calculation completed flag -
     jcalc = 1                       ! - calculation incomplete flag -
     lcalc = .false.                 ! - calculation active flag -
     lstop = .false.                 ! - stop calculation flag -
     If (ianal /= ia_pfv) iretro = 0 ! - retroactive calculation flag -
     nopt = 0                        ! - number of optimizations -
     Call close_progress_meter ()
  End Select
!
  Return
 End Subroutine analysis_flags
!
!
!
#if GUI == 0
 Function get_nt()
!
! Prompts for length of training period
!
! Modules
  Use data_time_constants, Only: isq_so
  Use data_numbers,        Only: ihuge
  Use maths,               Only: magnitude
  Use time,                Only: iseq
  Use labels,              Only: cg_ltp
  Use gui,                 Only: prompt_integer
  Use settings,            Only: mnu, nmt, nt
  Use iofiles,             Only: xfile, yfile, &
                                 get_maxnt
  Use season,              Only: icsv, lensn, lprst, lsn, nsn
!
! Function type
  Integer :: get_nt
!
! Locals
!
! Local scalars
  Integer :: mx ! - maximum length of training period -
  Integer :: mn ! - minimum length of training period -
!
  Character(Len=13) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Trim
!
! Executable Statements
!
! Determine maximum length of training period
  mx = xfile%nt
  If (xfile%nt > 0) Then
     If (yfile%nt > 0) Then
        mn = mnu
        If (.not.lprst) Then
           Call get_maxnt (icsv, lensn, mx, nmt)
        Else If (iseq /= isq_so) Then
           Call get_maxnt (icsv, lensn-lsn(0), mx, nmt)
        Else
           Call get_maxnt (icsv, lsn(nsn), mx, nmt)
        End If
     Else
        mn = 0
        mx = xfile%nt
     End If
  Else
     mn = 0
     If (yfile%nt > 0) Then
        mx = yfile%nt
     Else
        mx = 0
     End If
  End If
  If (mx > 0) Then
     Write (Unit=cfmt, Fmt='(A,2(I1,A))') '(A,I', magnitude(mn), ',A,I', magnitude(mx), ',A)'
  Else
     Write (Unit=cfmt, Fmt='(A,I1,A)') '(A,I', magnitude(mn), ',A)'
  End If
!
! Get training data settings
  Print *, ' '
  If (mx > 0) Then
     Write (Unit=*, Fmt=cfmt,ADVANCE='no') Trim(cg_ltp)//' (minimum is ', mn, '; maximum is ', mx, '): '
  Else
     Write (Unit=*, Fmt=cfmt,ADVANCE='no') Trim(cg_ltp)//' (minimum is ', mn, '): '
     mx = ihuge
  End If
  Call prompt_integer(' ', nt, &
       ilow=mn, ihgh=mx)
  get_nt = 0
!
  Return
 End Function get_nt
!
!
!
#endif
 Subroutine init_analysis1 (ianalc, lproj, ng, nret, nopt, ifail, &
            nlt, nlg, ndom)
!
! Initialises memory and settings to perform EOF prefiltering and model fitting
!
! Modules
  Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm
  Use arrays,             Only: b, b0, rwk, iwk, liwk, lrwk, xc, xiny, xm, xsd, xvp, yc, yhat, yhatt, ym, ysd, yopt, yret, yrett, &
                                yrpls
  Use settings,           Only: iensc, igauss, ig0, imc, iretro, hcw, lcw, lxt, lxyt, mxa, mya, nu, nur
  Use space,              Only: init_space
  Use fields,             Only: first_field
  Use scores,             Only: gm
  Use pcs,                Only: mcc, mxe, mye, ncc, nxe, nye, &
                                init_svds, init_svdwk
  Use categories,         Only: efps,rfps, rodds
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ianalc ! - analysis identifier -
  Integer, Intent(In) :: ng     ! - number of categories -
  Integer, Intent(In) :: nret   ! - number of retroactive iterations -
!
  Logical, Intent(In) :: lproj ! - restore from project? -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: nlt  ! - number of latitudes -
  Integer, Intent(In), Optional :: nlg  ! - number of longitudes -
  Integer, Intent(In), Optional :: ndom ! - number of domains -
!
! Output scalars
  Integer, Intent(Out) :: nopt  ! - number of optimization steps -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: lnn  ! - minimum of ndom, and nu -
  Integer :: mxt  ! - maximum of mxa, and nu -
  Integer :: mxyt ! - maximum of mxa, mya, and nu -
  Integer :: mopt ! - number of optimization steps -
!
! Functions and Subroutines
!
! Intrinsic routines
  Intrinsic Allocated, Max, Min
!
! Executable Statements
!
! Free existing memory
  If (Allocated(rwk)) Deallocate (rwk)
!
! Allocate additional memory
! - means -
  Select Case (ianalc)
   Case (ia_cca, ia_pcr, ia_mlr)
     Allocate (xm(mxa), Stat=ifail)
   Case (ia_gcm)
     Allocate (xm(mya), Stat=ifail)
  End Select
  If (ifail /= 0) GoTo 1
  Allocate (ym(mya), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - standard deviations -
  Select Case (ianalc)
   Case (ia_cca, ia_pcr, ia_mlr)
     Allocate (xsd(mxa), Stat=ifail)
   Case (ia_gcm)
     Allocate (xsd(mya), Stat=ifail)
  End Select
  If (ifail /= 0) GoTo 1
  Allocate (ysd(mya), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - cross-validated predictions -
  Allocate (yhat(mya,nu), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - cross-validated transformed predictions -
  If (igauss > 0) Then
     Allocate (yhatt(mya,nu), Stat=ifail)
     If (ifail /= 0) GoTo 1
  End If
! - optimization results -
  If (nret > 0) Then
     Select Case (ianalc)
      Case (ia_cca)
        nopt = get_nopt(nxe, mxe, &
               nye=nye, mye=mye, ncc=ncc, mcc=mcc)
      Case (ia_pcr)
        nopt = get_nopt(nxe, mxe)
      Case (ia_mlr)
        nopt = 1
      Case Default
        nopt = 0
     End Select
     If (nopt > 0) Then
        Allocate (lopt(nopt,nret*iretro+1), Stat=ifail)
        If (ifail /= 0) GoTo 1
        Allocate (gm(nopt,nret*iretro+1), Stat=ifail)
        If (ifail /= 0) GoTo 1
     End If
  Else
     nopt = 0
  End If
! - ensemble forecast probabilities -
  If (ianalc == ia_gcm) Then
     If (iensc > 1) Then
        Allocate (efps(mya,nu,ng), Stat=ifail)
        If (ifail /= 0) GoTo 1
     End If
  End If
! - retroactive predictions -
  If (iretro == 1) Then
     Allocate (yret(mya,nur), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - transformed retroactive predictions -
     If (igauss > 0) Then
        Allocate (yrett(mya,nur), Stat=ifail)
        If (ifail /= 0) GoTo 1
     End If
! - retroactive prediction limits -
     Allocate (yrpls(mya,nur), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - retroactive forecast probabilities -
     Allocate (rfps(mya,nur,ng), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - retroactive odds -
     Allocate (rodds(mya,nur,ng,2), Stat=ifail)
     If (ifail /= 0) GoTo 1
  End If
! - SVDs -
  Select Case (ianalc)
   Case (ia_cca)
     ifail = init_svds(ianalc)
     If (ifail /= 0) GoTo 1
   Case (ia_pcr, ia_mlr)
     ifail = init_svds(ianalc)
     If (ifail /= 0) GoTo 1
! - regression constants -
     Allocate (b0(mya,1), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - regression coefficients -
     Allocate (b(mxa,mya), Stat=ifail)
     If (ifail /= 0) GoTo 1
   Case (ia_gcm)
     If (ndom > 1) Then
        ig0 = 0
     Else
        ig0 = 1
     End If
! - interpolated X values -
     Allocate (xiny(mya,nu,ig0:ndom), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - regression constants -
     Allocate (b0(ig0:ndom,mya), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - regression coefficients -
     Allocate (b(ig0:ndom,mya), Stat=ifail)
     If (ifail /= 0) GoTo 1
! - nearest latitudes and longitudes -
     ifail = init_space(nlt, nlg, ndom)
     If (ifail /= 0) GoTo 1
  End Select
! - copy of training period independent data -
  Select Case (ianalc)
   Case (ia_cca, ia_pcr, ia_mlr)
     Allocate (xc(mxa,nu,1), Stat=ifail)
   Case (ia_gcm)
     Allocate (xc(mya,nu,ig0:ndom), Stat=ifail)
  End Select
  If (ifail /= 0) GoTo 1
! - copy of training period dependent data -
  Allocate (yc(mya,nu), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - prediction error variance for retroactive forecasts -
  If (iretro == 1) Then
     Select Case (ianalc)
      Case (ia_cca, ia_pcr, ia_mlr)
        Allocate (xvp(1,nur), Stat=ifail)
      Case (ia_gcm)
        Allocate (xvp(mya,nur), Stat=ifail)
     End Select
     If (ifail /= 0) GoTo 1
  End If
! - SVD workspace -
  If (ianalc == ia_cca) ifail = init_svdwk()
  If (ifail /= 0) GoTo 1
!
! Calculate required workspace
  Select Case (ianalc)
   Case (ia_cca)
     mxyt = Max(mxa, mya, 2*nu - 3*lxyt)
     lrwk = lxyt*(6 + 4*lxyt) + mxyt
     liwk = 8*lxyt
   Case (ia_pcr, ia_mlr)
     mxt = Max(mxa, 2*nu - 3*lxt)
     lrwk = lxt*(6 + 4*lxt) + Max(mxt, mya)
     liwk = 8*lxt
   Case (ia_gcm)
     lxt = Min(mxa, nu)
     mxt = Max(mya, 2*nu - 3*lxt)
     lrwk = lxt*(6 + 4*lxt) + Max(mxt, mya)
     liwk = 8*lxt
     If (imc == 4) Then ! - add additional space for model combination -
        lnn = Min(ndom, nu)
        lrwk = Max(lrwk, 3*lnn + Max(Max(ndom, nu), 4*lnn*(lnn + 1)))
        liwk = Max(lrwk, 8*lnn)
     End If
  End Select
!
! Allocate workspace
  Allocate (iwk(liwk), Stat=ifail)
  If (ifail /= 0) GoTo 1
  Allocate (rwk(lrwk), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - hindcasts for optimization -
  If (lproj) Then
     mopt = 1
  Else
     mopt = Max (1, nopt)
  End If
  Select Case (igauss)
   Case (0)
     Allocate (yopt(mya,nu,mopt), Stat=ifail)
   Case Default
     Allocate (yopt(mya,nu,Max(2, mopt)), Stat=ifail)
  End Select
  If (ifail /= 0) GoTo 1
!
! Calculate half cross-validation window
  hcw = (lcw + 1)/2
!
! Identify initial grid point
  Call first_field ()
!
  Return
!
! Errors
1 ifail = 1
!
  Return
 End Subroutine init_analysis1
!
!
!
 Subroutine init_analysis2 (ifail, &
            nco)
!
! Initialises additional memory for analysis
!
! Modules
  Use arrays,   Only: yopt, yt
  Use settings, Only: igauss, mya, nu
  Use pcs,      Only: init_ccamaps
!
! Arguments
!
! Input scalars
  Integer, Intent(In), Optional :: nco ! - optimal number of CCA modes -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic routines
  Intrinsic Allocated, Present
!
! Executable Statements
!
! Deallocate unneeded workspace
  ifail = 0
  If (Allocated(yopt)) Deallocate (yopt)
!
! Allocate additional memory
! - transformed response data -
  If (igauss > 0) Then
     Allocate (yt(mya,nu), Stat=ifail)
     If (ifail /= 0) GoTo 1
  End If
!
! Allocate additional memory for CCA maps
  If (Present(nco)) Then
     ifail = init_ccamaps(nco)
     If (ifail /= 0) GoTo 1
  End If
!
  Return
!
! Errors
1 ifail = 1
!
  Return
 End Subroutine init_analysis2
!
!
!
 Subroutine init_results (ianalc, nlt, nlg, ifail)
!
! Initialises memory for results
!
! Modules
  Use data_cpt_constants, Only: ia_pfv
  Use arrays,             Only: awk, rwk, iwk, xc, xvp, yc, ypc
  Use settings,           Only: iretro, mya, nt, nu, nur, nx, ny
  Use pcs,                Only: ce
  Use categories,         Only: init_categories
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ianalc ! - analysis identifier -
  Integer, Intent(In) :: nlt    ! - maximum number of latitudes -
  Integer, Intent(In) :: nlg    ! - maximum number of longitudes -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic routines
  Intrinsic Allocated, Max
!
! Executable Statements
!
! Deallocate unneeded workspace
  If (Allocated(rwk))  Deallocate (rwk)
  If (Allocated(iwk))  Deallocate (iwk)
  If (Allocated(ce))   Deallocate (ce)
  If (Allocated(xvp))  Deallocate (xvp)
  If (Allocated(yc))   Deallocate (yc)
  If (Allocated(xc))   Deallocate (xc)
  If (Allocated(ypc))  Deallocate (ypc)
!
! Allocate additional memory
  Select Case (ianalc)
   Case Default
     ifail = init_categories(ianalc, iretro, mya, nu, nur)
   Case (ia_pfv)
     ifail = init_categories(ianalc, iretro, mya, nu, nt)
  End Select
  If (ifail /= 0) GoTo 1
! - workspace -
  Allocate (awk(Max(nx, ny, nlg, nlt)), Stat=ifail)
  If (ifail /= 0) GoTo 1
  Allocate (rwk(Max(2*nu, nx, ny, nlg, nlt)), Stat=ifail)
  If (ifail /= 0) GoTo 1
!
  Return
!
! Errors
1 ifail = 1
!
  Return
 End Subroutine init_results
!
!
!
 Subroutine init_analysis_pfv (ng, n, nlt, nlg, ndom, ifail)
!
! Initialises memory for PFV
!
! Modules
  Use arrays,     Only: rwk
  Use settings,   Only: mya, nret, nt, ntr, nt1, nur, nx, ny
  Use space,      Only: init_space
  Use fields,     Only: imfs, &
                        first_field
  Use categories, Only: rfps, rodds
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ng   ! - number of categories -
  Integer, Intent(In) :: n    ! - number of cases -
  Integer, Intent(In) :: nlt  ! - number of latitudes -
  Integer, Intent(In) :: nlg  ! - number of longitudes -
  Integer, Intent(In) :: ndom ! - number of domains -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic routines
  Intrinsic Allocated, Max
!
! Executable Statements
!
! Free existing memory
  If (Allocated(rwk)) Deallocate (rwk)
!
! Allocate additional memory
! - matched X fields -
  Allocate (imfs(ndom), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - retroactive forecast probabilities -
  Allocate (rfps(mya*ndom,n,ng), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - retroactive odds -
  Allocate (rodds(mya*ndom,n,ng,2), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - nearest latitudes and longitudes -
  ifail=init_space(nlt, nlg, ndom)
  If (ifail /= 0) GoTo 1
!
! Allocate workspace
  Allocate (rwk(Max(n, nx, ny, nlg, nlt)), Stat=ifail)
  If (ifail /= 0) GoTo 1
!
! Identify initial grid point
  Call first_field ()
!
! Set retroactive period to the same as the training period
  nt1 = 0
  ntr = nt
  nur = n
  nret = 1
!
  Return
!
! Errors
1 ifail = 1
!
  Return
 End Subroutine init_analysis_pfv
!
!
!
 Function get_nopt(nxe, mxe, &
          nye, mye, ncc, mcc) &
          Result (nopt)
!
! Calculates number of optimization steps
!
! Function type
  Integer :: nopt
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nxe ! - minimum number of X EOF modes -
  Integer, Intent(In) :: mxe ! - maximum number of X EOF modes -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: nye ! - minimum number of Y EOF modes -
  Integer, Intent(In), Optional :: mye ! - maximum number of Y EOF modes -
  Integer, Intent(In), Optional :: ncc ! - minimum number of CCA modes -
  Integer, Intent(In), Optional :: mcc ! - maximum number of CCA modes -
!
! Locals
!
! Local scalars
  Integer :: ic ! - CCA mode index -
  Integer :: ix ! - X EOF index -
  Integer :: iy ! - Y EOF index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Min, Present
!
! Executable Statements
!
! Calculate number of retroactive cross-validations
  If (Present(nye) .and. Present(mye) .and. Present(ncc) .and. Present(mcc)) Then
     nopt = 0
     Do ix = nxe, mxe
        Do iy = nye, mye
           Do ic = ncc, Min(mcc, ix, iy)
              nopt = nopt + 1
           End Do
        End Do
     End Do
  Else
     nopt = mxe + 1 - nxe
  End If
!
  Return
 End Function get_nopt
!
!
!
 Function num_calcs(ianal, iretro, igauss, izero, ifit, mya, nt, nret, nretro, nt1, ntr, nopt)
!
! Calculates total number of calculations to perform for progress meter
!
! Modules
  Use data_cpt_constants, Only: ia_cca, ia_pcr, ia_mlr, ia_gcm, ia_pfv
!
! Function type
  Integer :: num_calcs
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: ianal  ! - analysis method -
  Integer, Intent(In) :: iretro ! - retroactive forecast flag -
  Integer, Intent(In) :: igauss ! - transform to gaussian flag -
  Integer, Intent(In) :: izero  ! - zero-bound flag -
  Integer, Intent(In) :: ifit   ! - calculate fitted values flag -
  Integer, Intent(In) :: mya    ! - number of available gridpoints -
  Integer, Intent(In) :: nt     ! - number of cases in training period -
  Integer, Intent(In) :: nret   ! - number of retroactive iterations -
  Integer, Intent(In) :: nretro ! - model update interval -
  Integer, Intent(In) :: nt1    ! - initial number of training cases -
  Integer, Intent(In) :: ntr    ! - number of cases in retroactive period -
  Integer, Intent(In) :: nopt   ! - number of optimization steps -
!
! Locals
!
! Local scalars
  Integer :: ncvr ! - total number of retroactive cross-validations -
  Integer :: itg  ! - transformation flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Max, Nint, Real, Sqrt
!
! Executable Statements
!
! Calculate number of retroactive cross-validations
  ncvr = nret*nt1 + nret*(nret - 1)*nretro/2
!
! Calculate number of calculations to perform
  Select Case (igauss)
   Case (0)
     itg = 0
   Case Default
     itg = 1
  End Select
  num_calcs = 1
  Select Case (ianal)
   Case (ia_cca) ! - CCA -
     If (iretro == 1) Then
        num_calcs = num_calcs + ncvr*(1 + 2*nopt) + nret*(1 + (1 - itg)*izero) ! - cv_analysis -
        num_calcs = num_calcs + 6*nret                                         ! - full_analysis -
        num_calcs = num_calcs + 2*ntr                                          ! - analysis_prediction -
        num_calcs = num_calcs + ifit*2*ncvr                                    ! - fit_analysis -
        num_calcs = num_calcs + nret*(Max(mya, ntr) + itg + izero)             ! - calc_probs -
     End If
     num_calcs = num_calcs + nt*(1 + 2*nopt) + 1 + itg + izero                 ! - cv_analysis -
     num_calcs = num_calcs + 8                                                 ! - full_analysis -
   Case (ia_pcr, ia_mlr) ! - PCR, MLR -
     If (iretro == 1) Then
        num_calcs = num_calcs + ncvr*(1 + nopt) + nret*(1 + (1 - itg)*izero)   ! - cv_analysis -
        num_calcs = num_calcs + 2*nret                                         ! - full_analysis -
        num_calcs = num_calcs + ntr                                            ! - analysis_prediction -
        num_calcs = num_calcs + ifit*ncvr                                      ! - fit_analysis -
        num_calcs = num_calcs + nret*(Max(mya, ntr) + itg + izero)             ! - calc_probs -
     End If
     num_calcs = num_calcs + nt*(1 + nopt) + 1 + itg + izero                   ! - cv_analysis -
     num_calcs = num_calcs + 5                                                 ! - full_analysis -
   Case (ia_gcm) ! - GCM -
     num_calcs = num_calcs + mya                                               ! - get_nearest_grids -
     num_calcs = num_calcs + mya                                               ! - get_interpolated -
     If (iretro == 1) Then
        num_calcs = num_calcs + ncvr + nret*(1 + (1 - itg)*izero)              ! - cv_analysis -
        num_calcs = num_calcs + 2*nret                                         ! - full_analysis -
        num_calcs = num_calcs + ntr                                            ! - analysis_prediction -
        num_calcs = num_calcs + ifit*ncvr                                      ! - fit_analysis -
        num_calcs = num_calcs + nret*(Max(mya, ntr) + itg + izero)             ! - calc_probs -
     End If
     num_calcs = num_calcs + nt + 1 + itg + izero                              ! - cv_analysis -
     num_calcs = num_calcs + 2                                                 ! - full_analysis -
   Case (ia_pfv) ! - PFV -
     num_calcs = num_calcs + mya                                               ! - get_nearest_grids -
     num_calcs = num_calcs + mya                                               ! - get_interpolated -
  End Select
  If (ianal /= ia_pfv) num_calcs = num_calcs + Nint(Sqrt(Real(mya*nt)))        ! - set_cv_categories -
  If (iretro == 1) num_calcs = num_calcs + Nint(Sqrt(Real(mya*ntr)))           ! - set_ra_categories -
!
  Return
 End Function num_calcs
!
!
!
 Subroutine proj_read_analysis (iin, ianalp, pver, lres, ifail)
!
! Reads program analysis from project file
!
! Modules
  Use data_numbers, Only: sp
  Use scores,       Only: gm
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iin ! - input file unit number -
!
  Real(Kind=sp), Intent(In) :: pver ! - project version -
!
  Logical, Intent(In) :: lres ! - read results? -
!
! Output scalars
  Integer, Intent(Out) :: ianalp ! - project analysis -
  Integer, Intent(Out) :: ifail  ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated
!
! Executable Statements
!
! Read analysis settings
  If (.not.lres) Then
     Read (Unit=iin, Err=1, End=2) ianalp, icalc, lconvert
!
! Read optimization settings
  Else
     If (pver >= 15.05_sp .and. nopt > 1) Then
        Read (Unit=iin, Err=1, End=2) lopt
        Read (Unit=iin, Err=1, End=2) gm
     Else
        If (Allocated(lopt)) Deallocate (lopt)
        If (Allocated(gm))   Deallocate (gm)
     End If
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
 End Subroutine proj_read_analysis
!
!
!
 Subroutine proj_write_analysis (iout, lres, ifail)
!
! Writes analysis settings to project file
!
! Modules
  Use scores, Only: gm
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: iout ! - output file unit number -
!
  Logical, Intent(In) :: lres ! - read results? -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Executable Statements
!
! Write analysis settings
  If (.not.lres) Then
     Write (Unit=iout, Err=1) ianal, icalc, lconvert
!
! Write optimization settings
  Else If (nopt > 0) Then
     Write (Unit=iout, Err=1) lopt
     Write (Unit=iout, Err=1) gm
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
 End Subroutine proj_write_analysis
!
!
!
 Function close_analysis()
!
! Frees memory allocated for analysis
!
! Modules
  Use arrays,     Only: awk, b, b0, rwk, iwk, x, xc, xiny, xm, xsd, xvp, y, yc, yhat, yhatt, ym, yopt, ypc, yret, yrett, yrpls, &
                        ysd, yt
  Use scores,     Only: close_scores
  Use space,      Only: close_space
  Use fields,     Only: imfs
  Use scores,     Only: gm  
  Use pcs,        Only: close_svds
  Use missing,    Only: close_missing
  Use categories, Only: efps, rfps, rodds, &
                        close_categories
  Use climate,    Only: close_climate
!
! Function type
  Integer :: close_analysis
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Allocated
!
! Executable Statements
!
! Free memory allocated for scoring
  close_analysis = close_scores()
!
! Free other memory
  If (Allocated(rwk))   Deallocate (rwk)
  If (Allocated(awk))   Deallocate (awk)
  If (Allocated(iwk))   Deallocate (iwk)
  If (Allocated(yt))    Deallocate (yt)
  If (Allocated(yopt))  Deallocate (yopt)
  If (Allocated(xvp))   Deallocate (xvp)
  If (Allocated(yc))    Deallocate (yc)
  If (Allocated(xc))    Deallocate (xc)
  If (Allocated(ypc))   Deallocate (ypc)
  If (Allocated(b))     Deallocate (b)
  If (Allocated(b0))    Deallocate (b0)
  close_analysis = close_space()
  If (Allocated(b))     Deallocate (b)
  If (Allocated(b0))    Deallocate (b0)
  If (Allocated(xiny))  Deallocate (xiny)
  close_analysis = close_space()
  If (Allocated(rodds)) Deallocate (rodds)
  If (Allocated(rfps))  Deallocate (rfps)
  If (Allocated(imfs))  Deallocate (imfs)
  If (Allocated(yrpls)) Deallocate (yrpls)
  If (Allocated(yret))  Deallocate (yret)
  If (Allocated(yrett)) Deallocate (yrett)
  If (Allocated(efps))  Deallocate (efps)
  If (Allocated(gm))    Deallocate (gm)
  If (Allocated(lopt))  Deallocate (lopt)
  If (Allocated(yhatt)) Deallocate (yhatt)
  If (Allocated(yhat))  Deallocate (yhat)
  If (Allocated(ysd))   Deallocate (ysd)
  If (Allocated(xsd))   Deallocate (xsd)
  If (Allocated(ym))    Deallocate (ym)
  If (Allocated(xm))    Deallocate (xm)
!
! Free memory allocated for climatologies
  close_analysis = close_climate()
!
! Free memory allocated for categories
  close_analysis = close_categories()
!
! Free memory allocated for SVDs
  close_analysis = close_svds()
!
! Free memory allocated for missing values
  close_analysis = close_missing()
!
! Free other memory
  If (Allocated(y)) Deallocate (y)
  If (Allocated(x)) Deallocate (x)
!
  close_analysis = 0
!
  Return
 End Function close_analysis
End Module analysis
