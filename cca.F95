! Author: Simon Mason
Module cca
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: cca_predict, perform_cca
!
Contains
!
!
 Function perform_cca()
!
! Performs CCA
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail = -1 Terminated
!
! Modules
  Use data_numbers,       Only: one
  Use data_cpt_constants, Only: ng
  Use data_io_constants,  Only: ffmts, iout, lprd
  Use time,               Only: Operator(+), &
                                get_cdate
  Use labels,             Only: cg_to_l, cg_tperiod
  Use gui,                Only: print_text
  Use progress_meter,     Only: lstop, &
                                set_progress_increment
  Use arrays,             Only: x, xvp, y, yhat, yhatt, yopt, yret, yrett, yrpls, yt
  Use settings,           Only: clf, iev, igauss, iretro, izero, lcw, lzero, mxa, mya, ncv, nret, nretro, nt, nt1, nu, nur, nu1
  Use iofiles,            Only: gifile, yfile, &
                                file_reset
  Use pcs,                Only: eofx, eofy, hx_map, hy_map, hx_ser, hy_ser, iec, ieofx, ieofy, iey, mcc, mxe, mye, mu, ncc, nco, &
                                nccu, npx, npy, nxe, nxo, nye, nyo, r, s, svx, svy, tsx, tsy
  Use missing,            Only: kuse
  Use categories,         Only: pobs, rfps, rodds, tobst, &
                                calc_reg_probs
  Use climate,            Only: ave, sdev
  Use season,             Only: lb, lprst
  Use analysis,           Only: ianal, lconvert, nopt, &
                                init_analysis1, init_analysis2, num_calcs
!
! Function type
  Integer :: perform_cca
!
! Locals
!
! Local scalars
  Integer :: ir    ! - index of current retroactive step -
  Integer :: irl   ! - index of last retroactive training-period date -
  Integer :: ir1   ! - index of first retroactive date -
  Integer :: irn   ! - index of last retroactive date -
  Integer :: iru   ! - index of last new date in retroactive training period -
  Integer :: nr    ! - total number of retroaoctive steps in current cycle -
  Integer :: ifail ! - error indicator -
!
  Character(Len=lprd) :: cprd1 ! - period -
  Character(Len=lprd) :: cprd2 ! - period -
  Character(Len=lprd) :: cprd3 ! - period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Min, Real, Trim
!
! Executable Statements
  perform_cca = -1
!
! Initialise analysis
  Call init_analysis1 (ianal, .false., ng, nret, nopt, ifail)
  If (ifail /= 0) Then
     perform_cca = 1
     Return
  End If
  lconvert = .false.
!
! Estimate number of calculations
  If (iev /= 3) Then
     Call set_progress_increment &
         (one/Real(num_calcs(ianal, iretro, igauss, izero, 0, mya, nu, nret, nretro, nu1, nur, nopt), Kind=rp), .false.)
  Else
     Call set_progress_increment &
         (one/Real(num_calcs(ianal, iretro, igauss, izero, 1, mya, nu, nret, nretro, nu1, nur, nopt), Kind=rp), .false.)
  End If
!
! Calculate optimization settings
  If (nopt == 1) Then
     nopt = 0
     nxo = nxe
     nyo = nye
     nco = ncc
  End If
  iey = 1
  iec = 1
!
! Open goodness index file
  If (gifile%cfile%lset) Then
     Open (Unit=iout, File=gifile%cfile%ffile, Access='sequential', Action='write', Form=ffmts(gifile%cfile%ffmt%ifmt), &
           Iostat=ifail, Status='unknown')
     If (ifail /= 0) Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
  End If
!
! Perform retroactive calculations
  cprd1 = get_cdate(yfile%prd1 + (yfile%it1 - 1), 1, 2)
! - optimise model for successive training periods -
  ir = 0
  If (iretro == 1) Then
     Print *, 'Calculating retroactive forecasts ...'
     ir1 = 1
     irn = 0
     iru = nu1
     retro: Do irl = nt1, nt-1, nretro
        ir = ir + 1
        irn = irn + Count(kuse(irl+1:Min(irl + nretro, nt)))
        nr = irn + 1 - ir1
        If (nr > 0) Then
           ncv = iru - lcw
           cprd2 = get_cdate(yfile%prd1 + (yfile%it1 + irl - 2), 1, 2)
           Print *, Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)
           Print *, ' '
           If (gifile%cfile%lset) Then
              Call print_text (Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2), &
                   iout=iout)
              Call print_text (' ', &
                   iout=iout)
           End If
           If (nopt > 0) Then
              Select Case (igauss)
               Case (0)
                 Call cv_cca (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
                      yhat=yhat, nxo=nxo, nyo=nyo, nco=nco)
               Case Default
                 Call cv_cca (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
                      yhatt=yhat, nxo=nxo, nyo=nyo, nco=nco)
              End Select
           Else
              Select Case (igauss)
               Case (0)
                 Call cv_cca (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
                      yhat=yhat) 
               Case Default
                 Call cv_cca (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
                      yhatt=yhat)
              End Select
           End If
           If (lstop) Return
! - construct full model -
           Select Case (igauss)
            Case (0)
              Call full_cca (iru, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, ieofy, nyo, svy, eofy, tsy, &
                   nco, mu, r, s, ifail)
            Case Default
              Call full_cca (iru, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, ieofy, nyo, svy, eofy, tsy, &
                   nco, mu, r, s, ifail, &
                   yt=yopt(:,:,2))
           End Select
           If (lstop) Return
           If (ifail /= 0) Then
              perform_cca = 2
              Return
           End If
           If (npx < nxo) nxo = npx
           If (npy < nyo) nyo = npy
! - produce retroactive forecast -
           cprd3 = get_cdate(yfile%prd1 + (yfile%it1 + irl - 1), 1, 2)
           If (nr > 1) Then
              cprd2 = get_cdate(yfile%prd1 + (yfile%it1 + irl + nr - 2), 1, 2)
              Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)//' - '//Trim(cprd2)
           Else
              Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)
           End If
           Select Case (igauss)
            Case (0)
              If (.not.lprst) Then
                 Call cca_predict (nr, mxa, mya, nxo, nyo, nco, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), ieofy, yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(1,ir1:irn))
              Else
                 Call cca_predict (nr, mxa, mya, nxo, nyo, nco, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), ieofy, yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(1,ir1:irn), yprst=y(1:mya,nu1+ir1:nu1+irn,0))
              End If
            Case Default
              If (.not.lprst) Then
                 Call cca_predict (nr, mxa, mya, nxo, nyo, nco, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), ieofy, yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(1,ir1:irn), nc=iru, tclim1=y(1:mya,1:iru,1), fcastt=yrett(:,ir1:irn))
              Else
                 Call cca_predict (nr, mxa, mya, nxo, nyo, nco, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), ieofy, yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(1,ir1:irn), nc=iru, tclim1=y(1:mya,1:iru,1), tclim0=y(1:mya,1:iru,0), &
                      fcastt=yrett(:,ir1:irn), yprst=y(1:mya,nu1+ir1:nu1+irn,0))
              End If
           End Select
           If (lstop) Return
           If (iev == 3) Then
              Call cca_predict (iru, mxa, mya, nxo, nyo, nco, ieofx, lzero, x(1:mxa,1:iru,1), ieofy, yhat(:,:))
              If (lstop) Return
           End If
! - calculate probabilities -
           Select Case (igauss)
            Case (0)
              Call calc_reg_probs (ianal, igauss, iru, nr, ng, nxo, mya, lb, ave(:,lb), sdev(:,lb), y(1:mya,:,lb), yhat, &
                   xvp(:,ir1:irn), yret(:,ir1:irn), tobst, pobs, clf, rfps(:,ir1:irn,:), rodds(:,ir1:irn,:,:), yrpls(:,ir1:irn))
            Case Default
! add persistence component to yopt?
              Call calc_reg_probs (ianal, igauss, iru, nr, ng, nxo, mya, lb, ave(:,lb), sdev(:,lb), yopt(:,:,2), yhat,           &
                   xvp(:,ir1:irn), yrett(:,ir1:irn), tobst, pobs, clf, rfps(:,ir1:irn,:),rodds(:,ir1:irn,:,:), yrpls(:,ir1:irn), &
                   nc=iru, clim=y(1:mya,1:iru,lb))
           End Select
           If (lstop) Return
           Print *, ' '
           If (gifile%cfile%lset) Call print_text (' ', &
                                 iout=iout)
        End If
! - update indices -
        iru = nu1 + irn
        ir1 = irn + 1
     End Do retro
  End If
!
! Optimise CCA using full training period
  cprd2 = get_cdate(yfile%prd1 + (yfile%it1 + nt - 2), 1, 2)
  ncv = nu - lcw
  ir = ir + 1
  If (nopt > 0) Then
     Print *, 'Optimizing cross-validated performance ...'
     Print *, Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)
     Print *, ' '
     If (gifile%cfile%lset) Then
        Call print_text (Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2), &
             iout=iout)
        Call print_text (' ', &
             iout=iout)
     End If
     Select Case (igauss)
      Case (0)
        Call cv_cca (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
             yhat=yhat, nxo=nxo, nyo=nyo, nco=nco)
      Case Default
        Call cv_cca (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
             yhat=yhat, yhatt=yhatt, nxo=nxo, nyo=nyo, nco=nco)
     End Select
!
! Cross-validate optimal / chosen model
  Else
     Print *, 'Cross-validating model ...'
     Print *, Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)
     Print *, ' '
     If (gifile%cfile%lset) Then
        Call print_text (Trim(cg_tperiod)//': '//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2), &
             iout=iout)
        Call print_text (' ', &
             iout=iout)
     End If
     Select Case (igauss)
      Case (0)
        Call cv_cca (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, nxo, ieofy, nyo, nyo, nco, nco, ir, &
             yhat=yhat)
      Case Default
        Call cv_cca (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, nxo, ieofy, nyo, nyo, nco, nco, ir, &
             yhat=yhat, yhatt=yhatt)
     End Select
  End If
  If (lstop) Return
!
! Close goodness file
  If (gifile%cfile%lset) Then
     Close (Unit=iout)
     Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
  End If
!
! Allocate additional memory
  nccu = Min(nxo, nyo, mcc)
  Call init_analysis2 (ifail, &
       nco=nccu)
  If (ifail /= 0) Then
     perform_cca = 1
     Return
  End If
!
! Fit model using all data
  Print *, 'Constructing model using full training period ('//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)//') ...'
  Select Case (igauss)
   Case (0)
     Call full_cca (nu, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, &
          ieofy, nyo, svy, eofy, tsy, nccu, mu, r, s, ifail, &
          hx_map=hx_map, hy_map=hy_map, hx_ser=hx_ser, hy_ser=hy_ser)
   Case Default
     Call full_cca (nu, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, &
          ieofy, nyo, svy, eofy, tsy, nccu, mu, r, s, ifail, &
          yt=yt, hx_map=hx_map, hy_map=hy_map, hx_ser=hx_ser, hy_ser=hy_ser)
  End Select
  If (lstop) Return
  If (ifail /= 0) Then
     perform_cca = 2
     Return
  End If
!
  perform_cca = 0
!
  Return
 End Function perform_cca
!
!
!
 Subroutine full_cca (nt, nx, x, ny, y, ieofx, nxe, svx, eofx, tsx, ieofy, nye, svy, eofy, tsy, ncc, mu, r, s, ifail, &
            yt, hx_map, hy_map, hx_ser, hy_ser)
!
! Performs CCA using all data
!
! Modules
  Use data_numbers,   Only: zero, tol
#if DP == 1
  Use cpt_lapack,     Only: gesdd=>dgesdd
#else
  Use cpt_lapack,     Only: gesdd=>sgesdd
#endif
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: iusex, iusey, iwk, lrwk, rwk, xc, xm, xsd, yc, ym, ysd, &
                            moments
  Use settings,       Only: igauss, istd, lxt, lxyt, lyt
  Use distribs,       Only: gaussian, set_gamma,stdize
  Use iofiles,        Only: xfile, yfile
  Use space,          Only: rlatx, rlaty, &
                            latitude_weight
  Use fields,         Only: xfield, yfield
  Use pcs,            Only: ce, lc1, lr1, npx, npy
  Use climate,        Only: ave, climate_per, clim, sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt    ! - number of cases -
  Integer, Intent(In) :: nx    ! - number of x spatial points -
  Integer, Intent(In) :: ny    ! - number of y spatial points -
  Integer, Intent(In) :: nxe   ! - number of X EOF modes -
  Integer, Intent(In) :: nye   ! - number of Y EOF modes -
  Integer, Intent(In) :: ncc   ! - number of CCA modes -
  Integer, Intent(In) :: ieofx ! - X EOF option -
  Integer, Intent(In) :: ieofy ! - Y EOF option -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! Output arrays
  Real(Kind=rp), Dimension(:), Intent(Out) :: mu  ! - canonical correlations -
  Real(Kind=rp), Dimension(:), Intent(Out) :: svx ! - singular values of x -
  Real(Kind=rp), Dimension(:), Intent(Out) :: svy ! - singular values of y -
!
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: eofx ! - x EOF patterns -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: eofy ! - y EOF patterns -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: tsx  ! - time-series of x EOFs (transposed) -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: tsy  ! - time-series of y EOFs (transposed) -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: r    ! - canonical Y EOF weights -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: s    ! - canonical X EOF weights (transposed) -
!
! - optional output arrays -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yt     ! - transformed response variables -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hx_map ! - X homogeneous covariance maps -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hy_map ! - Y homogeneous covariance maps -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hx_ser ! - X homogeneous covariance maps time series -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: hy_ser ! - Y homogeneous covariance maps time series -
!
! Locals
!
! Local scalars
  Integer :: k   ! - case index -
  Integer :: ie  ! - EOF mode index -
  Integer :: mnc ! - maximum number of CCA modes to be counted -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Abs, Count, MatMul, MaxVal, Min, MinVal, Present, Transpose
!
! Executable Statements
!
! Backup data
  ifail = 1
  xc(1:nx,1:nt,1) = x(1:nx,1:nt)
  yc(1:ny,1:nt) = y(1:ny,1:nt)
  If (igauss > 0) Then
     If (istd == 3) Call set_gamma (.false.)
     Call gaussian (igauss, 1, ny, nt, yc(1:ny,1:nt), &
          nr=climate_per%nc, ref=clim(1:ny,1:climate_per%nc,1), ave=ave(:,1), sdev=sdev(:,1))
  End If
!
! Copy transformed data
  If (Present(yt)) yt(:,1:nt) = yc(:,1:nt)
!
! Calculate means and standard deviations
  Call moments (nx, nt, xc(:,:,1), xm, xsd)
  Call moments (ny, nt, yc, ym, ysd)
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
!
! Calculate anomalies according to PCA type
  Call stdize (3-ieofx, 0, 1, nx, nt, xm, xsd, xc(:,1:nt,1))
  Call stdize (3-ieofy, 0, 1, ny, nt, ym, ysd, yc(:,1:nt))
!
! Scale by latitude
  If (xfile%idstr == 1) Call latitude_weight (xfile%nfl, xfield(1:)%nlt, xfield(1:)%region, rlatx(:,:), iusex, nt, xc(:,:,1))
  If (yfile%idstr == 1) Call latitude_weight (yfile%nfl, yfield(1:)%nlt, yfield(1:)%region, rlaty(:,:), iusey, nt, yc(:,:))
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
!
! Perform X EOF prefiltering
  Call gesdd ('S', nx, nt, xc(:,:,1), nx, svx, eofx, nx, tsx, lxt, rwk, lrwk, iwk, ifail)
  If (ifail /= 0) Return
! - determine number of non-zero eigenvalues -
  npx = Count(svx(1:Min(lxt, nt-1)) > zero)
  If (npx > 2) Then
     If (svx(npx)*svx(npx - 2)/svx(npx - 1)**2 < tol) npx = npx - 1 ! - check for probable rounding errors -
  End If
! - rescale loadings by latitude -
  If (xfile%idstr == 1) Call latitude_weight (xfile%nfl, xfield(:)%nlt, xfield(:)%region, rlatx(:,:), iusex, npx, eofx(:,:))
! - ensure that largest absolute loading is positive -
  Do ie = 1, npx
     If (Abs(MaxVal(eofx(1:nx,ie))) < Abs(MinVal(eofx(1:nx,ie)))) Then
        eofx(1:nx,ie) = -eofx(1:nx,ie)
        tsx(ie,1:nt) = -tsx(ie,1:nt)
     End If
  End Do
  If (npx < lxt) Then
     svx(npx+1:lxt) = zero
     tsx(npx+1:lxt,1:nt) = zero
     eofx(1:nx,npx+1:lxt) = zero
  End If
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
!
! Perform Y EOF prefiltering
  Call gesdd ('S', ny, nt, yc, ny, svy, eofy, ny, tsy, lyt, rwk, lrwk, iwk, ifail)
  If (ifail /= 0) Return
! - determine number of non-zero eigenvalues -
  npy = Count(svy(1:Min(lyt, nt-1)) > zero)
  If (npy > 2) Then
     If (svy(npy)*svy(npy - 2)/svy(npy - 1)**2 < tol) npy = npy - 1 ! - check for probable rounding errors -
  End If
! - rescale loadings by latitude -
  If (yfile%idstr == 1) Call latitude_weight (yfile%nfl, yfield(1:)%nlt, yfield(1:)%region, rlaty(:,:), iusey, npy, eofy(:,:))
! - ensure that largest absolute loading is positive -
  Do ie = 1, npy
     If (Abs(MaxVal(eofy(1:ny,ie))) < Abs(MinVal(eofy(1:ny,ie)))) Then
        eofy(1:ny,ie) = -eofy(1:ny,ie)
        tsy(ie,1:nt) = -tsy(ie,1:nt)
     End If
  End Do
  If (npy < lyt) Then
     svy(npy+1:lyt) = zero
     tsy(npy+1:lyt,1:nt) = zero
     eofy(1:ny,npy+1:lyt) = zero
  End If
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
!
! Compute CCA
  ce(1:nye,1:nxe) = MatMul(tsy(1:nye,1:nt), Transpose(tsx(1:nxe,1:nt)))
  Call gesdd ('S', nye, nxe, ce, lc1, mu, r, lr1, s, lxyt, rwk, lrwk, iwk, ifail)
  If (ifail /= 0) Return
  mnc = Min(nxe, nye)
  If (lxyt > mnc) Then
     mu(mnc+1:lxyt) = zero
     r(:,mnc+1:lxyt) = zero
     If (lr1 > nye) r(nye+1:lr1,1:mnc) = zero
     s(mnc+1:lxyt,:) = zero
  End If
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
!
! Compute homogeneous covariance maps
! - X maps -
  If (Present(hx_map)) Then
     Call hcov_maps (nx, nt, nxe, ncc, svx, eofx, tsx, s, hx_map, hx_ser, rwk(1:nxe*ncc))
! - update progress meter -
     If (update_progress_meter(.false.) /= 0) Return
  End If
! - Y maps -
  If (Present(hy_map)) Then
     Call hcov_maps (ny, nt, nye, ncc, svy, eofy, tsy, Transpose(r(1:nye,1:ncc)), hy_map, hy_ser, rwk(1:nye*ncc))
! - update progress meter -
     If (update_progress_meter(.false.) /= 0) Return
  End If
!
! Rescale EOF time series
! - X EOFs -
  Do k = 1, nt
     Do ie = 1, npx
        tsx(ie,k) = tsx(ie,k)*svx(ie)
     End Do
! - Y EOFs -
     Do ie = 1, npy
        tsy(ie,k) = tsy(ie,k)*svy(ie)
     End Do
  End Do
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
  ifail = 0
!
  Return
 End Subroutine full_cca
!
!
!
 Subroutine hcov_maps (nv, nt, ne, nc, sv, eof, ts, rs, hmap, hser, rwk)
!
! Constructs CCA maps
!
! Arguments
!
! Inpur scalars
  Integer, Intent(In) :: nv ! - number of variables -
  Integer, Intent(In) :: nt ! - number of cases -
  Integer, Intent(In) :: ne ! - number of EOF modes -
  Integer, Intent(In) :: nc ! - number of CCA modes -
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: sv ! - singular values -
!
  Real(Kind=rp), Dimension(:,:), Intent(In) :: eof ! - spatial loadings -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: ts  ! - temporal scores -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: rs  ! - canonical EOF weights -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: hmap ! - homogeneous covariance maps -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: hser ! - homogeneous covariance map time series -
!
! Workspace
!
! Workspace arrays
  Real(Kind=rp) :: rwk(:) ! - real workspace -
!
! Locals
!
! Local scalars
  Integer :: i1, i2 ! - indices -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MatMul,Reshape
!
! Executable Statements
!
! Calculate homogeneous covariance maps
  ForAll (i2 = 1:nc)
     ForAll (i1 = 1:ne)
        rwk((i2-1)*ne+i1) = sv(i1)*rs(i2,i1)
     End ForAll
  End ForAll
  hmap(1:nv,1:nc) = MatMul(eof(1:nv,1:ne), Reshape(rwk(1:ne*nc), (/ne,nc/)))
!
! Calculate corresponding time series
  hser(1:nc,1:nt) = MatMul(rs(1:nc,1:ne), ts(1:ne,1:nt))
!
  Return
 End Subroutine hcov_maps
!
!
!
 Subroutine cv_cca (nt, ncv, nx, x, ny, y, ieofx, nxe, mxe, ieofy, nye, mye, ncc, mcc, ir, &
            yhat, yhatt, nxo, nyo, nco)
!
! Performs cross-validated CCA
!
! Modules
  Use data_numbers,      Only: zero, one, tol
  Use data_io_constants, Only: iout
  Use maths,             Only: magnitude
#if DP == 1
  Use cpt_lapack,        Only: gesdd=>dgesdd
#else
  Use cpt_lapack,        Only: gesdd=>sgesdd
#endif
  Use gui,               Only: print_text
  Use progress_meter,    Only: lstop, &
                               update_progress_meter
  Use labels,            Only: l_goodness
  Use arrays,            Only: iusex, iusey, iwk, lrwk, rwk, xc, xm, xsd, yc, ym, yopt, ysd, &
                               get_cvsample,moments
  Use settings,          Only: iel, igauss, igood, istd, hcw, lxt, lxyt, lyt, lzero
  Use distribs,          Only: gaussian, set_gamma, stdize, transform_cv
  Use scores,            Only: gm, &
                               goodness
  Use iofiles,           Only: gifile, xfile, yfile
  Use space,             Only: rlatx, rlaty, &
                               latitude_weight
  Use fields,            Only: xfield, yfield
  Use pcs,               Only: ce, eofx, eofy, icco, lc1, lr1, npx, npy, mu, r, s, svx, svy, tsx, tsy
  Use climate,           Only: ave, sdev
  Use analysis,          Only: lopt
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt    ! - number of cases -
  Integer, Intent(In) :: ir    ! - retroactive step -
  Integer, Intent(In) :: ncv   ! - length of cross-validated training period -
  Integer, Intent(In) :: nx    ! - number of x spatial points -
  Integer, Intent(In) :: ny    ! - number of y spatial points -
  Integer, Intent(In) :: nxe   ! - minimum number of X EOF modes -
  Integer, Intent(In) :: mxe   ! - maximum number of X EOF modes -
  Integer, Intent(In) :: nye   ! - minimum number of Y EOF modes -
  Integer, Intent(In) :: mye   ! - maximum number of Y EOF modes -
  Integer, Intent(In) :: ncc   ! - minimum number of CCA modes -
  Integer, Intent(In) :: mcc   ! - maximum number of CCA modes -
  Integer, Intent(In) :: ieofx ! - X EOF option -
  Integer, Intent(In) :: ieofy ! - Y EOF option -
!
! Output scalars
! - optional output scalars -
  Integer, Intent(Out), Optional :: nxo ! - optimal number of X EOF modes -
  Integer, Intent(Out), Optional :: nyo ! - optimal number of Y EOF modes -
  Integer, Intent(Out), Optional :: nco ! - optimal number of CCA modes -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! Output arrays
! - optional output scalars -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yhat  ! - cross-validated hindcasts -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yhatt ! - transformed cross-validated hindcasts -
!
! Locals
!
! Local scalars
  Integer :: it    ! - cross-validation time-step -
  Integer :: ixe   ! - X EOF mode index -
  Integer :: iye   ! - Y EOF mode index -
  Integer :: icc   ! - current number of CCA modes -
  Integer :: ixu   ! - used X EOF mode index -
  Integer :: iyu   ! - used Y EOF mode index -
  Integer :: icu   ! - current number of used CCA modes -
  Integer :: iopt  ! - optimization step index -
  Integer :: jopt  ! - index of optimized settings -
  Integer :: ifail ! - error indicator -
!
  Character(Len=29) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AdjustR, Any, Count, MatMul, Min, Present, Transpose, Trim
!
! Executable Statements
!
! Set optimization checks
  lopt(:,ir) = .true.
!
! Cross-validate
  time_step: Do it = 1, nt
     Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
          v2=x(:,1:nt), v2cv=xc(:,1:nt,1))
!
! Transform
     If (igauss > 0) Then
        If (istd == 3) Call set_gamma (.false.)
        Call gaussian (igauss, 1, ny, ncv, yc(1:ny,1:ncv), &
             nr=ncv, ave=ave(:,1), sdev=sdev(:,1))
     End If
!
! Calculate means and standard deviations
     Call moments (nx, ncv, xc(:,1:ncv,1), xm, xsd)
     Call moments (ny, ncv, yc(:,1:ncv), ym, ysd)
!
! Calculate anomalies according to PC type
     Call stdize (3-ieofx, 0, 1, nx, ncv, xm, xsd, xc(:,1:ncv,1))
     Call stdize (3-ieofy, 0, 1, ny, ncv, ym, ysd, yc(:,1:ncv))
!
! Scale by latitude
     If (xfile%idstr == 1) Call latitude_weight (xfile%nfl, xfield(1:)%nlt, xfield(1:)%region, rlatx(:,:), iusex, ncv, xc(:,:,1))
     If (yfile%idstr == 1) Call latitude_weight (yfile%nfl, yfield(1:)%nlt, yfield(1:)%region, rlaty(:,:), iusey, ncv, yc(:,:))
!
! Perform EOF prefiltering
! - X variables -
     Call gesdd ('S', nx, ncv, xc(:,:,1), nx, svx, eofx, nx, tsx, lxt, rwk, lrwk, iwk, ifail)
! - Y variables -
     If (ifail == 0) Call gesdd ('S', ny, ncv, yc, ny, svy, eofy, ny, tsy, lyt, rwk, lrwk, iwk, ifail)
     If (update_progress_meter(.false.) /= 0) Return
!
! Determine number of non-zero eigenvalues
     iopt = 0
1    If (ifail == 0) Then
        npx = Count(svx(1:Min(mxe,ncv-1)) > zero)
        npy = Count(svy(1:Min(mye,ncv-1)) > zero)
! - check for near-zero eigenvalues -
        If (npx == 0) Then
           ifail = 1
           GoTo 1
        Else If (npx > 2) Then
           If (svx(npx)*svx(npx - 2)/svx(npx - 1)**2 < tol) npx = npx - 1
        End If
        If (npy == 0) Then
           ifail = 1
           GoTo 1
        Else If (npy > 2) Then
           If (svy(npy)*svy(npy - 2)/svy(npy - 1)**2 < tol) npy = npy - 1
        End If
! - rescale time scores -
        If (npx < mxe) Then
           svx(npx+1:mxe) = zero
           tsx(npx+1:mxe,1:ncv) = zero
           eofx(1:nx,npx+1:mxe) = zero
        End If
        If (npy < mye) Then
           svy(npy+1:mye) = zero
           tsy(npy+1:mye,1:ncv) = zero
           eofy(1:ny,npy+1:mye) = zero
        End If
!
! Rescale by latitude
        If (xfile%idstr == 1) Call latitude_weight (xfile%nfl, xfield(1:)%nlt, xfield(1:)%region, rlatx(:,:), iusex, npx, eofx(:,:))
        If (yfile%idstr == 1) Call latitude_weight (yfile%nfl, yfield(1:)%nlt, yfield(1:)%region, rlaty(:,:), iusey, npy, eofy(:,:))
!
! Compute CCA
        Do ixe = nxe, mxe
           ixu = Min(ixe, npx)
           Do iye = nye, mye
              iyu = Min(iye, npy)
              ce(1:iyu,1:ixu) = MatMul(tsy(1:iyu,1:ncv), Transpose(tsx(1:ixu,1:ncv)))
              Call gesdd ('S', iyu, ixu, ce, lc1, mu, r, lr1, s, lxyt, rwk, lrwk, iwk, ifail)
!
! Check for over-fitting
              If ((icco == 0) .and. (Any(mu(1:Min(ixu, iyu)) > one - tol))) Then
                 Do icc = ncc, Min(mcc, ixe, iye)
                    iopt = iopt + 1
                    lopt(iopt,ir) = .false.
                    yopt(1:ny,it,iopt) = ym(1:ny)
                    If (update_progress_meter(.false.) /= 0) Return
                    If (update_progress_meter(.false.) /= 0) Return
                 End Do
                 Cycle
              Else
!
! Predict anomaly
                 Do icc = ncc,Min(mcc, ixe, iye)
                    icu = Min(icc, ixu, iyu)
                    iopt = iopt + 1
                    If ((icco == 1) .or. (lopt(iopt,ir))) Then
                       Call cca_predict (1, nx, ny, ixu, iyu, icu, ieofx, lzero, x(1:nx,it:it), ieofy, yopt(1:ny,it:it,iopt))
                    Else
                       yopt(1:ny,it,iopt) = ym(1:ny)
                       If (update_progress_meter(.false.) /= 0) Return
                       If (update_progress_meter(.false.) /= 0) Return
                    End If
                 End Do
              End If
           End Do
        End Do
!
! Supply mean if CCA failed
     Else
        Do ixe = nxe, mxe
           Do iye = nye, mye
              Do icc = ncc, Min(mcc, ixe, iye)
                 iopt = iopt + 1
                 yopt(1:ny,it,iopt) = ym(1:ny)
                 If (update_progress_meter(.false.) /= 0) Return
                 If (update_progress_meter(.false.) /= 0) Return
              End Do
           End Do
        End Do
     End If
     If (lstop) Return
  End Do time_step
!
! Standardise
  If (igauss > 0) Then
     yc(1:ny,1:nt) = y(1:ny,1:nt)
     If (istd /= 3) Then
        Call gaussian (igauss, 1, ny, nt, yc(:,:), & ! - NB strictly, this step should be cross-validated (as below) -
             nr=nt, ave=ave(:,1), sdev=sdev(:,1))
     End If
  End If
!
! Calculate and print goodness metric
  If (((Present(nxo)) .or. (Present(nyo)) .or. (Present(nco))) .and. (Any(lopt(:,ir)))) Then
     If (iel == 1) Then
        Print *, '              CURRENT                         OPTIMUM'
        Print *, ' '
        Print *, '     Number of Modes    Goodness      Number of Modes    Goodness'
        Print *, '        X    Y   CCA       Index         X    Y   CCA       Index'
        Print *, ' '
     End If
     If (gifile%cfile%lset) Then
        Call print_text ('               CURRENT                         OPTIMUM', &
             iout=iout)
        Call print_text (' ', &
             iout=iout)
        Call print_text ('      Number of Modes    Goodness      Number of Modes    Goodness', &
             iout=iout)
        Call print_text ('         X    Y   CCA       Index         X    Y   CCA       Index', &
             iout=iout)
        Call print_text (' ', &
             iout=iout)
     End If
     iopt = 0
     jopt = 1
     Do ixe = nxe, mxe
        Do iye = nye, mye
           Do icc = ncc, Min(mcc, ixe, iye)
              iopt = iopt + 1
              If (lopt(iopt,ir)) Then
                 Select Case (igauss)
                  Case (0)
                    gm(iopt,ir) = goodness(igood, nt, ny, yopt(:,:,iopt), y)
                  Case Default
                    gm(iopt,ir) = goodness(igood, nt, ny, yopt(:,:,iopt), yc)
                 End Select
                 If ((iopt == 1) .or. (gm(iopt,ir) > gm(jopt,ir))) Then
                    jopt = iopt
                    nxo = ixe
                    nyo = iye
                    nco = icc
                 End If
                 If (iel == 1) Write (Unit=*, Fmt='(2(5X,2I5,I6,F12.3))') &
                                     ixe, iye, icc, gm(iopt,ir), nxo, nyo, nco, gm(jopt,ir)
                 If (gifile%cfile%lset) Write (Unit=iout, Fmt='(2(5X,2I5,I6,F12.3))') &
                                              ixe, iye, icc, gm(iopt,ir), nxo, nyo, nco, gm(jopt,ir)
              Else
                 If (iel == 1) Write (Unit=*, Fmt='(5X,2I5,I6,A12,5X,2I5,I6,F12.3)') &
                                     ixe, iye, icc, AdjustR('N/A'), nxo, nyo, nco, gm(jopt,ir)
                 If (gifile%cfile%lset) Write (Unit=iout, Fmt='(5X,2I5,I6,A12,5X,2I5,I6,F12.3)') &
                                              ixe, iye, icc, AdjustR('N/A'), nxo, nyo, nco, gm(jopt,ir)
              End If
           End Do
        End Do
        If (lstop) Return
     End Do
     If (iel == 0) Then
        Write (Unit=cfmt, Fmt='(A,3(I1,A))') &
             '(1X,A,F18.3,A,I', magnitude(nxo), ',A,I', magnitude(nyo), ',A,I', magnitude(nco), ',A)'
        Write (Unit=*, Fmt=cfmt) Trim(l_goodness%c)//': ', gm(jopt,ir), ' (using ', nxo, ' X,', nyo, ' Y, and ', nco, ' CCA modes)'        
     End If
  Else
     jopt = 1
     gm(jopt,ir) = goodness(igood, nt, ny, yopt(:,:,jopt), y)
     Write (Unit=*, Fmt='(1X,A,F18.3)') Trim(l_goodness%c)//': ',gm(jopt,ir)
     If (gifile%cfile%lset) Write (Unit=iout, Fmt='(1X,A,F18.3)') Trim(l_goodness%c)//': ', gm(jopt,ir)
  End If
  If (lstop) Return
  If (update_progress_meter(.false.) /= 0) Return
  If (Present(yhatt)) yhatt(:,1:nt) = yopt(:,1:nt,jopt)
  If (Present(yhat)) Then
     yhat(:,1:nt) = yopt(:,1:nt,jopt)
     If (istd /= 3) Call transform_cv (1, ny, nt, ncv, ave(:,1), sdev(:,1), yhat) ! - transform -
  End If
!
  Return
 End Subroutine cv_cca
!
!
!
 Subroutine cca_predict (nf, nx, ny, nxe, nye, ncc, ieofx, lzero, x, ieofy, fcast, &
            nt, xvp, fsx, fsxc, nc, tclim1, tclim0, fcastt, yprst)
!
! Calculates predictions given new predictor values
!
! Modules
  Use data_numbers,   Only: one
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: rwk, xc, xm, xsd, ym, ysd
  Use settings,       Only: igauss, istd, istdo
  Use distribs,       Only: gaussian, gaussian_inv, stdize, ustdize, zero_bound
  Use pcs,            Only: eofx, eofy, mxe, mu, prjc, r, s, svx, svy
  Use climate,        Only: ave, sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nf    ! - number of forecasts -
  Integer, Intent(In) :: nx    ! - number of X variables -
  Integer, Intent(In) :: ny    ! - number of Y variables -
  Integer, Intent(In) :: nxe   ! - number of X EOF modes -
  Integer, Intent(In) :: nye   ! - number of Y EOF modes -
  Integer, Intent(In) :: ncc   ! - number of CCA modes -
  Integer, Intent(In) :: ieofx ! - X EOF option -
  Integer, Intent(In) :: ieofy ! - Y EOF option -
!
  Logical, Intent(In) :: lzero ! - apply zero-bound? -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: nt  ! - number of cases in training period -
  Integer, Intent(In), Optional :: nc  ! - number of cases in transformation climatology -
!
! Arrays,
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - new predictor values -
!
! - optional input arrays -
  Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: tclim1 ! - climatological data for transformation (not for anomalies) -
  Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: tclim0 ! - climatological data for transformation (with persistence) -
  Real(Kind=rp), Dimension(:,:), Intent(In), Optional :: yprst  ! - persistence component -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: fcast ! - forecast values -
!
! - optional output arrays -
  Real(Kind=rp), Dimension(:), Intent(Out), Optional :: xvp ! - predictors over forecast period -
!
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fsx    ! - predictor time scores -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fsxc   ! - predictor CCA time scores -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fcastt ! - transformed forecast values -
!
! Locals
!
! Local scalars
  Integer :: i ! - EOF mode index -
  Integer :: k ! - case index -
!
  Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MatMul, Present, Real, Sum, Transpose
!
! Executable Statements
!
! Standardize predictors
  xc(:,1:nf,1) = x(:,:)
  Call stdize (3-ieofx, 0, 1, nx, nf, xm, xsd, xc(:,1:nf,1))
!
! Predict anomalies
  If (Present(xvp)) df = Real(nt, Kind=rp)
  Do k = 1, nf
!
! Project X anomalies onto the X EOFs and X CCA modes
     rwk(1:nxe) = MatMul(Transpose(eofx(:,1:nxe)), xc(:,k,1))
     If (Present(fsx)) Then
        fsx(1:nxe,k) = rwk(1:nxe)
        Do i = nxe+1, mxe
           fsx(i,k) = Sum(eofx(:,i)*xc(:,k,1))
        End Do
     End If
     If (Present(fsxc)) fsxc(1:ncc,k) = MatMul(s(1:ncc,1:nxe), fsx(1:nxe,k)/svx(1:nxe))
! - standardise to unit variance by scaling by inverse of singular values -
     rwk(1:nxe) = rwk(1:nxe)/svx(1:nxe)
!
! Project X EOFs onto the CCA modes
     prjc(1:ncc) = MatMul(s(1:ncc,1:nxe), rwk(1:nxe))
     If (Present(xvp)) xvp(k) = one/df + Sum(prjc(1:ncc))**2
! - scale by canonical correlations -
     prjc(1:ncc) = prjc(1:ncc)*mu(1:ncc)
     If (update_progress_meter(.false.) /= 0) Return
!
! Project CCA modes back onto the Y EOF modes
     rwk(1:nye) = MatMul(r(1:nye,1:ncc), prjc(1:ncc))
! - scale by singular values -
     rwk(1:nye) = rwk(1:nye)*svy(1:nye)
!
! Project Y EOF modes back onto the Y anomalies
     fcast(:,k) = MatMul(eofy(:,1:nye), rwk(1:nye))
!
! Rescale
     Select Case (ieofy)
      Case (1)
        fcast(:,k) = fcast(:,k)*ysd(:) + ym(:)
      Case (2)
        fcast(:,k) = fcast(:,k) + ym(:)
     End Select
     If (update_progress_meter(.false.) /= 0) Return
  End Do
!
! Transform
! - transform from gaussian -
  If (Present(tclim1) .and. Present(nc)) Then
     If (Present(fcastt) .and. .not.Present(yprst)) fcastt(:,:) = fcast(:,:)
     Call gaussian_inv (igauss, istd, 1, ny, nf, fcast(:,:), &
          nr=nc, ref=tclim1(:,1:nc), lgam=.false., ave=ave(:,1), sdev=sdev(:,1))
     If (.not.Present(yprst) .and. istd == 3) Call stdize (istd, igauss, 1, ny, nf, ave(:,1), sdev(:,1), fcast(:,:))
  End If
! - add persistence component -
  If (Present(yprst)) Then
     If (Present(fcastt) .and. (istd /= 0 .and. istd /= 3)) Call ustdize (istd, igauss, 0, ny, nf, ave(:,1), sdev(:,1), fcast(:,:))
     fcast(:,:) = fcast(:,:) + yprst(:,:)
     If (istd /= 0) Call stdize (istd, igauss, 0, ny, nf, ave(:,0), sdev(:,0), fcast(:,:))
     If (Present(fcastt)) Then
        fcastt(:,:) = fcast(:,:)
        If (istd /= 3) Then
           Call gaussian (igauss, 0, ny, nf, fcastt(:,:), &
                nr=nc, ref=tclim0(:,1:nc), ave=ave(:,0), sdev=sdev(:,0))
        End If
     End If
! - apply zero-transform -
     If (lzero) Call zero_bound (istd, ny, nf, ave(:,0), sdev(:,0), fcast)
  Else
     If (lzero) Call zero_bound (istdo, ny, nf, ave(:,1), sdev(:,1), fcast(:,:))
  End If
!
  Return
 End Subroutine cca_predict
End Module cca
