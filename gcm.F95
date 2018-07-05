! Author: Simon Mason
Module gcm
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: gcm_predict, get_combined, perform_gcm
!
Contains
!
!
 Function perform_gcm()
!
! Performs GCM gridpoint comparisons
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
                                pprd, &
                                get_cdate
  Use labels,             Only: cg_to_l, cg_tperiod
  Use gui,                Only: print_text, print_warning
  Use progress_meter,     Only: set_progress_increment
  Use arrays,             Only: b0, b1=>b, iusex, iusey, x, xiny, xvp, y, yhat, yhatt, yopt, yrpls, yret, yrett, yt, &
                                convert_units
  Use settings,           Only: clf, iensc, iev, igauss, ig0, imc, intp, iretro, izero, lcw, lzero, mxa, mya, ncv, nret, nretro, &
                                nt, nt1, nu, nur, nu1
  Use iofiles,            Only: gifile, xfile, yfile, &
                                file_reset
  Use space,              Only: get_interpolated, set_nearest_grids
  Use fields,             Only: xfield, yfield
  Use missing,            Only: kuse
  Use categories,         Only: pobs, rfps, rodds, tobst, &
                                calc_reg_probs
  Use climate,            Only: ave, sdev
  Use season,             Only: lb, lprst
  Use analysis,           Only: ianal, lconvert, nopt, &
                                init_analysis1, init_analysis2, num_calcs
!
! Function type
  Integer :: perform_gcm
!
! Locals
!
! Local scalars
  Integer :: iml   ! - model index -
  Integer :: irl   ! - index of last retroactive training-period year -
  Integer :: ir1   ! - index of first retroactive year -
  Integer :: irn   ! - index of last retroactive year -
  Integer :: iru   ! - index of last new year in retroactive training period -
  Integer :: mlt   ! - maximum number of latitudes -
  Integer :: mlg   ! - maximum number of longitudes -
  Integer :: nr    ! - total number of retroaoctive steps in current cycle -
  Integer :: ifail ! - error indicator -
!
  Character(Len=lprd) :: cprd1 ! - period -
  Character(Len=lprd) :: cprd2 ! - period -
  Character(Len=lprd) :: cprd3 ! - period -
!
  Type(pprd) :: prdc ! - current period -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Count, MaxVal, Min, Real, Trim
!
! Executable Statements
!
! Check for consistent units
  perform_gcm = -1
  lconvert = .false.
  Do iml = 1, xfile%nms
     If (Trim(xfield(iml)%var) == Trim(yfield(1)%var)) Then
        If ((Trim(xfield(iml)%unit) /= 'none') .and. (Trim(yfield(1)%unit) /= 'none')) Then
           If (Trim(xfield(iml)%unit) /= Trim(yfield(1)%unit)) Then
              Call convert_units (xfield(iml)%unitc, yfield(1)%unit, x(1:mxa,:,1), ifail)
              If (ifail == 0) Then
                 lconvert = .true.
              Else
                 Call print_warning ('Unable to convert from '//Trim(xfield(iml)%unit)//' to '//Trim(yfield(1)%unit), &
                      lcursor=.true.)
              End If
           End If
        End If
     End If
  End Do
!
! Initialise analysis
  Select Case (yfile%idstr)
   Case (1)
     mlt = MaxVal(yfield(:)%region%nlts)
     mlg = MaxVal(yfield(:)%region%nlgs)
   Case (2)
     mlt = MaxVal(yfield(:)%nva)
     mlg = MaxVal(yfield(:)%nva)
  End Select
  Call init_analysis1 (ianal, .false., ng, nret, nopt, ifail, &
       nlt=mlt, nlg=mlg, ndom=xfile%nms)
  If (ifail /= 0) Then
     perform_gcm = 1
     Return
  End If
  If ((imc == 4) .and. (xfile%nms > 1)) Then
     Call init_combine (xfile%nms, nt, ifail)
     If (ifail /= 0) Then
        perform_gcm = 1
        Return
     End If
  End If
!
! Estimate number of calculations
  If (iev /= 3) Then
     Call set_progress_increment &
          (one/Real(num_calcs(ianal, iretro, igauss, izero, 0, mya, nu, nret, nretro, nu1, nur, 1), Kind=rp), .false.)
  Else
     Call set_progress_increment &
          (one/Real(num_calcs(ianal, iretro, igauss, izero, 1, mya, nu, nret, nretro, nu1, nur, 1), Kind=rp), .false.)
  End If
!
! Identify nearest grids
  Call set_nearest_grids (yfile%idstr, intp, xfile%nms, yfile%nfl, xfield(:)%nlt, xfield(:)%nlg, yfield(1:)%nva, &
       xfield(:)%region, yfield(1:)%region, ifail)
  Select Case (ifail)
   Case (0)
     Continue
   Case (-1)
     GoTo 1
   Case Default  
     perform_gcm = ifail + 2
     GoTo 1
  End Select
!
! Calculate interpolated values
  Call get_interpolated (ianal, xfile%idstr, yfile%idstr, intp, mya, iusex, iusey, xfile%nms, yfile%nfl, nu, xfile%ngs, &
       xfield(:)%nva, yfield(1:)%nva, x, xfield(:)%rmiss, xfield(:)%region, yfield(1:)%region, xiny(:,:,1:))
!
! Open goodness index file
  If (gifile%cfile%lset) Then
     Open (Unit=iout, File=gifile%cfile%ffile, Access='sequential', Action='write', Form=ffmts(gifile%cfile%ffmt%ifmt), &
           IOstat=ifail, Status='unknown')
     If (ifail /= 0) Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
  End If
!
! Perform retroactive calculations
  prdc = yfile%prd1 + (yfile%it1 - 1)
  cprd1 = get_cdate(prdc, 1, 2)
! - cross-validate model for successive training periods -
  If (iretro == 1) Then
     Print *, 'Calculating retroactive forecasts ...'
     ir1 = 1
     irn = 0
     iru = nu1
     retro: Do irl = nt1, nt-1, nretro
        irn = irn + Count(kuse(irl+1:Min(irl+nretro, nt)))
        nr = irn + 1 - ir1
        If (nr > 0) Then
           ncv = iru - lcw
           cprd2 = get_cdate(yfile%prd1 + (yfile%it1+irl-2), 1, 2)
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
              Call cv_gcm (imc, xfile%nms, iru, ncv, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:), &
                   yhat=yhat)
            Case Default
              Call cv_gcm (imc, xfile%nms, iru, ncv, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:), &
                   yhatt=yhatt)
           End Select
! - construct full model -
           Select Case (igauss)
            Case (0)
              Call full_gcm (imc, xfile%nms, iru, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:))
            Case Default
              Call full_gcm (imc, xfile%nms, iru, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:), &
                   yt=yopt(:,:,2))
           End Select
! - produce retroactive forecast -
           cprd3 = get_cdate(yfile%prd1 + (yfile%it1+irl-1), 1, 2)
           If (nr > 1) Then
              cprd2 = get_cdate(yfile%prd1 + (yfile%it1+irl+nr-2), 1, 2)
              Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)//' - '//Trim(cprd2)
           Else
              Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)
           End If
           If (xfile%nms > 1) Call get_combined (mya, xfile%nms, b0(1:,:), b1(1:,:), xiny(1:mya,nu1+ir1:nu1+irn,1:), &
                                   xiny(1:mya,nu1+ir1:nu1+irn,0))
           Select Case (igauss)
            Case (0)
              If (.not.lprst) Then
                 Call gcm_predict (nr, mya, lzero, xiny(1:mya,nu1+ir1:nu1+irn,ig0), b0(ig0,:), b1(ig0,:), yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(:,ir1:irn))
              Else
                 Call gcm_predict (nr, mya, lzero, xiny(1:mya,nu1+ir1:nu1+irn,ig0), b0(ig0,:), b1(ig0,:), yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(:,ir1:irn), yprst=y(1:mya,nu1+ir1:nu1+irn,0))
              End If
            Case Default
              If (.not.lprst) Then
                 Call gcm_predict (nr,mya,lzero,xiny(1:mya,nu1+ir1:nu1+irn,ig0),b0(ig0,:),b1(ig0,:),yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(:,ir1:irn), nc=iru, tclim1=y(1:mya,1:iru,1), fcastt=yrett(:,ir1:irn))
              Else
                 Call gcm_predict (nr,mya,lzero,xiny(1:mya,nu1+ir1:nu1+irn,ig0),b0(ig0,:),b1(ig0,:),yret(:,ir1:irn), &
                      nt=iru, xvp=xvp(:,ir1:irn), nc=iru, tclim1=y(1:mya,1:iru,1), tclim0=y(1:mya,1:iru,0),        &
                      fcastt=yrett(:,ir1:irn), yprst=y(1:mya,nu1+ir1:nu1+irn,0))
              End If
           End Select
           If (iev == 3) Call gcm_predict (iru, mya, lzero, xiny(1:mya,1:iru,ig0), b0(ig0,:), b1(ig0,:), yhat(:,:))
! - calculate probabilities -
           If (iensc == 1) Then
              Select Case (igauss)
               Case (0)
                 Call calc_reg_probs (ianal, igauss, iru, nr, ng, 1, mya, lb, ave(:,lb), sdev(:,lb), y(1:mya,:,lb), yhat, &
                      xvp(:,ir1:irn), yret(:,ir1:irn), tobst, pobs, clf, rfps(:,ir1:irn,:), rodds(:,ir1:irn,:,:), yrpls(:,ir1:irn))
               Case Default
                 Call calc_reg_probs (ianal, igauss, iru, nr, ng, 1, mya, lb, ave(:,lb), sdev(:,lb), yopt(:,:,2), yhat, &
                      xvp(:,ir1:irn), yrett(:,ir1:irn), tobst, pobs, clf, rfps(:,ir1:irn,:), rodds(:,ir1:irn,:,:),      &
                      yrpls(:,ir1:irn), &
                      nc=iru,clim=y(1:mya,1:iru,lb))
              End Select
           Else
!              Call calc_ens_probs (iensc,iru,nr,ng,mya,xfile%nem,x(1:mxa,:,1),yhat,pobs,clf,rfps(:,ir1:irn,:), &
!                   rodds(:,ir1:irn,:,:),yrpls(:,ir1:irn))
           End If
           Print *, ' '
           If (gifile%cfile%lset) Call print_text (' ', &
                                 iout=iout)
        End If
! - update indices -
        iru = nu1 + irn
        ir1 = irn + 1
     End Do retro
  End If
  cprd2 = get_cdate(yfile%prd1 + (yfile%it1+nt-2), 1, 2)
  ncv = nu - lcw
!
! Cross-validate model
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
     Call cv_gcm (imc, xfile%nms, nu, ncv, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:), &
          yhat=yhat)
   Case Default
     Call cv_gcm (imc, xfile%nms, nu, ncv, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:), &
          yhat=yhat, yhatt=yhatt)
  End Select
!
! Allocate additional memory
  Call init_analysis2 (ifail)
  If (ifail /= 0) Then
     perform_gcm = 1
     GoTo 1
  End If
!
! Fit model using all data
  Print *, 'Constructing model using full training period ('//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)//') ...'
  If (xfile%nms > 1) Call get_combined (mya, xfile%nms, b0(1:,:), b1(1:,:), xiny(:,:,1:), xiny(:,:,0))
  Select Case (igauss)
   Case (0)
     Call full_gcm (imc, xfile%nms, nu, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:))
   Case Default
     Call full_gcm (imc, xfile%nms, nu, mya, ig0, xiny(:,:,ig0:), y(1:mya,:,1), b0(ig0:,:), b1(ig0:,:), &
          yt=yt)
  End Select
  perform_gcm = 0
!
! Free memory
1 If ((imc == 4) .and. (xfile%nms > 1)) Call close_combine ()
!
  Return
 End Function perform_gcm
!
!
!
 Subroutine full_gcm (imc, nms, nt, ny, ig0, x, y, b0, b1, &
            yt)
!
! Performs GCM using all data
!
! Modules
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: xc, yc
  Use settings,       Only: igauss, istd
  Use distribs,       Only: gaussian, set_gamma
  Use climate,        Only: ave, climate_per, clim,sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: imc ! - model combination identifier -
  Integer, Intent(In) :: nms ! - number of models -
  Integer, Intent(In) :: nt  ! - number of cases -
  Integer, Intent(In) :: ny  ! - number of y spatial points -
  Integer, Intent(In) :: ig0 ! - lower index limit -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! Input/output arrays
  Real(Kind=rp), Dimension(:,:,ig0:), Intent(InOut) :: x ! - explanatory variables -
!
! Output arrays
  Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b0 ! - regression constants -
  Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b1 ! - regression coefficients -
!
! - optional output arrays -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yt ! - transformed response variables -
!
! Executable Statements
!
! Backup data
  xc(1:ny,1:nt,ig0:) = x(1:ny,1:nt,ig0:)
  yc(1:ny,1:nt) = y(1:ny,1:nt)
  If (igauss > 0) Then
     If (istd == 3) Call set_gamma (.false.)
     Call gaussian (igauss, 1, ny, nt, yc(1:ny,1:nt), &
          nr=climate_per%nc, ref=clim(1:ny,1:climate_per%nc,1), ave=ave(:,1), sdev=sdev(:,1))
  End If
!
! Copy transformed data
  If (Present(yt)) yt(:,1:nt) = yc(:,1:nt)
  If (update_progress_meter(.false.) /= 0) Return
!
! Calculate combination/calibration parameters
  Call do_gcm (imc, nms, nt, ny, ig0, xc(:,:,ig0:), yc, b0(ig0:,:), b1(ig0:,:))
  If (update_progress_meter(.false.) /= 0) Return
!
  Return
 End Subroutine full_gcm
!
!
!
 Subroutine cv_gcm (imc, nms, nt, ncv, nv, ig0, x, y, b0, b1, &
            yhat,yhatt)
!
! Performs cross-validated GCM
!
! Modules
  Use data_io_constants, Only: iout
  Use labels,            Only: l_goodness
  Use progress_meter,    Only: update_progress_meter
  Use arrays,            Only: xc, yc, yopt, &
                               get_cvsample
  Use settings,          Only: hcw, igauss, igood, istd, lzero
  Use distribs,          Only: gaussian, set_gamma, transform_cv
  Use scores,            Only: goodness
  Use iofiles,           Only: gifile
  Use climate,           Only: ave, sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: imc ! - model combination identifier -
  Integer, Intent(In) :: nms ! - number of models -
  Integer, Intent(In) :: nt  ! - number of cases -
  Integer, Intent(In) :: ncv ! - length of cross-validated training period -
  Integer, Intent(In) :: nv  ! - number of y spatial points -
  Integer, Intent(In) :: ig0 ! - lower index limit -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! Input/output arrays
  Real(Kind=rp), Dimension(:,:,ig0:), Intent(InOut) :: x ! - explanatory variables -
!
! Output arrays
  Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b0 ! - regression constants -
  Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b1 ! - regression coefficients -
!
! - optional output arrays -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yhat  ! - cross-validated hindcasts -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yhatt ! - transformed cross-validated hindcasts -
!
! Locals
!
! Local scalars
  Integer :: it ! - cross-validation time-step -
!
  Real(Kind=rp) :: gm ! - goodness metric -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present, Trim
!
! Executable Statements
!
! Cross-validate
  time_step: Do it = 1, nt
     Call get_cvsample (hcw, it, y(:,1:nt), yc(:,1:nt), &
          v3=x(:,1:nt,ig0:), v3cv=xc(:,1:nt,ig0:))
!
! Transform
     If (igauss > 0) Then
        If (istd == 3) Call set_gamma (.false.)
        Call gaussian (igauss, 1, nv, ncv, yc(1:nv,1:ncv), &
             nr=ncv, ave=ave(:,1), sdev=sdev(:,1))
     End If
     If (update_progress_meter(.false.) /= 0) Return
!
! Calculate combination/calibration parameters
     Call do_gcm (imc, nms, ncv, nv, ig0, xc(:,:,ig0:), yc, b0(ig0:,:), b1(ig0:,:))
     If (nms>1) Call get_combined (nv, nms, b0(1:,:), b1(1:,:), x(:,it:it,1:), x(:,it:it,0))
     Call gcm_predict (1, nv, lzero, x(:,it:it,ig0), b0(ig0,:), b1(ig0,:), yopt(:,it:it,1))
     If (update_progress_meter(.false.) /= 0) Return
  End Do time_step
!
! Calculate and print goodness metric
  gm = goodness(igood, nt, nv, yopt(:,:,1), y(:,:))
  Write (Unit=*, Fmt='(1X,A,F18.3)') Trim(l_goodness%c)//': ', gm
  If (gifile%cfile%lset) Write (Unit=iout, Fmt='(1X,A,F18.3)') Trim(l_goodness%c)//': ', gm
  If (Present(yhatt)) yhatt(:,1:nt) = yopt(:,1:nt,1)
  If (Present(yhat)) Then
     yhat(:,1:nt) = yopt(:,1:nt,1)
     If (istd /= 3) Call transform_cv (1, nv, nt, ncv, ave(:,1), sdev(:,1), yhat) ! - transform -
  End If
!
  Return
 End Subroutine cv_gcm
!
!
!
 Subroutine do_gcm (imc, nms, nt, ny, ig0, x, yc, b0, b1)
!
! Performs GCM
!
! Modules
  Use data_numbers, Only: zero, eps, one
  Use arrays,       Only: xm, xsd, ym, ysd, &
                          moments
  Use settings,     Only: igcms, igood, lxt
  Use scores,       Only: get_regr, goodness
  Use pcs,          Only: bz, eofx, ieofx, npx, svx, tsx, xwk
  Use pcr,          Only: do_pcr
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: imc ! - model combination identifier -
  Integer, Intent(In) :: nms ! - number of models -
  Integer, Intent(In) :: nt  ! - number of cases -
  Integer, Intent(In) :: ny  ! - number of y spatial points -
  Integer, Intent(In) :: ig0 ! - lower index limit -
!
! Input/output arrays
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: yc ! - response variables -
!
  Real(Kind=rp), Dimension(:,:,ig0:), Intent(InOut) :: x ! - explanatory variables -
!
! Output arrays
  Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b0 ! - regression constants -
  Real(Kind=rp), Dimension(ig0:,:), Intent(Out) :: b1 ! - regression coefficients -
!
! Locals
!
! Local scalars
  Integer :: i     ! - location index -
  Integer :: ibest ! - best model -
  Integer :: imdl  ! - current model -
  Integer :: ifail ! - error indicator -
!
  Real(Kind=rp) :: gm    ! - goodness metric -
  Real(Kind=rp) :: gbest ! - best score -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MatMul, Real, Sum, Transpose
!
! Executable Statements
!
! Select model combination approach
  Call moments (ny, nt, yc, ym, ysd)
  If (nms > 1) Then
     Select Case (imc)
      Case (1) ! - uncalibrated average -
        b1(1:,:) = one/Real(nms, Kind=rp)
        b0(1:,:) = zero
      Case (2) ! - calibrated average -
        Select Case (igcms)
         Case (0, 1) ! - correct mean biases -
           b1(1:,:) = one/Real(nms, Kind=rp)
           Do imdl = 1, nms
              Call moments (ny, nt, x(:,:,imdl), xm, xsd)
              b0(imdl,:) = ym(:) - xm(:)
           End Do
           b0(1:,:) = zero
         Case Default ! - correct mean and variance biases -
           Do imdl = 1, nms
              Call moments (ny, nt, x(:,:,imdl), xm, xsd)
              b0(imdl,:) = ym(:) - xm(:)
              Do i = 1, ny
                 If (xsd(i) > eps) Then
                    b1(imdl,i) = ysd(i)/xsd(i)
                 Else
                    b1(imdl,i) = zero
                 End If
                 b0(imdl,i) = ym(i) - xm(i)*b1(imdl,i)
              End Do
           End Do
        End Select
      Case (3) ! - recalibrated average -
        Do imdl = 1, nms
           Do i = 1, ny
              Call get_regr (nt, x(i,:,imdl), yc(i,:), b0(imdl,i), b1(imdl,i))
           End Do
        End Do
      Case (4) ! - skill-weighted average -
        Do i = 1, ny
           xwk(:,1:nt) = Transpose(x(i,:,1:))
           Call do_pcr (nt, nms, xwk(:,1:nt), 1, yc(:,1:nt), ieofx, nms, svx, eofx, tsx, bz(:,i:i), .true., lxt, npx, ifail)
           b1(1:nms,i:i) = MatMul(eofx(1:nms,1:npx), bz(1:npx,1:1))
           b0(1:nms,i) = (ym(i) - Sum(xm(1:nms)*b1(1:nms,i)))/Real(nms, Kind=rp)
        End Do
      Case (5) ! - best model by location -
        Do i = 1, ny
           gbest = -one
           ibest = 0
           Do imdl = 1, nms
              gm=goodness(igood, nt, 1, x(imdl:imdl,:,1), yc(imdl:imdl,:))
              If (gm > gbest) Then
                 gbest = gm
                 ibest = imdl
              End If
           End Do
           b1(1:,i) = zero
           b1(ibest,i) = one
           b0(1:,i) = zero
        End Do
      Case (6) ! - best overall model -
        gbest = -one
        ibest = 0
        Do imdl = 1, nms
           gm = goodness(igood, nt, ny,x(:,:,1), yc(:,:))
           If (gm > gbest) Then
              gbest = gm
              ibest = imdl
           End If
        End Do
        b1(1:,:) = zero
        b1(ibest,:)=one
        b0(1:,:)=zero
     End Select
!
! Combine models
     Call get_combined (ny, nms, b0(1:,:), b1(1:,:), x(:,:,1:), x(:,:,0))
  End If
!
! Recalibrate combined forecasts if necessary
  Call moments (ny, nt, x(:,:,ig0), xm, xsd)
  Select Case (igcms)
   Case (0) ! - no correction -
     b0(ig0,:) = zero
     b1(ig0,:) = one
   Case (1) ! - correct mean biases -
     b1(ig0,:) = one
     b0(ig0,:) = ym(:) - xm(:)
   Case (2) ! - correct mean and variance biases -
     Do i = 1, ny
        If (xsd(i) > eps) Then
           b1(ig0,i) = ysd(i)/xsd(i)
        Else
           b1(ig0,i) = zero
        End If
        b0(ig0,i) = ym(i) - xm(i)*b1(ig0,i)
     End Do
   Case (3) ! - correct for skill -
     Do i = 1, ny
        Call get_regr (nt, x(i,:,ig0), yc(i,:), b0(ig0,i), b1(ig0,i))
     End Do
  End Select
!
  Return
 End Subroutine do_gcm
!
!
!
 Subroutine gcm_predict (nf, ny, lzero, x, b0, b1, fcast, &
            nt, xvp, nc, tclim1, tclim0, fcastt, yprst)
!
! Calculates predictions given new predictor values
!
! Modules
  Use data_numbers,   Only: zero, one
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: xm, xsd
  Use settings,       Only: igauss, istd, istdo
  Use distribs,       Only: gaussian, gaussian_inv, stdize, ustdize, zero_bound
  Use climate,        Only: ave, sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nf ! - number of forecasts -
  Integer, Intent(In) :: ny ! - number of Y variables -
!
  Logical, Intent(In) :: lzero ! - apply zero-bound? -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: nt ! - number of cases in training period -
  Integer, Intent(In), Optional :: nc ! - number of cases in climatology -
!
! Arrays,
!
! Input arrays
  Real(Kind=rp), Dimension(:), Intent(In) :: b0 ! - regression constants -
  Real(Kind=rp), Dimension(:), Intent(In) :: b1 ! - regression coefficients -
!
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
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: xvp    ! - predictors over forecast period -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: fcastt ! - transformed forecast values -
!
! Locals
!
! Local scalars
  Integer :: i ! - variable index -
  Integer :: k ! - case index -
!
  Real(Kind=rp) :: df ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Present, Real
!
! Executable Statements
!
! Predict anomalies
  If (Present(xvp)) df=Real(nt, Kind=rp)
  Do k = 1, nf
     fcast(:,k) = b0(:) + b1(:)*x(:,k)
!
! Calculate prediction error variance
     If (Present(xvp)) Then
        Do i = 1, ny
           If (xsd(i) > zero) Then
              xvp(i,k) = (one + ((x(i,k) - xm(i))**2)/xsd(i))/df
           Else
              xvp(i,k) = zero
           End If
        End Do
     End If
!
! Update progress meter
     If (update_progress_meter(.false.) /= 0) Return
  End Do
!
! Transform
! - transform from gaussian -
  If (Present(tclim1) .and. Present(nc)) Then
     If (Present(fcastt) .and. .not.Present(yprst)) fcastt(:,:) = fcast(:,:)
     Call gaussian_inv (igauss,istd,1,ny,nf,fcast(:,:), &
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
 End Subroutine gcm_predict
!
!
!
 Subroutine init_combine (nms, nt, ifail)
!
! Initialises memory and settings to combine model predictions
!
! Modules
  Use settings, Only: mya
  Use pcs,      Only: bz, eofx, svx, tsx, xwk
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nms ! - number of models -
  Integer, Intent(In) :: nt  ! - number of cases -
!
! Output scalars
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  Integer :: lnn ! - minimum of the two input arguments -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Min
!
! Executable Statements
!
! Allocate memory
! - independent variables singular vectors -
  lnn = Min(nms, nt)
  Allocate (svx(lnn), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - independent variables EOF patterns -
  Allocate (eofx(nms,lnn), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - independent variables EOF scores -
  Allocate (tsx(lnn,nt), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - principal component regression coefficients -
  Allocate (bz(lnn,mya), Stat=ifail)
  If (ifail /= 0) GoTo 1
! - workspace -
  Allocate (xwk(nms,nt), Stat=ifail)
  If (ifail /= 0) GoTo 1
!
  Return
!
! Errors
1 Call close_combine ()
!
  Return
 End Subroutine init_combine
!
!
!
 Subroutine get_combined (m, nms, b0, b1, x, c)
!
! Combines model predictions
!
! Modules
  Use data_numbers, Only: zero
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: m   ! - number of locations -
  Integer, Intent(In) :: nms ! - number of models -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: b0 ! - regression constants -
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: b1 ! - regression coefficients -
!
  Real(Kind=rp), Dimension(:,:,:), Intent(In) :: x ! - model predictions -
!
! Output arrays
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: c ! - combined predictions -
!
! Locals
!
! Local scalars
  Integer :: i ! - location index -
  Integer :: j ! - model index -
!
! Executable Statements
!
! Combine models
  Do i = 1, m
     c(i,:) = zero
     Do j = 1, nms
        c(i,:) = c(i,:) + b0(j,i) + b1(j,i)*x(i,:,j)
     End Do
  End Do
!
  Return
 End Subroutine get_combined
!
!
!
 Subroutine close_combine ()
!
! Frees memory for combining model predictions
!
! Modules
  Use pcs, Only: bz, eofx, svx, tsx, xwk
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic AllocateD
!
! Executable Statements
!
! Allocate memory
  If (Allocated(xwk))  Deallocate (xwk)
  If (Allocated(bz))   Deallocate (bz)
  If (Allocated(tsx))  Deallocate (tsx)
  If (Allocated(eofx)) Deallocate (eofx)
  If (Allocated(svx))  Deallocate (svx)
!
  Return
 End Subroutine close_combine
End Module gcm
