! Author: Simon Mason
Module pcr
!
! Modules
  Use data_numbers, Only: rp
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: do_pcr, pcr_predict, perform_pcr
!
Contains
!
!
 Function perform_pcr()
!
! Performs PCR
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail = -1 Terminated
!
! Modules
  Use data_numbers,       Only: one
  Use data_cpt_constants, Only: ia_pcr, ia_mlr, ng
  Use data_io_constants,  Only: ffmts, iout, lprd
  Use time,               Only: Operator(+), &
                                get_cdate
  Use labels,             Only: cg_to_l, cg_tperiod
  Use gui,                Only: print_text
  Use progress_meter,     Only: lstop, &
                                set_progress_increment
  Use arrays,             Only: b, b0, x, xm, xvp, y, yhat, yhatt, ym, yopt, yret, yrett, yrpls, yt
  Use settings,           Only: clf, iev, igauss, iretro, izero, lcw, lzero, mxa, mya, ncv, nret, nretro, nt, nt1, nu, nur, nu1
  Use iofiles,            Only: gifile, yfile, &
                                file_reset
  Use pcs,                Only: bz, eofx, ieofx, mxe, npx, nxe, nxo, svx, tsx
  Use missing,            Only: kuse
  Use categories,         Only: pobs, rfps, rodds, tobst, &
                                calc_reg_probs
  Use climate,            Only: ave, sdev
  Use season,             Only: lb, lprst
  Use analysis,           Only: ianal, lconvert, nopt, &
                                init_analysis1, init_analysis2, num_calcs
!
! Function type
  Integer :: perform_pcr
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
  Intrinsic Count, MatMul, Min, Real, Trim
!
! Executable Statements
!
! Initialise analysis
  Call init_analysis1 (ianal, .false., ng, nret, nopt, ifail)
  If (ifail /= 0) Then
     perform_pcr = 1
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
  Select Case (ianal)
   Case (ia_pcr) ! - PCR -
     If (nopt == 1) Then
        nopt = 0
        nxo = nxe
     End If
   Case (ia_mlr) ! - MLR -
     nopt = 0
     nxe = Min(mxa, nu-1)
     mxe = nxe
  End Select
!
! Open goodness index file
  If (gifile%cfile%lset) Then
     Open (Unit=iout, File=gifile%cfile%ffile, Access='sequential', Action='write', Form=ffmts(gifile%cfile%ffmt%ifmt), &
           IOstat=ifail, Status='unknown')
     If (ifail /= 0) Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
  End If
!
! Perform retroactive calculations
  cprd1 = get_cdate(yfile%prd1 + (yfile%it1-1), 1, 2)
! - optimise model for successive training periods -
  ir = 0
  If (iretro == 1) Then
     Print *, 'Calculating retroactive forecasts ...'
     ir1 = 1
     irn = 0
     iru = nu1
     retro: Do irl = nt1, nt-1, nretro
        ir = ir + 1
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
           If (nopt > 0) Then
              Select Case (igauss)
               Case (0)
                 Call cv_pcr (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, &
                      yhat=yhat, nxo=nxo)
               Case Default
                 Call cv_pcr (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, &
                      yhatt=yhat, nxo=nxo)
              End Select
           Else
              Select Case (igauss)
               Case (0)
                 Call cv_pcr (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, &
                      yhat=yhat)
               Case Default
                 Call cv_pcr (iru, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, &
                      yhatt=yhat)
              End Select
              If (ianal == ia_mlr) nxo = nxe
           End If
           If (lstop) Return
! - construct full model -
           Select Case (igauss)
            Case (0)
              Call full_pcr (iru, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, bz, b, npx, ifail)
            Case Default
              Call full_pcr (iru, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, bz, b, npx, ifail, &
                   yt=yopt(:,:,2))
           End Select
           If (lstop) Return
           If (ifail /= 0) Then
              perform_pcr = 2
              Return
           End If
           If (nxo > npx) nxo = npx
! - produce retroactive forecast -
           cprd3 = get_cdate(yfile%prd1 + (yfile%it1+irl-1), 1, 2)
           If (nr > 1) Then
              cprd2 = get_cdate(yfile%prd1 + (yfile%it1+irl+nr-2), 1, 2)
              Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)//' - '//Trim(cprd2)
           Else
              Print *, 'Calculating retroactive forecasts for '//Trim(cprd3)
           End If
           Select Case (igauss)
            Case (0)
              If (.not.lprst) Then
                 Call pcr_predict (nr, mxa, mya, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), b, yret(:,ir1:irn), &
                      nt=iru, nxe=nxo, xvp=xvp(1,ir1:irn))
              Else
                 Call pcr_predict (nr, mxa, mya, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), b, yret(:,ir1:irn), &
                      nt=iru, nxe=nxo, xvp=xvp(1,ir1:irn), yprst=y(1:mya,nu1+ir1:nu1+irn,0))
              End If
            Case Default
              If (.not.lprst) Then
                 Call pcr_predict (nr, mxa, mya, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), b, yret(:,ir1:irn), &
                      nt=iru, nxe=nxo, xvp=xvp(1,ir1:irn), nc=iru, tclim1=y(1:mya,1:iru,1), fcastt=yrett(:,ir1:irn))
              Else
                 Call pcr_predict (nr, mxa, mya, ieofx, lzero, x(1:mxa,nu1+ir1:nu1+irn,1), b, yret(:,ir1:irn),      &
                      nt=iru, nxe=nxo, xvp=xvp(1,ir1:irn), nc=iru,tclim1=y(1:mya,1:iru,1), tclim0=y(1:mya,1:iru,0), &
                      fcastt=yrett(:,ir1:irn), yprst=y(1:mya,nu1+ir1:nu1+irn,0))
              End If
           End Select
           If (iev == 3) Then
              Call pcr_predict (iru, mxa, mya, ieofx, lzero, x(1:mxa,1:iru,1), b, yhat(:,:))
              If (lstop) Return
           End If
! - calculate probabilities -
           Select Case (igauss)
            Case (0)
              Call calc_reg_probs (ianal, igauss, iru, nr, ng, nxo, mya, lb, ave(:,lb), sdev(:,lb), y(1:mya,:,lb), yhat, &
                   xvp(1:1,ir1:irn), yret(:,ir1:irn), tobst, pobs, clf, rfps(:,ir1:irn,:), rodds(:,ir1:irn,:,:), yrpls(:,ir1:irn))
            Case Default
              Call calc_reg_probs (ianal, igauss, iru, nr, ng, nxo, mya, lb, ave(:,lb), sdev(:,lb), yopt(:,:,2), yhat, &
                   xvp(1:1,ir1:irn), yrett(:,ir1:irn), tobst, pobs, clf, rfps(:,ir1:irn,:), rodds(:,ir1:irn,:,:),      &
                   yrpls(:,ir1:irn), &
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
! Optimise PCR using full training period
  If (ianal == ia_mlr) nxo = nxe
  cprd2 = get_cdate(yfile%prd1 + (yfile%it1+nt-2), 1, 2)
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
        Call cv_pcr (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, &
             yhat=yhat, nxo=nxo)
      Case Default
        Call cv_pcr (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxe, mxe, ir, &
             yhat=yhat, yhatt=yhatt, nxo=nxo)
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
        Call cv_pcr (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, nxo, ir, &
             yhat=yhat)
      Case Default
        Call cv_pcr (nu, ncv, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, nxo, ir, &
             yhat=yhat, yhatt=yhatt)
     End Select
  End If
!
! Close goodness file
  If (lstop) Return
  If (gifile%cfile%lset) Then
     Close (Unit=iout)
     Call file_reset (gifile%cfile%lset, gifile%cfile%fname)
  End If
!
! Allocate additional memory
  Call init_analysis2 (ifail)
  If (ifail /= 0) Then
     perform_pcr = 1
     Return
  End If
!
! Fit model using all data
  Print *, 'Constructing model using full training period ('//Trim(cprd1)//' '//Trim(cg_to_l)//' '//Trim(cprd2)//') ...'
  Select Case (igauss)
   Case (0)
     Call full_pcr (nu, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, bz, b, npx, ifail)
   Case Default
     Call full_pcr (nu, mxa, x(1:mxa,:,1), mya, y(1:mya,:,1), ieofx, nxo, svx, eofx, tsx, bz, b, npx, ifail, &
          yt=yt)
  End Select
  If (lstop) Return
  If (ifail /= 0) Then
     perform_pcr = 2
     Return
  End If
!
! Calculate regression constants
  b0(1:mya,1) = ym(1:mya) - MatMul(xm(1:mxa), b(1:mxa,1:mya))
  perform_pcr = 0
!
  Return
 End Function perform_pcr
!
!
!
 Subroutine full_pcr (nt, nx, x, ny, y, ieofx, nxe, svx, eofx, tsx, bz, b, npx, ifail, yt)
!
! Performs PCR using all data
!
! Modules
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: xc, yc
  Use settings,       Only: igauss, istd, lxt
  Use distribs,       Only: gaussian, set_gamma
  Use climate,        Only: ave, climate_per, clim, sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt    ! - number of cases -
  Integer, Intent(In) :: nx    ! - number of x spatial points -
  Integer, Intent(In) :: ny    ! - number of y spatial points -
  Integer, Intent(In) :: nxe   ! - number of X EOF modes -
  Integer, Intent(In) :: ieofx ! - X EOF option -
!
! Output scalars
  Integer, Intent(Out) :: npx   ! - number of positive eigenvalues -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables; shape(nx,nt) -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables; shape(ny,nt) -
!
! Output arrays
  Real(Kind=rp), Dimension(:), Intent(Out) :: svx    ! - singular values of x -
!
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: eofx ! - x EOF patterns -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: tsx  ! - time-series of x EOFs (transposed) -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: bz   ! - principal component regression coefficients -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: b    ! - regression coefficients -
!
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yt ! - transformed response variables -
!
! Locals
!
! Local scalars
  Integer :: k  ! - case index -
  Integer :: ie ! - EOF mode index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MatMul, Present
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
  Call do_pcr (nt, nx, xc(:,1:nt,1), ny, yc(:,1:nt), ieofx, nxe, svx, eofx, tsx, bz, .true., lxt, npx, ifail)
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
!
! Compute regression coefficients
  b(1:nx,1:ny) = MatMul(eofx(1:nx,1:nxe), bz(1:nxe,1:ny))
! - rescale EOF time series -
  Do k = 1, nt
     Do ie = 1, npx
        tsx(ie,k) = tsx(ie,k)*svx(ie)**2
     End Do
  End Do
! - update progress meter -
  If (update_progress_meter(.false.) /= 0) Return
  ifail = 0
!
  Return
 End Subroutine full_pcr
!
!
!
 Subroutine cv_pcr (nt, ncv, nx, x, ny, y, ieofx, nxe, mxe, ir, &
            yhat, yhatt, nxo)
!
! Performs cross-validated PCR
!
! Modules
  Use data_io_constants, Only: iout
  Use maths,             Only: magnitude
  Use labels,            Only: l_goodness
  Use gui,               Only: print_text
  Use progress_meter,    Only: lstop, &
                               update_progress_meter
  Use arrays,            Only: b, xc, yc, ym, yopt, &
                               get_cvsample
  Use settings,          Only: hcw, iel, igauss, igood, istd, lzero
  Use distribs,          Only: gaussian, set_gamma, transform_cv
  Use scores,            Only: gm, &
                               goodness
  Use iofiles,           Only: gifile
  Use pcs,               Only: bz, eofx, npx, svx, tsx
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
  Integer, Intent(In) :: ieofx ! - X EOF option -
!
! Output scalars
! - optional output scalars -
  Integer, Intent(Out), Optional :: nxo ! - optimal number of X EOF modes -
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - explanatory variables -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: y ! - response variables -
!
! Output arrays
! - optional output arrays -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yhat  ! - cross-validated hindcasts -
  Real(Kind=rp), Dimension(:,:), Intent(Out), Optional :: yhatt ! - transformed cross-validated hindcasts -
!
! Locals
!
! Local scalars
  Integer :: it    ! - cross-validation time-step -
  Integer :: ixe   ! - X EOF mode index -
  Integer :: ixu   ! - used X EOF mode index -
  Integer :: iopt  ! - optimization step index -
  Integer :: jopt  ! - index of optimized settings -
  Integer :: ifail ! - error indicator -
!
  Character(Len=19) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MatMul, Min, Present, Trim
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
! Perform PCR
     Call do_pcr (ncv, nx, xc(:,1:ncv,1), ny, yc(:,1:ncv), ieofx, mxe, svx, eofx, tsx, bz, .false., mxe, npx, ifail)
! - update progress meter -
     If (update_progress_meter(.false.) /= 0) Return
!
! Calculate regression coefficients
     If (ifail == 0) Then
        iopt = 0
        Do ixe = nxe, mxe
           ixu = Min(ixe, npx)
           iopt = iopt + 1
           b(1:nx,1:ny) = MatMul(eofx(1:nx,1:ixu), bz(1:ixu,1:ny))
!
! Predict anomaly
           Call pcr_predict (1, nx, ny, ieofx, lzero, x(1:nx,it:it), b, yopt(1:ny,it:it,iopt))
        End Do
!
! Supply mean if PCR failed
     Else
        iopt = 0
        Do ixe = nxe, mxe
           iopt = iopt + 1
           yopt(1:ny,it,iopt) = ym(1:ny)
           If (update_progress_meter(.false.) /= 0) Return
        End Do
     End If
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
  If (Present(nxo)) Then
     If (iel == 1) Then
        Print *, '               CURRENT                         OPTIMUM'
        Print *, ' '
        Print *, '      Number of Modes    Goodness      Number of Modes    Goodness'
        Print *, '                            Index                            Index'
        Print *, ' '
     End If
     If (gifile%cfile%lset) Then
        Call print_text ('               CURRENT                         OPTIMUM', &
             iout=iout)
        Call print_text (' ', &
             iout=iout)
        Call print_text ('      Number of Modes    Goodness      Number of Modes    Goodness', &
             iout=iout)
        Call print_text ('                            Index                            Index', &
             iout=iout)
        Call print_text (' ', &
             iout=iout)
     End If
     iopt = 0
     jopt=1
     Do ixe = nxe, mxe
        iopt = iopt + 1
        Select Case (igauss)
         Case (0)
           gm(iopt,ir) = goodness(igood, nt, ny, yopt(:,:,iopt), y)
         Case Default
           gm(iopt,ir) = goodness(igood, nt, ny, yopt(:,:,iopt), yc)
        End Select
        If ((iopt == 1) .or. (gm(iopt,ir) > gm(jopt,ir))) Then
           jopt = iopt
           nxo = ixe
        End If
        If (iel == 1) Write (Unit=*, Fmt='(1X,2(10X,I5,F18.3))') ixe, gm(iopt,ir), nxo, gm(jopt,ir)
        If (gifile%cfile%lset) Write (Unit=iout, Fmt='(1X,2(10X,I5,F18.3))') ixe, gm(iopt,ir), nxo, gm(jopt,ir)
     End Do
     If (iel == 0) Then
        Write (Unit=cfmt, Fmt='(A,I1,A)') '(1X,A,F18.3,A,I', magnitude(nxo), ',A)'
        Write (Unit=*, Fmt=cfmt) Trim(l_goodness%c)//': ', gm(jopt,ir), ' (using ', nxo, ' X modes)'
     End If
  Else
     jopt = 1
     Select Case (igauss)
      Case (0)
        gm(jopt,ir) = goodness(igood, nt, ny, yopt(:,:,jopt), y)
      Case Default
        gm(jopt,ir) = goodness(igood, nt, ny, yopt(:,:,jopt), yc)
     End Select
     Write (Unit=*, Fmt='(1X,A,F18.3)') Trim(l_goodness%c)//': ',gm(jopt,ir)
     If (gifile%cfile%lset) Write (Unit=iout, Fmt='(1X,A,F18.3)') Trim(l_goodness%c)//': ',gm(jopt,ir)
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
 End Subroutine cv_pcr
!
!
!
 Subroutine do_pcr (nt, nx, x, ny, y, ieofx, nxe, svx, eofx, tsx, bz, lpos, mpx, npx, ifail)
!
! Performs PCR
!
! Modules
  Use data_numbers, Only: zero, tol
#if DP == 1
  Use cpt_lapack,   Only: gesdd=>dgesdd
#else
  Use cpt_lapack,   Only: gesdd=>sgesdd
#endif
  Use arrays,       Only: iusex, iwk, lrwk, rwk, xm, xsd, ym, ysd, &
                          moments
  Use settings,     Only: igauss, lxt
  Use distribs,     Only: stdize
  Use iofiles,      Only: xfile
  Use space,        Only: rlatx, &
                          latitude_weight
  Use fields,       Only: xfield
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nt    ! - number of cases -
  Integer, Intent(In) :: nx    ! - number of x spatial points -
  Integer, Intent(In) :: ny    ! - number of y spatial points -
  Integer, Intent(In) :: nxe   ! - number of X EOF modes -
  Integer, Intent(In) :: ieofx ! - X EOF option -
  Integer, Intent(In) :: mpx   ! - minimum number of positive eigenvalues -
!
  Logical, Intent(In) :: lpos  ! - force largest loading to be positive? -
!
! Output scalars
  Integer, Intent(Out) :: npx   ! - number of positive eigenvalues -
  Integer, Intent(Out) :: ifail ! - error indicator -
!
! Input/output arrays
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: x ! - explanatory variables; shape(nx,nt) -
  Real(Kind=rp), Dimension(:,:), Intent(InOut) :: y ! - response variables; shape(ny,nt) -
!
! Output arrays
  Real(Kind=rp), Dimension(:), Intent(Out) :: svx    ! - singular values of x -
!
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: eofx ! - x EOF patterns -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: tsx  ! - time-series of x EOFs (transposed) -
  Real(Kind=rp), Dimension(:,:), Intent(Out) :: bz   ! - principal component regression coefficients -
!
! Locals
!
! Local scalars
  Integer :: k  ! - case index -
  Integer :: ie ! - EOF mode index -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic Abs, Count, MatMul, MaxVal, Min, MinVal, Transpose
!
! Executable Statements
!
! Calculate means and standard deviations
  Call moments (nx, nt, x, xm, xsd)
  Call moments (ny, nt, y, ym, ysd)
!
! Calculate anomalies
  Call stdize (3-ieofx, igauss, 1, nx, nt, xm, xsd, x)
  Call stdize (      1, igauss, 1, ny, nt, ym, ysd, y)
!
! Scale by latitude
  If (xfile%idstr == 1) Call latitude_weight (xfile%nfl, xfield(:)%nlt, xfield(:)%region, rlatx(:,:), iusex, nt, x(:,:))
!
! Perform EOF prefiltering
  Call gesdd ('S', nx, nt, x, nx, svx, eofx, nx, tsx, lxt, rwk, lrwk, iwk, ifail)
  If (ifail /= 0) Return
!
! Determine number of non-zero eigenvalues
  npx = Count(svx(1:Min(mpx, nt-1)) > zero)
  If (npx > 2) Then
     If (svx(npx)*svx(npx-2)/svx(npx-1)**2 < tol) npx = npx - 1 ! - check for probable rounding errors -
  End If
!
! Rescale loadings
! - rescale loadings by latitude -
  If (xfile%idstr == 1) Call latitude_weight (xfile%nfl, xfield(:)%nlt, xfield(:)%region, rlatx(:,:), iusex, npx, eofx(:,:))
! - ensure that largest absolute loading is positive -
  If (lpos) Then
     Do ie = 1, npx
        If (Abs(MaxVal(eofx(1:nx,ie))) < Abs(MinVal(eofx(1:nx,ie)))) Then
           eofx(1:nx,ie) = -eofx(1:nx,ie)
           tsx(ie,1:nt) = -tsx(ie,1:nt)
        End If
     End Do
  End If
  If (npx < mpx) Then
     svx(npx+1:mpx) = zero
     tsx(npx+1:mpx,1:nt) = zero
     eofx(1:nx,npx+1:mpx) = zero
  End If
!
! Compute regression coefficients
! - rescale EOF time series -
  Do k = 1, nt
     Do ie = 1, npx
        tsx(ie,k) = tsx(ie,k)/svx(ie)
     End Do
  End Do
! - principal component regression coefficients -
  bz(1:nxe,1:ny) = MatMul(tsx(1:nxe,1:nt), Transpose(y(1:ny,1:nt)))
  If (nxe < lxt) bz(nxe+1:,:) = zero
!
  Return
 End Subroutine do_pcr
!
!
!
 Subroutine pcr_predict (nf, nx, ny, ieofx, lzero, x, b, fcast, &
            nt, nxe, xvp, fsx, nc, tclim1, tclim0, fcastt, yprst)
!
! Calculates predictions given new predictor values
!
! Modules
  Use data_numbers,   Only: zero, one, tol
  Use progress_meter, Only: update_progress_meter
  Use arrays,         Only: xc, xm, xsd, ym
  Use settings,       Only: igauss, istd, istdo
  Use distribs,       Only: gaussian, gaussian_inv, stdize, ustdize, zero_bound
  Use pcs,            Only: eofx, mxe, svx
  Use climate,        Only: ave, sdev
!
! Arguments
!
! Input scalars
  Integer, Intent(In) :: nf    ! - number of forecasts -
  Integer, Intent(In) :: nx    ! - number of X variables -
  Integer, Intent(In) :: ny    ! - number of Y variables -
  Integer, Intent(In) :: ieofx ! - Y EOF option -
!
  Logical, Intent(In) :: lzero ! - apply zero-bound? -
!
! - optional input scalars -
  Integer, Intent(In), Optional :: nt  ! - number of cases in training period -
  Integer, Intent(In), Optional :: nxe ! - number of X EOF modes -
  Integer, Intent(In), Optional :: nc  ! - number of cases in climatology -
!
! Arrays,
!
! Input arrays
  Real(Kind=rp), Dimension(:,:), Intent(In) :: x ! - new predictor values -
  Real(Kind=rp), Dimension(:,:), Intent(In) :: b ! - regression coefficients -
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
  Intrinsic MatMul, Present, Real, Sum
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
     fcast(:,k) = MatMul(xc(:,k,1), b(:,:))+ym(:)
!
! Calculate prediction error variance
     If (Present(xvp)) Then
        xvp(k) = one/df
        Do i = 1, nxe
           If (svx(i) > tol) xvp(k) = xvp(k) + (Sum(eofx(:,i)*xc(:,k,1))/svx(i))**2
        End Do
     End If
!
! Project predictors onto EOFs
     If (Present(fsx)) Then
        Do i = 1, mxe
           If (svx(i) > tol) Then
              fsx(i,k) = Sum(eofx(:,i)*xc(:,k,1))/svx(i)
           Else
              fsx(i,k) = zero
           End If
        End Do
! - rescale EOFs (for output) -
        fsx(1:mxe,k) = fsx(1:mxe,k)*svx(1:mxe)
     End If
     If (update_progress_meter(.false.) /= 0) Return
  End Do
!
! Transform
! - transform from gaussian -
  If (Present(tclim1) .and. Present(nc)) Then
     If (Present(fcastt) .and. .not.Present(yprst)) fcastt(:,:)=fcast(:,:)
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
 End Subroutine pcr_predict
End Module pcr
