! Author: Simon Mason
Module cpt_view
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: change_view, set_view
!
Contains
!
!
 Function change_view()
!
! Changes analysis option
!
! Modules
  Use data_cpt_constants, Only: ia_cca, ia_dia, ia_for, ia_pcr, ia_mlr, ia_gcm, ia_pfv, mnt
  Use labels,             Only: cls_l
  Use gui,                Only: iyeof, &
                                set_viewflags, set_win_title, window_update
  Use settings,           Only: iel, iensc, iensc_bk, igauss, igauss_bk, igcms, iretro, istd, istd_bk, isynch, mnu
  Use iofiles,            Only: xfile
  Use pcs,                Only: ieofx, ieofx_bk, npx, npy
  Use categories,         Only: ithr, ithr_bk
  Use climate,            Only: ieclim, ieclim_bk
  Use season,             Only: icsv
  Use analysis,           Only: ianal, ianaln
!
! Function type
  Integer :: change_view
!
! Executable Statements
!
! Convert from beginner mode
  change_view = 2
  Select Case (ianaln)
   Case (ia_dia, ia_for)
     If (ianal == ia_pfv) Then
        ianaln = ia_cca
     Else
        ianaln = ianal
     End If
     If (ianaln == ia_cca) Then
        iyeof = 1
     Else
        iyeof = 0
     End If
     If (ianal == ia_dia) Then
        isynch = 1
     Else
        isynch = 0
     End If
!     jdia=1-isynch
!     jfor=isynch
  End Select
!
! Return If no change in analysis
  If (ianaln == ianal) Return
!
! Update CPT window
  Select Case (ianaln)
   Case (ia_cca, ia_pcr) ! - CCA and PCR -
     ieofx = ieofx_bk
     If (ianal == ia_pfv) Then
        igauss = igauss_bk
        ieclim = ieclim_bk
        istd = istd_bk
        ithr = ithr_bk
     End If
     If (ianal == ia_gcm) iensc_bk = iensc
     iensc = 1
     mnu = mnt
   Case (ia_mlr) ! - MLR -
     If (ianal == ia_cca) ieofx_bk = ieofx
     ieofx = 2
     If (ianal == ia_pfv) Then
        igauss = igauss_bk
        ieclim = ieclim_bk
        istd = istd_bk
        ithr = ithr_bk
     End If
     If (ianal == ia_gcm) iensc_bk = iensc
     iensc = 1
     mnu = mnt
   Case (ia_gcm) ! - GCM -
     If (ianal == ia_cca) ieofx_bk = ieofx
     ieofx = 2
     npx = 0
     npy = 0
     If (ianal == ia_pfv) Then
        ieclim = ieclim_bk
        istd = istd_bk
        ithr = ithr_bk
     End If
     iensc = iensc_bk
     mnu = mnt
   Case (ia_pfv) ! - PFV -
     If (ianal == ia_cca) ieofx_bk = ieofx
     ieofx = 2
     igauss_bk = igauss
     igauss = 0
     npx = 0
     npy = 0
     ieclim_bk = ieclim
     ieclim = 1
     If (ianal == ia_gcm) iensc_bk = iensc
     iensc = 1
     istd_bk = istd
     istd = 0
     ithr_bk = ithr
     ithr = 1
     mnu = 1
  End Select
  ianal = ianaln
  Call set_viewflags (ianal, istd, xfile%idstr, igcms, icsv, iel, isynch, &
                      iretro=iretro)
  Call set_win_title (ianal)
!
! Update lags/seasons label
  Call window_update (cls_l)
!
  Return
 End Function change_view
!
!
!
 Function set_view()
!
! Sets analysis option
!
! Modules
  Use data_cpt_constants, Only: ia_mlr, ia_gcm, ia_pfv, mnt
  Use settings,           Only: igauss, istd, mnu
  Use pcs,                Only: ieofx
  Use climate,            Only: ieclim
  Use analysis,           Only: ianal
!
! Function type
  Integer :: set_view
!
! Executable Statements
!
! Set X EOF option for MLR, GCM, and PFV
  set_view = 0
  If (ianal == ia_mlr .or. ianal == ia_gcm .or. ianal == ia_pfv) Then
     ieofx = 2
!
! Switch on extended climatology, and switch off standardization and transformation flags, for PFV
     If (ianal == ia_pfv) Then 
        ieclim = 1
        igauss = 0
        istd = 0
        mnu = 1
     End If
  End If
  If (ianal /= ia_pfv) mnu = mnt
!
  Return
 End Function set_view
End Module cpt_view
