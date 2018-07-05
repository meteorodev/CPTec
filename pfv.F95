! Author: Simon Mason
Module pfv
!
! Implicit declarations
  Implicit None
!
! Accessibility
  Private
  Public :: perform_pfv
!
Contains
!
!
 Function perform_pfv(ierr)
!
! Performs Probabilistic Forecast Verification
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail =  7 No matching grids
!    ifail = 11 No matching X field for the ierr-th Y field
!    ifail = 12 Non-matching station/index name for the ierr-th Y field
!    ifail = 13 Non-matching latitude for the ierr-th Y field
!    ifail = 14 Non-matching latitude for the ierr-th Y field
!    ifail = -1 Terminated
!
! Modules
  Use data_numbers,       Only: rp, one, oneh, tolp
  Use data_cpt_constants, Only: ng
  Use progress_meter,     Only: set_progress_increment
  Use arrays,             Only: iusex, iusey, x
  Use settings,           Only: iretro, izero, mya, ncv, nret, nretro, nu, nur, nu1
  Use iofiles,            Only: xfile, yfile
  Use space,              Only: get_interpolated, set_nearest_grids
  Use fields,             Only: imfs, xfield, yfield, &
                                match_fields
  Use categories,         Only: pobs, rfps, rodds, &
                                calc_odds
  Use analysis,           Only: ianal, &
                                init_analysis_pfv, num_calcs
!
! Function type
  Integer :: perform_pfv
!
! Arguments
!
! Ouput scalars
  Integer, Intent(Out) :: ierr ! - unmatched field -
!
! Locals
!
! Local scalars
  Integer :: i     ! - location index -
  Integer :: k     ! - case index -
  Integer :: nlt   ! - number of latitudes -
  Integer :: nlg   ! - number of longitudes -
  Integer :: ifail ! - error indicator -
!
  Real(Kind=rp) :: sp ! - sum of probabilities -
!
  Logical :: lpa ! - probabilities available? -
!
! Functions and Subroutines
!
! Intrinsic functions
  Intrinsic MaxVal, Real, Sum
!
! Executable Statements
!
! Initialise analysis
  perform_pfv = -1
  ierr = 0
  Select Case (yfile%idstr)
   Case (1)
     nlt = MaxVal(yfield(:)%region%nlts)
     nlg = MaxVal(yfield(:)%region%nlgs)
   Case Default
     nlt = MaxVal(yfield(:)%nva)
     nlg = nlt
  End Select
  Call init_analysis_pfv (ng, nu, nlt, nlg, yfile%nfl, ifail)
  If (ifail /= 0) Then
     perform_pfv = 1
     Return
  End If
  ncv = nu
!
! Check for matching fields
  Call match_fields (xfile%idstr, xfile%nfl, yfile%nfl, imfs, ierr, ifail)
  If (ifail /= 0) Then
      perform_pfv = ifail + 10
      Return
  End If
!
! Estimate number of calculations
  Call set_progress_increment (one/Real(num_calcs(ianal, iretro, 0, izero, 1, mya, nu, nret, nretro, nu1, nur, 0), Kind=rp), &
       ladjust=.true.)
!
! Identify nearest grids
  If (xfile%idstr == 1) Then
     Call set_nearest_grids (yfile%idstr, 0, xfile%nfl, yfile%nfl, xfield(:)%nlt, xfield(:)%nlg, yfield(1:)%nva, &
          xfield(:)%region, yfield(1:)%region, ifail, &
          imfs=imfs)
     Select Case (ifail)
      Case (0)
        Continue
      Case (-1)
        Return
      Case Default  
        perform_pfv = ifail + 2
        Return
     End Select
  End If
!
! Calculate interpolated values
  Call get_interpolated (ianal, xfile%idstr, yfile%idstr, 0, mya, iusex, iusey, xfile%nfl, yfile%nfl, nu, xfile%ngs, &
       xfield(:)%nv, yfield(1:)%nva, x, xfield(:)%rmiss, xfield(:)%region, yfield(1:)%region, rfps,                  &
       imfs=imfs)
!
! Reset any missing forecast probabilities
  lpa = .false.
  Do i = 1, mya
     Do k = 1, nu
        sp = Sum(rfps(i,k,:))
        If (((sp > one-tolp) .and. (sp < one+tolp)) .or. ((sp/oneh > one-tolp) .and. (sp/oneh < one+tolp))) Then
           lpa = .true.
        Else
           rfps(i,k,:) = -one
        End If
     End Do
  End Do
  If (.not.lpa) Then
     perform_pfv = 7
     Return
  End If
!
! Calculate odds and relative odds
  Call calc_odds (nu, ng, mya, pobs(:,:), rfps(:,:,:), rodds(:,:,:,:))
  perform_pfv = 0
!
  Return
 End Function perform_pfv
End Module pfv
