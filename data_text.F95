! Author: Simon Mason
Module data_text
!
! Declarations
!
! Implicit declarations
  Implicit None
!
! Parameters
!
! Integer parameters
  Integer, Parameter, Public ::  llab = 64 ! - length of label text -
  Integer, Parameter, Public :: nlang = 4  ! - number of languages -
  Integer, Parameter, Public ::  nlet = 26 ! - number of letters -
!
  Integer, Parameter, Private :: lla = 3 ! - length of language abbreviation -
!
! Character parameters
#if GUI == 1
  Character(Len=*), Parameter, Public :: c_akey = '&' ! - accelerator key -
#else
  Character(Len=*), Parameter, Public :: c_akey = ' ' ! - accelerator key -
#endif
!
  Character(Len=*), Dimension(nlang), Parameter, Public :: clang = & ! - languages -
     (/'English  ', &
       'Español  ', &
       'Français ', &
       'Português'/)
  Character(Len=lla), Dimension(nlang), Parameter, Public :: clang_a = & ! - language abbreviations -
     (/'eng', &
       'esp', &
       'fra', &
       'por'/)
#if GUI == 1
  Character(Len=*), Dimension(nlang), Parameter, Public :: cs_langlist = & ! - language list -
     (/'&English  ', &
       'E&spañol  ', &
       '&Français ', &
       '&Português'/)
#endif
!
! Scalars
!
! Integer scalars
  Integer, Public :: ilang     ! - current language -
  Integer, Public :: iachara_l ! - 'a' in collating sequence -
  Integer, Public :: iachara_u ! - 'A' in collating sequence -
  Integer, Public :: iacharz_l ! - 'z' in collating sequence -
  Integer, Public :: iacharz_u ! - 'Z' in collating sequence -
  Integer, Public :: idchara   ! - difference between 'A' and 'a' in collating sequence -
!
  Character(Len=1), Public :: c_tab ! - tab -
!
End Module data_text
