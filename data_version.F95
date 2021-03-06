! Author: Simon Mason
Module data_version
!
! Modules
  Use data_numbers, Only: sp
!
! Declarations
!
! Implicit declarations
  Implicit None
!
! Parameters
!
! Version
  Integer, Parameter, Public :: ver_major = 15 ! - major version number -
  Integer, Parameter, Public :: ver_minor = 7  ! - minor version number -
  Integer, Parameter, Public :: ver_patch = 2  ! - patch version number -
!
! Integer parameters
  Integer, Parameter, Private :: lct = 30 ! - length of CPT title -
!
! Character parameters
  Character(Len=*), Parameter, Public :: cpt_email = & ! - CPT's email -
     'cpt@iri.columbia.edu'
  Character(Len=*), Parameter, Public :: cpt_name = &  ! - CPT -
     'Climate Predictability Tool'
  Character(Len=*), Parameter, Public :: cpt_url = &   ! - CPT's URL -
     'http://iri.columbia.edu/CPT/'
  Character(Len=*), Parameter, Public :: date_this = & ! - date of this version -
     '11 December, 2017'
  Character(Len=*), Parameter, Public :: date_ver1 = & ! - date created -
     '29 December, 2002'
!
! Scalars
!
! Real scalars
  Real(Kind=sp), Public :: ver ! - version number -
!
! Character scalars
  Character(Len=lct), Public :: cpt_title ! - CPT title -
  Character(Len=  8), Public :: cpt_ver   ! - CPT version -
  Character(Len=  4), Public :: cyr1      ! - first copyright year -
  Character(Len=  4), Public :: cyr2      ! - last copyright year -
!
End Module data_version
