!> DictEntry: An intermediate object returned by the get method the Dictionary class
!!
!! Can be converted to intrinsic types via an .as. operator
!!
module DictEntry_class
  use ISO_FORTRAN_ENV
  implicit none
  private

#define NELEMS(x) (8*self%nbytes/storage_size(x))
#define BYTE CHARACTER_STORAGE_SIZE/8

  public :: DictEntry
  public :: DictTypes

  type :: DictTypes
    real(REAL64)    :: real64 = 1.0_REAL64
    real(REAL32)    :: real32 = 1.0_REAL32
    integer(INT64)  :: int64 = 1_INT64
    integer(INT32)  :: int32 = 1_INT32
    integer(INT16)  :: int16 = 1_INT16
    integer(INT8)   :: int8 = 1_INT8
    character(BYTE) :: string = 'a'
    character(BYTE) :: lines(1) = ['a']
    logical         :: logical = .true.
  end type

  type :: DictEntry
    character(:), pointer          :: key => NULL()
    character(:), pointer          :: val => NULL()
    logical                        :: dummy = .false.
    integer(INT64)                 :: hash = 0
    integer                        :: nelems = 0
    integer                        :: itemBytes = 0
  contains
    procedure, private  :: as_string => DictEntry_as_string
    procedure, private  :: as_lines => DictEntry_as_lines
    !procedure, private  :: as_real128 => DictEntry_as_real128
    procedure, private  :: as_real64 => DictEntry_as_real64
    procedure, private  :: as_real32 => DictEntry_as_real32
    procedure, private  :: as_int64 => DictEntry_as_int64
    procedure, private  :: as_int32 => DictEntry_as_int32
    procedure, private  :: as_int16 => DictEntry_as_int16
    procedure, private  :: as_int8 => DictEntry_as_int8
    procedure, private  :: as_logical => DictEntry_as_logical
    generic             :: operator(.as.) => as_string,&
                                             as_lines,&
                                             as_real64, &
                                             as_real32, &
                                             as_int64,  &
                                             as_int32,  &
                                             as_int16,  &
                                             as_int8,  &
                                             as_logical

  end type

contains

  ! Works for arbitrary byte sequences,too
  function DictEntry_as_string(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    character(BYTE), intent(in) :: kind
    character(self%itemBytes) :: val
    if (associated(self%key)) then
      val = self%val
    endif
  end function

  function DictEntry_as_lines(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    character(BYTE), intent(in), dimension(1) :: kind
    character(self%itemBytes), allocatable :: val(:)
    if (associated(self%key)) then
      val = transfer(self%val, mold=val, size=self%nelems)
    endif
  end function

  function DictEntry_as_real64(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    real(REAL64), intent(in) :: kind
    real(REAL64) :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

  function DictEntry_as_real32(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    real(REAL32), intent(in) :: kind
    real(REAL32) :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

  function DictEntry_as_int64(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    integer(INT64), intent(in) :: kind
    integer(INT64) :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

  function DictEntry_as_int32(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    integer(INT32), intent(in) :: kind
    integer(INT32) :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

  function DictEntry_as_int16(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    integer(INT16), intent(in) :: kind
    integer(INT16) :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

  function DictEntry_as_int8(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    integer(INT8), intent(in) :: kind
    integer(INT8) :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

  function DictEntry_as_logical(self, kind) result(val)
    class(DictEntry), intent(in) :: self
    logical, intent(in) :: kind
    logical :: val(self%nelems)
    if (associated(self%key)) then
      val = transfer(self%val, mold=kind, size=self%nelems)
    endif
  end function

end module
