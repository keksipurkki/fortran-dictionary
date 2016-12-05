module Dictionary_class
  use ISO_FORTRAN_ENV
  use DictEntry_class

#define DICTIONARY_INITIAL_SIZE 8
#define DICTIONARY_RESIZE(n) (((2*n)+1)/3)
#define DICTIONARY_GROWTH_RATE(n) (2*n%in_use + size(n%slot)/2)

  implicit none

  private
  public                                   :: Dictionary
  type(DictTypes), public, parameter       :: DICT = DictTypes()

  type :: Dictionary
    private
    type(DictEntry), allocatable :: slot(:)
    integer                      :: in_use
    integer                      :: pos
    integer                      :: usable
    character(:), allocatable    :: tmp
  contains

    procedure, private  :: entry_in_use => Dictionary_entry_in_use
    procedure, private  :: lookup => Dictionary_lookup
    procedure, private  :: resize => Dictionary_resize
    procedure, private  :: delete_by_idx => Dictionary_delete_by_idx
    procedure, private  :: update_single => Dictionary_update_single

    ! The public API
    procedure           :: size  => Dictionary_size
    procedure           :: insert => Dictionary_insert
    procedure           :: get    => Dictionary_get
    procedure           :: has => Dictionary_has
    procedure           :: rewind => Dictionary_rewind
    procedure           :: next => Dictionary_next
    procedure           :: delete => Dictionary_delete
    procedure           :: clear => Dictionary_clear
    procedure           :: pop => Dictionary_pop
    procedure           :: setdefault => Dictionary_setdefault
    procedure           :: update => Dictionary_update
  end type

  character(len=7), target    :: dummy = '<dummy>'

  interface Dictionary
    module procedure :: Dictionary_init
  end interface

contains

  !> New dictionary
  function Dictionary_init() result(new)
    type(Dictionary) :: new
    allocate(new%slot(DICTIONARY_INITIAL_SIZE))
    new%in_use = 0
    new%pos = 1
    new%usable = DICTIONARY_RESIZE(DICTIONARY_INITIAL_SIZE)
  end function

  !> The number of entries in the dictionary
  function Dictionary_size(self)
    class(Dictionary), intent(in) :: self
    integer(INT32) :: Dictionary_size
    Dictionary_size = self%in_use
  end function

  !> Containment test: check if `key` is in the dictionary
  !!
  !! Has the same cost as the lookup method
  function Dictionary_has(self, key)
    class(Dictionary), intent(in) :: self
    character(*), intent(in) :: key
    logical :: Dictionary_has
    Dictionary_has = self%entry_in_use(self%lookup(key))
  end function

  !> Containment test for private use
  !!
  !! Checks whether the `i`th slot entry is in use
  function Dictionary_entry_in_use(self, i)
    class(Dictionary), intent(in) :: self
    integer(INT32), intent(in) :: i
    logical :: Dictionary_entry_in_use
    Dictionary_entry_in_use = associated(self%slot(i)%val)
  end function

  !> Generic insertion method for scalars and vectors of arbitrary type
  !!
  !! Higher-dimensional data has to be flattened to a vector in order to
  !! insert it in a dictionary.
  subroutine Dictionary_insert(self, key, scalar, vector)
    class(Dictionary)              :: self
    character(*), intent(in)       :: key
    class(*), intent(in), optional :: scalar
    class(*), intent(in), optional :: vector(:)
    integer(INT64)                 :: hashval
    integer                        :: i

    i = self%lookup(key, hashval)

    if (present(scalar) .eqv. present(vector)) then
#ifdef _DEBUG
      write (*,'(A)') "WARNING: Dictionary_insert called with invalid arguments"
#endif
      return
    elseif (present(scalar)) then

      self%tmp = pickle_scalar(scalar, self%slot(i)%itemBytes)
      self%slot(i)%nelems = 1

    elseif (present(vector)) then

      self%tmp = pickle_vector(vector, self%slot(i)%itemBytes)
      self%slot(i)%nelems = size(vector)

    endif

    ! Update the entry

    self%slot(i)%hash = hashval

    if (.not.self%entry_in_use(i)) then
      ! Bookkeeping for a new entry

      if (self%slot(i)%dummy) then
        self%slot(i)%dummy = .false. ! Reusing a dummy entry
        nullify(self%slot(i)%key)
      else
        self%usable = self%usable - 1
      endif

      self%in_use = self%in_use + 1

    endif

    if (associated(self%slot(i)%key)) deallocate(self%slot(i)%key)
    allocate(self%slot(i)%key, source=key)

    if (associated(self%slot(i)%val)) deallocate(self%slot(i)%val)
    allocate(self%slot(i)%val, source=self%tmp)
    deallocate(self%tmp)

    if (self%usable < 1) then
      call self%resize(DICTIONARY_GROWTH_RATE(self))
    endif

  end subroutine

  !>
  subroutine Dictionary_update_single(self, slot)
    class(Dictionary)              :: self
    type(DictEntry), intent(in)    :: slot
    integer(INT64)                 :: hashval
    integer                        :: i

    i = self%lookup(slot%key, hashval)
#ifdef _DEBUG
    if (hashval /= slot%hash) then
      write (*,*) 'ERROR: Corrupted hash values in Dictionary_update_single!'
    endif
#endif

    if (.not.self%entry_in_use(i)) then
      ! Bookkeeping for a new entry

      if (self%slot(i)%dummy) then
        self%slot(i)%dummy = .false. ! Reusing a dummy entry
        nullify(self%slot(i)%key)
      else
        self%usable = self%usable - 1
      endif

      self%in_use = self%in_use + 1

    endif

    if (associated(self%slot(i)%val)) deallocate(self%slot(i)%val)
    if (associated(self%slot(i)%key)) deallocate(self%slot(i)%key)

    self%slot(i) = slot

    if (self%usable < 1) then
      call self%resize(DICTIONARY_GROWTH_RATE(self))
    endif

  end subroutine

  !> Combination of get and insert
  !!
  !! Tries to get the DictEntry matching `key`. If this does not exist,
  !! inserts a new entry for `key` with the value `scalar` or `vector`
  !! and then returns the newly created entry
  function Dictionary_setdefault(self, key, scalar, vector) result(slot)
    class(Dictionary)              :: self
    character(*), intent(in)       :: key
    class(*), intent(in), optional :: scalar
    class(*), intent(in), optional :: vector(:)
    type(DictEntry)                :: slot

    slot = self%get(key)
    if (associated(slot%key)) return

    if (present(scalar) .eqv. present(vector)) then
#ifdef _DEBUG
      write (*,'(A)') "WARNING: Dictionary_setdefault called with invalid arguments"
#endif
      return
    elseif (present(scalar)) then
      call self%insert(key, scalar)
    elseif (present(vector)) then
      call self%insert(key, vector=vector)
    endif

    slot = self%get(key)

  end function

  subroutine Dictionary_update(self, dict)
    class(Dictionary) :: self
    class(Dictionary), intent(in) :: dict
    character(:), allocatable :: key

    call dict%rewind()
    do while(dict%next(key))
      call self%update_single(dict%get(key))
    enddo

  end subroutine

  !> Entry deletion
  !!
  !! Frees memory and marks entries as `dummy`
  !!
  !! This method is used internally by the class as it avoids the overhead of a
  !! lookup call.
  subroutine Dictionary_delete_by_idx(self, i)
    class(Dictionary) :: self
    integer(INT32) :: i

    if (self%entry_in_use(i)) then
      deallocate(self%slot(i)%val)
      if (.not.self%slot(i)%dummy) deallocate(self%slot(i)%key)
      self%slot(i) = DictEntry(key=dummy)
      self%slot(i)%dummy = .true.

      self%in_use = self%in_use - 1

    else
#ifdef _DEBUG
      write (*,'(A)') "WARNING: Tried to delete a non-existing entry in Dictionary_delete_by_idx"
#endif
    endif

  end subroutine

  !> Entry deletion
  subroutine Dictionary_delete(self, key)
    class(Dictionary) :: self
    character(*), intent(in) :: key
    call self%delete_by_idx(self%lookup(key))
  end subroutine

  !> Removes all entries from the dictionary
  !!
  !! In addition, the dictionary is shrunk to its initial
  !! size
  subroutine Dictionary_clear(self)
    class(Dictionary) :: self
    integer :: i

    do i=1, size(self%slot)
      if (.not. self%entry_in_use(i)) cycle
      call self%delete_by_idx(i)
    enddo

    deallocate(self%slot)
    allocate(self%slot(DICTIONARY_INITIAL_SIZE))
    self%in_use = 0
    self%pos = 1
    self%usable = DICTIONARY_RESIZE(DICTIONARY_INITIAL_SIZE)

  end subroutine

  !>
  !!
  function Dictionary_pop(self, key) result(slot)
    class(Dictionary)                     :: self
    character(*), intent(in)              :: key
    type(DictEntry)                       :: slot
    character(:), pointer                 :: key_copy, val_copy
    integer                               :: i

    i = self%lookup(key)
    if (self%entry_in_use(i)) then

      slot = self%slot(i)
      allocate(key_copy, source=self%slot(i)%key)
      allocate(val_copy, source=self%slot(i)%val)
      slot%key => key_copy
      slot%val => val_copy

      call self%delete_by_idx(i)
    endif

  end function

  !> Dictionary get
  !!
  !! This method returns an intermediate DictEntry objects
  !! which contains the key-value pair (entry). The value can be extracted via the
  !! .as. operator, e.g.,
  !!
  !! d%get('key').as.(DICT%INT32)
  !!
  !! That is, to get sensible data out of the get method, you need to know the
  !! type of the `value`.
  !!
  !! For reasons of generality, the .as. operator promotes scalar values to
  !! size-1 vectors.
  !!
  !! Supported TYPE's are enumerated in the public `DICT` derived type.
  !!
  !! It is possible to convert values _between_ the intrinsic types in the
  !! calling code. For example, to get an INT32 integer value from a dictionary as a
  !! REAL32 floating-point number, you need to use real() intrinsic as
  !!
  !!     real(d%get('key').as.(DICT%INT32), kind=REAL32)
  !!
  function Dictionary_get(self, key) result(slot)
    class(Dictionary), intent(in) :: self
    character(*), intent(in)      :: key
    type(DictEntry)               :: slot
    integer(INT32)                :: i

    i = self%lookup(key)
    if (self%entry_in_use(i)) then
      slot = self%slot(i)
    else
#ifdef _DEBUG
      write(*, '("DEBUG: Key `", A, "'' not found!")') key
#endif
    endif

  end function

  !> Dictionary lookup
  !!
  !! Returns the index of either the dictionary slot matching `key` or the first
  !! slot where `key` would be inserted.
  !!
  !! The hash collision resolution algorithm has been adapted from CPython
  !! (Objects/dictobject.c)
  function Dictionary_lookup(self, key, hashval) result(k)
    class(Dictionary)                     :: self
    character(len=*), intent(in)          :: key
    integer(INT64), intent(out), optional :: hashval
    integer                               :: i, k_prev, k
    integer                               :: counter
    integer(INT64), parameter             :: PERTURB_SHIFT = 5
    integer(INT64)                        :: perturb
    integer(INT64)                        :: hashval_

    hashval_ = hash(key)

    if (present(hashval)) hashval = hashval_

    i = modulo(hashval_, size(self%slot))
    k = i + 1

    ! ----Most probable outcomes----

    ! Hit an empty slot on the first try...
    if (.not. self%entry_in_use(k)) then
      return
    endif

    ! ...or successfull lookup on the first try
    if (associated(self%slot(k)%key)) then
      if (self%slot(k)%key == key) then
        return
      endif
    endif

    ! ----There is a hash collision----

    ! Go the trail of backup locations
    perturb = hashval_
    counter = 0
    do
      counter = counter + 1
      k_prev = k
      i = modulo(5*i + perturb + 1, size(self%slot))
      perturb = ishft(perturb, -PERTURB_SHIFT)
      k = i + 1

      if (associated(self%slot(k)%key)) then ! Found a key
        ! Hash collision resolved
        if (self%slot(k)%key == key) then
#ifdef _DEBUG
          print '(A,I0,A)', 'DEBUG: Hash collision resolved in ', counter, ' iteration(s)'
#endif
          return
        endif

      else ! Found a new empty slot

        ! But we return the previous spot if it was a dummy
        if (self%slot(k_prev)%dummy) then
          k = k_prev
#ifdef _DEBUG
          print '(A,I0,A)', 'DEBUG: Replacing a dummy entry. Hash collision resolved in ', counter, ' iteration(s)'
#endif
        endif
        return

      endif

    enddo

  end function

  subroutine Dictionary_resize(self, minused)
    class(Dictionary) :: self
    integer, intent(in) :: minused

    integer(INT64), parameter               :: PERTURB_SHIFT = 5
    integer(INT64)                          :: hashval, perturb
    integer                                 :: i, j, allocstat, newsize
    type(DictEntry), allocatable            :: slot(:)

    ! Resize dynamically
    newsize = 8
    do while(newsize <= minused .and. newsize > 0)
      newsize = 2*newsize
    enddo

    allocate(slot(newsize), stat=allocstat)

    if (allocstat /= 0) stop 'Out of memory'

    do i = 1, size(self%slot)

      if (.not.self%entry_in_use(i)) cycle

      hashval = self%slot(i)%hash

      ! Find the new location for an old entry
      j = modulo(hashval, size(slot))
      perturb = hashval
      do while(associated(slot(j+1)%key))
        j = modulo(5*j + perturb + 1, size(slot))
        perturb = ishft(perturb, -PERTURB_SHIFT)
      enddo
      slot(j+1) = self%slot(i)

    enddo

    deallocate(self%slot)
    self%slot = slot
    self%usable = DICTIONARY_RESIZE(size(self%slot))
#ifdef _DEBUG
    allocstat = 0
    do i = 1, size(self%slot)
      if (self%entry_in_use(i)) then
        allocstat = allocstat + storage_size(self%slot(i)) &
                    + storage_size(self%slot(i)%key) + storage_size(self%slot(i)%val)
      endif
    enddo
    print '("DEBUG: New size: ", I0, " entries (",I0 , " kB)")', size(self%slot), allocstat / (8*1000)
#endif

  end subroutine

  !> Resets the iterator to the beginning
  !!
  !! If you want to iterate over the keys of the dictionary,
  !! you need to call this method before entering the loop.
  !!
  subroutine Dictionary_rewind (self)
    class(Dictionary) :: self
    self%pos = 1
  end subroutine

  !> Iterator over the keys of the dictionary
  !!
  !! The keys are returned in an arbitrary order
  !!
  function Dictionary_next(self, key)
    class(Dictionary)  :: self
    logical :: Dictionary_next
    character(:), allocatable, intent(out) :: key

    do while(.not.self%entry_in_use(self%pos) .and. self%pos <= size(self%slot))
      self%pos = self%pos + 1
    enddo

    if (self%pos <= size(self%slot)) then
      Dictionary_next = .true.
      key = self%slot(self%pos)%key
    else
      Dictionary_next = .false.
    endif

    self%pos = self%pos + 1

  end function

  !> Hash function for strings
  !!
  !! Adapted from CPython
  function hash(str)
    character(len=*), intent(in) :: str
    integer(INT64) :: hash

    integer, parameter :: HASH_MULTIPLIER = 1000003
    integer :: i
    integer(INT64) :: lenstr

    lenstr = len(str)
    if (lenstr==0) then
      hash = 0
      return
    endif
    hash = ishft(ichar(str(1:1)), 7)
    do i=1, lenstr
      hash = ieor((HASH_MULTIPLIER * hash), ichar(str(i:i)))
    enddo
    hash = ieor(hash, lenstr)
    if (hash == -1) hash = -2

  end function

  function pickle_scalar(scalar, item_size)
    class(*), intent(in) :: scalar
    character(:), allocatable :: pickle_scalar
    integer, intent(out) :: item_size

    select type (scalar)
    type is (character(len=*))
      pickle_scalar = scalar
      item_size = len(scalar)
    class default
      item_size = storage_size(scalar)/8
      allocate(character(item_size) :: pickle_scalar)
      pickle_scalar = transfer(scalar, pickle_scalar)
    end select

  end function

  function pickle_vector(vec, item_size)
    class(*), intent(in) :: vec(:)
    character(:), allocatable :: pickle_vector
    integer, intent(out) :: item_size
    integer :: nbytes

    ! Picks the storage_size of the first element
    item_size = storage_size(vec) / 8
    nbytes = size(vec) * item_size

    allocate(character(nbytes) :: pickle_vector)
    pickle_vector = transfer(vec, pickle_vector)

  end function

end module

program test
  use ISO_FORTRAN_ENV
  use Dictionary_class
  implicit none

  type(Dictionary) :: d
  character(len=256) :: word
  character(len=:), allocatable :: key
  integer :: io, i

  d = Dictionary()

  do i=1, 2

    open(32, file='/usr/share/dict/words')

    do
      read(32, '(A)', iostat=io) word
      if (io < 0) exit
      call d%insert(trim(word), "A DUMMY TEST STRING")
    enddo

    close(32)
    print *, d%size()

    call d%rewind()
    do while(d%next(key))
      print *, key, ': ', d%get(key).as.(DICT%string)
    enddo

    open(32, file='/usr/share/dict/words')

    do
      read(32, '(A)', iostat=io) word
      if (io < 0) exit
      call d%insert(trim(word), vector=[1.0d0, 2.0d0, 4.0d0])
    enddo

    close(32)

    call d%rewind()
    do while(d%next(key))
      print *, key, ': ', d%get(key).as.(DICT%REAL64)
    enddo

  enddo

  stop

end program
