module output_config_module

  use iso_fortran_env

  implicit none

  type :: variable_config
    character(len=20) :: var_name
    logical :: active
    character(len=20), dimension(:), allocatable :: agg_methods
  end type variable_config

  type :: group_config
    character(len=20) :: group_name
    logical :: active
    character(len=20), dimension(:), allocatable :: agg_methods
    type(variable_config), dimension(:), allocatable :: variables
  contains
    procedure :: apply_to_all => apply_to_all_group
    procedure :: get_variable => get_variable_group
  end type group_config

  type :: module_config
    character(len=20) :: module_name
    logical :: active
    character(len=20), dimension(:), allocatable :: agg_methods
    type(group_config), dimension(:), allocatable :: groups
  contains
    procedure :: apply_to_all => apply_to_all_module
    procedure :: set_module_options
    procedure :: set_group_options
    procedure :: set_variable_options
    procedure :: assign_group
    procedure :: assign_variable
    procedure :: get_group
    procedure :: get_variable => get_variable_module
  end type module_config

contains

!---------- Cascading apply methods -----------!

  subroutine apply_to_all_group(this)
    class(group_config), intent(inout) :: this

    integer :: i

    do i = 1, size(this%variables)
      this%variables(i)%agg_methods = this%agg_methods
      this%variables(i)%active = this%active
    end do
  end subroutine apply_to_all_group

  subroutine apply_to_all_module(this)
    class(module_config), intent(inout) :: this

    integer :: i

    do i = 1, size(this%groups)
      this%groups(i)%agg_methods = this%agg_methods
      this%groups(i)%active = this%active
      call this%groups(i)%apply_to_all()
    end do

  end subroutine apply_to_all_module

!---------- Level apply methods -----------!

  subroutine set_module_options(this, output_def)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: output_def

    integer :: sep_loc

    ! Assume it's on, unless it's specified off
    this%active = index(output_def, "off") == 0

    sep_loc = index(output_def, "|")
    this%agg_methods = split(output_def(sep_loc+1:), ",")

    call this%apply_to_all()

  end subroutine set_module_options

  subroutine set_group_options(this, output_def)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: output_def

    integer :: sep_loc
    character(len=:), allocatable :: group_name
    type(group_config) :: current_group
    character(len=50), dimension(2) :: split_def

    sep_loc = index(output_def, "|")

    split_def = split(output_def(:sep_loc-1), ":")
    group_name = trim(split_def(2))

    ! Check if it's active
    current_group%active = index(output_def, "off") == 0
    current_group%agg_methods = split(output_def(sep_loc+1:), ",")

    call this%assign_group(group_name, current_group)

  end subroutine set_group_options

  subroutine set_variable_options(this, output_def)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: output_def

    integer :: sep_loc
    character(len=:), allocatable :: var_name
    type(variable_config) :: current_var
    character(len=50), dimension(2) :: split_def

    sep_loc = index(output_def, "|")

    split_def = split(output_def(:sep_loc-1), ":")
    var_name = trim(split_def(2))

    ! We have to assign the values to the local, and then assign to the module config
    current_var%active = index(output_def, "off") == 0
    current_var%agg_methods = split(output_def(sep_loc+1:), ",")

    call this%assign_variable(var_name, current_var)

  end subroutine set_variable_options

!---------- Replace existing definitions -----------!

  subroutine assign_group(this, group_name, group)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: group_name
    type(group_config), intent(inout) :: group

    integer :: i, j

    group%group_name = group_name
    do i = 1, size(this%groups)
      if (trim(this%groups(i)%group_name) == trim(group_name)) then
        group%variables = this%groups(i)%variables
        this%groups(i) = group
        do j = 1, size(this%groups(i)%variables)
          this%groups(i)%variables(j)%active = group%active
          this%groups(i)%variables(j)%agg_methods = group%agg_methods
        end do
        exit
      end if
    end do

  end subroutine assign_group

  subroutine assign_variable(this, var_name, var)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    type(variable_config), intent(inout) :: var

    integer :: i, j

    var%var_name = var_name
    do i = 1, size(this%groups)
      do j = 1, size(this%groups(i)%variables)
        if (trim(this%groups(i)%variables(j)%var_name) == trim(var_name)) then
          this%groups(i)%variables(j) = var
          exit
        end if
      end do
    end do

  end subroutine assign_variable

!---------- Retrieve definitions -----------!

  function get_variable_group(this, var_name)
    class(group_config), intent(inout) :: this
    character(len=*), intent(in) :: var_name

    type(variable_config) :: get_variable_group

    integer :: i

    do i = 1, size(this%variables)
      if (trim(this%variables(i)%var_name) == trim(var_name)) then
        get_variable_group = this%variables(i)
        exit
      end if
    end do

  end function get_variable_group

  function get_variable_module(this, var_name)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: var_name

    type(variable_config) :: get_variable_module

    integer :: i, j

    logical :: found

    found = .false.

    do i = 1, size(this%groups)
      do j = 1, size(this%groups(i)%variables)
        if (trim(this%groups(i)%variables(j)%var_name) == trim(var_name)) then
          get_variable_module = this%groups(i)%variables(j)
          found = .true.
          exit
        end if
      end do
    end do

    if (.not. found) then
      write(error_unit,*) "Variable ", var_name, " is not a valid output variable."
      stop 1
    end if

  end function get_variable_module

  function get_group(this, group_name)
    class(module_config), intent(inout) :: this
    character(len=*), intent(in) :: group_name

    type(group_config) :: get_group

    integer :: i

    logical :: found

    found = .false.

    do i = 1, size(this%groups)
      if (trim(this%groups(i)%group_name) == trim(group_name)) then
        get_group = this%groups(i)
        found = .true.
        exit
      end if
    end do

    if (.not. found) then
      write(error_unit,*) "Group ", group_name, " is not a valid output group."
      stop 1
    end if

  end function get_group

  function new_variable(var_name)
    character(len=*), intent(in) :: var_name

    type(variable_config) :: new_variable

    new_variable%var_name = var_name

  end function new_variable

  function new_group(group_name, variables)
    character(len=*), intent(in) :: group_name
    type(variable_config), dimension(:), intent(in) :: variables

    type(group_config) :: new_group

    new_group%group_name = group_name
    new_group%variables = variables

  end function new_group

  function new_module(module_name, groups)
    character(len=*), intent(in)  :: module_name
    type(group_config), dimension(:), intent(in) :: groups

    type(module_config) :: new_module

    new_module%module_name = module_name
    new_module%groups = groups

  end function new_module
  
  subroutine process_config_file(file_name, config)
    character(len=*), intent(in) :: file_name
    type(module_config), intent(inout) :: config

    character(len=200) :: module_def
    character(len=200), dimension(:), allocatable :: group_defs
    character(len=200), dimension(:), allocatable :: var_defs
    character(len=200) :: tmp_line, io_msg

    integer :: ctr, funit, ios, max_lines

    ! Hierarchy of configurations, from top down:
    ! 1. Module level configuration- only one module per config file.
    ! 2. Group level configuration- any number of groups, all under the module
    !     configuration.
    ! 3. Variable level configuration- any number of variables, all under a
    !     specific group configuration.

    ! Process is:
    ! 1. Read the config file, and separate directives into module, group
    !    and variable level directives
    ! 2. Apply the module level directives, and apply to everything below it in the tree.
    ! 3. Apply the group level directives, and apply to everything below the group in the tree.
    ! 4. Apply the variable level directives. This is the lowest level, nothing below.

    max_lines = 10000
    open(newunit=funit, file=file_name, status="old", action="read")

    read_file: do ctr = 1, max_lines
      read(funit, '(A)', iostat=ios, iomsg=io_msg) tmp_line

      if (ios < 0) then
        exit read_file
      elseif (ios /= 0) then
        stop 1
      end if

      if (tmp_line(1:8) == "variable") then
        var_defs = [var_defs, tmp_line]
      elseif (tmp_line(1:5) == "group") then
        group_defs = [group_defs, tmp_line]
      elseif (tmp_line(1:6) == "module") then
        module_def = tmp_line
      else
        write(error_unit, '(A)') 'Unrecognised line in output config.'
        stop 1
      end if
    end do read_file

    close(funit)

    call config%set_module_options(trim(module_def))
    do ctr = 1, size(group_defs)
      call config%set_group_options(trim(group_defs(ctr)))
    end do

    do ctr = 1, size(var_defs)
      call config%set_variable_options(trim(var_defs(ctr)))
    end do

  end subroutine process_config_file

  function split(str, sep)
    character(len=*), intent(in) :: str, sep

    character(len=50), dimension(:), allocatable :: split
    integer :: pos, i, n
    character(len=:), allocatable :: buffer

    pos = 0
    n = 1
    buffer = str

    do
      pos = index(buffer, sep)
      if (pos == 0) then
        exit
      end if
      buffer = buffer(pos+len(sep):)
      n = n + 1
    end do

    allocate(split(n))
    buffer = str

    do i = 1, n-1
      pos = index(buffer, sep)
      split(i) = buffer(1:pos-1)
      buffer = buffer(pos+len(sep):)
    end do

    split(n) = buffer

  end function split

end module output_config_module
