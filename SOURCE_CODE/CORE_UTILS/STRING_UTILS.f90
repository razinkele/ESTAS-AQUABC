
!==================================================================
module para_aqua
!==================================================================

     implicit none

     !private
     !public :: para_get_fill , para_exists_name

     integer, parameter     :: ndim = 1000

     integer, save          :: nfill = 0
     double precision, save :: values(ndim)
     character*80, save     :: names(ndim)

!==================================================================
contains
!==================================================================

    function para_index_value(name)
        implicit none
	integer para_index_value
        character*(*) name
	integer i
	do i=1,nfill
	  if( names(i) == name ) then
	    para_index_value = i
	    return
          end if
	end do
	para_index_value = 0
    end function

    function para_get_fill()
        implicit none
	integer para_get_fill
	para_get_fill = nfill
    end function

    function para_exists_name(name)
        implicit none
	logical para_exists_name
        character*(*) name
	if( para_index_value(name) == 0 ) then
	  para_exists_name = .false.
	else
	  para_exists_name = .true.
	end if
    end function

    subroutine para_get_name(i,name)
        implicit none
	integer i
        character*(*) name
	name = names(i)
    end subroutine

    subroutine para_get_value(name,value)
        implicit none
        character*(*) name
        double precision value
	integer i
	i = para_index_value(name)
	if( i == 0 ) then
	  write(6,*) 'name = ',trim(name)
	  stop 'error stop para_get_value: name does not exist'
	end if
	value = values(i)
    end subroutine

    subroutine para_add_value(name,value)
        implicit none
        character*(*) name
        double precision value
	integer i
	i = para_index_value(name)
	if( i > 0 ) then
	  write(6,*) 'name = ',trim(name)
	  stop 'error stop para_add_value: name already exists'
	end if
	nfill = nfill + 1
	if( nfill > ndim ) then
	  stop 'error stop para_add_value: nfill > ndim'
	end if
	names(nfill) = name
	values(nfill) = value
    end subroutine

    subroutine para_put_value(name,value)
        implicit none
        character*(*) name
        double precision value
	integer i
	i = para_index_value(name)
	if( i == 0 ) then
	  write(6,*) 'name = ',trim(name)
	  stop 'error stop para_put_value: name does not exist'
	end if
	names(i) = name
	values(i) = value
    end subroutine

    subroutine para_insert_value(name,value)
        implicit none
        character*(*) name
        double precision value
	if( para_exists_name(name) ) then
          call para_put_value(name,value)
	else
          call para_add_value(name,value)
	end if
    end subroutine

!==================================================================
end module para_aqua
!==================================================================
    
function getpar(name)
    use para_aqua
    implicit none
    double precision getpar
    character*(*) name
    double precision value
    call para_get_value(name,value)
    getpar = value
end function getpar

!==================================================================

