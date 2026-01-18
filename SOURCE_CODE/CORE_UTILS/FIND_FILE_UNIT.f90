
!*******************************************************
! finds unit to open file - starts to search from iunit
! on return iunit is either the next unit available
! or it is 0 which means there was an error
!*******************************************************
subroutine find_unit(iunit)
    implicit none

    integer iunit

    logical opened
    integer, save :: iumax = 1000
    integer iu

    iu = iunit

    if (iu .le. 0) then
	    iu = 20                 !set standard unit
	end if

	iunit = 0

    do
	    !safeguard units 5 and 6
        if ( iu .eq. 5 ) then 
		    iu = 7
        end if
		
		if( iu > iumax ) then
            write(6,*) 'no unit available to open file: ',iu
            return
        end if

        inquire(iu,opened=opened)
        
		if( .not. opened )then 
            exit
	    end if

        iu = iu + 1
    end do

    iunit = iu

end

!*******************************************************

