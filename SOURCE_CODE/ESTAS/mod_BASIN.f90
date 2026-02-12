module BASIN

    use GLOBAL
    use INTERPOLATE
    implicit none

    type, public :: PELAGIC_BASIN_DS
        integer :: PELAGIC_BASIN_ID
        integer :: NUM_PELAGIC_BOXES
        integer :: BATHYMETRY_NO
        integer, pointer, dimension(:) :: PELAGIC_BOXES
    end type PELAGIC_BASIN_DS

    type, public :: BATHYMETRY_DS
        integer :: ID_NUM
        integer :: NUM_LAYERS

        real(kind = DBL), pointer, dimension(:) :: UPPER_ELEVATION
        real(kind = DBL), pointer, dimension(:) :: LOWER_ELEVATION
        real(kind = DBL), pointer, dimension(:) :: UPPER_AREA
        real(kind = DBL), pointer, dimension(:) :: LOWER_AREA
        real(kind = DBL), pointer, dimension(:) :: UPPER_LENGTH
        real(kind = DBL), pointer, dimension(:) :: LOWER_LENGTH

        integer :: NUM_DISCONTS
        integer, pointer, dimension(:) :: DISCONT_LAYERS

        real(kind = DBL), pointer, dimension(:)   :: DISCONT_ELEVATIONS
    end type BATHYMETRY_DS

contains

    subroutine ALLOCATE_BASIN_DATA(PELAGIC_BASIN_DATA, NUM_PELAGIC_BOXES)
        implicit none
        type(PELAGIC_BASIN_DS), intent(inout) :: PELAGIC_BASIN_DATA 
        integer, intent(in) :: NUM_PELAGIC_BOXES
        allocate(PELAGIC_BASIN_DATA % PELAGIC_BOXES(NUM_PELAGIC_BOXES))
        PELAGIC_BASIN_DATA % NUM_PELAGIC_BOXES = NUM_PELAGIC_BOXES
    end subroutine ALLOCATE_BASIN_DATA 


    subroutine DEALLOCATE_BASIN_DATA(PELAGIC_BASIN_DATA)
        implicit none
        type(PELAGIC_BASIN_DS), intent(inout) :: PELAGIC_BASIN_DATA 

        deallocate(PELAGIC_BASIN_DATA % PELAGIC_BOXES)
        PELAGIC_BASIN_DATA % NUM_PELAGIC_BOXES = 0
    end subroutine DEALLOCATE_BASIN_DATA 


    subroutine ALLOCATE_BATHYMETRY_DATA(BATHYMETRY_DATA, NUM_LAYERS)

        implicit none
        type(BATHYMETRY_DS), intent(inout) :: BATHYMETRY_DATA
        integer, intent(in) :: NUM_LAYERS

        allocate(BATHYMETRY_DATA % UPPER_ELEVATION(NUM_LAYERS))
        allocate(BATHYMETRY_DATA % LOWER_ELEVATION(NUM_LAYERS))

        allocate(BATHYMETRY_DATA % UPPER_AREA(NUM_LAYERS))
        allocate(BATHYMETRY_DATA % LOWER_AREA(NUM_LAYERS))

        allocate(BATHYMETRY_DATA % UPPER_LENGTH(NUM_LAYERS))
        allocate(BATHYMETRY_DATA % LOWER_LENGTH(NUM_LAYERS))

        BATHYMETRY_DATA % NUM_LAYERS   = NUM_LAYERS
        BATHYMETRY_DATA % NUM_DISCONTS = 0

        BATHYMETRY_DATA % UPPER_ELEVATION = 0.0D0
        BATHYMETRY_DATA % LOWER_ELEVATION = 0.0D0

        BATHYMETRY_DATA % UPPER_AREA = 0.0D0
        BATHYMETRY_DATA % LOWER_AREA = 0.0D0

        BATHYMETRY_DATA % UPPER_LENGTH = 0.0D0
        BATHYMETRY_DATA % LOWER_LENGTH = 0.0D0

    end subroutine ALLOCATE_BATHYMETRY_DATA


    subroutine DEALLOCATE_BATHYMETRY_DATA(BATHYMETRY_DATA)

        implicit none
        type(BATHYMETRY_DS), intent(inout) :: BATHYMETRY_DATA

        deallocate(BATHYMETRY_DATA % UPPER_ELEVATION)
        deallocate(BATHYMETRY_DATA % LOWER_ELEVATION)

        deallocate(BATHYMETRY_DATA % UPPER_AREA)
        deallocate(BATHYMETRY_DATA % LOWER_AREA)

        deallocate(BATHYMETRY_DATA % UPPER_LENGTH)
        deallocate(BATHYMETRY_DATA % LOWER_LENGTH)

        deallocate(BATHYMETRY_DATA % DISCONT_ELEVATIONS)
        deallocate(BATHYMETRY_DATA % DISCONT_LAYERS)

        BATHYMETRY_DATA % NUM_LAYERS   = 0
        BATHYMETRY_DATA % NUM_DISCONTS = 0
    end subroutine DEALLOCATE_BATHYMETRY_DATA


    subroutine READ_BATHYMETRY_DATA_FROM_FILE(BATHYMETRY_DATA, INPUT_NO)
        implicit none

        type(BATHYMETRY_DS), intent(inout) :: BATHYMETRY_DATA
        integer, intent(in) :: INPUT_NO

        real(kind = DBL) :: UPPER_ELEVATION
        real(kind = DBL) :: LOWER_ELEVATION
        real(kind = DBL) :: UPPER_AREA
        real(kind = DBL) :: LOWER_AREA
        real(kind = DBL) :: UPPER_LENGTH
        real(kind = DBL) :: LOWER_LENGTH

        integer :: LAYER_NO
        integer :: DISCONT_NO
        integer :: NUM_LAYERS

        integer :: i

        read (unit = INPUT_NO, fmt = *)

        read (unit = INPUT_NO, fmt = *)
        read (unit = INPUT_NO, fmt = *) NUM_LAYERS
        call ALLOCATE_BATHYMETRY_DATA(BATHYMETRY_DATA, NUM_LAYERS)
        read (unit = INPUT_NO, fmt = *)

        do i = 1, NUM_LAYERS
            read (unit = INPUT_NO, fmt = *) &
                  LAYER_NO, UPPER_ELEVATION, LOWER_ELEVATION, UPPER_AREA, LOWER_AREA, UPPER_LENGTH, LOWER_LENGTH

            BATHYMETRY_DATA % UPPER_ELEVATION(LAYER_NO) = UPPER_ELEVATION
            BATHYMETRY_DATA % LOWER_ELEVATION(LAYER_NO) = LOWER_ELEVATION
            BATHYMETRY_DATA % UPPER_AREA(LAYER_NO)      = UPPER_AREA
            BATHYMETRY_DATA % LOWER_AREA(LAYER_NO)      = LOWER_AREA
            BATHYMETRY_DATA % UPPER_LENGTH(LAYER_NO)    = UPPER_LENGTH
            BATHYMETRY_DATA % LOWER_LENGTH(LAYER_NO)    = LOWER_LENGTH

            if (i > 1) then

                if (dabs(BATHYMETRY_DATA % UPPER_AREA(i - 1) - BATHYMETRY_DATA % LOWER_ELEVATION(i)) > 1.0D-20) then
                    BATHYMETRY_DATA % NUM_DISCONTS = BATHYMETRY_DATA % NUM_DISCONTS + 1
                end if

            end if
        end do

        allocate(BATHYMETRY_DATA % DISCONT_ELEVATIONS(BATHYMETRY_DATA % NUM_DISCONTS))
        allocate(BATHYMETRY_DATA % DISCONT_LAYERS    (BATHYMETRY_DATA % NUM_DISCONTS))

        DISCONT_NO = 0

        do i = 2, NUM_LAYERS
            
            if (dabs(BATHYMETRY_DATA % UPPER_AREA(i - 1) - &
                     BATHYMETRY_DATA % LOWER_ELEVATION(i)) > 1.0D-20) then
                
                DISCONT_NO = DISCONT_NO + 1
                BATHYMETRY_DATA % DISCONT_ELEVATIONS(DISCONT_NO) = BATHYMETRY_DATA % LOWER_ELEVATION(i)
                BATHYMETRY_DATA % DISCONT_LAYERS(DISCONT_NO) = i
            end if
        end do

    end subroutine READ_BATHYMETRY_DATA_FROM_FILE


    function CALCULATE_SURFACE_AREA(BATHYMETRY_DATA, ELEVATION, DISCONT_BEHAVIOUR) result(SURFACE_AREA)

        implicit none
        type(BATHYMETRY_DS), intent(in) :: BATHYMETRY_DATA
        real(kind = DBL), intent(in) :: ELEVATION
        integer, intent(in), optional :: DISCONT_BEHAVIOUR

        real(kind = DBL) :: SURFACE_AREA

        integer :: NUM_LAYERS
        integer :: NUM_DISCONTS
        integer :: DISCONT_PROBLEM
        integer :: DISCONT_LAYER
        integer :: ELEVATION_LAYER
        integer :: i

        real(kind = DBL) :: UPPER_ELEVATION
        real(kind = DBL) :: LOWER_ELEVATION
        real(kind = DBL) :: UPPER_AREA
        real(kind = DBL) :: LOWER_AREA

        ELEVATION_LAYER = 1
        DISCONT_PROBLEM = 0
        DISCONT_LAYER   = 0

        if (present(DISCONT_BEHAVIOUR)) then
            NUM_DISCONTS = BATHYMETRY_DATA % NUM_DISCONTS

            do i = 1, NUM_DISCONTS
                if (dabs(ELEVATION - &
                         BATHYMETRY_DATA % DISCONT_ELEVATIONS(i)) < 1.0D-20) then
                    DISCONT_PROBLEM = 1
                    DISCONT_LAYER = BATHYMETRY_DATA % DISCONT_LAYERS(i)
                end if
            end do
        end if

        NUM_LAYERS = BATHYMETRY_DATA % NUM_LAYERS

        if ((ELEVATION < BATHYMETRY_DATA % LOWER_ELEVATION(1)).or. &
            (ELEVATION > BATHYMETRY_DATA % UPPER_ELEVATION(NUM_LAYERS))) then
            SURFACE_AREA = -1.0D0
        else
            do i = 1, NUM_LAYERS
                if (i == NUM_LAYERS) then
                    if ((ELEVATION >= BATHYMETRY_DATA % LOWER_ELEVATION(i)).and. &
                        (ELEVATION <= BATHYMETRY_DATA % UPPER_ELEVATION(i))) then
                        ELEVATION_LAYER = i
                        exit
                     end if
                else
                    if ((ELEVATION >= BATHYMETRY_DATA % LOWER_ELEVATION(i)).and. &
                        (ELEVATION <  BATHYMETRY_DATA % UPPER_ELEVATION(i))) then
                        ELEVATION_LAYER = i
                        exit
                     end if
                end if
            end do

            UPPER_ELEVATION = BATHYMETRY_DATA % UPPER_ELEVATION(ELEVATION_LAYER)
            LOWER_ELEVATION = BATHYMETRY_DATA % LOWER_ELEVATION(ELEVATION_LAYER)
            UPPER_AREA      = BATHYMETRY_DATA % UPPER_AREA(ELEVATION_LAYER)
            LOWER_AREA      = BATHYMETRY_DATA % LOWER_AREA(ELEVATION_LAYER)

            SURFACE_AREA = LDPS_INTERPOLATE(ELEVATION, UPPER_ELEVATION, UPPER_AREA, &
                                            LOWER_ELEVATION, LOWER_AREA)

            if (DISCONT_PROBLEM == 1) then
                if (DISCONT_BEHAVIOUR == 1) then
                    SURFACE_AREA = BATHYMETRY_DATA % LOWER_AREA(DISCONT_LAYER)
                end if
            end if
        end if
    end function CALCULATE_SURFACE_AREA


    function CALCULATE_VOLUME(BATHYMETRY_DATA, ELEVATION) result(VOLUME)

        implicit none
        type(BATHYMETRY_DS), intent(in) :: BATHYMETRY_DATA
        real(kind = DBL), intent(in) :: ELEVATION

        real(kind = DBL) :: VOLUME

        integer :: NUM_LAYERS
        integer :: ELEVATION_LAYER
        integer :: i

        real(kind = DBL) :: SURFACE_AREA
        real(kind = DBL) :: LAYER_VOLUME
        real(kind = DBL) :: LAYER_DEPTH
        real(kind = DBL) :: UPPER_ELEVATION
        real(kind = DBL) :: LOWER_ELEVATION
        real(kind = DBL) :: UPPER_AREA
        real(kind = DBL) :: LOWER_AREA

        NUM_LAYERS = BATHYMETRY_DATA % NUM_LAYERS
        VOLUME = 0.0D0
        ELEVATION_LAYER = 0

        if ((ELEVATION < BATHYMETRY_DATA % LOWER_ELEVATION(1)).or. &
            (ELEVATION > BATHYMETRY_DATA % UPPER_ELEVATION(NUM_LAYERS))) then
            VOLUME = -1.0D0
        else
            do i = 1, NUM_LAYERS
                if (i == NUM_LAYERS) then
                    if ((ELEVATION >= BATHYMETRY_DATA % LOWER_ELEVATION(i)).and. &
                        (ELEVATION <= BATHYMETRY_DATA % UPPER_ELEVATION(i))) then
                        ELEVATION_LAYER = i
                        exit
                    else
                        UPPER_ELEVATION = BATHYMETRY_DATA % UPPER_ELEVATION(i)
                        LOWER_ELEVATION = BATHYMETRY_DATA % LOWER_ELEVATION(i)
                        UPPER_AREA = BATHYMETRY_DATA % UPPER_AREA(i)
                        LOWER_AREA = BATHYMETRY_DATA % LOWER_AREA(i)
                        LAYER_DEPTH  = UPPER_ELEVATION - LOWER_ELEVATION
                        LAYER_VOLUME = 5.D-1 * (UPPER_AREA + LOWER_AREA) * LAYER_DEPTH
                        VOLUME = VOLUME + LAYER_VOLUME
                     end if
                else

                    if ((ELEVATION >= BATHYMETRY_DATA % LOWER_ELEVATION(i)).and. &
                        (ELEVATION <  BATHYMETRY_DATA % UPPER_ELEVATION(i))) then
                     
                        ELEVATION_LAYER = i
                        exit
                     else
                        UPPER_ELEVATION = BATHYMETRY_DATA % UPPER_ELEVATION(i)
                        LOWER_ELEVATION = BATHYMETRY_DATA % LOWER_ELEVATION(i)
                        UPPER_AREA = BATHYMETRY_DATA % UPPER_AREA(i)
                        LOWER_AREA = BATHYMETRY_DATA % LOWER_AREA(i)
                        LAYER_DEPTH  = UPPER_ELEVATION - LOWER_ELEVATION
                        LAYER_VOLUME = 5.D-1 * (UPPER_AREA + LOWER_AREA) * LAYER_DEPTH
                        VOLUME = VOLUME + LAYER_VOLUME
                     end if
                end if
            end do

            if (ELEVATION_LAYER == 0) then
                write(*,*) '**************************************************'
                write(*,*) '*  SOMETHING WRONG IN FUNCTION CALCULATE_VOLUME  *'
                write(*,*) '**************************************************'
                write(*,*) '*  BATHYMETRY ID NUM : ', BATHYMETRY_DATA % ID_NUM
                write(*,*) '*  ELEVATION         : ', ELEVATION
                write(*,*) '*  ELEVATION_LAYER   : ', ELEVATION_LAYER
                write(*,*) '*  VOLUME            : ', VOLUME
                write(*,*) '**************************************************'
            end if

            UPPER_ELEVATION = ELEVATION
            LOWER_ELEVATION = BATHYMETRY_DATA % LOWER_ELEVATION(ELEVATION_LAYER)
            LOWER_AREA   = BATHYMETRY_DATA % LOWER_AREA(ELEVATION_LAYER)
            SURFACE_AREA = CALCULATE_SURFACE_AREA(BATHYMETRY_DATA, ELEVATION)
            LAYER_DEPTH  = UPPER_ELEVATION - LOWER_ELEVATION
            LAYER_VOLUME = 5.D-1 * (SURFACE_AREA + LOWER_AREA) * LAYER_DEPTH
            VOLUME = VOLUME + LAYER_VOLUME
        end if

    end function CALCULATE_VOLUME


    function CALCULATE_VOLUME_BETWEEN_LEVELS(BATHYMETRY_DATA, ELEVATION_1, ELEVATION_2) result(VOLUME)

        implicit none
        type(BATHYMETRY_DS), intent(in) :: BATHYMETRY_DATA
        real(kind = DBL), intent(in) :: ELEVATION_1
        real(kind = DBL), intent(in) :: ELEVATION_2

        real(kind = DBL) :: VOLUME

        real(kind = DBL) :: UPPER_ELEVATION
        real(kind = DBL) :: LOWER_ELEVATION
        real(kind = DBL) :: UPPER_VOLUME
        real(kind = DBL) :: LOWER_VOLUME

        if (dabs(ELEVATION_1 - ELEVATION_2) < 1.0D-20) then
            VOLUME = 0.0D0
        else
            if (ELEVATION_1 > ELEVATION_2) then
                UPPER_ELEVATION = ELEVATION_1
                LOWER_ELEVATION = ELEVATION_2
            else
                UPPER_ELEVATION = ELEVATION_2
                LOWER_ELEVATION = ELEVATION_1
            end if

            UPPER_VOLUME = CALCULATE_VOLUME(BATHYMETRY_DATA, UPPER_ELEVATION)
            LOWER_VOLUME = CALCULATE_VOLUME(BATHYMETRY_DATA, LOWER_ELEVATION)
            VOLUME       = UPPER_VOLUME - LOWER_VOLUME
        end if

    end function CALCULATE_VOLUME_BETWEEN_LEVELS


    function CALCULATE_SURFACE_ELEVATION(BATHYMETRY_DATA, ELEVATION, VOLUME) result(SURFACE_ELEVATION)
        implicit none
        type(BATHYMETRY_DS), intent(in) :: BATHYMETRY_DATA
        real(kind = DBL), intent(in) :: ELEVATION
        real(kind = DBL), intent(in) :: VOLUME

        real(kind = DBL) :: SURFACE_ELEVATION

        real(kind = DBL) :: LOWER_AREA
        real(kind = DBL) :: UPPER_AREA
        real(kind = DBL) :: DEPTH
        real(kind = DBL) :: CALCULATED_VOLUME
        real(kind = DBL) :: LAYER_VOLUME

        integer :: NUM_LAYERS
        integer :: ELEVATION_LAYER
        integer :: i

        real(kind = DBL) :: QUAD_A
        real(kind = DBL) :: QUAD_B
        real(kind = DBL) :: QUAD_C
        real(kind = DBL) :: QUAD_D
        real(kind = DBL) :: QUAD_T
        real(kind = DBL) :: QUAD_R1
        real(kind = DBL) :: QUAD_R2

        ELEVATION_LAYER = 1
        LOWER_AREA = CALCULATE_SURFACE_AREA(BATHYMETRY_DATA, ELEVATION, 1)
        NUM_LAYERS = BATHYMETRY_DATA % NUM_LAYERS
        CALCULATED_VOLUME = 0.0D0
        SURFACE_ELEVATION = ELEVATION

        QUAD_A  = 0.0D0
        QUAD_B  = 0.0D0
        QUAD_C  = 0.0D0
        QUAD_D  = 0.0D0
        QUAD_T  = 0.0D0
        QUAD_R1 = 0.0D0
        QUAD_R2 = 0.0D0

        do i = 1, NUM_LAYERS
            if (i == NUM_LAYERS) then
                if ((ELEVATION >= BATHYMETRY_DATA % LOWER_ELEVATION(i)).and. &
                    (ELEVATION <= BATHYMETRY_DATA % UPPER_ELEVATION(i))) then
                    ELEVATION_LAYER = i
                    exit
                 end if
            else
                if ((ELEVATION >= BATHYMETRY_DATA % LOWER_ELEVATION(i)).and. &
                    (ELEVATION <  BATHYMETRY_DATA % UPPER_ELEVATION(i))) then
                    ELEVATION_LAYER = i
                    exit
                 end if
            end if
        end do
 
        CALCULATED_VOLUME = &
             CALCULATE_VOLUME_BETWEEN_LEVELS &
                 (BATHYMETRY_DATA, ELEVATION, &
                  BATHYMETRY_DATA % UPPER_ELEVATION(ELEVATION_LAYER))

        if (CALCULATED_VOLUME > VOLUME) then
            UPPER_AREA = BATHYMETRY_DATA % UPPER_AREA(ELEVATION_LAYER)
            DEPTH      = BATHYMETRY_DATA % UPPER_ELEVATION(ELEVATION_LAYER) - ELEVATION
            QUAD_A     = (UPPER_AREA - LOWER_AREA) / DEPTH
            QUAD_B     = 2.0D0 * LOWER_AREA
            QUAD_C     = 2.0D0 * VOLUME
            QUAD_D     = ((QUAD_B ** 2.0D0) - (4.0D0 * QUAD_A * QUAD_C)) ** 5.0D-1
            QUAD_T     = (-1.0D0) * 5.0D-1 * (QUAD_B + (QUAD_B / dabs(QUAD_B)) * (QUAD_D))

            if ((QUAD_A < 0.0D0).or.(QUAD_A > 0.0D0)) then
                QUAD_R1 = QUAD_T / QUAD_A
            end if

            if ((QUAD_T < 0.0D0).or.(QUAD_T > 0.0D0)) then
                QUAD_R2 = QUAD_C / QUAD_T
            end if

            if ((QUAD_R1 >= 0.0D0).and.((QUAD_A < 0.0D0).or.(QUAD_A > 0.0D0))) then
                SURFACE_ELEVATION = ELEVATION + QUAD_R1
            end if

            if ((QUAD_R2 >= 0.0D0).and.((QUAD_T < 0.0D0).or.(QUAD_T > 0.0D0))) then
                SURFACE_ELEVATION = ELEVATION + QUAD_R2
            end if
        else
            SURFACE_ELEVATION = BATHYMETRY_DATA % UPPER_ELEVATION(ELEVATION_LAYER)

            do i = ELEVATION_LAYER + 1, NUM_LAYERS

               LAYER_VOLUME = CALCULATE_VOLUME_BETWEEN_LEVELS(BATHYMETRY_DATA, &
                         BATHYMETRY_DATA % LOWER_ELEVATION(i), BATHYMETRY_DATA % UPPER_ELEVATION(i)) 

               if ((CALCULATED_VOLUME + LAYER_VOLUME) < VOLUME) then
                   CALCULATED_VOLUME = CALCULATED_VOLUME + LAYER_VOLUME
                   DEPTH             = BATHYMETRY_DATA % UPPER_ELEVATION(i) - BATHYMETRY_DATA % LOWER_ELEVATION(i)
                   SURFACE_ELEVATION = SURFACE_ELEVATION + DEPTH
               else
                    UPPER_AREA = BATHYMETRY_DATA % UPPER_AREA(i)
                    LOWER_AREA = BATHYMETRY_DATA % LOWER_AREA(i)
                    DEPTH      = BATHYMETRY_DATA % UPPER_ELEVATION(i) - BATHYMETRY_DATA % LOWER_ELEVATION(i)

                    QUAD_A = (UPPER_AREA - LOWER_AREA) / DEPTH
                    QUAD_B = 2.0D0 * LOWER_AREA
                    QUAD_C = (-1.0D0) * (2.0D0 * (VOLUME - CALCULATED_VOLUME))
                    QUAD_D = ((QUAD_B ** 2.0D0) - (4.0D0 * QUAD_A * QUAD_C)) ** 5.0D-1
                    QUAD_T = (-1.0D0) * 5.0D-1 * (QUAD_B + (QUAD_B / dabs(QUAD_B)) * (QUAD_D))

                    if ((QUAD_A < 0.0D0).or.(QUAD_A > 0.0D0)) then
                        QUAD_R1 = QUAD_T / QUAD_A
                    end if

                    if ((QUAD_T < 0.0D0).or.(QUAD_T > 0.0D0)) then
                        QUAD_R2 = QUAD_C / QUAD_T
                    end if

                    if ((QUAD_R1 >= 0.0D0).and.((QUAD_A < 0.0D0).or.(QUAD_A > 0.0D0))) then
                        SURFACE_ELEVATION = SURFACE_ELEVATION + QUAD_R1
                    end if

                    if ((QUAD_R2 >= 0.0D0).and.((QUAD_T < 0.0D0).or.(QUAD_T > 0.0D0))) then
                        SURFACE_ELEVATION = SURFACE_ELEVATION + QUAD_R2
                    end if

                    exit
               end if
            end do
        end if
    end function CALCULATE_SURFACE_ELEVATION

end module BASIN