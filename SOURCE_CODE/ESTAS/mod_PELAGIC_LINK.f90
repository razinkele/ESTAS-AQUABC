module PELAGIC_LINK

    implicit none

    type PELAGIC_ADVECTIVE_LINK_DS

        integer :: ADVECTIVE_LINK_NO

        integer :: UPSTREAM_BOX_NO
        integer :: DOWNSTREAM_BOX_NO
        integer :: FLOW_TS_NO
        integer :: FLOW_TS_VAR_NO

    end type PELAGIC_ADVECTIVE_LINK_DS


    type PELAGIC_DISPERSIVE_LINK_DS

        integer :: DISPERSIVE_LINK_NO

        integer :: FIRST_BOX_NO
        integer :: SECOND_BOX_NO
        integer :: MIXING_TS_NO
        real(kind = SELECTED_REAL_KIND(15,307)) :: MIXING_LENGTH

    end type PELAGIC_DISPERSIVE_LINK_DS


end module PELAGIC_LINK