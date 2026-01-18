subroutine READ_BOTTOM_SEDS_FLUXES_INPUTS&
           (PELAGIC_BOX_MODEL_DATA, IN_FILE, FLUX_SET_NO)

    use PELAGIC_BOX_MODEL
    implicit none

    type(PELAGIC_BOX_MODEL_DS), intent(inout) :: PELAGIC_BOX_MODEL_DATA
    integer, intent(in) :: IN_FILE
    integer, intent(in) :: FLUX_SET_NO

    integer :: BOX_NO
    integer :: STATE_VAR_NO
    integer :: FORCING_TS_NO
    integer :: FORCING_TS_VAR_NO
    integer :: i
    integer :: j

    read(unit = IN_FILE, fmt = *)
    read(unit = IN_FILE, fmt = *)

    do i = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_BOXES
        do j = 1, PELAGIC_BOX_MODEL_DATA % NUM_PELAGIC_STATE_VARS
            read(unit = IN_FILE, fmt = *) &
                BOX_NO, STATE_VAR_NO, FORCING_TS_NO, FORCING_TS_VAR_NO

            PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_NOS     &
                (FLUX_SET_NO, BOX_NO, STATE_VAR_NO) = FORCING_TS_NO

            PELAGIC_BOX_MODEL_DATA % SEDIMENT_FLUX_TS_VAR_NOS &
                (FLUX_SET_NO, BOX_NO, STATE_VAR_NO) = FORCING_TS_VAR_NO
        end do
    end do

end subroutine READ_BOTTOM_SEDS_FLUXES_INPUTS
