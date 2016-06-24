!=======================================================================
!     MAIN PROGRAM "TECHNICAL_EXERCISE_SRW" (TE_SRW) FOR THE PLETHORA
!     TECHNICAL CHALLENGE
!=======================================================================
!
      PROGRAM TE_SRW
!
!=======================================================================
!
      USE CUT_COST_M
      IMPLICIT NONE
!
!=======================================================================
!
!     declare local variables, which is one instance of the CUT_SHAPE
!     OBJECT
!
      TYPE (CUT_SHAPE) :: CSHAPE
!
!=======================================================================
!
!     call type bound procedures on the CUT_SHAPE object to determine
!     the cost of laser cutting a part
!
!     deserialize the JSON input and store the data as components on the
!     CUT_SHAPE object
!
      CALL CSHAPE%GET_JSON_DATA
!
!     compute necessary geometry quantities
!
      CALL CSHAPE%COMP_GEOM
!
!     compute the machine costs
!
      CSHAPE%COST = 0.D0

      CALL CSHAPE%MACHINE_COST
!
!     compute the material costs
!
      CALL CSHAPE%MATERIAL_COST
!
      print *, 'Part Cutting Cost: ', cshape%cost, 'dollars'
!
!     delete the CUT_SHAPE object
!
      CALL CSHAPE%DELETE
!
      END PROGRAM
