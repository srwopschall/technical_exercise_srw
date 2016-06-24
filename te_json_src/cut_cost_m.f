      MODULE CUT_COST_M
!
!     This module contains the derived types necessary to store and
!     compute on the data contained in the JSON file, per the Plethora
!     technical exercise, in order to determine the cost of laser
!     cutting a 2D part.  The main derived type is CUT_SHAPE, which
!     containes component variables, some of which are other derived
!     types defined herein.
!  
!     06.21.16      
!=======================================================================
!
      USE JSON_MODULE
      IMPLICIT NONE
!
!=======================================================================
!
!     This is the base derived type for an edge object.  It contains 
!     vertex ids and vertex coordinates for the two vertices that define 
!     an edge.  This being the lowest order object results in duplicate 
!     data being stored.  One way to improve this is to create a 
!     "vertex" derived type, which contains an array of ids and 
!     coordinates that the "edge" object can index into.
!
      TYPE EDGE
        INTEGER :: VRTX_IDS(2)
        REAL (KIND = KIND(0.D0)) :: XY_VRTCS(2,2)
      END TYPE EDGE
!
!=======================================================================
!
!     extended type for an edge that is a line segement 
!
      TYPE, EXTENDS (EDGE) :: LINE_SEGMENT
        REAL (KIND = KIND(0.D0)) :: LINE_LENGTH
        CONTAINS
        PROCEDURE :: COMP_LINE_LENGTH
      END TYPE LINE_SEGMENT
!
!=======================================================================
!
!     extended type for an edge that is a circular arc
!
      TYPE, EXTENDS (EDGE) :: CIRCULAR
        REAL (KIND = KIND(0.D0)) :: XY_CNTR(2), ARC_LENGTH
        INTEGER :: CC_FROM
        LOGICAL :: EXTRUDED
        CONTAINS
        PROCEDURE :: COMP_ARC_LENGTH
      END TYPE CIRCULAR
!
!=======================================================================
!
!     edge container object that allows CUT_SHAPE%EDGE_CNTNR to hold
!     objects of different edge types
!
      TYPE EDGE_CONTAINER
        CLASS (EDGE), ALLOCATABLE :: EDGE_DAT
      END TYPE EDGE_CONTAINER
!
!=======================================================================
!
!     the main object with which to compute the cost of laser cutting a
!     2D part.
!
      TYPE CUT_SHAPE
        TYPE (EDGE_CONTAINER), ALLOCATABLE :: EDGE_CNTNR(:)
        REAL (KIND = KIND(0.D0)) :: L_PERIM, CNTRD(2), AREA, COST
        REAL (KIND = KIND(0.D0)), ALLOCATABLE :: C_HULL(:,:)
        CONTAINS
        PROCEDURE :: COMP_GEOM
        PROCEDURE :: GET_JSON_DATA
        PROCEDURE :: COMP_CONVEX_HULL
        PROCEDURE :: COMP_BOUNDING_BOX
        PROCEDURE :: MACHINE_COST
        PROCEDURE :: MATERIAL_COST
        PROCEDURE :: DELETE => DELETE_SHAPE
      END TYPE CUT_SHAPE
!
!=======================================================================
!
      PRIVATE :: COMP_LINE_LENGTH, COMP_ARC_LENGTH, COMP_GEOM,          &
     &           GET_JSON_DATA, COMP_CONVEX_HULL, COMP_BOUNDING_BOX,    &
     &           MACHINE_COST, MATERIAL_COST, DELETE_SHAPE
!
!=======================================================================
      CONTAINS
!=======================================================================
!     Type Bound Procedures on CUT_SHAPE
!=======================================================================
!
      SUBROUTINE GET_JSON_DATA (CSHAPE)
!
!       This subroutine uses the JSON-FORTRAN library obtained from 
!       https://github.com/jacobwilliams/json-fortran, to deserialize
!       a JSON input file according to the schema presented in the
!       Plethora technical exercise. 
!
!       This subroutine is a type bound procedure on the CUT_SHAPE
!       object and populates the EDGE_CONTAINER component object with
!       the input data from the JSON file.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE), INTENT (INOUT) :: CSHAPE
!
!       declare local variables
!
        TYPE (JSON_FILE) :: F
        INTEGER :: I, ITER, NUMTOPES
        CHARACTER (LEN=:), ALLOCATABLE :: C
        CHARACTER (LEN = 1024) :: STR1, STR2, STR_TEMP
        LOGICAL :: FOUND
!
!=======================================================================
!
!       initialize the json file object
!
        CALL F%INITIALIZE()
!
!       load the json file.  the file to be loaded must be copied and
!       named "test_input.json".  An improvement here would be to handle
!       arbitrary input file names.  Note: I did not code error checking
!       of the file to see if it matches the schema.  This is an area of
!       future work, but for now, assume the file is correct.
!
        CALL F%LOAD_FILE(FILENAME = '../tests/test_input.json')
!
!       print json file to screen to test
!
!        CALL F%PRINT_FILE()
!
!       loop over "Edges" objects to determine the number
!       of edges and vertices in the shape (number of edges and vertices
!       are the same).  
!
        FOUND = .TRUE.
        ITER = 0 
        DO WHILE (FOUND)
          WRITE (STR_TEMP, '(I0)') ITER+1
          STR1 = 'Edges('//trim(str_temp)//').Type'
          CALL F%GET(STR1, C, FOUND)          
          IF (FOUND) ITER = ITER + 1
        END DO 
!
        NUMTOPES = ITER
!
!       allocate the edge_container object
!
        IF (.NOT. ALLOCATED(CSHAPE%EDGE_CNTNR)) then
          ALLOCATE (CSHAPE%EDGE_CNTNR(NUMTOPES))
        END IF
!
!       loop over edges and populate the CUT_SHAPE object
!
        DO I = 1,NUMTOPES
!
          ASSOCIATE (EC => CSHAPE%EDGE_CNTNR(I))
!
          WRITE (STR_TEMP, '(I0)') I
          STR1 = 'Edges('//trim(str_temp)//').Type'
          CALL F%GET(STR1, C, FOUND)
!
!         allocate the extended objects based on edge type
!
          IF (C .EQ. 'LineSegment') THEN
            ALLOCATE (LINE_SEGMENT :: EC%EDGE_DAT)
          ELSE ! is CircularArc
            ALLOCATE (CIRCULAR :: EC%EDGE_DAT)
          END IF
!
          STR1 = 'Edges('//trim(str_temp)//').Vertices(1)'
          STR2 = 'Edges('//trim(str_temp)//').Vertices(2)'
!
!         get vertex ids
!          
          CALL F%GET (STR1, EC%EDGE_DAT%VRTX_IDS(1))
          CALL F%GET (STR2, EC%EDGE_DAT%VRTX_IDS(2))
!
!         get vertex coordinates
!
          WRITE (STR_TEMP, '(I0)') EC%EDGE_DAT%VRTX_IDS(1)
          STR1 = 'Vertices.'//trim(str_temp)//'.Position.X'
          STR2 = 'Vertices.'//trim(str_temp)//'.Position.Y'
          CALL F%GET (STR1, EC%EDGE_DAT%XY_VRTCS(1,1))
          CALL F%GET (STR2, EC%EDGE_DAT%XY_VRTCS(2,1))
          WRITE (STR_TEMP, '(I0)') EC%EDGE_DAT%VRTX_IDS(2)
          STR1 = 'Vertices.'//trim(str_temp)//'.Position.X'
          STR2 = 'Vertices.'//trim(str_temp)//'.Position.Y'
          CALL F%GET (STR1, EC%EDGE_DAT%XY_VRTCS(1,2))
          CALL F%GET (STR2, EC%EDGE_DAT%XY_VRTCS(2,2))
!
!         if the run time type of EC%EDGE_DAT is CIRCULAR, then populate
!         the components on that object, namely, the circles center and
!         reference vertex
!
          SELECT TYPE (DAT => EC%EDGE_DAT)
          TYPE IS (CIRCULAR)
            WRITE (STR_TEMP, '(I0)') I
            STR1 = 'Edges('//trim(str_temp)//').Center.X'
            STR2 = 'Edges('//trim(str_temp)//').Center.Y'
            CALL F%GET (STR1, DAT%XY_CNTR(1))
            CALL F%GET (STR2, DAT%XY_CNTR(2))
            STR1 = 'Edges('//trim(str_temp)//').ClockwiseFrom'
            CALL F%GET (STR1, DAT%CC_FROM)
!            print *, 'xy_cntr: ', dat%xy_cntr
!            print *, 'cc_from: ', dat%cc_from
!
          END SELECT
!
!          print *, 'vrtcs_ids: ', ec%edge_dat%vrtx_ids(:)
!          print *, 'vrtcs1: ', ec%edge_dat%xy_vrtcs(:,1)
!          print *, 'vrtcs2: ', ec%edge_dat%xy_vrtcs(:,2)
!
          END ASSOCIATE
!
        END DO
!
!       clean up
!
        CALL F%DESTROY()
!
        RETURN
!
      END SUBROUTINE GET_JSON_DATA
!      
!=======================================================================
!
      SUBROUTINE COMP_GEOM (SHPE)
!
!       This is a type bound procedure on the CUT_SHAPE object.
!       This subroutine computes geometric data on the CUT_SHAPE object
!       necessary to compute machine and material costs. 
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE) :: SHPE
!
!       declare local variables
!
        INTEGER :: I, NUMTOPES
        REAL (KIND = KIND(0.D0)) :: N(2), DOT
!
!=======================================================================
!
        ASSOCIATE (EC => SHPE%EDGE_CNTNR)
!
        NUMTOPES = SIZE(EC)
!
!       loop over edges and compute edge lengths and sum the
!       contributions to the full part's perimeter length.  Also,
!       compute the shape's centroid, which will be used to properly
!       orient outward facing edge normals as necessary later on.
!
        DO I = 1,NUMTOPES
          SELECT TYPE (E_DAT => EC(I)%EDGE_DAT)
          TYPE IS (LINE_SEGMENT)
            CALL E_DAT%COMP_LINE_LENGTH
            SHPE%L_PERIM = SHPE%L_PERIM + E_DAT%LINE_LENGTH
            SHPE%CNTRD = SHPE%CNTRD + E_DAT%XY_VRTCS(:,1)
          TYPE IS (CIRCULAR)
            CALL E_DAT%COMP_ARC_LENGTH
            SHPE%L_PERIM = SHPE%L_PERIM + E_DAT%ARC_LENGTH
            SHPE%CNTRD = SHPE%CNTRD + E_DAT%XY_VRTCS(:,1)
          END SELECT
        END DO
!
!       complete shape centroid calculation
!
        SHPE%CNTRD = 1.D0 / NUMTOPES * SHPE%CNTRD
!
!       check to see if any circular arc edges are extruded or cut. We
!       cannot assume counter clockwise ordering of the vertices, in
!       which we can infer extrusion or cutting, so we have to check a
!       little more explicitly
!
        DO I = 1, NUMTOPES
          SELECT TYPE (E_DAT => EC(I)%EDGE_DAT)
          TYPE IS (CIRCULAR)
            N = (/(E_DAT%XY_VRTCS(2,2) - E_DAT%XY_VRTCS(2,1)),          &
     &           -(E_DAT%XY_VRTCS(1,2) - E_DAT%XY_VRTCS(1,1))/)
            N = N / NORM2(N)
            DOT = DOT_PRODUCT(SHPE%CNTRD - E_DAT%XY_VRTCS(:,1), N)
            IF (DOT .LT. 0.D0) THEN !vertex ordering is cc
              IF (E_DAT%CC_FROM .EQ. E_DAT%VRTX_IDS(1)) THEN
                E_DAT%EXTRUDED = .FALSE.
              ELSE
                E_DAT%EXTRUDED = .TRUE.
              END IF
            ELSE 
              IF (E_DAT%CC_FROM .EQ. E_DAT%VRTX_IDS(1)) THEN
                E_DAT%EXTRUDED = .TRUE.
              ELSE
                E_DAT%EXTRUDED = .FALSE.
              END IF
            END IF
          END SELECT
        END DO
!
!       compute the shape's convex hull (see readme notes)
!
        CALL SHPE%COMP_CONVEX_HULL
!
!       compute the shape's minimum oriented bounding box (see readme
!       notes)
!
        CALL SHPE%COMP_BOUNDING_BOX
!
!        print *, 'perim length: ', shpe%l_perim
!        
        END ASSOCIATE
!
        RETURN
!
      END SUBROUTINE COMP_GEOM
!
!=======================================================================
!
      SUBROUTINE COMP_CONVEX_HULL (SHPE)
!
!       This subroutine is a type bound procedure on the CUT_SHAPE
!       object.  This subroutine computes the convex hull of the cut 
!       shape by traversing the edges and for every edge where all 
!       non-selected edge vertices fall on one side of the edge vector, 
!       that edge is used to form the convex hull.  For an edge that 
!       contains non-selected edge vertices on either side of the edge 
!       "line" vector, that edge is not used to form the convex hull.
!       This procedure is less that O(n^2).  This procedure populates
!       the C_HULL(:,:) array on the CUT_SHAPE object
!
!       For extruded circles, vertices are introduced that form a box
!       around the circle, which thereby forms new edges, which are then
!       checked using the aforementioned method. This forms an
!       approximate convex hull when an edge is an extruded circular
!       arc.  The approximation still results in the correct minimum
!       oriented bounding box for the example json files in the Plethora
!       technical exercise, but may result in slightly larger bounding
!       boxes for complex n-gons with extruded circular arc edges.  Out
!       of time considerations, this "box" method appeared to be a
!       reasonable and sufficient approach. 
!
!       I did not include a check on the convexity of the shape before
!       proceeding with the convex hull calculation.  If the shape does
!       not contain any circular arc edges and is convex, then the shape
!       is the convex hull.  If the shape contains a circular arc, we
!       can compute a box around that arc, and if the resulting shape is
!       convex we can use the vertex data as the convex hull.  This
!       would improve the code in the future.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE), INTENT (INOUT) :: SHPE
!
!       declare local variables
!
        INTEGER :: I, J, NUMTOPES, CNT
        INTEGER, ALLOCATABLE :: C_HULL_IDS(:)
        REAL (KIND = KIND(0.D0)), ALLOCATABLE :: SHPE_VRTCS(:,:)
        REAL (KIND = KIND(0.D0)) :: TRIAL_V(2), V1(2), V2(2), N(2),     &
     &                              RAD, TEST_VEC(2), TEST_MAG, DOT 
        LOGICAL :: INSIDE
!
!=======================================================================
!
        ASSOCIATE (EC => SHPE%EDGE_CNTNR)
!
        NUMTOPES = SIZE(EC)
!
!       loop over edges to see if any are circular AND extruded.  Then
!       we will introduce two new vertices that form three new edges,
!       forming a three sided "box" around the circular edge
!
        CNT = 0
        DO I = 1,NUMTOPES
          SELECT TYPE (E_DAT => EC(I)%EDGE_DAT)
          TYPE IS (CIRCULAR)
            IF (E_DAT%EXTRUDED) CNT = CNT + 2
          END SELECT 
        END DO
!
!       allocate the convex hull vector ids.  At most there will be the
!       same number of cut-shape vertices plus two vertices for every
!       circular edge.  
!
        ALLOCATE (C_HULL_IDS(NUMTOPES + CNT))
!
!       allocate the modified shape vertex array, including additional
!       vertices for circular edges AND two ghost cells containing the
!       vertices of the first edge for when we loop over the SHPE_VRTCS
!       array to determine the edges that belong to the convex hull.
!
        ALLOCATE (SHPE_VRTCS(2,NUMTOPES + CNT + 2))
!
!       loop over edges to populate the SHPE_VRTCS array
!
        CNT = 1
        DO I = 1,NUMTOPES
!
          SELECT TYPE (E_DAT => EC(I)%EDGE_DAT)
!
          TYPE IS (LINE_SEGMENT)
!
            SHPE_VRTCS(:,CNT) = E_DAT%XY_VRTCS(:,1)
            CNT = CNT + 1
!
          TYPE IS (CIRCULAR)
!
            IF (E_DAT%EXTRUDED) THEN
              SHPE_VRTCS(:,CNT) = E_DAT%XY_VRTCS(:,1)
              CNT = CNT + 1
!
!             compute the outward facing edge unit normal
!
              N = (/(E_DAT%XY_VRTCS(2,2) - E_DAT%XY_VRTCS(2,1)),        &
     &             -(E_DAT%XY_VRTCS(1,2) - E_DAT%XY_VRTCS(1,1))/)
              N = N / NORM2(N)
!
!             check for correct orientation of the unit normal
!
              DOT = DOT_PRODUCT(SHPE%CNTRD - E_DAT%XY_VRTCS(:,1), N)
              IF (DOT .GT. 0.D0) N = -N
!
!             compute the radius of the extruded circular arc
!
              RAD = 0.5D0 * NORM2(E_DAT%XY_VRTCS(:,2) -                 &
     &                            E_DAT%XY_VRTCS(:,1))
!
!             compute the new vertices that form the box around the arc
!
              V1 = E_DAT%XY_VRTCS(:,1) + RAD * N
              V2 = E_DAT%XY_VRTCS(:,2) + RAD * N
              SHPE_VRTCS(:,CNT) = V1
              CNT = CNT + 1
              SHPE_VRTCS(:,CNT) = V2
              CNT = CNT + 1
!
            ELSE
!
              SHPE_VRTCS(:,CNT) = E_DAT%XY_VRTCS(:,1)
              CNT = CNT + 1
!
            END IF 
!
          END SELECT
!
        END DO
!
        SHPE_VRTCS(:,SIZE(SHPE_VRTCS,2)-1) =                            &
     &                                  EC(1)%EDGE_DAT%XY_VRTCS(:,1)
        SHPE_VRTCS(:,SIZE(SHPE_VRTCS,2)) = EC(1)%EDGE_DAT%XY_VRTCS(:,2)
!
!       check that shpe_vrtcs has been populated correctly
!
!        do i = 1,size(shpe_vrtcs,2)
!          print *, 'shpe_vrtcs: ', shpe_vrtcs(:,i)
!        end do
!
!       loop over the SHPE_VRTCS array to determine which edges are used
!       to compose the convex hull
!
        C_HULL_IDS = 0
        CNT = 1
        INSIDE = .TRUE.
!
        DO I = 1,SIZE(SHPE_VRTCS,2) - 2
!
!         compute the "edge vector", or directional vector along the
!         line segment
!
          TRIAL_V = SHPE_VRTCS(:,I+1) - SHPE_VRTCS(:,I)
!
!         compute the "outward" facing unit normal to the edge
!
          N = (/TRIAL_V(2), -TRIAL_V(1)/)
          N = N / NORM2(N)
          DOT = DOT_PRODUCT(SHPE%CNTRD - SHPE_VRTCS(:,I), N)
          IF (DOT .GT. 0.D0) N = -N
!
!         loop over non-trial edge vertices and check to see if they lie
!         on one side (i.e. the "interior" side) of the trial edge
!
          DO J = I+2,SIZE(SHPE_VRTCS,2)-(I+2)
            TEST_VEC = SHPE_VRTCS(:,J) - TRIAL_V
            TEST_MAG = (DOT_PRODUCT(TEST_VEC,N))
!            print *, 'test_mag: ', test_mag
            IF (TEST_MAG .GT. 0.D0) THEN !don't include edge in convex hull
              INSIDE = .FALSE.
              EXIT
            END IF  
          END DO
!          print *, 'inside: ', inside
!
          IF (INSIDE .EQV. .FALSE.) THEN
            INSIDE = .TRUE.
            CYCLE
          END IF
!
          IF (INSIDE .EQV. .TRUE.) THEN
            C_HULL_IDS(CNT) = I
            CNT = CNT + 1 
          END IF
!
        END DO
!
!        print *, 'c hull ids: ', c_hull_ids
!
!       populate the convex hull vertex array on the CUT_SHAPE object
!       using the IDS in C_HULL_IDS.  This will include the vertices
!       of any additional edge introduced due to an extruded circular
!       arc, should those edges participate in the convex hull
!
        CNT = COUNT(C_HULL_IDS .GT. 0)
!
        IF (ALLOCATED (SHPE%C_HULL)) THEN
          IF (SIZE(SHPE%C_HULL,2) .NE. CNT) THEN
            DEALLOCATE (SHPE%C_HULL)
            ALLOCATE (SHPE%C_HULL(2,CNT))
          END IF
        ELSE
          ALLOCATE (SHPE%C_HULL(2,CNT))
        END IF
!
        CNT = 1
        DO I = 1,SIZE(SHPE%C_HULL,2)
          IF (C_HULL_IDS(I) .NE. 0) THEN
            SHPE%C_HULL(:,CNT) = SHPE_VRTCS(:,I)
!            print *, 'c hull i: ', i, shpe%c_hull(:,cnt)
            CNT = CNT + 1
          END IF
        END DO
!
        END ASSOCIATE
!
        DEALLOCATE (C_HULL_IDS, SHPE_VRTCS)
!        
        RETURN
!
      END SUBROUTINE COMP_CONVEX_HULL
!
!=======================================================================
!
      SUBROUTINE COMP_BOUNDING_BOX (SHPE)
!
!       This subroutine is a type bound procedure on the CUT_SHAPE
!       object.  This subroutine computes the minimum oriented bounding
!       box from the convex hull vertex data stored on the object.  This
!       routine is O(n^2).  Looping over all edges (outer loop), then 
!       for a given edge, we loop over all other edges (inner loop) and 
!       project the vertices onto the current edge-vector. The outermost
!       vertices are used to form the bounding box width, and the vertex
!       that is the furthest from the current edge is used to form the
!       height of the bounding box.  The resulting area is stored.  If
!       the next edge (outer loop) produces a smaller bounding box area,
!       then that area is stored.  At the end, we have the smallest
!       bounding box around the convex hull of the shape.  This routine
!       can be optimized and done in a fashion that reduces the
!       complexity of the algorithm
!
!       The bounding box data is not currently stored on the CUT_SHAPE
!       object, but is simply used to compute the smallest area, which
!       is then used to compute the material costs.  This can easily be
!       changed if the bounding box and orientation (i.e. which edge of
!       the rectangle is coincident with which edge of the shape) are
!       required.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE), INTENT (INOUT) :: SHPE
!
!       declare local variables
!
        INTEGER :: I, J, NUMTOPES
        REAL (KIND = KIND(0.D0)) :: LENGTH, EDGE_VEC(2),                &
     &                              D, MAX_PLUS, MAX_MINUS, MAX_VEC(2), &
     &                              MIN_VEC(2), W, H, H_TEMP, N(2)     
!
!=======================================================================
!
        ASSOCIATE (EC => SHPE%EDGE_CNTNR, C_HULL => SHPE%C_HULL)
!
        NUMTOPES = SIZE(C_HULL,2)
        N = 0.D0
        MAX_PLUS = 0.D0
        MAX_MINUS = 0.D0
        H = 0.D0
        H_TEMP = 0.D0
!
        DO I = 1,NUMTOPES
!
          H = 0.D0
          H_TEMP = 0.D0
!
          IF (I .LT. NUMTOPES) THEN
            EDGE_VEC = C_HULL(:,I+1) - C_HULL(:,I)
            LENGTH = NORM2(EDGE_VEC)
            EDGE_VEC = EDGE_VEC / NORM2(EDGE_VEC)
            MAX_VEC = C_HULL(:,I+1)
          ELSE
            EDGE_VEC = C_HULL(:,1) - C_HULL(:,I)
            LENGTH = NORM2(EDGE_VEC)
            EDGE_VEC = EDGE_VEC / NORM2(EDGE_VEC)
            MAX_VEC = C_HULL(:,1)
          END IF 
!
          MIN_VEC = C_HULL(:,I)
          MAX_PLUS = LENGTH
          MAX_MINUS = 0.D0
!
!         compute the normal for the current edge vector
!
          N = (/EDGE_VEC(2), -EDGE_VEC(1)/)
          N = N / NORM2(N)
          D = DOT_PRODUCT(SHPE%CNTRD - C_HULL(:,I), N)
          IF (D .GT. 0.D0) N = -N
!
!         determine the width and height of the bounding box for a box 
!         with an edge coincident with the current "I" edge.
!
          DO J = 1,NUMTOPES
!
            D = DOT_PRODUCT(C_HULL(:,J),EDGE_VEC)
!
!           check to see if J vector is parallel to I vector, in which
!           case we do not need to consider it
!
            IF (ABS(D) .EQ. LENGTH) THEN
              CYCLE
            END IF
!
            H = ABS(DOT_PRODUCT(C_HULL(:,J),N))
            IF (H .GT. H_TEMP) THEN
              H_TEMP = H
            END IF
            IF ((D .GT. LENGTH) .AND. (D .GT. MAX_PLUS)) THEN
              MAX_PLUS = D
              MAX_VEC = C_HULL(:,J)
            ELSE IF ((D .LT. 0.D0) .AND. (D .LT. MAX_MINUS)) THEN
              MAX_MINUS = ABS(D)
              MIN_VEC = C_HULL(:,J)
            END IF
          END DO
!
        END DO
!
        W = ABS(MAX_MINUS) + ABS(MAX_PLUS) + 0.1D0 
        H = H_TEMP + 0.1D0
        SHPE%AREA = W * H
!        print *, 'area', shpe%area
!
        END ASSOCIATE
!
        RETURN
!
      END SUBROUTINE COMP_BOUNDING_BOX
!
!=======================================================================
!
      SUBROUTINE MACHINE_COST (SHPE)
!
!       This is a type bound procedure on the CUT_SHAPE object and
!       computes the cost of machining (i.e. laser cutting) the part.
!       The max laser velocities and the cost per cutting time are
!       given in the Plethora technical exercise.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE), INTENT (INOUT) :: SHPE
!
!       declare local variables
!
        INTEGER :: I, NUMTOPES
        REAL (KIND = KIND(0.D0)) :: T, RAD
!
!=======================================================================
!
!       loop over edges and compute cutting times based on edge type and
!       length
!
        T = 0.D0
        NUMTOPES = SIZE(SHPE%EDGE_CNTNR)
        DO I = 1,NUMTOPES
          SELECT TYPE (DAT => SHPE%EDGE_CNTNR(I)%EDGE_DAT)
          TYPE IS (LINE_SEGMENT)
            T = T + DAT%LINE_LENGTH / 0.5D0
          TYPE IS (CIRCULAR)
            RAD = NORM2(DAT%XY_VRTCS(:,1) - DAT%XY_CNTR)
            T = T + DAT%ARC_LENGTH / EXP(-1.D0 / RAD)
          END SELECT
        END DO
!
!       compute cutting cost based on calculated cutting time
!
        SHPE%COST = SHPE%COST + 0.07 * T
!
        RETURN
!
      END SUBROUTINE MACHINE_COST
!
!=======================================================================
!
      SUBROUTINE MATERIAL_COST (SHPE)
!
!       This subroutine is a type bound procedure on the CUT_SHAPE
!       object.  This routine computes the material cost from the area
!       of the minimum oriented bounding box based on cost per square
!       inch provided in the Plethora technical exercise.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE), INTENT (INOUT) :: SHPE
!
!       declare local variables
!
!       none
!
!=======================================================================
!
        SHPE%COST = SHPE%COST + 0.75 * SHPE%AREA
!
        RETURN
!
      END SUBROUTINE MATERIAL_COST
!
!=======================================================================
!
      SUBROUTINE DELETE_SHAPE (SHPE)
!
!       This subroutine deallocates the allocatable arrays on the
!       CUT_SHAPE object
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CUT_SHAPE), INTENT (INOUT) :: SHPE
!
!       declare local variables
!
!       none
!
!=======================================================================
!
        IF (ALLOCATED(SHPE%EDGE_CNTNR)) DEALLOCATE (SHPE%EDGE_CNTNR)
        IF (ALLOCATED(SHPE%C_HULL)) DEALLOCATE (SHPE%C_HULL)
!
        RETURN
!
      END SUBROUTINE DELETE_SHAPE
!
!=======================================================================
!     Type Bound Procedures on EDGE%LINE_SEGMENT
!=======================================================================
!
      SUBROUTINE COMP_LINE_LENGTH (LINE)
!
!       This subroutine computes (trivially), the length of an edge that
!       is a line segment.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (LINE_SEGMENT), INTENT (INOUT) :: LINE
!
!       declare local variables
!
!=======================================================================
!
        ASSOCIATE (XY => LINE%XY_VRTCS)
!
        LINE%LINE_LENGTH = NORM2(XY(:,2) - XY(:,1))
!
        END ASSOCIATE
!
        RETURN
!
      END SUBROUTINE COMP_LINE_LENGTH
!
!=======================================================================
!     Type Bound Procedures on EDGE%CIRCULAR
!=======================================================================
!
      SUBROUTINE COMP_ARC_LENGTH (ARC)
!
!       This subroutine computes (trivially), the length of an edge that
!       is a line segment.
!
!=======================================================================
!
!       declare dummy variables
!
        CLASS (CIRCULAR), INTENT (INOUT) :: ARC
!
!       declare local variables
!
        REAL (KIND = KIND(0.D0)) :: V1(2), V2(2), X
!
!=======================================================================
!
        V1 = ARC%XY_VRTCS(:,1) - ARC%XY_CNTR
        V2 = ARC%XY_VRTCS(:,2) - ARC%XY_CNTR
        X = DOT_PRODUCT(V1,V2) / (NORM2(V1) * NORM2(V2))
        ARC%ARC_LENGTH = ACOS(X)
!
        RETURN
!
      END SUBROUTINE COMP_ARC_LENGTH
!
!=======================================================================
!
      END MODULE CUT_COST_M
