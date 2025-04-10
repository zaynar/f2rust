C     See GrammarPatcher::patch_swap

      SUBROUTINE SWAPI_ARRAY ()

      IMPLICIT NONE

      INTEGER ARRAY (*)
      INTEGER TEMP
      INTEGER A1, A2
      INTEGER B1, B2

      RETURN

      ENTRY SWAPI_ARRAY_1 (ARRAY, A1, B1)
      TEMP = ARRAY(A1)
      ARRAY(A1) = ARRAY(B1)
      ARRAY(B1) = TEMP
      RETURN

      ENTRY SWAPI_ARRAY_2 (ARRAY, A1, A2, B1, B2)
      TEMP = ARRAY(A1,A2)
      ARRAY(A1,A2) = ARRAY(B1,B2)
      ARRAY(B1,B2) = TEMP
      RETURN

      END
