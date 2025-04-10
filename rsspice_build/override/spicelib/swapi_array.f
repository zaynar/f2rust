C     See GrammarPatcher::patch_swap

      SUBROUTINE SWAPI_ARRAY (A, B, ARRAY)

      IMPLICIT NONE

      INTEGER ARRAY (*)
      INTEGER TEMP
      INTEGER A, B

      TEMP = ARRAY(A)
      ARRAY(A) = ARRAY(B)
      ARRAY(B) = TEMP
      RETURN
      END
