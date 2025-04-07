      SUBROUTINE MOVED (ARRFRM, NDIM, ARRTO)

C     SPICE's implementation calls MOVEI(ARRFRM, 2*NDIM, ARRTO) which
C     is not going to work in Rust without unsafe code, so we'll
C     replace it with a trivial equivalent implementation.

      IMPLICIT NONE

      DOUBLE PRECISION ARRFRM (*)
      INTEGER NDIM
      DOUBLE PRECISION ARRTO (*)

      INTEGER I
      DO I = 1, NDIM
        ARRTO(I) = ARRFRM(I)
      END DO
      RETURN
      END