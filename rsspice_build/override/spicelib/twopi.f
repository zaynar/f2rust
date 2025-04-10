C$ Keywords
C     CONSTANTS
C-&

      DOUBLE PRECISION FUNCTION TWOPI ( )

C     SPICE uses SAVE to cache this value, but that means we need ctx, which causes
C     some difficulties with the borrow checker, so just recompute it each time.

      TWOPI = ACOS ( -1.D0 ) * 2.D0
      RETURN
      END
