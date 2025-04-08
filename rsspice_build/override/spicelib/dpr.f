C$ Keywords
C     CONSTANTS
C-&

      DOUBLE PRECISION FUNCTION DPR ( )

C     SPICE uses SAVE to cache this value, but that means we need ctx, which causes
C     some difficulties with the borrow checker, so just recompute it each time.

      DPR = 180.D0 / ACOS ( -1.D0 )
      RETURN
      END