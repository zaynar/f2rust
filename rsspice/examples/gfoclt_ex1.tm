KPL/MK

File name: gfoclt_ex1.tm

This meta-kernel is intended to support operation of SPICE
example programs. The kernels shown here should not be
assumed to contain adequate or correct versions of data
required by SPICE-based user applications.

In order for an application to use this meta-kernel, the
kernels referenced here must be present in the user's
current working directory.

The names and contents of the kernels referenced
by this meta-kernel are as follows:

   File name                     Contents
   ---------                     --------
   de421.bsp                     Planetary ephemeris
   pck00008.tpc                  Planet orientation and
                                 radii
   naif0009.tls                  Leapseconds


\begindata

   KERNELS_TO_LOAD = ( 'kernels/de432s.bsp',
                       'kernels/pck00010.tpc',
                       'kernels/naif0012.tls'  )

\begintext

End of meta-kernel
