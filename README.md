# NOEM
The Nitric Oxide Empirical Model

An empirical model of nitric oxide (NO) in the lower thermosphere
(100 - 150 km altitude), based on measurements from the Student
Nitric Oxide Explorer (SNOE). Model uses empirical orthogonal functions
(EOFs) derived from the SNOE dataset to describe spatial variability
in NO. Model NO is the sum of a mean distribution and EOFs multiplied
by coefficients based on geophysical parameters. These geophysical
parameters are day of year, Kp magnetic index and F10.7 solar uv index.

Model is utilized by calling subroutine snoe_zm(), which returns
a 2-D zonal mean distribution of NO on geomagnetic coordinates. 
Altitude is fixed to SNOE grid (every 3.33 km).

Reference: Marsh, D. R., S. C. Solomon, and A. E. Reynolds (2004), Empirical model
of nitric oxide in the lower thermosphere, J. Geophys. Res., 109, 
A07301, doi:10.1029/2003JA010199.
