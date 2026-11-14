# Incompressible Orszag-Tang example

The article uses the periodic initial data in section 4.1 of Kraus, Tassi and
Grasso (2016). Their canonical Poisson bracket has the opposite sign to the
article's bracket. Translation to the requested curl convention gives
streamfunction `-2*cos(x) + 2*sin(y)` and magnetic flux
`2*cos(x) - cos(2*y)`. Both fields have component amplitudes 2. The original
Orszag and Tang (1979) paper's equations (1)-(4) fix the curl signs and
the energy dissipation identity. In the article's notation,
`[f,g] = f_y*g_x - f_x*g_y`, `u = (psi_y,-psi_x)` and
`B = (a_y,-a_x)`, so `[psi,g] = u.grad(g)` and `j = -Laplacian(a)`.

The solver uses base R, two-dimensional `stats::fft`, a strict Cartesian
two-thirds cutoff and classical explicit RK4. The state consists of Fourier
coefficients of vorticity and magnetic flux. Initial coefficients and all
nonlinear right-hand sides have the same cutoff. The zero-mode inverse
Laplacian is zero. The grid excludes the periodic endpoint.

## Parameters and checks

- Grid: 64 by 64; retained component wavenumbers -21 through 21.
- Viscosity and resistivity: 0.04 each; timestep 0.0025; final time 1.2.
- Frames: every 30 steps, including both endpoints; 17 complete spatial grids.
- The analytic initial right-hand sides agree within 1.26e-11 in maximum norm.
- Ideal finite-truncation energy starts at 4 and cross helicity at -2. Their
  maximum relative drifts are 1.213e-9 and 1.575e-10 through time 1.2.
- With diffusion, the maximum instantaneous Parseval energy-balance residual
  is 1.45e-15. The central-difference time derivative gives a residual of
  7.155e-6 relative to the peak dissipation; halving the timestep reduces it
  to 1.789e-6. The trapezoidal integrated balance has relative error 5.478e-7,
  reducing to 1.371e-7 on halving the timestep.
- Final maximum absolute current: 25.7754516945 at 64 squared and
  26.1305564843 at 128 squared, a 1.359% difference. Both use timestep 0.0025.
- Peak absolute current: 32.1824421456 at time 1.0525 on the article grid.
  Mean squared current peaks at 1.0075 and energy dissipation at 1.005.
  At 128 squared the corresponding peak times are identical at this timestep.
  The dissipation maximum is 1.34362109, compared with approximately 1.3
  near time 1 in curve (b) of figure 1 in Orszag and Tang (1979).
- The maximum spectral CFL measure
  `dt*21*max(abs(u_x)+abs(u_y)+abs(B_x)+abs(B_y))` is 0.408154
  for the resistive run and 0.493495 for the ideal check. The diffusion
  measure `dt*max(nu,eta)*2*21^2` is 0.0882. Both are small enough for
  explicit RK4; an integrating factor is unnecessary at these parameters.
- With `epsilon = nu*mean(omega^2) + eta*mean(j^2)` and the dissipative
  length estimate `ell_d = (nu^3/epsilon)^(1/4)`, the minimum `21*ell_d`
  is 1.74460. The measured current-gradient length
  `sqrt(mean(j^2)/mean(abs(grad(j))^2))` gives a minimum product of 3.36717.
  At the final time, the outermost retained component-wavenumber band
  `max(abs(kx),abs(ky)) > 0.8*21` contains 0.4192% of squared current.

The ideal invariants refer to the finite Fourier truncation. Spatial
resolution of the resistive example is assessed separately by grid doubling,
the dissipative length estimate and the spectral tail.

The numerical chunks in a complete `rmarkdown::render()` took 1.74 seconds
for the solve and frame table and 5.23 seconds for rendering and MP4 encoding,
6.97 seconds in total on this machine. The video is 480 by 480 pixels, 17
frames at 8 frames per second, with duration 2.125 seconds.

Run `dev_notes/mhd-validation.R` from the package root with
`R_MAX_VSIZE=4Gb`. It extracts the three solver chunks from
`vignettes/pde.Rmd` with knitr, parses them and instruments that code for
the checks above. Results are written under `dev_notes/mhd/`.

The rendered article was checked in headless Edge. The embedded MP4 was
extracted and its frames decoded with `av::av_video_images()`. Inspected
files under `dev_notes/mhd/`:

- `article-spectral.png`
- `article-rk4.png`
- `article-current.png`
- `render-frames/image_000001.jpg`
- `render-frames/image_000015.jpg`
- `render-frames/image_000017.jpg`

## Sources inspected

Orszag, S. A., and Tang, C.-M. (1979). Small-scale structure of two-dimensional
magnetohydrodynamic turbulence. Journal of Fluid Mechanics 90, 129-143.
https://doi.org/10.1017/S002211207900210X

Scanned pages 130-131 were checked for equations and initial amplitudes;
pages 132, 134 and 136 for dissipation curves and current-sheet morphology.

Kraus, M., Tassi, E., and Grasso, D. (2016). Variational integrators for reduced
magnetohydrodynamics. Journal of Computational Physics 321, 435-458.
https://doi.org/10.1016/j.jcp.2016.05.047

Section 4.1, page 19 of the arXiv manuscript, was checked visually for both
initial potentials and their amplitudes. Its current sheets provide the
comparison in the same spatial orientation as the article.
