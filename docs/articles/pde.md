# Animating differential-equation solutions

This article draws exact solutions of eight partial differential
equations (PDEs). The transport, heat, wave and Laplace equations are
the four linear PDEs with explicit solution formulas that Evans (2010)
treats first. The Kadomtsev–Petviashvili, sine-Gordon, Fisher–KPP and
nonlinear Schrödinger equations are four well-known nonlinear PDEs; for
them no general solution formula exists, but particular solutions can be
written in closed form. Each solution is evaluated on a grid of
positions and times, drawn as a surface, and rendered one frame per time
with
[`animate_3d()`](https://gillescolling.com/ggplot3d/reference/animate_3d.md).
A final example solves incompressible magnetohydrodynamics numerically
and draws the computed current density through the same plotting
pipeline. Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md).
[Animating cameras and
data](https://gillescolling.com/ggplot3d/articles/motion.md) explains
frame timing and animation output.

1.  **Define** the solution $`u(x, y, t)`$ and a data frame with one row
    per time.

2.  **Draw** one surface per time with
    [`geom_function_3d()`](https://gillescolling.com/ggplot3d/reference/stat_function_3d.md).

3.  **Animate** the frames with `animate_3d(time = "t")`.

## Setup

``` r

library(ggplot2)
library(ggplot3d)
```

Each exact animated solution below is a function `u(x, y, t)` of two
space coordinates and time.
[`geom_function_3d()`](https://gillescolling.com/ggplot3d/reference/stat_function_3d.md)
evaluates the function on an `n` by `n` grid for each row of
`data.frame(t = times)`, passing that row’s `t` as a named argument. The
x and y scale limits set the spatial range. The layer retains `t` with
each surface.

`animate_3d(time = "t")` draws one value of `t` per frame, in increasing
order. The scales are trained on all frames, so the z axis and the fill
colours keep the same range throughout, and a decaying solution is seen
to decay. The calls below render looping GIF animations at 480 by 480
pixels. In the article, the returned animation embeds as an image
described by the chunk’s figure description.

## Linear equations

### Transport equation

The transport equation moves an initial profile $`g`$ at constant
velocity $`b = (b_1, b_2)`$ without changing its shape:

``` math
\begin{aligned} u_t + b \cdot \nabla u &= 0, \\ u(x, 0) &= g(x), \\ u(x, t) &= g(x - t b). \end{aligned}
```

The profile here is a bump that is periodic in both coordinates with
period 4,
$`g(x, y) = e^{3(\cos(\pi x / 2) - 1)}\, e^{3(\cos(\pi y / 2) - 1)}`$,
moved along the diagonal $`b = (1, 1)`$. After $`t = 4`$ it is back at
its start, so the 32 frames from $`t = 0`$ to $`t = 3.875`$ loop without
a jump.

``` r

transport <- function(x, y, t) {
  exp(3 * (cos(pi * (x - t) / 2) - 1) + 3 * (cos(pi * (y - t) / 2) - 1))
}
moving <- data.frame(t = seq(0, 4, length.out = 33)[-33])
```

``` r

p <- ggplot(moving, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = transport, n = 31, colour = NA) +
  xlim(-2, 2) + ylim(-2, 2) +
  scale_fill_gradient(low = "grey85", high = "#2E6F9E") +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of a blue bump on a flat surface that travels diagonally
across the square and re-enters at the opposite
corner.](pde_files/figure-html/transport-show-1.gif)

![Animation of a blue bump on a flat surface that travels diagonally
across the square and re-enters at the opposite
corner.](pde_files/figure-html/transport-show-2.gif)

### Heat equation

The heat equation spreads an initial temperature $`g`$ by diffusion.
With unit diffusivity and the Gaussian $`g(x) = \exp(-|x|^2)`$ on the
whole plane, the solution stays Gaussian; its width grows and its peak
falls:

``` math
u_t = \Delta u, \qquad u(x, t) = \frac{1}{1 + 4t} \exp\left(-\frac{|x|^2}{1 + 4t}\right).
```

The peak value $`1 / (1 + 4t)`$ falls from 1 at $`t = 0`$ to 0.2 at
$`t = 1`$, while the integral of $`u`$ over the plane stays at $`\pi`$.

``` r

heat <- function(x, y, t) exp(-(x^2 + y^2) / (1 + 4 * t)) / (1 + 4 * t)
cooling <- data.frame(t = seq(0, 1, length.out = 25))
```

``` r

p <- ggplot(cooling, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = heat, n = 31, colour = NA) +
  xlim(-2, 2) + ylim(-2, 2) +
  scale_fill_gradient(low = "#2E6F9E", high = "#B5342B") +
  coord_3d(azimuth = -50, elevation = 25) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of a tall red peak that flattens and widens into a low broad
mound while the z axis stays fixed from zero to
one.](pde_files/figure-html/heat-show-1.gif)

![Animation of a tall red peak that flattens and widens into a low broad
mound while the z axis stays fixed from zero to
one.](pde_files/figure-html/heat-show-2.gif)

### Wave equation

The wave equation describes a membrane fixed along its edges. On the
unit square, each product $`\sin(m \pi x) \sin(n \pi y)`$ vibrates at
angular frequency $`\pi \sqrt{m^2 + n^2}`$. The modes
$`(m, n) = (1, 2)`$ and $`(2, 1)`$ share the frequency
$`\omega = \sqrt{5} \pi`$; adding them a quarter period apart gives a
pattern that turns round the centre:

``` math
\begin{aligned} u_{tt} &= \Delta u, \\ u(x, y, t) &= \sin(\pi x) \sin(2 \pi y) \cos(\omega t) \\ &\quad + \sin(2 \pi x) \sin(\pi y) \sin(\omega t). \end{aligned}
```

The solution repeats after one period
$`T = 2 \pi / \omega = 2 / \sqrt{5}`$, so 30 frames over $`[0, T)`$
loop.

``` r

omega <- sqrt(5) * pi
wave <- function(x, y, t) {
  sin(pi * x) * sin(2 * pi * y) * cos(omega * t) +
    sin(2 * pi * x) * sin(pi * y) * sin(omega * t)
}
period <- 2 * pi / omega
membrane <- data.frame(t = seq(0, period, length.out = 31)[-31])
```

``` r

p <- ggplot(membrane, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = wave, n = 31, colour = NA) +
  xlim(0, 1) + ylim(0, 1) +
  scale_fill_gradient2(low = "#2E6F9E", mid = "grey90", high = "#B5342B") +
  coord_3d(azimuth = -50, elevation = 30) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of a square membrane with a raised red lobe and a lowered
blue lobe that circle round its centre while the edges stay at
zero.](pde_files/figure-html/wave-show-1.gif)

![Animation of a square membrane with a raised red lobe and a lowered
blue lobe that circle round its centre while the edges stay at
zero.](pde_files/figure-html/wave-show-2.gif)

### Laplace equation

A solution of the Laplace equation is the steady state of the heat
equation: it has no time derivative, and its values inside a region are
fixed by its values on the boundary. The real part of any polynomial in
$`x + iy`$ solves it. The frames below take
$`u = \operatorname{Re}\{e^{-i\varphi} (x + iy)^3\} = r^3 \cos(3\theta - \varphi)`$
in polar coordinates $`(r, \theta)`$, and use the phase $`\varphi`$ as
the frame variable `t`: every frame is an exact harmonic function, with
boundary values that rotate as $`\varphi`$ grows. Raising $`\varphi`$ by
$`2\pi`$ turns the saddle by a third of a turn, which maps it onto
itself, so 36 frames over $`\varphi \in [0, 2\pi)`$ loop.

``` math
\begin{aligned} \Delta u &= u_{xx} + u_{yy} = 0, \\ u(x, y) &= r^3 \cos(3\theta - \varphi). \end{aligned}
```

``` r

laplace <- function(x, y, t) Re(exp(-1i * t) * complex(real = x, imaginary = y)^3)
saddle <- data.frame(t = seq(0, 2 * pi, length.out = 37)[-37])
```

``` r

p <- ggplot(saddle, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = laplace, n = 31, colour = NA) +
  xlim(-1, 1) + ylim(-1, 1) +
  scale_fill_gradient2(low = "#2E6F9E", mid = "grey90", high = "#B5342B") +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of a monkey saddle with three raised red ridges and three
lowered blue valleys that rotate slowly about the vertical
axis.](pde_files/figure-html/laplace-show-1.gif)

![Animation of a monkey saddle with three raised red ridges and three
lowered blue valleys that rotate slowly about the vertical
axis.](pde_files/figure-html/laplace-show-2.gif)

## Nonlinear equations

### Kadomtsev–Petviashvili equation

The Korteweg–de Vries (KdV) equation $`u_t + 6uu_x + u_{xxx} = 0`$
describes long waves in shallow water. Its solitary waves, or solitons,
keep their shape as they travel, and two solitons pass through each
other and come out unchanged apart from a shift in position. The
Kadomtsev–Petviashvili (KP) equation extends it to waves on a plane that
vary slowly across their direction of travel:

``` math
(u_t + 6uu_x + u_{xxx})_x + 3u_{yy} = 0.
```

Hirota’s direct method (Hirota 2004) writes its solitons as
$`u = 2 (\log \tau)_{xx}`$. With $`\eta_i = k_i x + l_i y - \omega_i t`$
and $`\omega_i = (k_i^4 + 3 l_i^2) / k_i`$, the function
$`\tau = 1 + e^{\eta_1}`$ gives one line soliton, a straight crest of
height $`k_1^2 / 2`$, and

``` math
\begin{aligned} \tau &= 1 + e^{\eta_1} + e^{\eta_2} + A_{12}\, e^{\eta_1 + \eta_2}, \\ A_{12} &= \dfrac{l^2}{l^2 - 1} \\ &\quad \text{for } k_1 = k_2 = 1,\ l_1 = -l_2 = l, \end{aligned}
```

gives two line solitons that cross in an X. The solution is regular for
$`l > 1`$. With $`l = 1.2`$ both crests travel in the $`x`$ direction at
speed $`\omega_1 / k_1 = 5.32`$, and the segment where they meet is
shifted forward and stands higher than either crest alone.

``` r

l <- 1.2
kp_omega <- 1 + 3 * l^2
kp <- function(x, y, t) {
  e1 <- exp(x + l * y - kp_omega * t)
  e2 <- exp(x - l * y - kp_omega * t)
  a12 <- l^2 / (l^2 - 1)
  tau <- 1 + e1 + e2 + a12 * e1 * e2
  tau_x <- e1 + e2 + 2 * a12 * e1 * e2
  tau_xx <- e1 + e2 + 4 * a12 * e1 * e2
  2 * (tau_xx * tau - tau_x^2) / tau^2
}
crossing <- data.frame(t = seq(-2.2, 2.2, length.out = 40))
```

``` r

p <- ggplot(crossing, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = kp, n = 61, colour = NA) +
  xlim(-10, 10) + ylim(-10, 10) +
  scale_fill_gradient(low = "grey85", high = "#2E6F9E") +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of two straight blue crests forming an X that travels across
the square, with a raised segment where the crests
meet.](pde_files/figure-html/kp-show-1.gif)

![Animation of two straight blue crests forming an X that travels across
the square, with a raised segment where the crests
meet.](pde_files/figure-html/kp-show-2.gif)

### Sine-Gordon equation

The sine-Gordon equation models, among other systems, the phase
difference across a long Josephson junction and dislocations in a
crystal lattice:

``` math
u_{tt} - u_{xx} - u_{yy} + \sin u = 0.
```

Its kink $`u = 4 \arctan e^{\eta}`$, with
$`\eta = p x + q y - \omega t`$ and $`p^2 + q^2 - \omega^2 = 1`$, is a
front across which $`u`$ steps from $`0`$ to $`2\pi`$ and which moves at
speed $`\omega / \sqrt{p^2 + q^2} < 1`$. Two kinks combine in closed
form (Hirota 2004):

``` math
\begin{aligned} u &= 4 \arctan \dfrac{e^{\eta_1} + e^{\eta_2}}{1 + a_{12}\, e^{\eta_1 + \eta_2}}, \\ a_{12} &= \dfrac{s_-}{s_+}, \\ s_\pm &= (p_1 \pm p_2)^2 + (q_1 \pm q_2)^2 - (\omega_1 \pm \omega_2)^2. \end{aligned}
```

The frames use one front along the $`y`$ axis moving in $`+x`$ and one
along the $`x`$ axis moving in $`-y`$, both at speed 0.6. The surface
stands at $`2\pi`$ in the two opposite quadrants where exactly one of
$`\eta_1`$, $`\eta_2`$ is positive and lies near 0 in the other two; the
corner where the fronts cross moves along the diagonal.

``` r

sine_gordon <- function(x, y, t) {
  p <- 1.25
  w <- sqrt(p^2 - 1)
  e1 <- exp(p * x - w * t)
  e2 <- exp(p * y + w * t)
  a12 <- (2 * p^2 - 4 * w^2) / (2 * p^2)
  4 * atan((e1 + e2) / (1 + a12 * e1 * e2))
}
kinks <- data.frame(t = seq(-8, 8, length.out = 33))
```

``` r

p <- ggplot(kinks, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = sine_gordon, n = 51, colour = NA) +
  xlim(-8, 8) + ylim(-8, 8) +
  scale_fill_gradient(low = "grey85", high = "#7A4F9E") +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of a surface with two raised purple quadrants and two low
quadrants; the corner where the fronts meet slides diagonally across the
square.](pde_files/figure-html/sg-show-1.gif)

![Animation of a surface with two raised purple quadrants and two low
quadrants; the corner where the fronts meet slides diagonally across the
square.](pde_files/figure-html/sg-show-2.gif)

### Fisher–KPP equation

Fisher (1937) and Kolmogorov, Petrovsky and Piskunov introduced this
equation for the spread of an advantageous gene; in ecology it is the
standard model of a population that disperses by diffusion and grows
logistically up to its carrying capacity $`u = 1`$:

``` math
u_t = \Delta u + u (1 - u).
```

Travelling fronts exist for every speed $`c \ge 2`$. For the speed
$`c = 5 / \sqrt{6}`$, Ablowitz and Zeppetella (1979) found the front in
closed form. Moving in the direction of a unit vector $`n`$, it is

``` math
u(x, t) = \left(1 + e^{(n \cdot x - c t) / \sqrt{6}}\right)^{-2}.
```

The frames use $`n = (0.8, 0.6)`$: the occupied region at
$`u \approx 1`$ advances into the empty region at $`u \approx 0`$
without changing the shape of the front.

``` r

fisher <- function(x, y, t) {
  1 / (1 + exp((0.8 * x + 0.6 * y - 5 / sqrt(6) * t) / sqrt(6)))^2
}
invasion <- data.frame(t = seq(-6, 6, length.out = 33))
```

``` r

p <- ggplot(invasion, aes(fill = after_stat(z_mean))) +
  geom_function_3d(fun = fisher, n = 41, colour = NA) +
  xlim(-20, 20) + ylim(-20, 20) +
  scale_fill_gradient(low = "grey85", high = "#3F7A3D") +
  coord_3d(azimuth = -50, elevation = 30) +
  labs(z = "u")
animate_3d(p, time = "t", fps = 12, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of a green plateau whose sloping edge advances diagonally
across a flat grey plain until it covers most of the
square.](pde_files/figure-html/fisher-show-1.gif)

![Animation of a green plateau whose sloping edge advances diagonally
across a flat grey plain until it covers most of the
square.](pde_files/figure-html/fisher-show-2.gif)

### Nonlinear Schrödinger equation

The cubic nonlinear Schrödinger (NLS) equation governs the slowly
varying envelope $`\psi`$ of deep-water wave trains and of light pulses
in optical fibres:

``` math
i \psi_t + \tfrac{1}{2} \psi_{xx} + |\psi|^2 \psi = 0.
```

Its Peregrine solution (Peregrine 1983),

``` math
\psi(x, t) = \left(1 - \frac{4 (1 + 2 i t)}{1 + 4 x^2 + 4 t^2}\right) e^{i t},
```

is a plane wave of amplitude 1 carrying one local disturbance that grows
out of the background, reaches three times the background amplitude at
$`x = t = 0`$ and disappears again. It is a standard model of a rogue
wave. Because it depends on a single space coordinate, the whole event
fits in one surface of $`|\psi|`$ over the $`(x, t)`$ plane, drawn here
as a still figure.

``` r

peregrine <- expand.grid(x = seq(-4, 4, length.out = 81),
                         t = seq(-3, 3, length.out = 61))
peregrine$amplitude <- with(peregrine,
  Mod(1 - 4 * (1 + 2i * t) / (1 + 4 * x^2 + 4 * t^2)))
```

``` r

ggplot(peregrine, aes(x, t, z = amplitude, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(low = "grey85", high = "#2A7A7A") +
  coord_3d(azimuth = -50, elevation = 30) +
  labs(z = "|ψ|")
```

![Surface over space and time that is flat at height one except for a
single sharp peak of height three at the centre, flanked by two shallow
dips.](pde_files/figure-html/nls-1.svg)

## Numerical solutions

A numerical solver supplies the values when a solution formula is
unavailable. The Orszag–Tang vortex describes the coupled motion of an
incompressible conducting fluid and its magnetic field in two
dimensions. Orszag and Tang (1979) studied it with a spectral method on
the periodic square $`[0,2\pi)^2`$.

Write the velocity as $`u=(\psi_y,-\psi_x)`$ and the magnetic field,
measured in velocity units, as $`B=(a_y,-a_x)`$. Here $`\psi`$ is the
streamfunction and $`a`$ the magnetic flux function. The vorticity
$`\omega`$ and current density $`j`$ are

``` math
\omega=-\Delta\psi,\qquad j=-\Delta a.
```

With the bracket $`[f,g]=f_y g_x-f_x g_y`$, we have
$`[\psi,g]=u\cdot\nabla g`$. The equations, with kinematic viscosity
$`\nu`$ and magnetic diffusivity $`\eta`$, are

``` math
\begin{aligned}
\omega_t+[\psi,\omega]&=[a,j]+\nu\Delta\omega,\\
a_t+[\psi,a]&=\eta\Delta a.
\end{aligned}
```

Subscripts denote partial derivatives, and
$`\Delta=\partial_x^2+\partial_y^2`$. These signs follow the curl
convention in Orszag and Tang (1979). The initial data of Kraus, Tassi
and Grasso (2016, section 4.1), translated from their opposite bracket
convention, are

``` math
\begin{aligned} \psi(x,y,0)&=-2\cos x+2\sin y,\\ a(x,y,0)&=2\cos x-\cos(2y). \end{aligned}
```

The solver uses a $`64\times64`$ grid, $`\nu=\eta=0.04`$ and time step
$`0.0025`$. Fourier differentiation gives spatial derivatives; products
are evaluated on the physical grid. The two-thirds rule keeps only
component wavenumbers $`|k_x|,|k_y|<n/3`$ in the initial state and every
nonlinear term. The inverse Laplacian sets the mean streamfunction to
zero.

``` r

n <- 64L
nu <- eta <- 0.04
dt <- 0.0025
steps <- 480L
save_every <- 30L

x <- 2 * pi * (0:(n - 1)) / n
grid <- expand.grid(x = x, y = x)
k <- c(0:(n / 2 - 1), (-n / 2):-1)
kx <- matrix(k, n, n)
ky <- t(kx)
k2 <- kx^2 + ky^2
inverse_k2 <- 1 / pmax(k2, 1)
inverse_k2[1, 1] <- 0
keep <- abs(kx) < n / 3 & abs(ky) < n / 3
physical <- function(f) Re(fft(f, inverse = TRUE)) / n^2
gradient <- function(f) {
  list(x = physical(1i * kx * f), y = physical(1i * ky * f))
}
bracket <- function(df, dg) df$y * dg$x - df$x * dg$y
```

The complex array `q` stores Fourier coefficients of $`\omega`$ and
$`a`$ in its two slices. Each evaluation of the right-hand side recovers
$`\psi`$, differentiates the four fields and transforms the two
nonlinear terms back to Fourier space.

``` r

rhs <- function(q) {
  w <- q[, , 1]
  a <- q[, , 2]
  dp <- gradient(w * inverse_k2)
  dw <- gradient(w)
  da <- gradient(a)
  dj <- gradient(k2 * a)
  array(c(keep * fft(-bracket(dp, dw) + bracket(da, dj)) - nu * k2 * w,
          -keep * fft(bracket(dp, da)) - eta * k2 * a), dim = dim(q))
}
```

Classical fourth-order Runge–Kutta (RK4) advances both fields. With
largest retained component wavenumber $`K=21`$, the fixed step satisfies
the Courant–Friedrichs–Lewy (CFL) bound

``` math
\Delta t\,K\max_{x,y}(|\psi_x|+|\psi_y|+|a_x|+|a_y|)<1.
```

Its measured maximum over the run is 0.409; the diffusion quantity
$`\Delta t\,\max(\nu,\eta)\,2K^2`$ is 0.0882. These values allow
diffusion to be integrated explicitly in the same RK4 stages. Every 30
steps the loop stores a complete grid of $`j`$ in a data frame with
columns `x`, `y`, `t` and `value`: 17 frames from $`t=0`$ to $`t=1.2`$.

``` r

psi0 <- with(grid, matrix(-2 * cos(x) + 2 * sin(y), n, n))
a0 <- with(grid, matrix(2 * cos(x) - cos(2 * y), n, n))
q <- array(c(k2 * fft(psi0), fft(a0)), c(n, n, 2))
q[, , 1] <- keep * q[, , 1]
q[, , 2] <- keep * q[, , 2]
frames <- vector("list", steps / save_every + 1L)
for (step in 0:steps) {
  if (step %% save_every == 0L) {
    frames[[step / save_every + 1L]] <- data.frame(
      grid, t = step * dt, value = as.vector(physical(k2 * q[, , 2])))
  }
  if (step == steps) break
  k1 <- rhs(q)
  k2_rk <- rhs(q + dt * k1 / 2)
  k3 <- rhs(q + dt * k2_rk / 2)
  k4 <- rhs(q + dt * k3)
  q <- q + dt * (k1 + 2 * k2_rk + 2 * k3 + k4) / 6
}
current <- do.call(rbind, frames)
```

For validation, let angle brackets denote the spatial mean,
$`E=\tfrac12\langle|u|^2+|B|^2\rangle`$ the total energy and
$`H=\langle u\cdot B\rangle`$ the cross helicity. The energy balance is

``` math
\frac{dE}{dt}=-\varepsilon,\qquad
\varepsilon=\nu\langle\omega^2\rangle+\eta\langle j^2\rangle.
```

Using $`\ell_d=(\nu^3/\varepsilon)^{1/4}`$ as a dissipation-length
estimate gives $`\min_t K\ell_d=1.74`$ for these parameters. In the
ideal Fourier truncation ($`\nu=\eta=0`$), the maximum relative energy
and cross-helicity drifts over the same interval are
$`1.3\times10^{-9}`$ and $`1.6\times10^{-10}`$; with diffusion, a
centred time difference checks the energy balance to
$`7.2\times10^{-6}`$ relative to peak dissipation. Doubling the grid to
$`128^2`$ changes final $`\max|j|`$ from 25.7755 to 26.1306, or 1.36%.

The current sheets sharpen as magnetic-field gradients grow. Peak
$`\max|j|`$ is about 32.2 at $`t=1.05`$; dissipation peaks near
$`t=1.0`$, consistent with the growth and decay in Orszag and Tang’s
published curves. The computed values now pass directly to
[`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md)
and `animate_3d(time = "t")`.

``` r

p <- ggplot(current, aes(x, y, z = value, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient2(low = "#2E6F9E", mid = "grey90", high = "#B5342B") +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "j")
animate_3d(p, time = "t", fps = 8, width = 480, height = 480,
  format = "gif", progress = FALSE)
```

![Animation of current density in the incompressible Orszag-Tang vortex,
with blue and red current sheets sharpening into narrow ridges and
valleys.](pde_files/figure-html/mhd-show-1.gif)

![Animation of current density in the incompressible Orszag-Tang vortex,
with blue and red current sheets sharpening into narrow ridges and
valleys.](pde_files/figure-html/mhd-show-2.gif)

## Where next

- [Animating cameras and
  data](https://gillescolling.com/ggplot3d/articles/motion.md): camera
  paths, MP4 output and the interactive viewer.

- [Choosing layers for observations and
  grids](https://gillescolling.com/ggplot3d/articles/layers.md): the
  layers used here and their aesthetics.

## References

Ablowitz, M. J., & Zeppetella, A. (1979). Explicit solutions of Fisher’s
equation for a special wave speed. *Bulletin of Mathematical Biology*,
41, 835–840.

Evans, L. C. (2010). *Partial Differential Equations* (2nd ed.).
Graduate Studies in Mathematics 19. American Mathematical Society.

Fisher, R. A. (1937). The wave of advance of advantageous genes. *Annals
of Eugenics*, 7, 355–369.

Hirota, R. (2004). *The Direct Method in Soliton Theory*. Cambridge
University Press.

Kraus, M., Tassi, E., & Grasso, D. (2016). Variational integrators for
reduced magnetohydrodynamics. *Journal of Computational Physics*, 321,
435–458. <https://doi.org/10.1016/j.jcp.2016.05.047>

Orszag, S. A., & Tang, C.-M. (1979). Small-scale structure of
two-dimensional magnetohydrodynamic turbulence. *Journal of Fluid
Mechanics*, 90, 129–143. <https://doi.org/10.1017/S002211207900210X>

Peregrine, D. H. (1983). Water waves, nonlinear Schrödinger equations
and their solutions. *Journal of the Australian Mathematical Society,
Series B*, 25, 16–43.
