# Linear and nonlinear PDEs

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
Installation is covered in [Get
started](https://gillescolling.com/ggplot3d/articles/ggplot3d.md); the
videos need [av](https://docs.ropensci.org/av/).

1.  **Tabulate** the solution $`u(x, y, t)`$ in a data frame with one
    row per position and time.

2.  **Draw** one surface per time with
    [`geom_surface_3d()`](https://gillescolling.com/ggplot3d/reference/geom_surface_3d.md).

3.  **Animate** the frames with `animate_3d(time = "t")`.

## Setup

``` r

library(ggplot2)
library(ggplot3d)
theme_set(theme_minimal(base_size = 17) + theme(
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.background = element_rect(fill = "transparent", colour = NA),
  legend.position = "none"))
```

Every solution below is a function `u(x, y, t)` of two space coordinates
and time. `solution_frames()` is a small helper written for this
article, not a function of ggplot3d: it evaluates such a function on a
square grid of `n` by `n` positions at each time in `times`, and returns
one row per position and time with columns `x`, `y`, `t` and `u`.

``` r

solution_frames <- function(u, times, range = c(-2, 2), n = 31) {
  grid <- expand.grid(x = seq(range[1], range[2], length.out = n),
                      y = seq(range[1], range[2], length.out = n),
                      t = times)
  grid$u <- u(grid$x, grid$y, grid$t)
  grid
}
```

`animate_3d(time = "t")` draws the rows of one value of `t` per frame,
in increasing order. The scales are trained on all frames, so the z axis
and the fill colours keep the same range throughout, and a decaying
solution is seen to decay. `animate()` below renders a plot at 480 by
480 pixels, writes an MP4 video into the article’s figure folder and
returns a looping, muted `<video>` element that plays without controls,
labelled with the chunk’s figure description. The page links the video
by its last three path components, the figure folder next to the page.

``` r

animate <- function(plot, fps = 12, ...) {
  path <- knitr::fig_path(".mp4")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  animate_3d(plot, time = "t", fps = fps, file = path, width = 480,
             height = 480, progress = FALSE, ...)
  alt <- knitr::opts_current$get("fig.alt")
  parts <- strsplit(normalizePath(path, winslash = "/"), "/")[[1]]
  src <- paste(tail(parts, 3), collapse = "/")
  htmltools::tags$video(src = src, autoplay = NA, loop = NA, muted = NA,
    playsinline = NA, width = 480, height = 480, `aria-label` = alt,
    style = "width: 100%; max-width: 480px; height: auto;", alt)
}
```

## Linear equations

### Transport equation

The transport equation moves an initial profile $`g`$ at constant
velocity $`b = (b_1, b_2)`$ without changing its shape:

``` math
u_t + b \cdot \nabla u = 0, \qquad u(x, 0) = g(x), \qquad u(x, t) = g(x - t b).
```

The profile here is a bump that is periodic in both coordinates with
period 4,
$`g(x, y) = \exp\{3(\cos(\pi x / 2) - 1) + 3(\cos(\pi y / 2) - 1)\}`$,
moved along the diagonal $`b = (1, 1)`$. After $`t = 4`$ it is back at
its start, so the 32 frames from $`t = 0`$ to $`t = 3.875`$ loop without
a jump.

``` r

transport <- function(x, y, t) {
  exp(3 * (cos(pi * (x - t) / 2) - 1) + 3 * (cos(pi * (y - t) / 2) - 1))
}
moving <- solution_frames(transport, times = seq(0, 4, length.out = 33)[-33])
```

``` r

p <- ggplot(moving, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(low = "grey85", high = PAL[["blue"]]) +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate(p)
```

Animation of a blue bump on a flat surface that travels diagonally
across the square and re-enters at the opposite corner.

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
cooling <- solution_frames(heat, times = seq(0, 1, length.out = 25))
```

``` r

p <- ggplot(cooling, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(low = PAL[["blue"]], high = PAL[["red"]]) +
  coord_3d(azimuth = -50, elevation = 25) +
  labs(z = "u")
animate(p)
```

Animation of a tall red peak that flattens and widens into a low broad
mound while the z axis stays fixed from zero to one.

### Wave equation

The wave equation describes a membrane fixed along its edges. On the
unit square, each product $`\sin(m \pi x) \sin(n \pi y)`$ vibrates at
angular frequency $`\pi \sqrt{m^2 + n^2}`$. The modes
$`(m, n) = (1, 2)`$ and $`(2, 1)`$ share the frequency
$`\omega = \sqrt{5} \pi`$; adding them a quarter period apart gives a
pattern that turns round the centre:

``` math
u_{tt} = \Delta u, \qquad u(x, y, t) = \sin(\pi x) \sin(2 \pi y) \cos(\omega t) + \sin(2 \pi x) \sin(\pi y) \sin(\omega t).
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
membrane <- solution_frames(wave, times = seq(0, period, length.out = 31)[-31],
                            range = c(0, 1))
```

``` r

p <- ggplot(membrane, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient2(low = PAL[["blue"]], mid = "grey90", high = PAL[["red"]]) +
  coord_3d(azimuth = -50, elevation = 30) +
  labs(z = "u")
animate(p)
```

Animation of a square membrane with a raised red lobe and a lowered blue
lobe that circle round its centre while the edges stay at zero.

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
\Delta u = u_{xx} + u_{yy} = 0, \qquad u(x, y) = r^3 \cos(3\theta - \varphi).
```

``` r

laplace <- function(x, y, t) Re(exp(-1i * t) * complex(real = x, imaginary = y)^3)
saddle <- solution_frames(laplace, times = seq(0, 2 * pi, length.out = 37)[-37],
                          range = c(-1, 1))
```

``` r

p <- ggplot(saddle, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient2(low = PAL[["blue"]], mid = "grey90", high = PAL[["red"]]) +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate(p)
```

Animation of a monkey saddle with three raised red ridges and three
lowered blue valleys that rotate slowly about the vertical axis.

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
\tau = 1 + e^{\eta_1} + e^{\eta_2} + A_{12}\, e^{\eta_1 + \eta_2}, \qquad A_{12} = \frac{l^2}{l^2 - 1} \quad \text{for } k_1 = k_2 = 1,\ l_1 = -l_2 = l,
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
crossing <- solution_frames(kp, times = seq(-2.2, 2.2, length.out = 40),
                            range = c(-10, 10), n = 61)
```

``` r

p <- ggplot(crossing, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(low = "grey85", high = PAL[["blue"]]) +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate(p)
```

Animation of two straight blue crests forming an X that travels across
the square, with a raised segment where the crests meet.

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
u = 4 \arctan \frac{e^{\eta_1} + e^{\eta_2}}{1 + a_{12}\, e^{\eta_1 + \eta_2}}, \qquad a_{12} = \frac{(p_1 - p_2)^2 + (q_1 - q_2)^2 - (\omega_1 - \omega_2)^2}{(p_1 + p_2)^2 + (q_1 + q_2)^2 - (\omega_1 + \omega_2)^2}.
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
kinks <- solution_frames(sine_gordon, times = seq(-8, 8, length.out = 33),
                         range = c(-8, 8), n = 51)
```

``` r

p <- ggplot(kinks, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(low = "grey85", high = PAL[["purple"]]) +
  coord_3d(azimuth = -50, elevation = 35) +
  labs(z = "u")
animate(p)
```

Animation of a surface with two raised purple quadrants and two low
quadrants; the corner where the fronts meet slides diagonally across the
square.

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
invasion <- solution_frames(fisher, times = seq(-6, 6, length.out = 33),
                            range = c(-20, 20), n = 41)
```

``` r

p <- ggplot(invasion, aes(x, y, z = u, fill = after_stat(z_mean))) +
  geom_surface_3d(colour = NA) +
  scale_fill_gradient(low = "grey85", high = PAL[["green"]]) +
  coord_3d(azimuth = -50, elevation = 30) +
  labs(z = "u")
animate(p)
```

Animation of a green plateau whose sloping edge advances diagonally
across a flat grey plain until it covers most of the square.

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
  scale_fill_gradient(low = "grey85", high = PAL[["teal"]]) +
  coord_3d(azimuth = -50, elevation = 30) +
  labs(z = "|ψ|")
```

![Surface over space and time that is flat at height one except for a
single sharp peak of height three at the centre, flanked by two shallow
dips.](pde_files/figure-html/nls-1.svg)

## Where next

- [Camera animation and
  interaction](https://gillescolling.com/ggplot3d/articles/motion.md):
  camera paths, MP4 output and the interactive viewer.

- [Layers](https://gillescolling.com/ggplot3d/articles/layers.md): the
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

Peregrine, D. H. (1983). Water waves, nonlinear Schrödinger equations
and their solutions. *Journal of the Australian Mathematical Society,
Series B*, 25, 16–43.
