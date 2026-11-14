test_that("Mobius maps handle poles inverse composition and projective scaling", {
  reciprocal <- mobius(0,1,1,0)
  expect_equal(reciprocal(c(0,1,Inf)),as.complex(c(Inf,1,0)))
  z <- c(1+2i,-.3i,2)
  f <- mobius(2,1i,.2,1); inverse <- mobius(1,-1i,-.2,2)
  expect_equal(inverse(f(z)),z,tolerance=1e-12)
  expect_equal(mobius(4,2i,.4,2)(z),f(z),tolerance=1e-12)
  expect_error(mobius(1,1,1,1),"determinant")
  expect_error(mobius(a=NA),"finite")
})

test_that("the Riemann sphere represents the complex point at infinity finitely", {
  X <- rbind(c(0,0,1),c(0,0,-1),c(1,0,0))
  got <- mobius(0,1,1,0)(X,model="sphere")
  expect_equal(unname(got),rbind(c(0,0,-1),c(0,0,1),c(1,0,0)),tolerance=1e-12)
  expect_true(all(is.finite(got)))
  expect_equal(rowSums(got^2),rep(1,3),tolerance=1e-12)
  rotation <- mobius(exp(.2i),0,0,exp(-.2i))
  Y <- rotation(X,model="sphere")
  expect_equal(Y%*%t(Y),X%*%t(X),tolerance=1e-12,ignore_attr=TRUE)
  expect_error(rotation(matrix(c(0,0,2),1),model="sphere"),"unit norm")
})

test_that("the half-space extension preserves hyperbolic distance", {
  X <- rbind(c(.2,-.4,.7),c(-1,.5,2),c(.8,.1,.4))
  distance <- function(X) acosh(1+sum((X[1,]-X[2,])^2)/(2*X[1,3]*X[2,3]))
  for(f in list(mobius(2,0,0,1),mobius(1,1+2i,0,1),mobius(0,1,1,0),mobius(1+.2i,.3i,.1,1))) {
    Y <- f(X,model="halfspace")
    expect_true(all(Y[,3]>0)); expect_equal(unname(distance(Y)),distance(X),tolerance=1e-10)
  }
  expect_equal(unname(mobius(2,0,0,1)(X,model="halfspace")),2*X,tolerance=1e-12)
  expect_error(mobius()(matrix(c(0,0,0),1),model="halfspace"),"positive")
})

test_that("sphere portraits expose phase modulus and fine domain colours", {
  p <- ggplot2::ggplot()+stat_riemann_sphere_3d(fun=function(z) z,n=20)+coord_3d()
  d <- ggplot2::layer_data(p)
  expect_equal(d$x^2+d$y^2+d$z^2,rep(1,nrow(d)),tolerance=1e-12)
  expect_true(all(c("arg","mod","domain_colour")%in%names(d)))
  expect_gt(length(unique(d$domain_colour)),30)
  expect_equal(phase_colour_3d(c(0,Inf,NA_complex_),TRUE),c("#000000","#FFFFFF","#808080"))
  expect_silent(validate_scene_packet(stats_scene(p)$packet))
})

test_that("root and logarithm surfaces obey their defining inverse relation", {
  for(domain in c("polar","cartesian")) {
    inverse <- if(domain=="polar") function(w) w^3 else exp
    p <- ggplot2::ggplot()+stat_riemann_surface_3d(fun_inverse=inverse,sheets=3,domain=domain,n=c(10,30))+coord_3d()
    d <- ggplot2::layer_data(p)
    w <- complex(real=d$w_re,imaginary=d$w_im); z <- inverse(w)
    expect_equal(d$x,Re(z),tolerance=1e-10)
    expect_equal(d$y,Im(z),tolerance=1e-10)
    expect_equal(d$z,Re(w),tolerance=1e-10)
    expect_equal(sort(unique(d$sheet)),1:3)
    expect_silent(validate_scene_packet(stats_scene(p)$packet))
  }
})
