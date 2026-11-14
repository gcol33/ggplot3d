purled <- tempfile(fileext = ".R")
article <- readLines("vignettes/pde.Rmd", encoding = "UTF-8")
first <- match("```{r mhd-spectral}", article)
last <- which(startsWith(article, "```{r mhd,")) - 1L
stopifnot(length(first) == 1L, length(last) == 1L, !is.na(first), last > first)
invisible(knitr::purl(text = article[first:last], output = purled,
                     documentation = 0, quiet = TRUE))
code <- parse(purled, keep.source = FALSE)
unlink(purled)
dir.create("dev_notes/mhd", showWarnings = FALSE)
run <- function(n_grid = 64L, diffusivity = 0.02, timestep = 0.01, end = 2) {
  e <- new.env(parent = globalenv())
  eval(code[1:5], e)
  e$n <- n_grid
  e$nu <- e$eta <- diffusivity
  e$dt <- timestep
  e$steps <- as.integer(round(end / timestep))
  e$save_every <- 1L
  instrumented <- code[-(1:5)]
  loop_index <- which(vapply(instrumented, function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("for"))
  }, logical(1)))
  loop <- instrumented[[loop_index]]
  loop[[4]] <- as.call(c(list(as.name("{"),
    quote(trajectory[[step + 1L]] <- diagnostics(environment()))),
    as.list(loop[[4]])[-1L]))
  instrumented[[loop_index]] <- loop
  e$trajectory <- vector("list", e$steps + 1L)
  elapsed <- system.time(eval(instrumented, e))[["elapsed"]]
  e$elapsed <- elapsed
  e
}
diagnostics <- function(e) {
  eval(quote({
    a <- q[, , 2]
    w <- q[, , 1]
    p <- w * inverse_k2
    E <- sum(k2 * (Mod(p)^2 + Mod(a)^2)) / (2 * n^4)
    H <- sum(Re(w * Conj(a))) / n^4
    J2 <- sum(Mod(k2 * a)^2) / n^4
    W2 <- sum(Mod(w)^2) / n^4
    dissipation <- nu * W2 + eta * J2
    ell_j <- sqrt(J2 / (sum(k2 * Mod(k2 * a)^2) / n^4))
    dq <- rhs(q)
    E_dot <- sum(Re(Conj(p) * dq[, , 1] +
                        k2 * Conj(a) * dq[, , 2])) / n^4
    du <- gradient(p)
    db <- gradient(a)
    c(E = E, H = H, J2 = J2, max_j = max(abs(physical(k2 * a))),
      current_tail = sum(Mod(k2 * a)[pmax(abs(kx), abs(ky)) >
                              0.8 * floor(n / 3)]^2) / (n^4 * J2),
      dissipation = dissipation, balance = E_dot + dissipation,
      kmax_ell_j = floor(n / 3) * ell_j,
      kmax_ell_K = if (nu > 0) floor(n / 3) * (nu^3 / dissipation)^0.25 else NA,
      CFL = dt * floor(n / 3) * max(abs(du$x) + abs(du$y) +
                                    abs(db$x) + abs(db$y)),
      diffusion = dt * max(nu, eta) * 2 * floor(n / 3)^2)
  }), e)
}
initial <- run(64, 0.04, 0.0025, 0)
initial_rhs <- initial$rhs(initial$q)
expected_w <- with(initial$grid, matrix(
  12 * sin(x) * sin(2 * y) - 0.04 * (-2 * cos(x) + 2 * sin(y)), 64, 64))
expected_a <- with(initial$grid, matrix(
  4 * sin(x) * (cos(y) + sin(2 * y)) -
    0.04 * (2 * cos(x) - 4 * cos(2 * y)), 64, 64))
sign_error <- max(abs(initial$physical(initial_rhs[, , 1]) - expected_w),
                  abs(initial$physical(initial_rhs[, , 2]) - expected_a))
stopifnot(sign_error < 1e-10)
cat("Analytic initial RHS error", sign_error, "\n")
results <- list()
for (setting in list(c(64, 0, 0.0025, 1.2), c(64, 0.04, 0.0025, 1.2),
                     c(128, 0.04, 0.0025, 1.2), c(64, 0.04, 0.00125, 1.2))) {
  e <- run(setting[1], setting[2], setting[3], setting[4])
  d <- diagnostics(e)
  history <- t(vapply(e$frames, function(f) {
    c(max_j = max(abs(f$value)), J2 = mean(f$value^2))
  }, numeric(2)))
  label <- paste(setting, collapse = "/")
  trajectory <- do.call(rbind, e$trajectory)
  energy <- trajectory[, "E"]
  diss <- trajectory[, "dissipation"]
  central_residual <- diff(energy, lag = 2) / (2 * e$dt) +
    diss[-c(1, length(diss))]
  results[[label]] <- c(d, elapsed = e$elapsed,
                       energy_drift = max(abs(energy / energy[1] - 1)),
                       helicity_drift = max(abs(trajectory[, "H"] /
                                                 trajectory[1, "H"] - 1)),
                       max_CFL = max(trajectory[, "CFL"]),
                       min_kmax_ell_j = min(trajectory[, "kmax_ell_j"]),
                       min_kmax_ell_K = min(trajectory[, "kmax_ell_K"]),
                       max_balance = max(abs(trajectory[, "balance"])),
                       central_balance_relative = if (max(diss) > 0)
                         max(abs(central_residual)) / max(diss) else NA,
                       integrated_balance_relative = if (max(diss) > 0)
                         abs(tail(energy, 1) - energy[1] +
                         e$dt * sum((head(diss, -1) + tail(diss, -1)) / 2)) /
                         abs(tail(energy, 1) - energy[1]) else NA,
                       peak_j = max(history[, 1]),
                       peak_j_time = e$dt * (which.max(history[, 1]) - 1L),
                       peak_J2_time = e$dt * (which.max(history[, 2]) - 1L),
                       peak_dissipation = max(diss),
                       peak_dissipation_time = e$dt * (which.max(diss) - 1L))
  print(label)
  print(results[[label]], digits = 12)
  saveRDS(list(diagnostics = results[[label]], history = history,
               trajectory = trajectory, q = e$q),
          paste0("dev_notes/mhd/run-", gsub("/", "-", label), ".rds"))
}
saveRDS(results, "dev_notes/mhd/validation.rds")
ideal <- results[[1]]
coarse <- results[[2]]
fine <- results[[3]]
half_dt <- results[[4]]
resolution_error <- abs(coarse[["max_j"]] / fine[["max_j"]] - 1)
stopifnot(ideal[["energy_drift"]] < 1e-6,
          ideal[["helicity_drift"]] < 1e-6,
          all(vapply(results, function(d) d[["max_CFL"]], numeric(1)) < 1),
          coarse[["max_balance"]] < 1e-12,
          coarse[["central_balance_relative"]] < 1e-5,
          resolution_error < 0.02,
          half_dt[["central_balance_relative"]] <
            coarse[["central_balance_relative"]] / 3.9,
          coarse[["min_kmax_ell_K"]] > 1.7)
cat("Final peak-current grid difference", resolution_error, "\n")
benchmark <- new.env(parent = globalenv())
elapsed <- system.time(eval(code, benchmark))[["elapsed"]]
stopifnot(nrow(benchmark$current) == 17L * 64L^2,
          length(unique(benchmark$current$t)) == 17L,
          max(benchmark$current$t) == 1.2,
          all(is.finite(benchmark$current$value)))
cat("Article solver seconds", elapsed, "\n")
