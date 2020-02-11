library(dplyr)
library(rlang)
library(purrr)
library(tibble)
library(profvis)

if (!dir.exists("../bench-libs")) dir.create("../bench-libs")

if (!dir.exists("../bench-libs/0.8.3")) {
  dir.create("../bench-libs/0.8.3")
  pak::pkg_install("dplyr", lib = "../bench-libs/0.8.3", ask = FALSE)
}

libs <- list.files("../bench-libs", full.names = TRUE)
names(libs) <- basename(libs)

libs <- c(libs, "master" = .libPaths())

prof <- function(setup, code, libpath = .libPaths(), times = 1000){
  setup <- enquo(setup)
  code <- enquo(code)
  f <- new_function(NULL, quo_squash(expr({
    library(dplyr, warn.conflicts = FALSE)
    !!setup
    profvis::profvis(for(i in 1:(!!times)){
      !!code
    })
  })))
  callr::r(f, libpath = libpath)
}
prof(
  df <- tibble(x = rnorm(1e6), g = sample(rep(1:1e4, 100))) %>% group_by(g),
  summarise(df, n = .Internal(mean(x)))
)
