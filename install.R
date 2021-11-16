if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Must install devtools. Run: install.packages('devtools')", call. = FALSE)
}

devtools::install_deps()
devtools::install()

