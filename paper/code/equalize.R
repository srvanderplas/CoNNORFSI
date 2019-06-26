

equalize <- function(x) {
  # Stolen from EBImage and modified
  if (max(x) > 1) { # Assume LAB
    range <- c(0, 100)
  } else {
    range <- c(0, 1)
  }

  levels <- 1024
  r <- range(x)
  if (r[1L] == r[2L]) {
    warning("Image histogram is single-valued and cannot be equalized.")
    x
  } else {
    if (!is.numeric(range) || length(range) != 2L) {
      stop("'range' must be a numeric vector of length 2.")
    }

    levels <- as.integer(levels)
    if (is.na(levels) || levels < 1L) {
      stop("Levels must be at least equal 1.")
    }
    breaks <- seq(range[1L], range[2L], length.out = levels + 1L)
    d <- dim(x)
    n <- d[1L] * d[2L]

    # equalize each frame separately
    .equalize <- function(y) {
      h <- hist.default(y, breaks = breaks, plot = FALSE)
      cdf <- cumsum(h$counts)
      cdf_min <- min(cdf[cdf > 0])

      equalized <- ((cdf - cdf_min) / (prod(dim(y)) - cdf_min) * (range[2L] - range[1L])) + range[1L]
      bins <- round((y - range[1L]) / (range[2L] - range[1L]) * (levels - 1L)) + 1L

      res <- equalized[bins]
    }

    ld <- length(d[d != 1])
    res <- if (ld > 2L) {
      apply(x, 3L:ld, .equalize)
    } else {
      .equalize(x)
    }

    imager::as.cimg(res, dim = d)
  }
}

im_equalize <- function(im, plot_res = F) {
  reconv <- FALSE

  if (!imager::is.cimg(im)) {
    reconv <- TRUE
    im <- imager::as.cimg(im) %>% imager::imrotate(angle = 90) %>% mirror("x")
  }

  tmp <- im %>%
    imager::RGBtoHSL() %>%
    imager::imsplit("c")

  total_var <- purrr::map_dbl(imager::imsplit(im, "c"), var) %>% sum()
  if (var(tmp[[3]]) > 0.0001 & total_var > 0.001) {
    tmp[[3]] <- equalize(tmp[[3]])
  }
  res <- tmp %>%
    imager::imappend("c") %>%
    imager::HSLtoRGB()

  if (plot_res) {
    plot(imlist(im, res), layout = "row")
  }

  if (reconv) {
    as.raster(res)
  } else {
    res
  }
}

im_hist_normalize <- function(im) {
  m <- if (length(dim) > 3) 4 else 3
  res <- vapply(1:3, function(x) (im[,,x] - min(im[,,x]))/(max(im[,,x]) - min(im[,,x])),
                matrix(0, nrow = 256, ncol = 256))
}
