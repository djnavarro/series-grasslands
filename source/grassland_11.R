
# this reimplements grassland 7, but at the resolution required
# for the UAD requirements. 36000 wide x 16800 high

library(ggforce)
library(jasmines)
library(here)

seeds <- 201:209

blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255,
           green = z[2, ]/255,
           blue = z[3, ]/255)
  return(z)
}

grassland <- function(seed, dpi = 600, width = 10, height = 10, grain = 5000,
                      iterations = 50, scale = .003) {

  version <- 11
  file <- make_filename("grassland", version, seed, ".png")
  cat("making:", file, "\n")

  aspect <- height/width
  xl <- c(0, 1) * .4 + .3
  yl <- c(0, 1) * aspect * .4 + .3

  set.seed(seed)
  bg <- rscico()
  pal <- make_palette(pin = "black", mix = bg)

  dat <- use_seed(seed) %>%
    scene_rows(grain = 1000, vertical = TRUE) %>%
    dplyr::mutate(x = x * 1.1, y = y * 1.1) %>%
    unfold_breeze(40, drift = 0, scale = .0005) %>%
    dplyr::filter(time > 3) %>%
    dplyr::mutate(x = x * 3, y = y * 3) %>%
    dplyr::mutate(ind = 1:dplyr::n()) %>%
    unfold_warp(iterations = iterations, scale = scale) %>%
    prepare_data(
      palette = pal,
      colour = "time",
      alpha = c(0.15, scale)
    ) %>%
    dplyr::sample_frac(.5)

  background <- blend(bg, "white", .3)

  pic <- ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(
      x = x * 1.05,
      y = y * 1.05,
      xend = xend * 1.05,
      yend = yend * 1.05,
      alpha = al,
      colour = factor(order))
  ) +
    ggplot2::scale_color_manual(
      values = colours_from(pal, dat$order)
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      ),
      axis.line = ggplot2::element_line(colour = background),
      plot.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    ) +
    ggplot2::coord_equal(
      xlim = xl,
      ylim = yl
    ) +
  #  scale_x_continuous(expand = c(0,0)) +
  #  scale_y_continuous(expand = c(0,0)) +
    ggplot2::geom_segment(
      show.legend = FALSE,
      linewidth = .1,
      alpha = 1
    )

  ggplot2::ggsave(
    filename = here("image", file),
    plot = pic,
    width = width,
    height = height,
    dpi = dpi,
    device = ragg::agg_png
  )

  return(invisible(NULL))
}

rscico <- function() {
  pal <- sample(scico::scico_palette_names(), 1)
  sample(scico::scico(n = 256, palette=pal), 1)
}

make_palette <- function(pin = "black", mix = "black") {
  base <- sample(colours(distinct=TRUE), 3)
  base[1] <- pin
  blends <- purrr::map_chr(base, ~blend(.x, mix, .5))
  colorRampPalette(blends)
}

make_filename <- function(prefix, sys_num, seed, suffix,
                          sys_digits = 2, seed_digits = 3, sep = "_") {
  seed <- as.character(seed)
  sys_num <- as.character(sys_num)
  while(nchar(seed) < seed_digits) seed <- paste0("0", seed)
  while(nchar(sys_num) < sys_digits) sys_num <- paste0("0", sys_num)
  filename <- paste0(prefix, sep, sys_num, sep, seed, suffix)
  return(filename)
}

colours_from <- function (palette, order, ...) {
  palette(n = length(unique(order)))
}


prepare_data <- function(
  data,
  palette = "viridis",
  colour = "order",
  alpha = c(0.3, 0)
){

  ribbon <- data
  ribbon$order <- ribbon[[colour]]
  if (is.character(palette)) {
    palette <- palette_named(palette)
  }
  alpha_init <- alpha[1]
  if (length(alpha) > 1) {alpha_decay <- alpha[2]}
  else {alpha_decay <- 0}
  if (!("order" %in% names(ribbon))) {
    ribbon$order <- 1:nrow(ribbon)
  }

  xmin <- min(ribbon$x)
  xmax <- max(ribbon$x)
  ymin <- min(ribbon$y)
  ymax <- max(ribbon$y)
  xmin <- min(xmin, ymin)
  xmax <- max(xmax, ymax)
  ymin <- xmin
  ymax <- xmax

  ribbon <- ribbon %>%
    dplyr::mutate(
      x = (x - xmin)/(xmax - xmin),
      y = (y - ymin)/(ymax - ymin),
      al = alpha_init * (1 - alpha_decay)^(time - 1)
    )

  ribbon2 <- ribbon %>%
    dplyr::rename(xend = x, yend = y) %>%
    dplyr::mutate(time = time - 1) %>%
    dplyr::filter(time > 0)
  ribbon <- ribbon %>%
    dplyr::filter(time < max(time))
  ribbon$xend <- ribbon2$xend
  ribbon$yend <- ribbon2$yend
  ribbon$order <- ribbon2$order

  return(ribbon)
}


for(s in seeds) {
  dpi <- 1200
  pixels_wide <- 36000 * .9
  pixels_high <- 16800 * .9
  grassland(
    seed = s,
    dpi = dpi,
    width = pixels_wide / dpi,
    height = pixels_high / dpi
  )

  # ---- high resolution ----
  # night_sky(
  #   seed = s,
  #   width = 100/3,
  #   height = 100/3,
  #   dpi = 600,
  #   grain = 15000,
  #   iterations = 200,
  #   scale = .005
  # )
}
