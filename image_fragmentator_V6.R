# ============================================================
# image_fragmentator_V4_experimental.R
# ------------------------------------------------------------
# Standalone / bridge Shiny app for petrographic image segmentation.
#
# Version: V5.2 standalone — caption preview
#
# Main methods:
#   1. Grayscale threshold segmentation
#   2. K-means RGB clustering
#   3. Marker-controlled watershed segmentation (experimental)
#
# Main concept:
#   Fragmentator is intended for texture / structure analysis.
#   It is NOT an automatic mineral identification tool.
#
# Standalone concept:
#   The user uploads an image manually.
#   Fragmentator performs segmentation and exports:
#       1) PNG figure with segmentation + legend
#       2) CSV table with segmentation statistics
#
#   There is no bridge mode in this version.
#   The app does not write to Make_Thin_Section SQLite database folders.
#
# Author: adapted for Make_Thin_Section workflow
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

repos <- getOption("repos")
if (is.null(repos) || length(repos) == 0 || is.na(repos[["CRAN"]]) || repos[["CRAN"]] %in% c("", "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

required_cran_packages <- c("shiny", "magick", "png", "tools")

for (pkg in required_cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!requireNamespace("EBImage", quietly = TRUE)) {
  message("EBImage is not installed. Installing from Bioconductor...")
  BiocManager::install("EBImage", ask = FALSE, update = FALSE)
}

library(shiny)
library(magick)
library(png)
library(tools)
library(EBImage)


# ============================================================
# 1. Global settings
# ============================================================

APP_NAME <- "Image Fragmentator"
APP_VERSION <- "V5.2 standalone — caption preview"

MAX_UPLOAD_MB <- 80
MAX_WORKING_DIM <- 2000
JPEG_QUALITY_DEFAULT <- 90

options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)


# ============================================================
# 2. Path helpers and standalone export settings
# ============================================================

safe_normalize_path <- function(path, mustWork = FALSE) {
  if (is.null(path) || !nzchar(path)) return("")
  normalizePath(path, winslash = "/", mustWork = mustWork)
}

APP_DIR <- safe_normalize_path(getwd(), mustWork = FALSE)

# Standalone output folder.
# Fragmentator does not write to Make_Thin_Section database folders.
# Results are exported manually by the user.
FRAGMENTATOR_OUTPUT_DIR <- file.path(APP_DIR, "fragmentator_output")

if (!dir.exists(FRAGMENTATOR_OUTPUT_DIR)) {
  dir.create(FRAGMENTATOR_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
}


# ============================================================
# 3. General helper functions
# ============================================================

safe_file_stub <- function(x) {
  x <- ifelse(is.null(x) || !nzchar(x), "unknown", x)
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "unknown" else x
}

timestamp_stub <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

safe_range01 <- function(x) {
  x[is.na(x)] <- 0
  x[x < 0] <- 0
  x[x > 1] <- 1
  x
}

rgb_to_gray <- function(arr) {
  # arr: height x width x 3, values 0..1
  0.299 * arr[, , 1] + 0.587 * arr[, , 2] + 0.114 * arr[, , 3]
}

array_to_rgb_matrix <- function(arr) {
  # Convert height x width x 3 array to n_pixels x 3 RGB matrix.
  # Each row is one pixel: R, G, B.
  arr <- safe_range01(arr)

  if (length(dim(arr)) != 3 || dim(arr)[3] < 3) {
    stop("array_to_rgb_matrix expects height x width x 3 RGB array.")
  }

  cbind(
    as.vector(arr[, , 1]),
    as.vector(arr[, , 2]),
    as.vector(arr[, , 3])
  )
}

rgb_matrix_to_array <- function(mat, h, w) {
  arr <- array(0, dim = c(h, w, 3))
  arr[, , 1] <- matrix(mat[, 1], nrow = h, ncol = w)
  arr[, , 2] <- matrix(mat[, 2], nrow = h, ncol = w)
  arr[, , 3] <- matrix(mat[, 3], nrow = h, ncol = w)
  safe_range01(arr)
}

read_image_as_array <- function(path, max_dim = MAX_WORKING_DIM) {
  img <- magick::image_read(path)
  info_original <- magick::image_info(img)

  original_width <- info_original$width[1]
  original_height <- info_original$height[1]

  scale_factor <- min(1, max_dim / max(original_width, original_height))
  was_reduced <- scale_factor < 1

  if (was_reduced) {
    new_w <- max(1, round(original_width * scale_factor))
    new_h <- max(1, round(original_height * scale_factor))
    geometry <- paste0(new_w, "x", new_h, "!")
    img <- magick::image_resize(img, geometry)
  }

  # Very robust image import route:
  # 1. magick reads and optionally resizes the original file.
  # 2. The working image is written to a temporary PNG.
  # 3. png::readPNG() reads it back as a standard array:
  #      height x width x channels
  #
  # This avoids channel/width/height permutation problems that can appear
  # with magick::image_data() on some systems and lead to three stretched noisy bands.
  img <- magick::image_convert(img, colorspace = "sRGB", format = "png")

  tmp_png <- tempfile(fileext = ".png")
  on.exit({
    if (file.exists(tmp_png)) unlink(tmp_png)
  }, add = TRUE)

  magick::image_write(img, path = tmp_png, format = "png")

  arr <- png::readPNG(tmp_png, native = FALSE)

  # png::readPNG may return:
  #   height x width          for grayscale
  #   height x width x 3      for RGB
  #   height x width x 4      for RGBA
  if (length(dim(arr)) == 2) {
    arr <- array(
      rep(arr, 3),
      dim = c(dim(arr)[1], dim(arr)[2], 3)
    )
  } else if (length(dim(arr)) == 3) {
    if (dim(arr)[3] == 4) {
      # Drop alpha channel
      arr <- arr[, , 1:3, drop = FALSE]
    } else if (dim(arr)[3] == 3) {
      arr <- arr[, , 1:3, drop = FALSE]
    } else {
      stop("Unsupported PNG channel number: ", dim(arr)[3])
    }
  } else {
    stop("Unsupported image array dimensions after PNG conversion.")
  }

  arr <- safe_range01(arr)

  info_working <- list(
    width = dim(arr)[2],
    height = dim(arr)[1]
  )

  list(
    arr = arr,
    original_width = original_width,
    original_height = original_height,
    working_width = info_working$width,
    working_height = info_working$height,
    was_reduced = was_reduced,
    scale_factor = scale_factor,
    path = path
  )
}

rgb_array_to_raster <- function(arr) {
  # Convert height x width x 3 numeric RGB array into a raster matrix of HEX colors.
  # This is more robust than passing a numeric 3D array directly to as.raster(),
  # because some R graphics devices may display numeric arrays incorrectly.
  arr <- safe_range01(arr)

  if (length(dim(arr)) != 3 || dim(arr)[3] < 3) {
    stop("rgb_array_to_raster expects an array with dimensions height x width x 3.")
  }

  h <- dim(arr)[1]
  w <- dim(arr)[2]

  r <- as.vector(arr[, , 1])
  g <- as.vector(arr[, , 2])
  b <- as.vector(arr[, , 3])

  hex <- grDevices::rgb(r, g, b)

  matrix(hex, nrow = h, ncol = w)
}

plot_rgb_array <- function(arr, title = NULL) {
  par(mar = c(1, 1, ifelse(is.null(title), 1, 3), 1))
  plot.new()
  rasterImage(
    as.raster(rgb_array_to_raster(arr)),
    xleft = 0,
    ybottom = 0,
    xright = 1,
    ytop = 1,
    interpolate = FALSE
  )
  if (!is.null(title)) title(main = title)
}

cluster_boundary_overlay <- function(arr, cluster_map, alpha = 0.65, boundary_color = c(1, 0, 0)) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  boundary <- matrix(FALSE, nrow = h, ncol = w)

  boundary[-1, ] <- boundary[-1, ] | (cluster_map[-1, ] != cluster_map[-h, ])
  boundary[, -1] <- boundary[, -1] | (cluster_map[, -1] != cluster_map[, -w])

  overlay <- arr

  for (ch in 1:3) {
    tmp <- overlay[, , ch]
    tmp[boundary] <- (1 - alpha) * tmp[boundary] + alpha * boundary_color[ch]
    overlay[, , ch] <- tmp
  }

  safe_range01(overlay)
}

crop_array_by_roi <- function(arr, roi) {
  if (is.null(roi)) return(list(arr = arr, roi = NULL))

  h <- dim(arr)[1]
  w <- dim(arr)[2]

  xmin <- max(1, floor(min(roi$xmin, roi$xmax, na.rm = TRUE)))
  xmax <- min(w, ceiling(max(roi$xmin, roi$xmax, na.rm = TRUE)))
  ymin <- max(1, floor(min(roi$ymin, roi$ymax, na.rm = TRUE)))
  ymax <- min(h, ceiling(max(roi$ymin, roi$ymax, na.rm = TRUE)))

  if (xmax <= xmin || ymax <= ymin) {
    return(list(arr = arr, roi = NULL))
  }

  list(
    arr = arr[ymin:ymax, xmin:xmax, , drop = FALSE],
    roi = list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  )
}




# ============================================================
# 4. Segmentation methods
# ============================================================

segment_threshold <- function(arr, threshold = 0.5, invert = FALSE) {
  gray <- rgb_to_gray(arr)
  mask <- gray >= threshold
  if (isTRUE(invert)) mask <- !mask

  seg <- array(0, dim = dim(arr))
  seg[, , 1] <- mask
  seg[, , 2] <- mask
  seg[, , 3] <- mask

  h <- dim(arr)[1]
  w <- dim(arr)[2]
  total_px <- h * w
  selected_px <- sum(mask, na.rm = TRUE)

  legend <- data.frame(
    class = c("selected", "background"),
    pixels = c(selected_px, total_px - selected_px),
    percent = round(c(selected_px, total_px - selected_px) / total_px * 100, 3),
    hex_color = c("#FFFFFF", "#000000"),
    stringsAsFactors = FALSE
  )

  list(
    mask = mask,
    seg_arr = safe_range01(seg),
    overlay_arr = cluster_boundary_overlay(arr, mask * 1, alpha = 0.65),
    stats = legend,
    legend = legend
  )
}

plot_threshold_legend <- function(legend_df, title = "Threshold legend") {
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 2))
  title(main = title)

  if (is.null(legend_df) || nrow(legend_df) == 0) {
    text(0.5, 1, "No threshold result")
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(legend_df))) {
    y <- 2 - i + 0.5
    rect(0.05, y - 0.25, 0.18, y + 0.25, col = legend_df$hex_color[i], border = "black")
    label <- paste0(legend_df$class[i], " — ", legend_df$percent[i], "%")
    text(0.22, y, label, adj = c(0, 0.5), cex = 0.85)
  }
}

segment_kmeans <- function(arr, centers = 4, nstart = 25, seed = 123) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  mat <- array_to_rgb_matrix(arr)

  set.seed(seed)
  km <- kmeans(mat, centers = centers, nstart = nstart)

  cluster_map <- matrix(km$cluster, nrow = h, ncol = w)

  centers_rgb <- km$centers
  seg_mat <- centers_rgb[km$cluster, , drop = FALSE]
  seg_arr <- rgb_matrix_to_array(seg_mat, h, w)

  total_px <- h * w
  tab <- table(km$cluster)

  legend <- data.frame(
    cluster = paste0("cluster_", as.integer(names(tab))),
    cluster_id = as.integer(names(tab)),
    pixels = as.integer(tab),
    percent = round(as.integer(tab) / total_px * 100, 3),
    mean_red = round(centers_rgb[as.integer(names(tab)), 1] * 255),
    mean_green = round(centers_rgb[as.integer(names(tab)), 2] * 255),
    mean_blue = round(centers_rgb[as.integer(names(tab)), 3] * 255),
    stringsAsFactors = FALSE
  )

  legend$hex_color <- grDevices::rgb(
    legend$mean_red,
    legend$mean_green,
    legend$mean_blue,
    maxColorValue = 255
  )

  legend <- legend[order(-legend$pixels), ]
  rownames(legend) <- NULL

  list(
    cluster_map = cluster_map,
    seg_arr = safe_range01(seg_arr),
    overlay_arr = cluster_boundary_overlay(arr, cluster_map, alpha = 0.65),
    stats = legend,
    legend = legend,
    kmeans = km
  )
}

plot_segmentation_legend <- function(legend_df, title = "K-means cluster legend", max_items = 30) {
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)

  if (is.null(legend_df) || nrow(legend_df) == 0) {
    plot.new()
    title(main = title)
    text(0.5, 0.5, "No clusters detected")
    return(invisible(NULL))
  }

  legend_show <- head(legend_df, max_items)
  n <- nrow(legend_show)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, max(1, n)))
  title(main = title)

  for (i in seq_len(n)) {
    y <- n - i + 0.5

    rect(
      0.05, y - 0.25,
      0.18, y + 0.25,
      col = legend_show$hex_color[i],
      border = "black"
    )

    label <- paste0(
      legend_show$cluster[i],
      " — ",
      legend_show$percent[i],
      "%, RGB(",
      legend_show$mean_red[i], ", ",
      legend_show$mean_green[i], ", ",
      legend_show$mean_blue[i], ")"
    )

    text(0.22, y, label, adj = c(0, 0.5), cex = 0.75)
  }
}

make_watershed_palette <- function(n) {
  if (n <= 0) return(character(0))
  grDevices::hcl.colors(n, palette = "Dynamic")
}

segment_marker_watershed <- function(
    arr,
    threshold = 0.5,
    invert_mask = FALSE,
    blur_sigma = 1.0,
    seed_threshold = 0.35,
    min_object_px = 50
) {
  h <- dim(arr)[1]
  w <- dim(arr)[2]

  gray <- rgb_to_gray(arr)

  if (!is.null(blur_sigma) && blur_sigma > 0) {
    gray_img <- EBImage::Image(gray, colormode = "Grayscale")
    gray <- drop(as.array(EBImage::gblur(gray_img, sigma = blur_sigma)))
  }

  mask <- gray >= threshold
  if (isTRUE(invert_mask)) mask <- !mask

  mask_img <- EBImage::Image(mask * 1, colormode = "Grayscale")

  # Distance transform inside binary mask.
  dist_img <- EBImage::distmap(mask_img)
  dist_mat <- drop(as.array(dist_img))

  max_dist <- max(dist_mat, na.rm = TRUE)

  if (!is.finite(max_dist) || max_dist <= 0) {
    empty_map <- matrix(0L, nrow = h, ncol = w)
    empty_arr <- array(0, dim = c(h, w, 3))

    return(list(
      object_map = empty_map,
      seg_arr = empty_arr,
      overlay_arr = arr,
      stats = data.frame(),
      legend = data.frame(),
      n_objects = 0
    ))
  }

  # Markers are high-distance internal cores.
  # Higher seed_threshold produces fewer markers.
  seeds_mask <- dist_mat >= seed_threshold * max_dist
  seeds_img <- EBImage::Image(seeds_mask * 1, colormode = "Grayscale")
  seeds <- EBImage::bwlabel(seeds_img)

  n_seeds <- max(drop(as.array(seeds)), na.rm = TRUE)

  # Fallback: if marker generation failed, use connected mask objects.
  if (!is.finite(n_seeds) || n_seeds < 1) {
    seeds <- EBImage::bwlabel(mask_img)
  }

  # Marker-controlled watershed-like propagation constrained by mask.
  labels_img <- EBImage::propagate(
    x = EBImage::Image(-dist_mat, colormode = "Grayscale"),
    seeds = seeds,
    mask = mask_img,
    lambda = 1e-4
  )

  object_map <- round(drop(as.array(labels_img)))
  object_map[!mask] <- 0L

  # Remove small objects
  ids <- sort(unique(as.integer(object_map)))
  ids <- ids[ids > 0]

  if (length(ids) > 0) {
    tab <- table(object_map[object_map > 0])
    small_ids <- as.integer(names(tab)[tab < min_object_px])
    if (length(small_ids) > 0) {
      object_map[object_map %in% small_ids] <- 0L
    }
  }

  # Recalculate IDs after cleanup
  ids <- sort(unique(as.integer(object_map)))
  ids <- ids[ids > 0]
  n_objects <- length(ids)

  seg_arr <- array(0, dim = c(h, w, 3))

  if (n_objects == 0) {
    return(list(
      object_map = object_map,
      seg_arr = seg_arr,
      overlay_arr = arr,
      stats = data.frame(),
      legend = data.frame(),
      n_objects = 0
    ))
  }

  palette <- make_watershed_palette(n_objects)
  stats_list <- vector("list", n_objects)

  total_px <- h * w

  for (i in seq_along(ids)) {
    id <- ids[i]
    idx <- object_map == id

    px <- sum(idx, na.rm = TRUE)

    mean_r <- mean(arr[, , 1][idx], na.rm = TRUE)
    mean_g <- mean(arr[, , 2][idx], na.rm = TRUE)
    mean_b <- mean(arr[, , 3][idx], na.rm = TRUE)

    col_rgb <- grDevices::col2rgb(palette[i]) / 255

    seg_arr[, , 1][idx] <- col_rgb[1]
    seg_arr[, , 2][idx] <- col_rgb[2]
    seg_arr[, , 3][idx] <- col_rgb[3]

    stats_list[[i]] <- data.frame(
      object = paste0("object_", id),
      object_id = id,
      pixels = px,
      percent = round(px / total_px * 100, 3),
      mean_red = round(mean_r * 255),
      mean_green = round(mean_g * 255),
      mean_blue = round(mean_b * 255),
      hex_color = palette[i],
      stringsAsFactors = FALSE
    )
  }

  legend <- do.call(rbind, stats_list)
  legend <- legend[order(-legend$pixels), ]
  rownames(legend) <- NULL

  overlay_arr <- cluster_boundary_overlay(
    arr = arr,
    cluster_map = object_map,
    alpha = 0.65,
    boundary_color = c(1, 0, 0)
  )

  list(
    object_map = object_map,
    seg_arr = safe_range01(seg_arr),
    overlay_arr = safe_range01(overlay_arr),
    stats = legend,
    legend = legend,
    n_objects = n_objects
  )
}

plot_watershed_legend <- function(legend_df, title = "Watershed object legend", max_items = 30) {
  op <- par(mar = c(1, 1, 3, 1))
  on.exit(par(op), add = TRUE)

  if (is.null(legend_df) || nrow(legend_df) == 0) {
    plot.new()
    title(main = title)
    text(0.5, 0.5, "No watershed objects detected")
    return(invisible(NULL))
  }

  legend_show <- head(legend_df, max_items)
  n <- nrow(legend_show)

  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, max(1, n)))
  title(main = title)

  for (i in seq_len(n)) {
    y <- n - i + 0.5

    rect(
      0.05, y - 0.25,
      0.18, y + 0.25,
      col = legend_show$hex_color[i],
      border = "black"
    )

    label <- paste0(
      legend_show$object[i],
      " — ",
      legend_show$percent[i],
      "%, mean RGB(",
      legend_show$mean_red[i], ", ",
      legend_show$mean_green[i], ", ",
      legend_show$mean_blue[i], ")"
    )

    text(0.22, y, label, adj = c(0, 0.5), cex = 0.70)
  }

  if (nrow(legend_df) > max_items) {
    text(
      0.05, 0.15,
      paste0("Only largest ", max_items, " objects shown."),
      adj = c(0, 0.5),
      cex = 0.75
    )
  }
}


# ============================================================
# 5. Export helpers
# ============================================================

build_segmentation_caption <- function(
    req = NULL,
    method,
    use_roi = FALSE,
    k_centers = NA,
    threshold = NA,
    nstart = NA,
    seed = NA,
    ws_seed_threshold = NA,
    ws_min_object_px = NA
) {
  # Concise caption for exported PNG.
  # Keep only method and parameters used for this segmentation.
  # Do not add interpretative / limitation text here; that belongs to the About tab.

  method_label <- if (method == "kmeans") {
    paste0(
      "k-means RGB clustering",
      if (!is.na(k_centers)) paste0("; k = ", k_centers) else "",
      if (!is.na(nstart)) paste0("; nstart = ", nstart) else "",
      if (!is.na(seed)) paste0("; seed = ", seed) else ""
    )
  } else if (method == "watershed") {
    paste0(
      "marker-controlled watershed segmentation",
      if (!is.na(threshold)) paste0("; mask threshold = ", threshold) else "",
      if (!is.na(ws_seed_threshold)) paste0("; marker threshold = ", ws_seed_threshold) else "",
      if (!is.na(ws_min_object_px)) paste0("; min object size = ", ws_min_object_px, " px") else ""
    )
  } else {
    paste0(
      "grayscale threshold",
      if (!is.na(threshold)) paste0("; threshold = ", threshold) else ""
    )
  }

  roi_label <- if (isTRUE(use_roi)) "ROI used" else "full image"

  paste0(
    "Segmentation result with legend. Method: ",
    method_label,
    "; ",
    roi_label,
    "."
  )
}

export_segmentation_with_legend_png <- function(res, filepath, caption = NULL) {
  png(filename = filepath, width = 1800, height = 1100, res = 150)

  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(5, 1.2), widths = c(2.2, 1))

  if (res$type == "threshold") {
    plot_rgb_array(res$seg_arr, title = "Threshold segmentation mask")
    plot_threshold_legend(res$legend, title = "Threshold legend")
  } else if (res$type == "kmeans") {
    plot_rgb_array(res$seg_arr, title = "K-means segmented image")
    plot_segmentation_legend(res$legend, title = "K-means cluster legend")
  } else if (res$type == "watershed") {
    plot_rgb_array(res$seg_arr, title = "Marker-controlled watershed")
    plot_watershed_legend(res$legend, title = "Watershed object legend")
  } else {
    plot_rgb_array(res$seg_arr, title = "Segmentation")
    plot.new()
    text(0.5, 0.5, "No legend")
  }

  par(mar = c(1, 1, 1, 1))
  plot.new()
  if (!is.null(caption) && nzchar(caption)) {
    text(0.02, 0.55, caption, adj = c(0, 0.5), cex = 0.85)
  } else {
    text(0.02, 0.55, "Segmentation result with legend.", adj = c(0, 0.5), cex = 0.85)
  }

  dev.off()

  filepath
}

write_standalone_result_metadata <- function(output_png, output_csv, caption, method = NULL) {
  # Optional local metadata beside exported PNG/CSV.
  # This is not a bridge file and is not used by Make_Thin_Section.
  output_png_abs <- safe_normalize_path(output_png, mustWork = FALSE)
  output_csv_abs <- safe_normalize_path(output_csv, mustWork = FALSE)

  result <- list(
    created_at = Sys.time(),
    output_png = output_png_abs,
    output_csv = output_csv_abs,
    caption = caption,
    method = method,
    app_name = APP_NAME,
    app_version = APP_VERSION,
    standalone_mode = TRUE
  )

  meta_rds <- sub("\\.png$", "_metadata.rds", output_png_abs, ignore.case = TRUE)
  saveRDS(result, meta_rds)

  result
}


# ============================================================
# 6. UI
# ============================================================

ui <- fluidPage(
  titlePanel(paste(APP_NAME, APP_VERSION)),

  tags$div(
    id = "busy_indicator",
    tags$span(class = "fragmentator-spinner"),
    tags$span("Fragmentation / processing is running...")
  ),

  tags$head(
    tags$style(HTML("
      .small-note {
        font-size: 12px;
        color: #666666;
      }
      .warning-box {
        background: #fff3cd;
        border: 1px solid #ffeeba;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      .info-box {
        background: #e9f5ff;
        border: 1px solid #b6dcff;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      .danger-box {
        background: #f8d7da;
        border: 1px solid #f5c6cb;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      #busy_indicator {
        display: none;
        position: fixed;
        top: 18px;
        right: 18px;
        z-index: 99999;
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid #cccccc;
        border-radius: 10px;
        padding: 12px 16px;
        box-shadow: 0 4px 18px rgba(0,0,0,0.18);
        font-weight: 600;
      }
      .fragmentator-spinner {
        display: inline-block;
        width: 18px;
        height: 18px;
        border: 3px solid #dddddd;
        border-top: 3px solid #337ab7;
        border-radius: 50%;
        animation: fragmentator_spin 0.8s linear infinite;
        vertical-align: middle;
        margin-right: 8px;
      }
      @keyframes fragmentator_spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:busy', function() {
        $('#busy_indicator').show();
      });
      $(document).on('shiny:idle', function() {
        $('#busy_indicator').hide();
      });
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Input image"),

      fileInput(
        "upload_image",
        "Upload image",
        accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff")
      ),

      tags$hr(),

      h4("ROI selection"),

      checkboxInput("use_roi", "Use brushed ROI", value = FALSE),
      actionButton("use_brush_roi", "Use brushed ROI from image"),
      actionButton("clear_roi", "Clear ROI"),
      br(),
      tags$span(class = "small-note", "Brush a rectangle on the input image preview, then click 'Use brushed ROI from image'."),

      tags$hr(),

      h4("Segmentation method"),

      radioButtons(
        "method",
        "Method",
        choices = c(
          "Grayscale threshold" = "threshold",
          "K-means RGB clustering" = "kmeans",
          "Marker-controlled watershed (experimental)" = "watershed"
        ),
        selected = "threshold"
      ),

      conditionalPanel(
        condition = "input.method == 'threshold'",
        sliderInput(
          "threshold",
          "Threshold",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.01
        ),
        checkboxInput("threshold_invert", "Invert mask", value = FALSE)
      ),

      conditionalPanel(
        condition = "input.method == 'kmeans'",
        numericInput(
          "k_centers",
          "Number of clusters, k",
          value = 4,
          min = 2,
          max = 20,
          step = 1
        ),
        numericInput(
          "k_nstart",
          "k-means nstart",
          value = 25,
          min = 1,
          max = 200,
          step = 1
        ),
        numericInput(
          "k_seed",
          "Random seed",
          value = 123,
          min = 1,
          max = 999999,
          step = 1
        ),
        tags$span(class = "small-note", "nstart controls repeated random starts; seed makes the result reproducible.")
      ),

      conditionalPanel(
        condition = "input.method == 'watershed'",
        sliderInput(
          "ws_threshold",
          "Mask threshold",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.01
        ),
        checkboxInput(
          "ws_invert_mask",
          "Invert mask",
          value = FALSE
        ),
        numericInput(
          "ws_blur_sigma",
          "Smoothing sigma",
          value = 1.0,
          min = 0,
          max = 10,
          step = 0.5
        ),
        sliderInput(
          "ws_seed_threshold",
          "Marker threshold",
          min = 0.05,
          max = 0.95,
          value = 0.35,
          step = 0.05
        ),
        numericInput(
          "ws_min_object_px",
          "Minimum object size, pixels",
          value = 50,
          min = 1,
          max = 100000,
          step = 10
        ),
        tags$span(
          class = "small-note",
          "Experimental. Higher marker threshold usually gives fewer objects. Higher smoothing reduces noise and over-segmentation."
        )
      ),

      tags$hr(),

      h4("Export"),

      actionButton("save_to_output_folder", "Save PNG + CSV to output folder", class = "btn btn-primary"),
      br(), br(),

      downloadButton("download_png", "Download segmentation PNG + legend"),
      downloadButton("download_csv", "Download statistics CSV"),

      tags$hr(),

      h4("Status"),
      verbatimTextOutput("status_text")
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        tabPanel(
          "Original Image",
          br(),
          div(
            class = "info-box",
            strong("ROI selection"),
            br(),
            "Brush a rectangle directly on the original image, then click ",
            strong("Use brushed ROI from image"),
            " in the left panel. The selected ROI will be used for segmentation if ",
            strong("Use brushed ROI"),
            " is checked."
          ),
          plotOutput(
            "plot_original_image",
            height = "650px",
            brush = brushOpts(
              id = "input_brush",
              resetOnNew = TRUE
            )
          )
        ),

        tabPanel(
          "ROI preview",
          br(),
          div(
            class = "info-box",
            "This tab shows the currently selected ROI that will be passed to the segmentation method. ",
            "If no ROI is selected, the full image is shown."
          ),
          verbatimTextOutput("roi_text"),
          plotOutput("plot_roi_preview", height = "650px")
        ),

        tabPanel(
          "Segmentation",
          br(),
          fluidRow(
            column(
              width = 6,
              h4("Input image / selected ROI"),
              plotOutput("plot_input", height = "520px")
            ),
            column(
              width = 6,
              h4("Segmentation result"),
              plotOutput("plot_segmented", height = "520px")
            )
          ),
          fluidRow(
            column(
              width = 6,
              h4("Overlay"),
              plotOutput("plot_overlay", height = "420px")
            ),
            column(
              width = 6,
              h4("Legend"),
              plotOutput("plot_legend", height = "420px")
            )
          )
        ),

        tabPanel(
          "Statistics",
          br(),
          tableOutput("stats_table")
        ),

        tabPanel(
          "Output info",
          br(),
          h4("Standalone output folder"),
          verbatimTextOutput("output_paths"),
          h4("Caption for last fragmentation"),
          verbatimTextOutput("last_fragmentation_caption_text"),
          h4("Last saved standalone result"),
          verbatimTextOutput("last_saved_result_text")
        ),

        tabPanel(
          "About",
          br(),
          h3("Image Fragmentator"),
          p(strong("Version:"), APP_VERSION),
          p(
            "Fragmentator is a standalone Shiny app for segmentation of petrographic images. ",
            "It does not depend on Make_Thin_Section and does not write files to the petrographic SQLite database folder."
          ),

          h4("Operating mode"),
          tags$ol(
            tags$li(strong("Standalone mode only:"), " the user uploads an image manually."),
            tags$li("The program exports a PNG figure with legend and a CSV statistics table."),
            tags$li("The exported files can be used independently, archived, inserted into reports, or manually imported elsewhere if needed.")
          ),

          h4("Current workflow"),
          tags$ol(
            tags$li("Upload an original unlabelled image."),
            tags$li("Select ROI if needed."),
            tags$li("Run threshold, k-means, or experimental watershed segmentation."),
            tags$li("Export PNG with legend and CSV statistics."),
            tags$li("Use the exported files outside Fragmentator as needed.")
          ),

          h4("Important limitation"),
          div(
            class = "warning-box",
            "Fragmentator is not a magic-wand mineral identification tool. ",
            "It does not automatically recognize minerals or produce a final geological interpretation after one click. ",
            "The program helps separate visible image domains such as textures, structures, veins, alteration zones, matrix, clasts, phenocrysts, or other visually contrasting areas. ",
            "Geological interpretation remains the responsibility of the user."
          ),

          h4("Segmentation methods"),
          tags$ul(
            tags$li(strong("Grayscale threshold:"), " simple separation by brightness."),
            tags$li(strong("K-means RGB clustering:"), " separates pixels into color-based clusters."),
            tags$li(strong("Marker-controlled watershed:"), " experimental method for separating adjacent grains, spots, pores, clasts, or domains. Results may be over-segmented and require careful interpretation.")
          ),

          h4("Future development ideas"),
          tags$ul(
            tags$li("Copy segmentation image directly to clipboard."),
            tags$li("Automatic attachment of segmentation result to Image container."),
            tags$li("Persistent segmentation metadata tables in SQLite."),
            tags$li("Additional segmentation algorithms."),
            tags$li("Texture-oriented analysis."),
            tags$li("Object statistics and size distribution."),
            tags$li("Morphological filtering."),
            tags$li("Interactive polygon ROI."),
            tags$li("Multi-image batch segmentation.")
          )
        )
      )
    )
  )
)


# ============================================================
# 7. Server
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    image_info = NULL,
    roi = NULL,
    last_saved_result = NULL,
    last_fragmentation_caption = NULL,
    status = "Ready."
  )

  load_image_from_path <- function(path, source_label = "manual") {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      rv$status <- paste("Image file not found:", path)
      showNotification(rv$status, type = "error")
      return(FALSE)
    }

    info <- tryCatch(
      read_image_as_array(path, max_dim = MAX_WORKING_DIM),
      error = function(e) {
        rv$status <- paste("Image loading error:", conditionMessage(e))
        showNotification(rv$status, type = "error")
        NULL
      }
    )

    if (is.null(info)) return(FALSE)

    rv$image_info <- info
    rv$roi <- NULL

    rv$status <- paste0(
      "Loaded image from ", source_label, ": ",
      basename(path),
      " | original ",
      info$original_width, "x", info$original_height,
      " | working ",
      info$working_width, "x", info$working_height,
      if (isTRUE(info$was_reduced)) " | reduced for processing" else ""
    )

    showNotification("Image loaded.", type = "message")
    TRUE
  }

  observeEvent(input$upload_image, {
    req(input$upload_image$datapath)
    load_image_from_path(input$upload_image$datapath, source_label = "manual upload")
  })

  observeEvent(input$use_brush_roi, {
    req(rv$image_info)
    brush <- input$input_brush

    if (is.null(brush)) {
      showNotification("No brushed ROI found. Brush a rectangle on the input image first.", type = "warning")
      return()
    }

    h <- dim(rv$image_info$arr)[1]
    w <- dim(rv$image_info$arr)[2]

    # Plot coordinates are 0..1 in both axes.
    # Convert brush coordinates to pixel coordinates.
    xmin <- round(brush$xmin * w)
    xmax <- round(brush$xmax * w)

    # y axis in plot coordinates is 0..1 bottom to top.
    # Raster image uses row index top to bottom.
    ymin <- round((1 - brush$ymax) * h)
    ymax <- round((1 - brush$ymin) * h)

    rv$roi <- list(
      xmin = max(1, min(xmin, xmax)),
      xmax = min(w, max(xmin, xmax)),
      ymin = max(1, min(ymin, ymax)),
      ymax = min(h, max(ymin, ymax))
    )

    updateCheckboxInput(session, "use_roi", value = TRUE)

    rv$status <- paste0(
      "ROI selected: x=", rv$roi$xmin, "-", rv$roi$xmax,
      ", y=", rv$roi$ymin, "-", rv$roi$ymax
    )
  })

  observeEvent(input$clear_roi, {
    rv$roi <- NULL
    updateCheckboxInput(session, "use_roi", value = FALSE)
    rv$status <- "ROI cleared."
  })

  working_array <- reactive({
    req(rv$image_info)

    arr <- rv$image_info$arr

    if (isTRUE(input$use_roi) && !is.null(rv$roi)) {
      crop_array_by_roi(arr, rv$roi)$arr
    } else {
      arr
    }
  })

  segmentation_result <- reactive({
    req(working_array())

    arr <- working_array()

    if (input$method == "threshold") {
      th <- withProgress(message = "Running threshold segmentation...", value = 0.3, {
        segment_threshold(
          arr,
          threshold = input$threshold,
          invert = input$threshold_invert
        )
      })

      return(list(
        type = "threshold",
        seg_arr = th$seg_arr,
        overlay_arr = th$overlay_arr,
        stats = th$stats,
        legend = th$legend,
        mask = th$mask
      ))
    }

    if (input$method == "kmeans") {
      km <- withProgress(message = "Running k-means RGB clustering...", value = 0.3, {
        segment_kmeans(
          arr,
          centers = input$k_centers,
          nstart = input$k_nstart,
          seed = input$k_seed
        )
      })

      return(list(
        type = "kmeans",
        seg_arr = km$seg_arr,
        overlay_arr = km$overlay_arr,
        stats = km$stats,
        legend = km$legend,
        cluster_map = km$cluster_map
      ))
    }

    if (input$method == "watershed") {
      ws <- withProgress(message = "Running marker-controlled watershed...", value = 0.3, {
        segment_marker_watershed(
          arr = arr,
          threshold = input$ws_threshold,
          invert_mask = input$ws_invert_mask,
          blur_sigma = input$ws_blur_sigma,
          seed_threshold = input$ws_seed_threshold,
          min_object_px = input$ws_min_object_px
        )
      })

      return(list(
        type = "watershed",
        seg_arr = ws$seg_arr,
        overlay_arr = ws$overlay_arr,
        stats = ws$stats,
        legend = ws$legend,
        object_map = ws$object_map,
        n_objects = ws$n_objects
      ))
    }

    NULL
  })

  output$plot_original_image <- renderPlot({
    req(rv$image_info)

    arr <- rv$image_info$arr

    plot_rgb_array(arr, title = "Original image — brush ROI here")

    if (!is.null(rv$roi)) {
      h <- dim(arr)[1]
      w <- dim(arr)[2]

      xleft <- rv$roi$xmin / w
      xright <- rv$roi$xmax / w

      # Convert image row coordinates to plot coordinates.
      ytop <- 1 - rv$roi$ymin / h
      ybottom <- 1 - rv$roi$ymax / h

      rect(xleft, ybottom, xright, ytop, border = "red", lwd = 3)
    }
  })

  output$plot_roi_preview <- renderPlot({
    req(rv$image_info)

    arr <- rv$image_info$arr

    if (isTRUE(input$use_roi) && !is.null(rv$roi)) {
      roi_arr <- crop_array_by_roi(arr, rv$roi)$arr
      plot_rgb_array(roi_arr, title = "ROI preview — image passed to segmentation")
    } else {
      plot_rgb_array(arr, title = "No ROI selected — full image will be used")
    }
  })

  output$roi_text <- renderText({
    if (is.null(rv$image_info)) {
      return("No image loaded.")
    }

    if (isTRUE(input$use_roi) && !is.null(rv$roi)) {
      paste0(
        "ROI active: yes\n",
        "x = ", rv$roi$xmin, " - ", rv$roi$xmax, "\n",
        "y = ", rv$roi$ymin, " - ", rv$roi$ymax, "\n",
        "Width: ", rv$roi$xmax - rv$roi$xmin + 1, " px\n",
        "Height: ", rv$roi$ymax - rv$roi$ymin + 1, " px"
      )
    } else {
      paste0(
        "ROI active: no\n",
        "The full image will be used for segmentation.\n",
        "Image size: ",
        rv$image_info$working_width,
        " x ",
        rv$image_info$working_height,
        " px"
      )
    }
  })

  output$plot_input <- renderPlot({
    req(rv$image_info)

    if (isTRUE(input$use_roi) && !is.null(rv$roi)) {
      arr <- crop_array_by_roi(rv$image_info$arr, rv$roi)$arr
      plot_rgb_array(arr, title = "Input for segmentation — selected ROI")
    } else {
      arr <- rv$image_info$arr
      plot_rgb_array(arr, title = "Input for segmentation — full image")
    }
  })

  output$plot_segmented <- renderPlot({
    req(segmentation_result())
    res <- segmentation_result()

    title <- switch(
      res$type,
      threshold = "Threshold segmentation",
      kmeans = "K-means segmentation",
      watershed = "Marker-controlled watershed",
      "Segmentation"
    )

    plot_rgb_array(res$seg_arr, title = title)
  })

  output$plot_overlay <- renderPlot({
    req(segmentation_result())
    res <- segmentation_result()
    plot_rgb_array(res$overlay_arr, title = "Boundary overlay")
  })

  output$plot_legend <- renderPlot({
    req(segmentation_result())
    res <- segmentation_result()

    if (res$type == "threshold") {
      plot_threshold_legend(res$legend, title = "Threshold legend")
    } else if (res$type == "kmeans") {
      plot_segmentation_legend(res$legend, title = "K-means cluster legend")
    } else if (res$type == "watershed") {
      plot_watershed_legend(res$legend, title = "Watershed object legend")
    }
  })

  output$stats_table <- renderTable({
    req(segmentation_result())
    segmentation_result()$stats
  })

  output$status_text <- renderText({
    paste(
      rv$status,
      "\n\nCurrent method:", input$method,
      "\nROI active:", isTRUE(input$use_roi) && !is.null(rv$roi),
      if (!is.null(rv$image_info)) {
        paste0(
          "\nImage: ", basename(rv$image_info$path),
          "\nOriginal size: ", rv$image_info$original_width, " x ", rv$image_info$original_height,
          "\nWorking size: ", rv$image_info$working_width, " x ", rv$image_info$working_height,
          "\nRGB range: ", round(min(rv$image_info$arr, na.rm = TRUE), 4), " - ",
          round(max(rv$image_info$arr, na.rm = TRUE), 4),
          "\nArray dim: ", paste(dim(rv$image_info$arr), collapse = " x ")
        )
      } else {
        "\nNo image loaded."
      }
    )
  })

  output$output_paths <- renderText({
    paste(
      "APP_DIR:", APP_DIR,
      "FRAGMENTATOR_OUTPUT_DIR:", FRAGMENTATOR_OUTPUT_DIR,
      sep = "\n"
    )
  })

  output$last_fragmentation_caption_text <- renderText({
    cap <- rv$last_fragmentation_caption

    if (is.null(cap) || !nzchar(cap)) {
      "No fragmentation caption yet. Run a segmentation or save/export a result first."
    } else {
      cap
    }
  })

  output$last_saved_result_text <- renderPrint({
    rv$last_saved_result
  })

  make_current_caption <- reactive({
    req(input$method)

    build_segmentation_caption(
      req = NULL,
      method = input$method,
      use_roi = isTRUE(input$use_roi) && !is.null(rv$roi),
      k_centers = if (input$method == "kmeans") input$k_centers else NA,
      threshold = if (input$method == "threshold") input$threshold else if (input$method == "watershed") input$ws_threshold else NA,
      nstart = if (input$method == "kmeans") input$k_nstart else NA,
      seed = if (input$method == "kmeans") input$k_seed else NA,
      ws_seed_threshold = if (input$method == "watershed") input$ws_seed_threshold else NA,
      ws_min_object_px = if (input$method == "watershed") input$ws_min_object_px else NA
    )
  })

  observeEvent(segmentation_result(), {
    rv$last_fragmentation_caption <- make_current_caption()
  }, ignoreInit = TRUE)

  make_output_filename <- function(ext = "png") {
    base <- if (!is.null(rv$image_info) && !is.null(rv$image_info$path)) {
      safe_file_stub(tools::file_path_sans_ext(basename(rv$image_info$path)))
    } else {
      "image"
    }

    paste0(
      "fragmentator_",
      base, "_",
      input$method, "_",
      timestamp_stub(),
      ".",
      ext
    )
  }


  observeEvent(input$save_to_output_folder, {
    req(segmentation_result())

    res <- segmentation_result()
    caption <- make_current_caption()
    rv$last_fragmentation_caption <- caption

    output_png <- file.path(FRAGMENTATOR_OUTPUT_DIR, make_output_filename("png"))
    output_csv <- sub("\\.png$", ".csv", output_png, ignore.case = TRUE)

    tryCatch({
      withProgress(message = "Saving segmentation PNG and CSV...", value = 0.2, {
        export_segmentation_with_legend_png(res, output_png, caption = caption)
        incProgress(0.4)

        write.csv(res$stats, output_csv, row.names = FALSE)
        incProgress(0.3)

        standalone_result <- write_standalone_result_metadata(
          output_png = output_png,
          output_csv = output_csv,
          caption = caption,
          method = input$method
        )

        rv$last_saved_result <- standalone_result
      })

      rv$status <- paste("Standalone result saved:", output_png)
      showNotification("PNG + CSV saved to standalone output folder.", type = "message")
    }, error = function(e) {
      rv$status <- paste("Save error:", conditionMessage(e))
      showNotification(rv$status, type = "error")
    })
  })


  output$download_png <- downloadHandler(
    filename = function() {
      make_output_filename("png")
    },
    content = function(file) {
      req(segmentation_result())
      cap <- make_current_caption()
      rv$last_fragmentation_caption <- cap

      export_segmentation_with_legend_png(
        segmentation_result(),
        filepath = file,
        caption = cap
      )
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      make_output_filename("csv")
    },
    content = function(file) {
      req(segmentation_result())
      rv$last_fragmentation_caption <- make_current_caption()

      stats <- segmentation_result()$stats
      write.csv(stats, file, row.names = FALSE)
    }
  )
}


# ============================================================
# 8. Run app
# ============================================================

shinyApp(ui = ui, server = server)
