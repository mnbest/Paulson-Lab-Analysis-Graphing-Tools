# ============================================================================
# FINAL PSP PAPER CODE — complete single-file pipeline
# ----------------------------------------------------------------------------
# Stages (all in this file; no other script is required):
#   1. Rebuild image_by_class_and_merger.csv directly from raw object exports
#      inside a newly created run folder. No old scripts/results are read.
#   2. IF feature study with the revised AT8/TRIM46 mergers, group
#      comparisons, and print-size publication figures (n under group names,
#      per-panel y-axis titles, morphology in microns).
#   3. Embedded Western blot analysis: Welch ANOVA, pairwise brackets,
#      pooled 2-group tests, and a rank-based (Kruskal-Wallis / Wilcoxon)
#      sensitivity analysis.
#   4. Cognition: case-level Last MoCA associations.
#   5. Sample-size (power) requirements for WB, IF, and MoCA analyses.
#   6. Sensitivity of IF case-level results to header-only export handling.
# Figures exported (PNG only): WB 3-/2-group; IF case- and image-level
# 3-/2-group for Trusted AT8 and Trusted TRIM46; Diffuse AT8 vs Last MoCA.
# All statistics for every class/feature are always written to tables/.
# The AT8-TRIM46 association analysis is NOT part of this pipeline; it is kept
# separately in exploratory_AT8_TRIM46_association_NOT_IN_PAPER.r.
# Usage: edit CONFIG below, then click Source in RStudio.
# Source without running: options(psp.features.autorun = FALSE)
# ============================================================================

# Isolated from objects/functions left in the RStudio global environment.
# R packages and the four original data inputs below are the only dependencies.
FINAL_PSP <- local({
required <- c("dplyr","tidyr","readr","ggplot2","tibble","gtable","lme4","lmerTest")
missing_packages <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing_packages))
  stop("Install missing packages first: install.packages(c(",
       paste(sprintf('"%s"',missing_packages),collapse=", "),"))")
if(utils::packageVersion("ggplot2") < "3.5.0")
  stop("Update ggplot2 to >= 3.5.0 for repeated panel axes.")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
})
# Import dependency exports explicitly: lookup must not depend on the search
# path order or the user's existing global functions.
for(pkg in c("stats","utils","grDevices","graphics","dplyr","tidyr","readr","ggplot2"))
  list2env(mget(getNamespaceExports(pkg),envir=asNamespace(pkg),inherits=TRUE),
           envir=environment())
CLASS_DEFS <- list(
  AT8=c("Mature Tangle","Diffuse AT8","Single Aggregate","Combined Aggregate"),
  TRIM46=c("Full Axon Initial Segment","Partial Axon Initial Segment",
           "Combined Axon Initial Segment","Artifact")
)
TRUSTED <- c("Mature Tangle","Diffuse AT8","Combined Aggregate",
             "Full Axon Initial Segment","Partial Axon Initial Segment",
             "Combined Axon Initial Segment")
ILASTIK_RAW <- new.env(parent=environment())
local({
# Embedded raw-feature extractor. All functions are defined in this file.
# Defaults: 2000 2000 exclude TRUE. Case is inferred from case-bearing folders.
# No hypothesis tests or automatic object exclusions are performed.


  
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})
# CLASS_DEFS and TRUSTED are inherited from the single registry above.
FEATURES <- c(
  Area_pixels = "Size in pixels",
  Object_area_export = "Object Area",
  Mean_intensity = "Mean Intensity",
  Total_intensity_export = "Total Intensity",
  Diameter_pixels = "Diameter",
  Euclidean_diameter_pixels = "Euclidean Diameter",
  Skeleton_length_pixels = "Length of the Skeleton",
  Branches = "Number of Branches",
  Average_branch_length_pixels = "Average Branch Length",
  Convexity = "Convexity",
  Radius_0_pixels = "Radii of the object_0",
  Radius_1_pixels = "Radii of the object_1",
  Bbox_min_0 = "Bounding Box Minimum_0",
  Bbox_min_1 = "Bounding Box Minimum_1",
  Bbox_max_0 = "Bounding Box Maximum_0",
  Bbox_max_1 = "Bounding Box Maximum_1"
)
canon_class <- function(x) {
  x <- trimws(as.character(x))
  x[x == "Mature Tangles"] <- "Mature Tangle"
  x[x == "Artefact"] <- "Artifact"
  x
}
num_feature <- function(d, column, required = FALSE) {
  if (!column %in% names(d)) {
    if (required) stop("Missing required feature: ", column, call. = FALSE)
    return(rep(NA_real_, nrow(d)))
  }
  raw <- d[[column]]
  value <- suppressWarnings(as.numeric(raw))
  bad <- is.na(value) & !is.na(raw) & !trimws(as.character(raw)) %in% c("", "NA", "NaN")
  if (any(bad)) stop("Non-numeric values in feature: ", column, call. = FALSE)
  value
}
mean_ok <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
median_ok <- function(x) if (any(is.finite(x))) median(x[is.finite(x)]) else NA_real_
sum_complete <- function(x) if (!length(x)) 0 else if (any(!is.finite(x))) NA_real_ else sum(x)
rel_error <- function(a, b) abs(a-b)/pmax(1, abs(a), abs(b))

canonical_image <- function(x) {
  x <- sub("[.]csv$", "", basename(x), ignore.case = TRUE)
  # Keep well, field, acquisition time, and z identity. Remove channel only.
  sub("C[0-9]+(_table)?$", "", x)
}
infer_case <- function(path) {
  candidates <- regmatches(toupper(path),
                           gregexpr("(?<![A-Z0-9])(NPC|POD|PO)[0-9]+(?![0-9])",
                                    toupper(path), perl = TRUE))[[1]]
  candidates <- unique(candidates)
  if (length(candidates) != 1)
    stop("Cannot uniquely infer case from path. Supply case_id or file_manifest: ", path,
         call. = FALSE)
  candidates
}
diagnosis_of <- function(case) {
  if (grepl("^NPC[0-9]+$", case)) "Control" else
    if (grepl("^POD[0-9]+$", case)) "PSP+" else
      if (grepl("^PO[0-9]+$", case)) "PSP" else NA_character_
}
infer_assay <- function(d, path, assay) {
  if (!is.null(assay) && toupper(assay) != "AUTO") return(toupper(assay))
  evidence <- paste(c(path, names(d), if ("Predicted Class" %in% names(d))
    unique(d[["Predicted Class"]]) else character()), collapse = " ")
  hit <- c(AT8 = grepl("AT8|Mature Tangle|Single Aggregate", evidence, ignore.case = TRUE),
           TRIM46 = grepl("TRIM46|Axon Initial Segment", evidence, ignore.case = TRUE))
  if (sum(hit) != 1) stop("Assay ambiguous; supply AT8 or TRIM46: ", path, call. = FALSE)
  names(hit)[hit]
}

make_manifest_template <- function(input_dir, output_file) {
  input_dir <- normalizePath(input_dir, mustWork = TRUE)
  output_file <- normalizePath(output_file, mustWork = FALSE)
  if (startsWith(output_file, paste0(input_dir, "/")))
    stop("Save the manifest outside the raw input folder.")
  if (file.exists(output_file)) stop("Manifest output already exists.")
  files <- sort(list.files(input_dir, pattern = "[.]csv$", recursive = TRUE,
                           full.names = TRUE, ignore.case = TRUE))
  files <- files[!grepl("(^|/)(__MACOSX|[.]_[^/]*)($|/)", files)]
  if (!length(files)) stop("No raw CSV files found.")
  rows <- lapply(files, function(f) {
    header <- read_csv(f, n_max = 0, name_repair = "minimal",
                       col_types = cols(.default = col_character()), show_col_types = FALSE)
    tibble(Source_file = substring(f,nchar(input_dir)+2),
           Case = tryCatch(infer_case(f),error=function(e) NA_character_),
           Assay = tryCatch(infer_assay(header,f,"auto"),error=function(e) NA_character_))
  })
  template <- bind_rows(rows)
  write_csv(template,output_file,na="")
  message("Review every Case/Assay assignment and fill blanks before using the manifest.")
  invisible(template)
}

read_object_export <- function(path, assay, case, source, width, height) {
  # All original features are retained with their exact column names.
  d <- read_csv(path, col_types = cols(.default = col_character()),
                name_repair = "minimal", show_col_types = FALSE, progress = FALSE,
                na = c("", "NA", "NaN"))
  if (nrow(problems(d))) stop("CSV parsing problems: ", path, call. = FALSE)
  if (anyDuplicated(names(d)[nzchar(names(d))]))
    stop("Duplicate named columns: ", path, call. = FALSE)
  d <- d[, nzchar(names(d)), drop = FALSE] # Ignore unnamed CSV index only.
  a <- infer_assay(d, source, assay)
  if (!a %in% names(CLASS_DEFS)) stop("Assay must be AT8, TRIM46, or auto")
  if (!nrow(d)) {
    return(list(objects = NULL, raw = d, assay = a, empty = TRUE))
  }
  if (!"Predicted Class" %in% names(d)) stop("Nonempty file lacks Predicted Class: ", path)
  if (!"labelimage_oid" %in% names(d)) stop("Missing labelimage_oid: ", path)
  predicted <- canon_class(d[["Predicted Class"]])
  allowed <- CLASS_DEFS[[a]]
  if (any(is.na(predicted) | !predicted %in% allowed))
    stop("Missing or unexpected predicted class in ", path, ": ",
         paste(unique(predicted[is.na(predicted) | !predicted %in% allowed]), collapse = ", "))
  raw_image <- if ("File Name" %in% names(d)) d[["File Name"]] else
    rep(tools::file_path_sans_ext(basename(path)), nrow(d))
  if (any(is.na(raw_image) | !nzchar(raw_image))) stop("Missing image identifier: ", path)
  timestep <- if ("timestep" %in% names(d)) d[["timestep"]] else rep("0", nrow(d))
  timestep_num <- suppressWarnings(as.numeric(timestep))
  if (any(!is.finite(timestep_num) | timestep_num<0 | timestep_num!=round(timestep_num)))
    stop("Missing or invalid integer timestep: ", path)
  timestep <- as.character(timestep_num)
  object_id <- num_feature(d, "labelimage_oid", required = TRUE)
  if (any(!is.finite(object_id) | object_id<=0 | object_id!=round(object_id)))
    stop("Missing or invalid positive integer object ID: ", path)
  z <- tibble(Case = case, Diagnosis = diagnosis_of(case), Assay = a,
              Image = canonical_image(raw_image), Raw_image = raw_image,
              Timestep = timestep, Source_file = source,
              Object_id = as.character(object_id), Predicted_class = predicted,
              Biologically_trusted = predicted %in% TRUSTED)
  if (any(is.na(z$Object_id))) stop("Missing object ID: ", path)
  z$Image_key <- paste(z$Case, z$Assay, z$Image, z$Timestep, sep = "|")
  z$Pair_key <- paste(z$Case, z$Image, z$Timestep, sep = "|")
  z$Object_key <- paste(z$Image_key, z$Object_id, sep = "|")
  if (anyDuplicated(z$Object_key)) stop("Duplicate object ID within image: ", path)
  for (nm in names(FEATURES))
    z[[nm]] <- num_feature(d, FEATURES[[nm]], required = nm %in% c("Area_pixels", "Mean_intensity"))
  if (any(!is.finite(z$Area_pixels) | z$Area_pixels <= 0 | z$Area_pixels > width*height))
    stop("Invalid object pixel area: ", path)
  if (any(z$Area_pixels != round(z$Area_pixels))) stop("Non-integer object pixel area: ", path)
  # Missing intensities are retained as missing, never silently zero-filled.
  if (any(is.finite(z$Mean_intensity) & z$Mean_intensity < 0))
    stop("Negative mean intensity: ", path)
  z$Area_columns_disagree <- is.finite(z$Object_area_export) &
    rel_error(z$Area_pixels, z$Object_area_export) > 1e-6
  calculated <- z$Area_pixels*z$Mean_intensity
  z$Signal_columns_disagree <- is.finite(z$Total_intensity_export) &
    is.finite(calculated) & rel_error(z$Total_intensity_export, calculated) > 1e-5
  # Preserve actual exported total when present; derive only when absent/missing.
  z$Total_intensity <- ifelse(is.finite(z$Total_intensity_export),
                              z$Total_intensity_export, calculated)
  z$Intensity_total_derived <- !is.finite(z$Total_intensity_export)
  # Coordinates are ilastik feature-axis order; axes must be verified for rectangles.
  coord <- as.matrix(z[, c("Bbox_min_0", "Bbox_min_1", "Bbox_max_0", "Bbox_max_1")])
  complete_bbox <- rowSums(is.finite(coord)) == 4
  z$Touches_image_boundary <- NA
  if (width == height) {
    z$Touches_image_boundary[complete_bbox] <-
      z$Bbox_min_0[complete_bbox] <= 0 | z$Bbox_min_1[complete_bbox] <= 0 |
      z$Bbox_max_0[complete_bbox] >= width | z$Bbox_max_1[complete_bbox] >= height
    z$Bbox_outside_image <- complete_bbox &
      (z$Bbox_min_0 < 0 | z$Bbox_min_1 < 0 |
         z$Bbox_max_0 > width | z$Bbox_max_1 > height)
  } else z$Bbox_outside_image <- NA
  z$Object_fraction_image <- z$Area_pixels/(width*height)
  # A flag for inspection, not a validated exclusion threshold.
  z$Review_large_object <- z$Object_fraction_image > .01
  prob_names <- names(d)[grepl("^Probability of ", names(d))]
  z$Predicted_class_score <- z$Top_score <- z$Top_two_score_margin <- NA_real_
  z$Predicted_label_not_max_score <- NA
  if (length(prob_names)) {
    scores <- do.call(cbind, lapply(prob_names, function(n) num_feature(d, n)))
    colnames(scores) <- canon_class(sub("^Probability of ", "", prob_names))
    complete_scores <- rowSums(is.finite(scores)) == ncol(scores)
    if (any(scores < 0 | scores > 1, na.rm = TRUE)) stop("Class scores outside [0,1]: ", path)
    for (i in which(complete_scores)) {
      si <- sort(scores[i, ], decreasing = TRUE)
      j <- match(z$Predicted_class[i], colnames(scores))
      if (!is.na(j)) z$Predicted_class_score[i] <- scores[i, j]
      z$Top_score[i] <- si[1]
      z$Top_two_score_margin[i] <- if (length(si)>1) si[1]-si[2] else NA_real_
      z$Predicted_label_not_max_score[i] <-
        is.finite(z$Predicted_class_score[i]) && z$Predicted_class_score[i] < si[1]-1e-8
    }
  }
  list(objects = z, raw = d, assay = a, empty = FALSE)
}

summarise_objects <- function(z) {
  a <- if (nrow(z)) sum(z$Area_pixels) else 0
  signal <- sum_complete(z$Total_intensity)
  out <- tibble(
    Count = nrow(z), Total_area_pixels = a,
    Intensity_objects_observed = sum(is.finite(z$Mean_intensity)),
    Sum_object_mean_intensities = sum_complete(z$Mean_intensity),
    Mean_object_intensity = mean_ok(z$Mean_intensity),
    Total_intensity = signal,
    Pixel_weighted_mean_intensity = if (a>0) signal/a else NA_real_,
    Median_object_area_pixels = median_ok(z$Area_pixels),
    Mean_object_area_pixels = mean_ok(z$Area_pixels),
    Max_object_area_pixels = if (nrow(z)) max(z$Area_pixels) else NA_real_,
    Boundary_objects = sum(z$Touches_image_boundary %in% TRUE),
    Boundary_status_known = sum(!is.na(z$Touches_image_boundary)),
    Large_objects_to_review = sum(z$Review_large_object %in% TRUE),
    Area_disagreement_objects = sum(z$Area_columns_disagree %in% TRUE),
    Signal_disagreement_objects = sum(z$Signal_columns_disagree %in% TRUE),
    Missing_mean_intensity_objects = sum(!is.finite(z$Mean_intensity)),
    Median_predicted_class_score = median_ok(z$Predicted_class_score),
    Median_top_two_score_margin = median_ok(z$Top_two_score_margin)
  )
  for (nm in c("Diameter_pixels", "Euclidean_diameter_pixels", "Skeleton_length_pixels",
               "Branches", "Average_branch_length_pixels", "Convexity",
               "Radius_0_pixels", "Radius_1_pixels")) {
    out[[paste0("N_", nm)]] <- sum(is.finite(z[[nm]]))
    out[[paste0("Mean_", nm)]] <- mean_ok(z[[nm]])
    out[[paste0("Median_", nm)]] <- median_ok(z[[nm]])
  }
  out
}

aggregate_ilastik_raw <- function(input_dir, output_dir, assay = "auto", case_id = NULL,
                                  width_px = 2000, height_px = 2000,
                                  empty_policy = c("zero", "exclude"),
                                  save_objects = TRUE, file_manifest = NULL) {
  empty_policy <- match.arg(empty_policy)
  stopifnot(is.finite(width_px), width_px > 0, width_px == round(width_px),
            is.finite(height_px), height_px > 0, height_px == round(height_px))
  input_dir <- normalizePath(path.expand(input_dir), mustWork = TRUE)
  output_dir <- normalizePath(path.expand(output_dir), mustWork = FALSE)
  if (output_dir == input_dir || startsWith(paste0(output_dir, "/"), paste0(input_dir, "/")))
    stop("Put outputs outside the input folder so reruns cannot ingest their own summaries.")
  files <- list.files(input_dir, pattern = "[.]csv$", recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  files <- sort(files[!grepl("(^|/)(__MACOSX|[.]_[^/]*)($|/)", files)])
  if (!length(files)) stop("No raw CSV files found.")
  relative <- substring(files, nchar(input_dir)+2)
  manifest <- NULL
  if (!is.null(file_manifest)) {
    manifest <- read_csv(file_manifest, col_types = cols(.default = col_character()),
                         show_col_types = FALSE)
    if (!all(c("Source_file", "Case", "Assay") %in% names(manifest)))
      stop("file_manifest needs Source_file (relative path), Case, Assay.")
    if (anyDuplicated(manifest$Source_file)) stop("Duplicate manifest Source_file.")
    if (any(!relative %in% manifest$Source_file)) stop("Manifest does not cover all input CSVs.")
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
                                                  no.. = TRUE)))
    stop("Output folder is nonempty. Use a fresh output folder; existing results are never overwritten.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (save_objects) dir.create(file.path(output_dir, "objects"))
  summaries <- inventories <- audits <- list()
  seen_images <- character()
  completed <- FALSE
  # A partial output directory remains marked incomplete after any error.
  writeLines("INCOMPLETE: aggregation has not finished.", file.path(output_dir, "RUN_STATUS.txt"))
  for (j in seq_along(files)) {
    case <- if (!is.null(manifest)) manifest$Case[match(relative[j], manifest$Source_file)] else
      if (!is.null(case_id) && tolower(case_id) != "auto") toupper(case_id) else infer_case(files[j])
    a_arg <- if (!is.null(manifest)) manifest$Assay[match(relative[j], manifest$Source_file)] else assay
    if (is.na(case) || !nzchar(case)) stop("Missing case assignment: ", relative[j])
    case <- toupper(trimws(case))
    r <- read_object_export(files[j], a_arg, case, relative[j], width_px, height_px)
    z <- r$objects
    if (r$empty) {
      image <- canonical_image(basename(files[j]))
      inv <- tibble(Case = case, Diagnosis = diagnosis_of(case), Assay = r$assay,
                    Image = image, Timestep = "0", Source_file = relative[j],
                    Image_key = paste(case, r$assay, image, "0", sep = "|"),
                    Pair_key = paste(case, image, "0", sep = "|"))
      # Typed zero-row feature table for common summary code.
      z <- tibble()
      for (nm in c(names(FEATURES), "Total_intensity", "Predicted_class_score",
                   "Top_two_score_margin", "Predicted_class")) z[[nm]] <- numeric()
      for (nm in c("Touches_image_boundary", "Review_large_object", "Area_columns_disagree",
                   "Signal_columns_disagree")) z[[nm]] <- logical()
    } else {
      inv <- z %>% distinct(Case, Diagnosis, Assay, Image, Timestep, Source_file, Image_key, Pair_key)
    }
    if (any(inv$Image_key %in% seen_images) || anyDuplicated(inv$Image_key))
      stop("The same case/assay/image/timestep occurs in multiple sources: ", relative[j],
           ". Do not mix per-image exports with concatenated copies.")
    seen_images <- c(seen_images, inv$Image_key)
    inv$Empty_export <- r$empty
    inv$Analysis_eligible <- !r$empty || empty_policy == "zero"
    inv$Export_status <- if (!r$empty) "Objects exported" else
      if (empty_policy == "zero") "Header-only: explicitly treated as zero" else
        "Header-only: unverified, excluded"
    inv$Width_pixels <- width_px
    inv$Height_pixels <- height_px
    inv$Image_area_pixels <- width_px*height_px
    inv$Exported_objects <- integer(nrow(inv))
    inv$All_segmented_area_pixels <- numeric(nrow(inv))
    inv$Area_sum_exceeds_frame <- logical(nrow(inv))
    for (i in seq_len(nrow(inv))) {
      zi <- if (r$empty) z else z[z$Image_key == inv$Image_key[i], ]
      inv$Exported_objects[i] <- nrow(zi)
      inv$All_segmented_area_pixels[i] <- sum(zi$Area_pixels)
      inv$Area_sum_exceeds_frame[i] <- sum(zi$Area_pixels) > width_px*height_px
      # Exactly four atomic classes per image, taken from the shared registry.
      classes <- CLASS_DEFS[[r$assay]]
      groups <- setNames(as.list(classes), classes)
      groups[[paste0("All ", r$assay)]] <- CLASS_DEFS[[r$assay]]
      groups[[paste0("Trusted ", r$assay)]] <- intersect(CLASS_DEFS[[r$assay]], TRUSTED)
      for (g in names(groups)) {
        selected <- zi[zi$Predicted_class %in% groups[[g]], , drop = FALSE]
        value <- summarise_objects(selected)
        value$Area_fraction_image <- value$Total_area_pixels/(width_px*height_px)
        value$Area_percent_image <- 100*value$Area_fraction_image
        value$Biologically_trusted <- all(groups[[g]] %in% TRUSTED)
        value$Is_merger <- length(groups[[g]])>1
        value$Class <- g
        if (!inv$Analysis_eligible[i]) {
          metrics <- setdiff(names(value), c("Class", "Biologically_trusted", "Is_merger"))
          for (nm in metrics) value[[nm]] <- NA_real_
        }
        summaries[[length(summaries)+1]] <- bind_cols(inv[i, ], value)
      }
    }
    inventories[[j]] <- inv
    object_path <- NA_character_
    if (save_objects && !r$empty) {
      object_path <- file.path("objects", sprintf("%05d_objects.csv.gz", j))
      original <- r$raw
      names(original) <- paste0("raw::", names(original))
      write_csv(bind_cols(z, original), file.path(output_dir, object_path), na = "")
    }
    audits[[j]] <- tibble(Source_file = relative[j], Case = case, Assay = r$assay,
                          Exported_objects = nrow(z), Images = nrow(inv),
                          Raw_columns = ncol(r$raw), Empty_export = r$empty,
                          Objects_file = object_path,
                          Input_md5 = unname(tools::md5sum(files[j])))
    if (j %% 25 == 0 || j == length(files)) message("Processed ", j, "/", length(files), " CSVs")
  }
  image <- bind_rows(summaries)
  inventory <- bind_rows(inventories)
  # Case calculations use sums, not means of image means.
  cases <- image %>% group_by(Case, Diagnosis, Assay, Class, Biologically_trusted, Is_merger) %>%
    summarise(
      N_images_exported = n(), N_images_used = sum(Analysis_eligible),
      N_empty_exports = sum(Empty_export),
      Total_count = if (any(Analysis_eligible)) sum_complete(Count[Analysis_eligible]) else NA_real_,
      Mean_count_per_image = if (N_images_used>0) Total_count/N_images_used else NA_real_,
      Total_area_pixels = if (any(Analysis_eligible))
        sum_complete(Total_area_pixels[Analysis_eligible]) else NA_real_,
      Total_image_area_pixels = sum(Image_area_pixels[Analysis_eligible]),
      Area_fraction_image = if (Total_image_area_pixels>0)
        Total_area_pixels/Total_image_area_pixels else NA_real_,
      Area_percent_image = 100*Area_fraction_image,
      Total_intensity = if (any(Analysis_eligible))
        sum_complete(Total_intensity[Analysis_eligible]) else NA_real_,
      Mean_object_intensity = if (is.finite(Total_count) && Total_count>0)
        sum_complete(Sum_object_mean_intensities[Analysis_eligible])/Total_count else NA_real_,
      Pixel_weighted_mean_intensity = if (is.finite(Total_area_pixels) && Total_area_pixels>0)
        Total_intensity/Total_area_pixels else NA_real_,
      Mean_object_area_pixels = if (is.finite(Total_count) && Total_count>0)
        Total_area_pixels/Total_count else NA_real_,
      .groups = "drop")
  # Object-weighted morphology, not an unweighted average of image means.
  morphology <- image %>%
    group_by(Case, Diagnosis, Assay, Class, Biologically_trusted, Is_merger) %>%
    group_modify(function(d, key) {
      d <- d[d$Analysis_eligible, , drop = FALSE]
      out <- tibble(.rows = 1)
      for (feature in c("Diameter_pixels", "Euclidean_diameter_pixels", "Skeleton_length_pixels",
                        "Branches", "Average_branch_length_pixels", "Convexity",
                        "Radius_0_pixels", "Radius_1_pixels")) {
        n <- d[[paste0("N_", feature)]]
        m <- d[[paste0("Mean_", feature)]]
        observed <- is.finite(n) & n>0 & is.finite(m)
        out[[paste0("N_", feature)]] <- sum(n[observed])
        out[[paste0("Mean_", feature)]] <- if (any(observed))
          sum(n[observed]*m[observed])/sum(n[observed]) else NA_real_
      }
      out
    }) %>% ungroup()
  cases <- left_join(cases, morphology,
                     by = c("Case", "Diagnosis", "Assay", "Class", "Biologically_trusted", "Is_merger"))
  pair_inventory <- inventory %>%
    dplyr::select(
      Pair_key, Case, Image, Timestep, Assay, Analysis_eligible
    ) %>%
    tidyr::pivot_wider(
      names_from = Assay,
      values_from = Analysis_eligible,
      names_prefix = "Eligible_",
      values_fill = FALSE
    )
  for (nm in c("Eligible_AT8", "Eligible_TRIM46"))
    if (!nm %in% names(pair_inventory)) pair_inventory[[nm]] <- FALSE
  pair_inventory$Paired_eligible <- pair_inventory$Eligible_AT8 & pair_inventory$Eligible_TRIM46
  write_csv(image, file.path(output_dir, "image_by_class_and_merger.csv"), na = "")
  write_csv(cases, file.path(output_dir, "case_by_class_and_merger.csv"), na = "")
  write_csv(inventory, file.path(output_dir, "image_inventory.csv"), na = "")
  write_csv(pair_inventory, file.path(output_dir, "channel_pairing_inventory.csv"), na = "")
  write_csv(bind_rows(audits), file.path(output_dir, "file_audit.csv"), na = "")
  flagged <- image %>% filter(Area_sum_exceeds_frame |
                                coalesce(Large_objects_to_review>0, FALSE) |
                                coalesce(Area_disagreement_objects>0, FALSE) |
                                coalesce(Signal_disagreement_objects>0, FALSE) |
                                coalesce(Missing_mean_intensity_objects>0, FALSE) |
                                !Analysis_eligible)
  write_csv(flagged, file.path(output_dir, "QC_flagged_image_classes.csv"), na = "")
  capture.output(sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  writeLines(c(
    "COMPLETE", paste("Input:", input_dir),
    paste("Image size:", width_px, "x", height_px, "pixels"),
    paste("Empty export policy:", empty_policy),
    "Denominator: full image area, NOT tissue area.",
    "Combined objects are not split or counted twice. All mergers contain all four assay-specific classes.",
    "All AT8 includes Mature Tangle, Diffuse AT8, Single Aggregate, Combined Aggregate.",
    "All TRIM46 includes Full AIS, Partial AIS, Combined AIS, Artifact.",
    "Trusted mergers contain Mature Tangle + Diffuse AT8 + Combined Aggregate, or Full AIS + Partial AIS + Combined AIS.",
    "Area sums assume disjoint labels from one segmentation per image; masks are not available to verify this.",
    "Large-object flag (>1% of frame) is descriptive and never excludes objects.",
    "Raw class scores are retained, not assumed calibrated probabilities.",
    "The same fields must be paired across channels before primary joint analyses."
  ), file.path(output_dir, "RUN_STATUS.txt"))
  completed <- TRUE
  message("Completed: ", nrow(inventory), " images; ",
          sum(inventory$Exported_objects), " objects; ",
          sum(inventory$Empty_export), " header-only exports. Output: ", output_dir)
  invisible(list(image = image, case = cases, inventory = inventory))
}

},envir=ILASTIK_RAW)
aggregate_ilastik_raw <- ILASTIK_RAW$aggregate_ilastik_raw
make_manifest_template <- ILASTIK_RAW$make_manifest_template

# PSP BRAIN TISSUE: FINAL SELF-CONTAINED FEATURE STUDY
# Edit CONFIG below, then click Source in RStudio. No CLI arguments are required.
# Existing WB statistics are retained. Imaging is a feature/distribution study;
# AT8-TRIM46 correlations are not run. Cognition is exploratory and case-only.
# Source without running: options(psp.features.autorun = FALSE)

CONFIG <- list(
  AT8_RAW_DIR = "~/Desktop/40X_AT8_CSV",
  TRIM46_RAW_DIR = "~/Desktop/40X_TRIM46_CSV",
  WB_FILE = "~/Desktop/PSP_Protein_Level_Data.csv",
  COG_FILE = "~/Desktop/PSP_Cognition.csv",
  # A unique child folder is created on every run; existing files are untouched.
  OUT = "~/Desktop/PSP_final_paper_runs",
  # Header-only exports are fully analyzed fields in which ilastik detected no
  # objects (confirmed from image_inventory.csv), so they count as zero.
  EMPTY_POLICY = "zero",
  # Re-run case-level IF comparisons with header-only fields excluded and
  # write tables/IF_sensitivity_empty_policy.csv.
  RUN_EMPTY_POLICY_SENSITIVITY = TRUE,
  # Linear calibration at 40X: microns per pixel. Areas use the square.
  MICRONS_PER_PIXEL = 0.1625,
  MAKE_PLOTS = TRUE,
  DPI = 600,
  EXPORT_FORMATS = c("png"),
  # ---- Figure scope -------------------------------------------------------
  PLOT_IF_CLASSES = c("Trusted AT8", "Trusted TRIM46"),
  PLOT_IF_LEVELS = c("case", "image"),
  PLOT_COG_CLASSES = c("Diffuse AT8"),
  # TRUE: IF panels are drawn only for features with at least one bracket
  # p < PLOT_ALPHA at that level/grouping. All results stay in tables/.
  PLOT_SIGNIFICANT_ONLY = TRUE,
  PLOT_ALPHA = 0.05,
  # ---- Print-size styling (shared by WB, IF, and MoCA figures) -----------
  # Figures are drawn at final print size: place them in the manuscript at
  # 100% so that text prints at BASE_FONT_SIZE points.
  FONT_FAMILY = "sans",
  BASE_FONT_SIZE = 8,
  # Every multi-panel figure (IF and MoCA) uses PANEL_NCOL columns, even when
  # fewer panels are drawn, and every panel (IF, WB, MoCA) is PANEL_W_IN x
  # PANEL_H_IN, so panel size, fonts, and figure width are identical everywhere.
  PANEL_NCOL = 4L,
  PANEL_W_IN = 1.25,
  PANEL_H_IN = 1.35,
  # Panel tags: "number" (1, 2, 3, ...) or "letter" (A, B, C, ...).
  PANEL_TAG_STYLE = "number",
  CASE_POINT_SIZE = 1.9,
  FIELD_POINT_SIZE = 0.55,
  SHOW_PLOT_TEXT = FALSE,
  SHOW_CASE_LABELS = FALSE,
  # ---- Statistics ---------------------------------------------------------
  N_PERM = 19999L,
  EXACT_MAX = 50000L,
  N_BOOT = 999L,
  MIN_CASES = 4L,
  POWER_TARGETS = c(0.80, 0.90),
  POWER_ALPHA = 0.05
)
# Bind state here so <<- updates this private environment, never old globals.
ACTIVE_CONFIG <- CONFIG
OUT <- WB_FILE <- INPUT <- output_dir <- NULL
PLOT_INDEX <- list()

required <- c("dplyr", "tidyr", "readr", "ggplot2")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages))
  stop("Install first: ", paste(missing_packages, collapse = ", "))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
})
# Protect the retained WB code and RStudio sessions with other packages attached.
select <- dplyr::select
filter <- dplyr::filter
lag <- dplyr::lag
GRP3 <- c("Control", "PSP", "PSP+")
GRP2 <- c("Control", "PSP spectrum")
COLORS3 <- c("Control" = "#489FA7", "PSP" = "#C43E96", "PSP+" = "#F2B342")
COLORS2 <- c("Control" = "#000000", "PSP spectrum" = "#767676")
COLORS <- COLORS3

MORPH <- c("Diameter_pixels", "Euclidean_diameter_pixels", "Skeleton_length_pixels",
           "Branches", "Average_branch_length_pixels", "Convexity",
           "Radius_0_pixels", "Radius_1_pixels")
FEATURES_V6 <- tibble::tribble(
  ~Feature, ~Label, ~Domain, ~Case_estimator, ~Image_estimator,
  "Count", "Objects / image", "Abundance", "Total objects / fields", "Segmented objects",
  "Area", "Segmented area (% image)", "Abundance", "Total area / total frame area", "Object area / frame area",
  "Detection", "Fields with detection (%)", "Detection", "100 x positive fields / fields", "100 if detected; 0 otherwise",
  "Positive_area", "Area in positive fields (% image)", "Positive extent", "Total area / frame area of positive fields", "Area percentage conditional on detection",
  "Object_size", "Mean object size (\u00b5m\u00b2)", "Object size", "Total area / total objects", "Area / objects",
  "Object_intensity", "Object-weighted mean intensity (a.u.)", "Intensity", "Sum of object means / objects", "Sum of object means / objects",
  "Pixel_intensity", "Pixel-weighted mean intensity (a.u.)", "Intensity", "Total signal / total object area", "Total signal / object area",
  "Signal", "Integrated signal / image (a.u.)", "Intensity", "Total signal / fields", "Total signal"
)
FEATURES_V6 <- dplyr::bind_rows(
  FEATURES_V6,
  tibble::tibble(
    Feature = MORPH,
    Label = c("Skeleton diameter (\u00b5m)", "Euclidean diameter (\u00b5m)",
              "Skeleton length (\u00b5m)", "Branches / object",
              "Mean branch length (\u00b5m)", "Convexity",
              "Object radius 0 (\u00b5m)", "Object radius 1 (\u00b5m)"),
    Domain = "Morphology",
    Case_estimator = "Object-weighted mean of available measurements",
    Image_estimator = "Mean over objects with this measurement"
  )
)
# Physical units. Linear morphology is multiplied by MICRONS_PER_PIXEL and
# object size (pixel count) by MICRONS_PER_PIXEL^2. Area (% image) is unitless.
LINEAR_MORPH <- c("Diameter_pixels", "Euclidean_diameter_pixels", "Skeleton_length_pixels",
                  "Average_branch_length_pixels", "Radius_0_pixels", "Radius_1_pixels")
FEATURES_V6$Units <- dplyr::case_when(
  FEATURES_V6$Feature %in% LINEAR_MORPH ~ "\u00b5m",
  FEATURES_V6$Feature == "Object_size" ~ "\u00b5m\u00b2",
  FEATURES_V6$Feature %in% c("Area", "Positive_area", "Detection") ~ "%",
  FEATURES_V6$Feature %in% c("Object_intensity", "Pixel_intensity", "Signal") ~ "a.u.",
  FEATURES_V6$Feature == "Count" ~ "objects per field",
  TRUE ~ "unitless"
)
case_id <- function(x) sub("^((NPC|POD|PO)[0-9]+).*$", "\\1",
                           toupper(gsub("\\s+", "", as.character(x))))
diagnosis <- function(x) dplyr::case_when(
  grepl("^NPC[0-9]+$", x) ~ "Control",
  grepl("^POD[0-9]+$", x) ~ "PSP+",
  grepl("^PO[0-9]+$", x) ~ "PSP", TRUE ~ NA_character_
)
varies <- function(x) sum(is.finite(x)) >= 2 && length(unique(x[is.finite(x)])) > 1
mean_safe <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
sum_complete <- function(x) if (!length(x)) 0 else
  if (any(!is.finite(x))) NA_real_ else sum(x)
weighted_feature <- function(m, n) {
  ok <- is.finite(m) & is.finite(n) & n>0
  if (any(ok)) sum(m[ok]*n[ok])/sum(n[ok]) else NA_real_
}
fmt <- function(p) ifelse(!is.finite(p), "NA",
                          ifelse(p<.001, "<0.001", sprintf("%.3f", p)))
format_pval <- fmt
format_pval_3 <- function(p) {
  p <- as.numeric(p)[1]
  if (!is.finite(p)) "p=NA" else if (p<.001) "p<0.001" else paste0("p=", sprintf("%.3f",p))
}
bh <- function(p) p.adjust(p, "BH", n=length(p))
first_present <- function(x) {
  i <- which(!is.na(x) & as.character(x)!="")
  if (length(i)) x[i[1]] else x[NA_integer_][1]
}
# TABLE_SUBDIR redirects sensitivity re-runs into their own subfolder.
TABLE_SUBDIR <- ""
write_table <- function(x,name,qc=FALSE) {
  dir <- file.path(OUT,if(qc) "qc" else "tables")
  if(nzchar(TABLE_SUBDIR)) dir <- file.path(dir,TABLE_SUBDIR)
  dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  readr::write_csv(x,file.path(dir,paste0(name,".csv")),na="")
}
save_plot <- function(p,subdir,name,width=10,height=8) {
  if(!isTRUE(ACTIVE_CONFIG$MAKE_PLOTS))return(invisible(NULL))
  dir.create(file.path(OUT,"figures",subdir),recursive=TRUE,showWarnings=FALSE)
  formats <- ACTIVE_CONFIG$EXPORT_FORMATS
  if(is.null(formats))formats <- c("png")
  dpi <- ACTIVE_CONFIG$DPI
  for(format in formats) {
    path <- file.path(OUT,"figures",subdir,paste0(name,".",format))
    if(format=="png") {
      device <- if(requireNamespace("ragg",quietly=TRUE))ragg::agg_png else grDevices::png
      ggplot2::ggsave(path,p,device=device,width=width,height=height,
                     units="in",dpi=dpi,bg="white")
    } else stop("Unsupported EXPORT_FORMATS entry: ",format)
    PLOT_INDEX[[length(PLOT_INDEX)+1L]] <<- tibble::tibble(
      File=path,Category=subdir,Format=format,Width_inches=width,Height_inches=height
    )
  }
}

# Cache permutations by sample size, never by the observed result.
PERM_CACHE <- new.env(parent=emptyenv())
RANK_CACHE <- new.env(parent=emptyenv())
group_rank_test <- function(x,y,min_n=4L) {
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  n1 <- length(x); n2 <- length(y)
  out <- list(P=NA_real_,Method=NA_character_,Cliffs_delta=NA_real_,
              Median_difference=if(n1&&n2) median(x)-median(y) else NA_real_,
              CI_low=NA_real_,CI_high=NA_real_,Status="SKIPPED: insufficient cases")
  if(n1 && n2) {
    ranks <- rank(c(x,y),ties.method="average")
    u <- sum(ranks[seq_len(n1)])-n1*(n1+1)/2
    out$Cliffs_delta <- 2*u/(n1*n2)-1
  }
  if(n1<min_n || n2<min_n) return(out)
  if(!varies(c(x,y))) {out$Status <- "SKIPPED: all values identical"; return(out)}
  n <- n1+n2
  key <- paste(n1,n2,ACTIVE_CONFIG$N_PERM,ACTIVE_CONFIG$EXACT_MAX,sep="_")
  if(!exists(key,PERM_CACHE,inherits=FALSE)) {
    exact <- choose(n,n1)<=ACTIVE_CONFIG$EXACT_MAX
    idx <- if(exact) combn(n,n1) else
      replicate(ACTIVE_CONFIG$N_PERM,sample.int(n,n1))
    assign(key,list(idx=idx,exact=exact),PERM_CACHE)
  }
  perm <- get(key,PERM_CACHE)
  vals <- colSums(matrix(ranks[perm$idx],nrow=n1))
  center <- n1*(n+1)/2
  observed <- abs(sum(ranks[seq_len(n1)])-center)
  k <- sum(abs(vals-center)>=observed-1e-12)
  out$P <- if(perm$exact) k/length(vals) else (k+1)/(length(vals)+1)
  out$Method <- if(perm$exact) "Exact two-sided label-permutation rank-sum" else
    paste0("Monte Carlo rank-sum; ",length(vals)," draws; plus-one p")
  if(ACTIVE_CONFIG$N_BOOT>0) {
    a <- apply(matrix(sample(x,n1*ACTIVE_CONFIG$N_BOOT,replace=TRUE),nrow=n1),2,median)
    b <- apply(matrix(sample(y,n2*ACTIVE_CONFIG$N_BOOT,replace=TRUE),nrow=n2),2,median)
    ci <- quantile(a-b,c(.025,.975),names=FALSE)
    out$CI_low <- ci[1]; out$CI_high <- ci[2]
  }
  out$Status <- "Exploratory distribution comparison"
  out
}
rank_test <- function(x,y) {
  n <- length(x)
  rho <- cor(x,y,method="spearman")
  key <- paste(n,ACTIVE_CONFIG$N_PERM,sep="_")
  if(!exists(key,RANK_CACHE,inherits=FALSE)) {
    enumerate <- function(v) {
      if(length(v)==1) return(matrix(v,nrow=1))
      do.call(rbind,lapply(seq_along(v),function(j)cbind(v[j],enumerate(v[-j]))))
    }
    idx <- if(n<=8) enumerate(seq_len(n)) else
      t(replicate(ACTIVE_CONFIG$N_PERM,sample.int(n)))
    assign(key,idx,RANK_CACHE)
  }
  idx <- get(key,RANK_CACHE)
  xr <- rank(x)-mean(rank(x)); yr <- rank(y)-mean(rank(y))
  vals <- drop(matrix(xr[idx],nrow=nrow(idx))%*%yr)/sqrt(sum(xr^2)*sum(yr^2))
  k <- sum(abs(vals)>=abs(rho)-1e-12)
  list(estimate=rho,p.value=if(n<=8) k/length(vals) else (k+1)/(length(vals)+1),
       method=if(n<=8) "Exact two-sided rank-label permutation; ties retained" else
         paste0("Monte Carlo rank-label permutation; ",nrow(idx)," draws; plus-one p"))
}

read_features <- function(config) {
  paths <- setNames(path.expand(c(config$AT8_CSV,config$TRIM46_CSV)),c("AT8","TRIM46"))
  if(any(!file.exists(paths))) stop("Feature CSV missing: ",paste(paths[!file.exists(paths)],collapse=", "))
  raw <- dplyr::bind_rows(lapply(names(paths),function(a) {
    d <- readr::read_csv(paths[a],show_col_types=FALSE)
    if(!"Assay"%in%names(d) || any(d$Assay!=a)) stop("Assay/file mismatch: ",paths[a])
    d
  }))
  needed <- c("Case","Assay","Image","Timestep","Class","Analysis_eligible","Count",
              "Total_area_pixels","Image_area_pixels","Sum_object_mean_intensities",
              "Total_intensity")
  if(!all(needed%in%names(raw)))
    stop("Fresh raw aggregation is missing required columns: ",
         paste(setdiff(needed,names(raw)),collapse=", "))
  raw$Class[raw$Class=="Mature Tangles"] <- "Mature Tangle"
  # Internal labels are never renamed before filtering, pooling or testing.
  raw$Case <- case_id(raw$Case)
  raw$Diagnosis <- factor(diagnosis(raw$Case),levels=GRP3)
  if(anyNA(raw$Diagnosis)) stop("Unrecognized case prefix; expected NPC, PO, or POD.")
  write_table(raw %>% dplyr::count(Assay,Case,Analysis_eligible),
              "Input_eligibility_inventory",qc=TRUE)
  d <- raw %>% dplyr::filter(
    Analysis_eligible,
    (Assay=="AT8"&Class%in%CLASS_DEFS$AT8) |
      (Assay=="TRIM46"&Class%in%CLASS_DEFS$TRIM46)
  )
  if(!nrow(d)) stop("No eligible feature rows.")
  d$Image_key <- paste(d$Case,d$Assay,d$Image,d$Timestep,sep="|")
  if(anyDuplicated(d[c("Image_key","Class")]))
    stop("Duplicate atomic image/class rows. Inspect the raw-export inventory.")
  if(any(!is.finite(d$Count)|d$Count<0|d$Count!=round(d$Count)|
         !is.finite(d$Total_area_pixels)|d$Total_area_pixels<0|
         !is.finite(d$Image_area_pixels)|d$Image_area_pixels!=4000000))
    stop("Invalid count/area or unexpected frame size.")
  if(any((d$Count==0)!=(d$Total_area_pixels==0)))
    stop("Zero object counts and zero object areas disagree.")
  check <- d %>% dplyr::group_by(Case,Assay,Image_key) %>%
    dplyr::summarise(N_classes=dplyr::n(),Area_sum=sum(Total_area_pixels),
                    Classes=paste(sort(Class),collapse="; "),.groups="drop") %>%
    dplyr::mutate(
      Expected=vapply(Assay,function(a)
        paste(sort(CLASS_DEFS[[a]]),collapse="; "),character(1)),
      Grid_valid=N_classes==4L & Classes==Expected,
      Area_valid=Area_sum<=4000000)
  write_table(check,"Atomic_class_area_QC",qc=TRUE)
  if(any(!check$Grid_valid | !check$Area_valid)) {
    bad <- dplyr::filter(check,!Grid_valid | !Area_valid)
    write_table(bad,"Atomic_class_area_FAILURES",qc=TRUE)
    stop("Atomic input validation failed for ",nrow(bad),
         " images. See qc/Atomic_class_area_FAILURES.csv in ",OUT,
         ". Missing rows are not silently zero-filled; area overflow is not ignored.")
  }
  optional <- c("Median_predicted_class_score","Median_top_two_score_margin",
                "Intensity_objects_observed",
                paste0("N_",MORPH),paste0("Mean_",MORPH))
  for(nm in setdiff(optional,names(d))) d[[nm]] <- NA_real_
  d$Biological_tier <- ifelse(d$Class%in%TRUSTED,"Trusted class","Untrusted class")
  write_table(d %>% dplyr::select(dplyr::any_of(c(
    "Case","Diagnosis","Assay","Image","Timestep","Class","Biological_tier","Count",
    "Median_predicted_class_score","Median_top_two_score_margin",
    "Large_objects_to_review","Area_disagreement_objects","Signal_disagreement_objects",
    "Missing_mean_intensity_objects","Boundary_objects","Area_sum_exceeds_frame"))),
    "Classifier_and_segmentation_QC",qc=TRUE)
  merges <- list(
    list("AT8","All AT8",CLASS_DEFS$AT8),
    list("AT8","Trusted AT8",c("Mature Tangle","Diffuse AT8","Combined Aggregate")),
    list("TRIM46","All TRIM46",CLASS_DEFS$TRIM46),
    list("TRIM46","Trusted TRIM46",setdiff(CLASS_DEFS$TRIM46,"Artifact"))
  )

  aggregate_merger <- function(spec) {
    z <- d %>% dplyr::filter(Assay==spec[[1]],Class%in%spec[[3]])
    z %>% dplyr::group_by(Case,Diagnosis,Assay,Image,Timestep,Image_key) %>%
      dplyr::group_modify(function(s,key) {
        o <- tibble::tibble(
          Class=spec[[2]],Biological_tier="Burden merger",
          Image_area_pixels=dplyr::first(s$Image_area_pixels),
          Count=sum(s$Count),Total_area_pixels=sum(s$Total_area_pixels),
          Intensity_objects_observed=sum_complete(s$Intensity_objects_observed),
          Sum_object_mean_intensities=sum_complete(s$Sum_object_mean_intensities),
          Total_intensity=sum_complete(s$Total_intensity)
        )
        for(m in MORPH) {
          o[[paste0("N_",m)]] <- sum(s[[paste0("N_",m)]],na.rm=TRUE)
          o[[paste0("Mean_",m)]] <- weighted_feature(s[[paste0("Mean_",m)]],s[[paste0("N_",m)]])
        }
        o
      }) %>% dplyr::ungroup()
  }
  all <- dplyr::bind_rows(d,dplyr::bind_rows(lapply(merges,aggregate_merger))) %>%
    dplyr::mutate(Measure=paste(Assay,Class,sep=": "))
  availability <- dplyr::bind_rows(lapply(unique(all$Assay),function(a) {
    z <- all[all$Assay==a,]
    tibble::tibble(
      Assay=a,Feature=FEATURES_V6$Feature,
      Available=vapply(FEATURES_V6$Feature,function(f) {
        if(f%in%MORPH) any(is.finite(z[[paste0("Mean_",f)]])&
                            is.finite(z[[paste0("N_",f)]])&z[[paste0("N_",f)]]>0)
        else TRUE
      },logical(1))
    )
  }))
  write_table(availability,"Feature_availability",qc=TRUE)
  list(data=all,availability=availability,paths=paths)
}

summarise_feature_rows <- function(d,case_level=FALSE) {
  # Each row is one image/class. Case summaries pool the appropriate numerators.
  n <- nrow(d); cnt <- sum(d$Count); area <- sum(d$Total_area_pixels)
  pos <- d$Count>0
  signal <- sum_complete(d$Total_intensity)
  out <- c(
    Count=cnt/n,
    Area=100*area/sum(d$Image_area_pixels),
    Detection=100*mean(pos),
    Positive_area=if(any(pos))100*sum(d$Total_area_pixels[pos])/sum(d$Image_area_pixels[pos]) else NA_real_,
    Object_size=if(cnt>0)area/cnt else NA_real_,
    Object_intensity=if(cnt>0)sum_complete(d$Sum_object_mean_intensities)/cnt else NA_real_,
    Pixel_intensity=if(area>0)signal/area else NA_real_,
    Signal=signal/n
  )
  for(m in MORPH) out[m] <- weighted_feature(d[[paste0("Mean_",m)]],d[[paste0("N_",m)]])
  # Convert pixels to microns (linear) and square microns (object size).
  um <- ACTIVE_CONFIG$MICRONS_PER_PIXEL
  if(is.null(um) || !is.finite(um) || um <= 0) stop("Set CONFIG$MICRONS_PER_PIXEL.")
  out["Object_size"] <- out["Object_size"]*um^2
  out[LINEAR_MORPH] <- out[LINEAR_MORPH]*um
  observed <- rep(NA_real_,length(out))
  names(observed) <- names(out)
  observed["Object_size"] <- cnt
  observed["Object_intensity"] <- sum_complete(d$Intensity_objects_observed)
  for(m in MORPH) observed[m] <- sum(d[[paste0("N_",m)]],na.rm=TRUE)
  tibble::tibble(Feature=names(out),Value=unname(out),
                N_objects_observed=unname(observed))
}
build_feature_tables <- function(input) {
  d <- input$data
  cols <- c("Case","Diagnosis","Assay","Class","Measure","Biological_tier")
  img <- d %>% dplyr::group_by(dplyr::across(dplyr::all_of(c(cols,"Image_key")))) %>%
    dplyr::group_modify(~summarise_feature_rows(.x)) %>% dplyr::ungroup()
  cas <- d %>% dplyr::group_by(dplyr::across(dplyr::all_of(cols))) %>%
    dplyr::group_modify(function(s,key) {
      summarise_feature_rows(s,TRUE) %>%
        dplyr::mutate(N_images=nrow(s),N_positive_images=sum(s$Count>0),
                      N_objects=sum(s$Count))
    }) %>% dplyr::ungroup()
  annotate <- function(z) z %>%
    dplyr::inner_join(input$availability %>% dplyr::filter(Available),
                      by=c("Assay","Feature")) %>%
    dplyr::left_join(FEATURES_V6,by="Feature")
  img <- annotate(img); cas <- annotate(cas)
  image_case <- img %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(cols,"Feature","Label","Domain")))) %>%
    dplyr::summarise(Value=mean_safe(Value),N_images_observed=sum(is.finite(Value)),.groups="drop")
  cas <- cas %>% dplyr::left_join(
    image_case %>% dplyr::select(dplyr::all_of(c(cols,"Feature","N_images_observed"))),
    by=c(cols,"Feature"))
  write_table(img,"IF_image_features")
  write_table(cas,"IF_case_features")
  write_table(image_case,"IF_image_case_balanced_features")
  list(image=img,case=cas,image_case=image_case)
}

group_features <- function(ft) {
  out <- summaries <- list()
  for(level in c("case","image_case_balanced")) {
    dat <- if(level=="case") ft$case else ft$image_case
    for(grouping in c("3group","2group")) {
      lv <- if(grouping=="3group")GRP3 else GRP2
      dd <- dat %>% dplyr::mutate(
        G=factor(if(grouping=="3group")as.character(Diagnosis) else
          ifelse(Diagnosis=="Control","Control","PSP spectrum"),levels=lv))
      summaries[[length(summaries)+1L]] <- dd %>%
        dplyr::group_by(Assay,Class,Feature,Label,G) %>%
        dplyr::summarise(
          N_cases=sum(is.finite(Value)),
          N_zero_cases=sum(Value==0,na.rm=TRUE),
          Mean=mean_safe(Value),
          Median=if(any(is.finite(Value)))median(Value[is.finite(Value)])else NA_real_,
          Q1=if(any(is.finite(Value)))quantile(Value[is.finite(Value)],.25,names=FALSE)else NA_real_,
          Q3=if(any(is.finite(Value)))quantile(Value[is.finite(Value)],.75,names=FALSE)else NA_real_,
          .groups="drop"
        ) %>% dplyr::mutate(Level=level,Grouping=grouping)
      chunks <- dd %>% dplyr::group_by(Assay,Class,Measure,Biological_tier,Feature,Label,Domain) %>%
        dplyr::group_split(.keep=TRUE)
      for(s in chunks) {
        pairs <- combn(lv,2,simplify=FALSE)
        for(pair in pairs) {
          x <- s$Value[s$G==pair[1]]; y <- s$Value[s$G==pair[2]]
          tst <- group_rank_test(x,y,ACTIVE_CONFIG$MIN_CASES)
          out[[length(out)+1L]] <- dplyr::bind_cols(
            s[1,c("Assay","Class","Measure","Biological_tier","Feature","Label","Domain")],
            tibble::tibble(
              Level=level,Grouping=grouping,
              Contrast=paste(pair,collapse=" - "),
              N_A=sum(is.finite(x)),N_B=sum(is.finite(y)),
              P=tst$P,P_method=tst$Method,Cliffs_delta=tst$Cliffs_delta,
              Median_difference_A_minus_B=tst$Median_difference,
              Median_difference_CI_low=tst$CI_low,Median_difference_CI_high=tst$CI_high,
              Status=tst$Status
            )
          )
        }
      }
    }
  }
  results <- dplyr::bind_rows(out) %>%
    dplyr::group_by(Level,Grouping,Assay,Class,Feature) %>%
    dplyr::mutate(P_Holm_within_feature= p.adjust(P,"holm")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Grouping) %>% dplyr::mutate(
      BH_Q=bh(P),
      Family="All IF assays/classes/features/contrasts and both summary levels within grouping"
    ) %>% dplyr::ungroup()
  write_table(results,"IF_feature_group_comparisons")
  write_table(dplyr::bind_rows(summaries),"IF_feature_group_descriptives")
  invisible(results)
}

# ---------------------------------------------------------------------------
# NEW: true image-level inference (exploratory log1p-Gaussian mixed model).
# Fields are retained as observations; case is a random intercept, so fields
# cluster within case. Detection (binary 0/100 per field) is fit with a
# binomial GLMM instead of the log1p-Gaussian LMM. Writes a NEW table
# separately from the case-level permutation table and its Holm/BH families.
# Image-panel brackets consume this mixed-model table.
# ---------------------------------------------------------------------------
image_level_lmm <- function(ft) {
  if (!requireNamespace("lme4", quietly = TRUE) ||
      !requireNamespace("lmerTest", quietly = TRUE))
    stop("Install first: install.packages(c('lme4','lmerTest'))")
  
  raw <- ft$image
  out <- list()
  
  for (grouping in c("3group", "2group")) {
    lv <- if (grouping == "3group") GRP3 else GRP2
    
    chunks <- raw %>%
      dplyr::group_by(
        Assay, Class, Measure, Biological_tier,
        Feature, Label, Domain
      ) %>%
      dplyr::group_split(.keep = TRUE)
    
    for (s in chunks) {
      d <- s %>%
        dplyr::filter(is.finite(Value)) %>%
        dplyr::mutate(
          G = factor(
            if (grouping == "3group") {
              as.character(Diagnosis)
            } else {
              ifelse(
                Diagnosis == "Control",
                "Control", "PSP spectrum"
              )
            },
            levels = lv
          )
        )
      
      n_cases <- vapply(
        lv,
        function(g)
          as.integer(dplyr::n_distinct(d$Case[d$G == g])),
        integer(1)
      )
      
      is_detection <- identical(s$Feature[1], "Detection")
      
      # Keep annotation rows even when a Detection test is not estimable.
      failure_rows <- function(reason) {
        lapply(combn(lv, 2, simplify = FALSE), function(pair) {
          dplyr::bind_cols(
            s[1, c(
              "Assay", "Class", "Measure", "Biological_tier",
              "Feature", "Label", "Domain"
            )],
            tibble::tibble(
              Level = "image_lmm",
              Grouping = grouping,
              Contrast = paste(pair, collapse = " - "),
              N_A_cases = unname(n_cases[pair[1]]),
              N_B_cases = unname(n_cases[pair[2]]),
              N_A_fields = sum(d$G == pair[1]),
              N_B_fields = sum(d$G == pair[2]),
              P = NA_real_,
              P_method = "Binomial GLMM: not estimable",
              Status = reason
            )
          )
        })
      }
      
      reason <- ""
      
      if (any(n_cases < ACTIVE_CONFIG$MIN_CASES)) {
        reason <- "Insufficient independent cases"
      } else if (!varies(d$Value)) {
        reason <- "All fields have the same detection outcome"
      }
      
      if (is_detection && !nzchar(reason)) {
        d$Detected <- as.integer(d$Value > 0)
        
        if (any(vapply(
          lv,
          function(g)
            length(unique(d$Detected[d$G == g])) < 2L,
          logical(1)
        ))) {
          reason <- "No outcome variation in a group; separation"
        }
      }
      
      if (nzchar(reason)) {
        if (is_detection)
          out <- c(out, failure_rows(reason))
        next
      }
      
      fit <- tryCatch(
        if (is_detection) {
          lme4::glmer(
            Detected ~ G + (1 | Case),
            data = d,
            family = stats::binomial(),
            control = lme4::glmerControl(
              optimizer = "bobyqa",
              optCtrl = list(maxfun = 200000L)
            )
          )
        } else {
          lmerTest::lmer(
            log1p(Value) ~ G + (1 | Case),
            data = d
          )
        },
        error = function(e) NULL
      )
      
      if (is.null(fit)) {
        if (is_detection)
          out <- c(out, failure_rows("Binomial model failed"))
        next
      }
      
      singular <- lme4::isSingular(fit)
      
      conv <- tryCatch(
        paste(fit@optinfo$conv$lme4$messages, collapse = "; "),
        error = function(e) ""
      )
      
      # Conservatively withhold Detection p-values when the fit is flagged.
      if (is_detection && nzchar(conv)) {
        out <- c(
          out,
          failure_rows(paste("Convergence failure:", conv))
        )
        next
      }
      
      for (pair in combn(lv, 2, simplify = FALSE)) {
        a <- pair[1]
        b <- pair[2]
        
        X <- stats::model.matrix(
          ~ G,
          data = data.frame(
            G = factor(pair, levels = levels(d$G))
          ),
          contrasts.arg = list(G = stats::contrasts(d$G))
        )
        
        L <- (X[1, ] - X[2, ])[names(lme4::fixef(fit))]
        est <- sum(L * lme4::fixef(fit))
        se <- sqrt(as.numeric(t(L) %*% vcov(fit) %*% L))
        
        if (is_detection) {
          p <- if (is.finite(se) && se > 0)
            2 * stats::pnorm(-abs(est / se)) else NA_real_
          
          den_df <- NA_real_
          meth <- "Binomial GLMM, asymptotic Wald z"
          
        } else {
          ct <- tryCatch(
            lmerTest::contest(
              fit, matrix(L, nrow = 1), joint = TRUE
            ),
            error = function(e) NULL
          )
          
          if (is.null(ct)) {
            p <- NA_real_
            den_df <- NA_real_
            meth <- "FAILED: contrast"
          } else {
            p <- ct[1, "Pr(>F)"]
            den_df <- ct[1, "DenDF"]
            meth <- "LMM, Satterthwaite"
          }
        }
        
        out[[length(out) + 1L]] <- dplyr::bind_cols(
          s[1, c(
            "Assay", "Class", "Measure", "Biological_tier",
            "Feature", "Label", "Domain"
          )],
          tibble::tibble(
            Level = "image_lmm",
            Grouping = grouping,
            Contrast = paste(pair, collapse = " - "),
            N_A_cases = n_cases[a],
            N_B_cases = n_cases[b],
            N_A_fields = sum(d$G == a),
            N_B_fields = sum(d$G == b),
            
            Estimate_log1p_A_minus_B =
              if (is_detection) NA_real_ else est,
            SE_log1p =
              if (is_detection) NA_real_ else se,
            
            Log_odds_A_minus_B =
              if (is_detection) est else NA_real_,
            SE_log_odds =
              if (is_detection) se else NA_real_,
            
            DenDF = den_df,
            P = p,
            P_method = meth,
            Singular_random_effect = singular,
            Convergence = conv,
            
            Status = if (is_detection) {
              "Exploratory binomial GLMM; case random intercept"
            } else {
              "Exploratory image-level LMM; case random intercept"
            }
          )
        )
      }
    }
  }
  
  if (!length(out))
    return(invisible(NULL))
  
  res <- dplyr::bind_rows(out) %>%
    dplyr::group_by(Level, Grouping, Assay, Class, Feature) %>%
    dplyr::mutate(
      P_Holm_within_feature = p.adjust(P, "holm")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Grouping) %>%
    dplyr::mutate(
      BH_Q = bh(P),
      Family = paste(
        "Image mixed-model screen:",
        "all assays/classes/features/contrasts within grouping"
      )
    ) %>%
    dplyr::ungroup()
  
  write_table(res, "IF_feature_group_comparisons_imageLMM")
  invisible(res)
}

# Publication plotting functions. They consume results computed in THIS run.
# Plotting does not recalculate p or q values.

publication_setting <- function(name,default) {
  x <- ACTIVE_CONFIG[[name]]
  if(is.null(x)) default else x
}
# One print-size theme for every WB, IF, and MoCA figure. Text sizes are in
# points at 100% placement, all derived from BASE_FONT_SIZE.
pub_font <- function() publication_setting("BASE_FONT_SIZE",9)
pub_text_mm <- function(offset=-1) (pub_font()+offset)/ggplot2::.pt
publication_theme <- function() {
  b <- pub_font()
  ggplot2::theme_classic(base_size=b,base_family=publication_setting("FONT_FAMILY","sans")) +
    ggplot2::theme(
      panel.grid=ggplot2::element_blank(),
      axis.line=ggplot2::element_line(color="black",linewidth=.4),
      axis.ticks=ggplot2::element_line(color="black",linewidth=.35),
      axis.ticks.length=grid::unit(2,"pt"),
      axis.text=ggplot2::element_text(size=b-1,color="black"),
      axis.text.x=ggplot2::element_text(size=b-1,color="black",lineheight=.9),
      axis.title=ggplot2::element_text(size=b,color="black"),
      strip.background=ggplot2::element_blank(),
      strip.text=ggplot2::element_text(size=b,face="bold",color="black",hjust=0,
                                       margin=ggplot2::margin(b=2)),
      strip.clip="off",
      panel.spacing.x=grid::unit(10,"pt"),
      panel.spacing.y=grid::unit(10,"pt"),
      legend.position="none",
      legend.text=ggplot2::element_text(size=b-1),
      legend.key.size=grid::unit(9,"pt"),
      plot.title=ggplot2::element_text(size=b+1,face="bold"),
      plot.subtitle=ggplot2::element_text(size=b-1),
      plot.caption=ggplot2::element_text(size=b-2,hjust=0),
      plot.margin=ggplot2::margin(4,6,4,4)
    )
}
publication_p <- function(p,prefix="p") {
  ifelse(!is.finite(p),"n.e.",
         ifelse(p<.001,paste0(prefix,"<0.001"),
                paste0(prefix,"=",sprintf("%.3f",p))))
}
publication_pages <- function(features) {
  per_page <- publication_setting("PANELS_PER_PAGE",Inf)
  if(!length(features))return(list())
  if(!is.finite(per_page))return(list(features))
  split(features,ceiling(seq_along(features)/max(1L,as.integer(per_page))))
}
# Plain axis numbers: commas below 100,000, short scale (K/M) above; never 2e+06.
pub_number <- function(x) {
  out <- rep(NA_character_,length(x)); ok <- is.finite(x)
  if(!any(ok)) return(out)
  if(max(abs(x[ok]))>=1e5) {
    out[ok] <- scales::label_number(scale_cut=scales::cut_short_scale())(x[ok])
  } else {
    out[ok] <- format(x[ok],big.mark=",",scientific=FALSE,trim=TRUE,drop0trailing=TRUE)
  }
  out
}

# Panel tags (CONFIG$PANEL_TAG_STYLE): "number" -> 1,2,3...; "letter" -> A,B,C...
panel_tags <- function(n) {
  if(identical(publication_setting("PANEL_TAG_STYLE","number"),"letter"))
    LETTERS[seq_len(n)] else as.character(seq_len(n))
}

# Add a separate x-axis title under every panel (used by the MoCA figure).
add_panel_x_titles <- function(g, layout, titles) {
  cells <- g$layout[grepl("^panel", g$layout$name), ]
  panel_rows <- sort(unique(cells$t))
  panel_cols <- sort(unique(cells$l))
  grobs <- lapply(titles, function(label) grid::textGrob(
    paste(strwrap(label, width = 26), collapse = "\n"),
    gp = grid::gpar(fontsize = pub_font(), lineheight = .9,
                    fontfamily = publication_setting("FONT_FAMILY", "sans"), col = "black")))
  for(row in rev(sort(unique(layout$ROW)))) {
    index <- which(layout$ROW == row)
    position <- panel_rows[row] + 1L   # directly below the panel x-axis
    height <- max(do.call(grid::unit.c, lapply(grobs[index], grid::grobHeight))) +
      grid::unit(2, "mm")
    g <- gtable::gtable_add_rows(g, height, pos = position)
    for(i in index)
      g <- gtable::gtable_add_grob(g, grobs[[i]], t = position + 1L,
                                   l = panel_cols[layout$COL[i]], clip = "off",
                                   name = paste0("facet-x-title-", i))
  }
  g
}

add_panel_y_titles <- function(p, titles) {
  # Remove the shared title; add a separate title beside every panel.
  p <- p + ggplot2::labs(y = NULL)
  layout <- ggplot2::ggplot_build(p)$layout$layout
  g <- ggplot2::ggplotGrob(p)
  cells <- g$layout[grepl("^panel", g$layout$name), ]
  panel_rows <- sort(unique(cells$t))
  panel_cols <- sort(unique(cells$l))
  text <- if(length(titles) == 1L) {
    rep(unname(titles), nrow(layout))
  } else {
    unname(titles[as.character(layout$Panel_label)])
  }
  if(anyNA(text)) stop("Missing y-axis title for a panel.")
  title_grobs <- lapply(text, function(label) {
    grid::textGrob(
      paste(strwrap(label, width = 24), collapse = "\n"),
      rot = 90,
      gp = grid::gpar(fontsize = pub_font(), lineheight = .9,
                      fontfamily = publication_setting("FONT_FAMILY", "sans"),
                      col = "black")
    )
  })
  for(column in rev(sort(unique(layout$COL)))) {
    index <- which(layout$COL == column)
    position <- panel_cols[column] - 2L
    title_width <- max(do.call(grid::unit.c, lapply(title_grobs[index], grid::grobWidth))) +
      grid::unit(1.5, "mm")
    g <- gtable::gtable_add_cols(g, title_width, pos = position)
    for(i in index) {
      g <- gtable::gtable_add_grob(g, title_grobs[[i]], t = panel_rows[layout$ROW[i]],
                                   l = position + 1L, clip = "off",
                                   name = paste0("facet-y-title-", i))
    }
  }
  g
}
# Fix every panel to PANEL_W_IN x PANEL_H_IN and return the exact figure size.
fix_panel_size <- function(g) {
  cells <- g$layout[grepl("^panel", g$layout$name), ]
  g$widths[unique(cells$l)] <- grid::unit(publication_setting("PANEL_W_IN",1.9),"in")
  g$heights[unique(cells$t)] <- grid::unit(publication_setting("PANEL_H_IN",1.7),"in")
  list(grob=g,
       width=grid::convertWidth(sum(g$widths),"in",valueOnly=TRUE),
       height=grid::convertHeight(sum(g$heights),"in",valueOnly=TRUE))
}

# ---------------------------------------------------------------------------
# Shared box-and-point figure used by WB and IF (case and image level).
#   d:      Key, G (factor, levels lv), Value, Case
#   panels: Key, Strip, Ytitle (row order = panel order)
#   ann:    Key, Contrast ("A - B"), P (already adjusted as intended)
# Brackets: adjacent contrasts share the lower tier; Control - PSP+ spans the
# upper tier. Points are jittered narrowly so every case remains visible.
# ---------------------------------------------------------------------------
box_figure <- function(d, panels, ann, lv, pal, field_level=FALSE,
                       p_prefix="p", show_strip=TRUE, title=NULL) {
  d <- d %>% dplyr::filter(is.finite(Value), Key %in% panels$Key)
  panels <- panels %>% dplyr::filter(Key %in% unique(d$Key))
  if(!nrow(panels)) return(NULL)
  strip_levels <- panels$Strip
  d$Panel_label <- factor(panels$Strip[match(d$Key,panels$Key)],levels=strip_levels)
  ranges <- d %>% dplyr::group_by(Key,Panel_label) %>%
    dplyr::summarise(Hi=max(Value),Lo=min(Value),.groups="drop") %>%
    dplyr::mutate(Span=ifelse(Hi>Lo,Hi-Lo,pmax(.25,abs(Hi)*.18)))
  ns <- d %>% dplyr::group_by(Key,Panel_label,G) %>%
    dplyr::summarise(N_case=dplyr::n_distinct(Case),N_obs=dplyr::n(),.groups="drop") %>%
    tidyr::complete(tidyr::nesting(Key,Panel_label),G=factor(lv,levels=lv),
                    fill=list(N_case=0L,N_obs=0L)) %>%
    dplyr::left_join(ranges,by=c("Key","Panel_label")) %>%
    dplyr::mutate(N_text=if(field_level) paste0("n=",N_case,"\n(",N_obs,")") else
      paste0("n=",N_case)) %>%
    dplyr::arrange(G) %>%
    dplyr::mutate(G_axis=paste(as.character(G),N_text,sep="\n"))
  ns$G_axis <- factor(ns$G_axis,levels=unique(ns$G_axis))
  d <- d %>% dplyr::left_join(ns %>% dplyr::select(Key,G,G_axis),by=c("Key","G"))
  pairs <- if(length(lv)==3) c("Control - PSP","PSP - PSP+","Control - PSP+") else
    paste(lv,collapse=" - ")
  # One tier per contrast so that labels never overlap horizontally.
  geo <- if(length(lv)==3)
    tibble::tibble(Contrast=pairs,Tier=c(1,2,3),X_left=c(1.04,2.04,1),X_right=c(1.96,2.96,3)) else
    tibble::tibble(Contrast=pairs,Tier=1,X_left=1,X_right=2)
  tier_y <- c(.09,.29,.49)
  ann <- ann %>% dplyr::filter(Key %in% panels$Key) %>%
    dplyr::inner_join(geo,by="Contrast") %>%
    dplyr::left_join(ranges,by="Key") %>%
    dplyr::mutate(
      P_text=publication_p(P,p_prefix),
      Y=Hi+Span*tier_y[Tier],
      Feet=Y-.035*Span,
      Text_y=Y+.03*Span
    )
  top <- ranges %>% dplyr::mutate(Y=Hi+Span*(if(nrow(ann)) tier_y[max(ann$Tier)]+.15 else .05))
  case_size <- publication_setting("CASE_POINT_SIZE",1.9)
  p <- ggplot2::ggplot(d,ggplot2::aes(G_axis,Value,fill=G)) +
    ggplot2::geom_boxplot(outlier.shape=NA,alpha=.45,width=.55,color="gray15",
                          linewidth=.35,fatten=1.6)
  if(field_level) {
    means <- d %>% dplyr::group_by(Case,G,G_axis,Panel_label) %>%
      dplyr::summarise(Value=mean(Value),.groups="drop")
    p <- p +
      ggplot2::geom_point(position=ggplot2::position_jitter(width=.16,height=0,seed=42),
                          shape=21,color="gray25",stroke=0,
                          size=publication_setting("FIELD_POINT_SIZE",.55),alpha=.4) +
      ggplot2::geom_point(data=means,shape=23,size=case_size,color="black",stroke=.35,
                          position=ggplot2::position_jitter(width=.08,height=0,seed=7))
  } else {
    p <- p + ggplot2::geom_point(
      position=ggplot2::position_jitter(width=.13,height=0,seed=42),
      shape=21,color="black",stroke=.3,size=case_size,alpha=.9)
  }
  p <- p +
    ggplot2::geom_blank(data=ns,ggplot2::aes(G_axis,Lo),inherit.aes=FALSE) +
    ggplot2::geom_blank(data=top,ggplot2::aes(y=Y),inherit.aes=FALSE) +
    ggplot2::geom_segment(data=ann,ggplot2::aes(x=X_left,xend=X_right,y=Y,yend=Y),
                          inherit.aes=FALSE,linewidth=.35,color="black") +
    ggplot2::geom_segment(data=ann,ggplot2::aes(x=X_left,xend=X_left,y=Y,yend=Feet),
                          inherit.aes=FALSE,linewidth=.35,color="black") +
    ggplot2::geom_segment(data=ann,ggplot2::aes(x=X_right,xend=X_right,y=Y,yend=Feet),
                          inherit.aes=FALSE,linewidth=.35,color="black") +
    ggplot2::geom_text(data=ann,ggplot2::aes(x=(X_left+X_right)/2,y=Text_y,label=P_text),
                       inherit.aes=FALSE,vjust=0,size=pub_text_mm(-1.5),color="black",
                       family=publication_setting("FONT_FAMILY","sans")) +
    ggplot2::facet_wrap(~Panel_label,scales="free",
                        ncol=if(nrow(panels)>1) publication_setting("PANEL_NCOL",4L) else 1L,
                        drop=FALSE) +
    ggplot2::scale_fill_manual(values=pal,drop=FALSE) +
    ggplot2::scale_x_discrete(drop=TRUE) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(.05,.02)),labels=pub_number) +
    ggplot2::labs(x=NULL,y=NULL,fill=NULL,title=title) +
    publication_theme()
  if(!show_strip) p <- p + ggplot2::theme(strip.text=ggplot2::element_blank())
  g <- add_panel_y_titles(p,setNames(panels$Ytitle,panels$Strip))
  fix_panel_size(g)
}

plot_features <- function(ft,stats) {
  if(!isTRUE(ACTIVE_CONFIG$MAKE_PLOTS))return(invisible(NULL))
  keep_classes <- publication_setting("PLOT_IF_CLASSES",c("Trusted AT8","Trusted TRIM46"))
  sig_only <- isTRUE(publication_setting("PLOT_SIGNIFICANT_ONLY",TRUE))
  alpha <- publication_setting("PLOT_ALPHA",.05)
  selection <- list()
  for(level in intersect(c("case","image"),publication_setting("PLOT_IF_LEVELS",c("case","image")))) {
    source <- if(level=="case")ft$case else ft$image
    source <- source %>% dplyr::filter(Class %in% keep_classes)
    test_level <- if(level=="case")"case" else "image_lmm"
    for(measure in unique(source$Measure)) {
      for(grouping in c("3group","2group")) {
        lv <- if(grouping=="3group") GRP3 else GRP2
        pal <- if(grouping=="3group") COLORS3 else COLORS2
        # Field-level Detection is 0/100 per field; it is shown at case level only.
        dat <- source %>% dplyr::filter(Measure==measure,is.finite(Value),
                                        !(level=="image" & Feature=="Detection")) %>%
          dplyr::mutate(G=factor(if(grouping=="3group")as.character(Diagnosis)else
            ifelse(Diagnosis=="Control","Control","PSP spectrum"),levels=lv))
        if(!nrow(dat))next
        ann <- stats %>% dplyr::filter(Measure==measure,Grouping==grouping,Level==test_level) %>%
          dplyr::transmute(Key=Feature,Contrast,P=P_Holm_within_feature)
        features <- FEATURES_V6$Feature[FEATURES_V6$Feature%in%unique(dat$Feature)]
        sig <- ann %>% dplyr::group_by(Key) %>%
          dplyr::summarise(Min_p=suppressWarnings(min(P[is.finite(P)])),.groups="drop")
        min_p <- sig$Min_p[match(features,sig$Key)]
        plotted <- if(sig_only) features[is.finite(min_p) & min_p<alpha] else features
        selection[[length(selection)+1L]] <- tibble::tibble(
          Level=level,Measure=measure,Grouping=grouping,Feature=features,
          Min_bracket_p=min_p,Plotted=features%in%plotted,
          Rule=if(sig_only) paste0("Plotted if any bracket p < ",alpha) else "All features")
        if(!length(plotted))next
        panels <- tibble::tibble(
          Key=plotted,Strip=panel_tags(length(plotted)),
          Ytitle=FEATURES_V6$Label[match(plotted,FEATURES_V6$Feature)])
        fig <- box_figure(
          dat %>% dplyr::transmute(Key=Feature,G,Value,Case=as.character(Case)),
          panels,ann,lv,pal,field_level=level=="image",
          p_prefix=if(grouping=="3group")"adj. p" else "p",
          title=if(isTRUE(publication_setting("SHOW_PLOT_TEXT",FALSE)))
            paste0(measure," | ",level," level") else NULL)
        if(is.null(fig))next
        save_plot(fig$grob,paste0("IF_",level),
                  gsub("[^A-Za-z0-9_]","_",paste(sub("^[^:]+: ","",measure),level,grouping,sep="_")),
                  width=fig$width,height=fig$height)
      }
    }
  }
  if(length(selection)) write_table(dplyr::bind_rows(selection),"Figure_panel_selection",qc=TRUE)
  invisible(NULL)
}

plot_cognition_panels <- function(joined,results) {
  if(!isTRUE(ACTIVE_CONFIG$MAKE_PLOTS))return(invisible(NULL))
  keep <- publication_setting("PLOT_COG_CLASSES",c("Diffuse AT8"))
  panel_data <- joined %>% dplyr::filter(Source=="IF",Class%in%keep,
                                         is.finite(Value),is.finite(Last_MOCA)) %>%
    dplyr::left_join(results %>% dplyr::select(Measure,N_cases,Spearman_rho,P,BH_Q),
                     by="Measure") %>%
    dplyr::filter(is.finite(P)) %>%
    dplyr::left_join(FEATURES_V6 %>% dplyr::select(Feature,Label),by="Feature") %>%
    dplyr::mutate(Diagnosis=factor(Diagnosis,levels=GRP3))
  for(cls in unique(panel_data$Class)) {
    ds <- panel_data %>% dplyr::filter(Class==cls)
    ordered <- FEATURES_V6$Feature[FEATURES_V6$Feature%in%ds$Feature]
    measures <- ds$Measure[match(ordered,ds$Feature)]
    # Strip shows only the panel number; the feature name is the x-axis title.
    labels <- setNames(panel_tags(length(measures)),measures)
    x_titles <- setNames(ds$Label[match(measures,ds$Measure)],labels)
    d <- ds %>% dplyr::mutate(Panel_label=factor(unname(labels[Measure]),levels=unname(labels)))
    ann <- d %>% dplyr::distinct(Panel_label,N_cases,Spearman_rho,P,BH_Q) %>%
      dplyr::mutate(Text=paste0("  \u03c1=",sprintf("%.2f",Spearman_rho),"; n=",N_cases,"\n  ",
                                publication_p(P),"; ",publication_p(BH_Q,"q")))
    p <- ggplot2::ggplot(d,ggplot2::aes(Value,Last_MOCA,fill=Diagnosis)) +
      ggplot2::geom_point(shape=21,color="black",stroke=.3,
                          size=publication_setting("CASE_POINT_SIZE",1.9)+.2,alpha=.9)
    if(isTRUE(publication_setting("SHOW_CASE_LABELS",FALSE)))
      p <- p+ggplot2::geom_text(ggplot2::aes(label=Case),color="gray20",vjust=-.9,
                                size=pub_text_mm(-2))
    ncol <- publication_setting("PANEL_NCOL",4L)
    p <- p +
      ggplot2::geom_text(data=ann,ggplot2::aes(x=-Inf,y=Inf,label=Text),inherit.aes=FALSE,
                         hjust=0,vjust=1.15,size=pub_text_mm(-2),lineheight=.95,color="black",
                         family=publication_setting("FONT_FAMILY","sans")) +
      ggplot2::facet_wrap(~Panel_label,scales="free",ncol=ncol) +
      ggplot2::scale_fill_manual(values=COLORS3,drop=TRUE) +
      ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=.08),labels=pub_number,
                                  breaks=scales::breaks_extended(n=4)) +
      ggplot2::scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10),
                                  expand=ggplot2::expansion(mult=c(.04,.42))) +
      ggplot2::labs(x=NULL,y=NULL,fill=NULL,
                    title=if(isTRUE(publication_setting("SHOW_PLOT_TEXT",FALSE)))
                      paste0(cls," vs Last MoCA (PSP spectrum)") else NULL) +
      publication_theme() +
      ggplot2::theme(legend.position=if(dplyr::n_distinct(d$Diagnosis)>1)"bottom" else "none",
                     strip.text=ggplot2::element_text(size=pub_font()+1,face="bold",hjust=0))
    # Same panel size and fonts as the IF/WB box plots; x and y titles on every panel.
    layout <- ggplot2::ggplot_build(p)$layout$layout
    g <- add_panel_y_titles(p,"Last MoCA score")
    g <- add_panel_x_titles(g,layout,unname(x_titles[as.character(layout$Panel_label)]))
    fig <- fix_panel_size(g)
    save_plot(fig$grob,"cognition",
              paste0("MoCA_vs_",gsub("[^A-Za-z0-9_]","_",cls)),
              width=fig$width,height=fig$height)
  }
  invisible(NULL)
}


run_feature_cognition <- function(ft,wb_data,config) {
  path <- path.expand(config$COG_FILE)
  if(!file.exists(path)) {
    write_table(tibble::tibble(Status="SKIPPED: cognition file absent"),"Cognition_status",qc=TRUE)
    return(invisible(NULL))
  }
  raw <- readr::read_csv(path,show_col_types=FALSE,na=c("","NA"))
  if(!all(c("Code","Last MOCA Score")%in%names(raw)))stop("Cognition columns missing.")
  raw <- raw %>% dplyr::mutate(Case=case_id(Code))
  conflicts <- raw %>% dplyr::group_by(Case) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),~dplyr::n_distinct(.x[!is.na(.x)])),
                     .groups="drop") %>%
    tidyr::pivot_longer(-Case,names_to="Variable",values_to="Distinct_values") %>%
    dplyr::filter(Distinct_values>1)
  write_table(conflicts,"Cognition_duplicate_conflicts",qc=TRUE)
  if(nrow(conflicts))stop("Resolve conflicting duplicate cognition records.")
  cog <- raw %>% dplyr::group_by(Case) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),first_present),.groups="drop") %>%
    dplyr::mutate(Diagnosis=factor(diagnosis(Case),levels=GRP3),
                  Last_MOCA=suppressWarnings(as.numeric(`Last MOCA Score`)))
  if(any(!is.na(cog$`Last MOCA Score`)&is.na(cog$Last_MOCA)))
    stop("Nonnumeric Last MOCA Score; resolve the original cognition file.")
  if(any(cog$Last_MOCA<0|cog$Last_MOCA>30,na.rm=TRUE))stop("MoCA outside 0-30.")
  ifvars <- ft$case %>% dplyr::transmute(
    Case,Measure=paste("IF",Assay,Class,Feature,sep=" | "),Assay,
    Source="IF",Class,Feature,Value)
  wbvars <- if(!is.null(wb_data)&&nrow(wb_data)) {
    wb_data %>% dplyr::transmute(Case=case_id(Sample),Measure=paste0("WB | ",Marker),
                                Assay="WB",Source="WB",Class=Marker,Feature="Protein",Value) %>%
      dplyr::group_by(Case,Measure,Assay,Source,Class,Feature) %>%
      dplyr::summarise(Value=mean(Value),.groups="drop")
  } else tibble::tibble()
  sqcols <- names(cog)[grepl("^Semiquantitative",names(cog))]
  sqvars <- if(length(sqcols)) cog %>% dplyr::select(Case,dplyr::all_of(sqcols)) %>%
    tidyr::pivot_longer(-Case,names_to="Class",values_to="Value") %>%
    dplyr::mutate(Measure=paste0("Semiquant | ",Class),Assay="Semiquant",Source="Semiquant",
                  Feature="Score",Value=suppressWarnings(as.numeric(Value))) else tibble::tibble()
  paths <- dplyr::bind_rows(ifvars,wbvars,sqvars)
  write_table(paths,"All_case_features_for_MoCA")
  write_table(cog %>% dplyr::select(dplyr::any_of(c(
    "Case","Diagnosis","Last_MOCA","Age","Sex","PMI","YOD","Last MOCA Date"))),
    "Cognition_case_availability",qc=TRUE)
  joined <- paths %>% dplyr::inner_join(
    cog %>% dplyr::select(Case,Diagnosis,Last_MOCA),by="Case") %>%
    dplyr::filter(Diagnosis%in%c("PSP","PSP+"))
  results <- loco <- list()
  for(m in unique(paths$Measure)) {
    d <- joined %>% dplyr::filter(Measure==m,is.finite(Value),is.finite(Last_MOCA))
    meta <- paths[match(m,paths$Measure),c("Measure","Assay","Source","Class","Feature")]
    valid <- nrow(d)>=4 && varies(d$Value)&&varies(d$Last_MOCA)
    ct <- if(valid)rank_test(d$Value,d$Last_MOCA)else NULL
    adj <- adj_r <- NA_real_
    if(valid && nrow(d)>=6 && dplyr::n_distinct(d$Diagnosis)==2) {
      d$Rx <- rank(d$Value);d$Ry <- rank(d$Last_MOCA);d$G <- droplevels(d$Diagnosis)
      fit <- lm(Ry~Rx+G,d)
      if(!anyNA(coef(fit))&&df.residual(fit)>=3) {
        adj <- summary(fit)$coefficients["Rx","Pr(>|t|)"]
        adj_r <- cor(residuals(lm(Rx~G,d)),residuals(lm(Ry~G,d)))
      }
    }
    results[[length(results)+1L]] <- dplyr::bind_cols(meta,tibble::tibble(
      N_cases=nrow(d),N_PSP=sum(d$Diagnosis=="PSP"),N_PSPplus=sum(d$Diagnosis=="PSP+"),
      Spearman_rho=if(valid)ct$estimate else NA_real_,
      P=if(valid)ct$p.value else NA_real_,P_method=if(valid)ct$method else NA_character_,
      Rank_partial_rho_diagnosis_adjusted=adj_r,P_diagnosis_adjusted=adj,
      Status=if(valid)"Preliminary/exploratory"else"SKIPPED: n<4/no variance/no overlap"))
    if(valid) {
      if(nrow(d)>=5)for(cc in d$Case) {
        s <- d[d$Case!=cc,]
        rho <- if(varies(s$Value)&&varies(s$Last_MOCA))cor(s$Value,s$Last_MOCA,method="spearman")else NA_real_
        loco[[length(loco)+1L]] <- tibble::tibble(Measure=m,Omitted_case=cc,Rho=rho,N_cases=nrow(s))
      }
    }
  }
  results <- dplyr::bind_rows(results) %>% dplyr::mutate(
    BH_Q=bh(P),BH_Q_diagnosis_adjusted=bh(P_diagnosis_adjusted),
    Family="All available IF features + WB + semiquant scores; case-level Last MoCA; PSP spectrum")
  plot_cognition_panels(joined,results)
  write_table(results,"Last_MoCA_vs_all_features")
  write_table(dplyr::bind_rows(loco),"MoCA_leave_one_case_out",qc=TRUE)
  invisible(results)
}

run_retained_WB <- function() {
# PART 12: Western Blot Analysis — WELCH'S ANOVA + BRACKET P-VALUES
# ============================================================================

library(dplyr)
library(ggplot2)

colors_wb_3g <- COLORS3
colors_wb_2g <- COLORS2

# Y-axis label per WB marker. 
wb_y_label <- function(marker) {
  if (identical(marker, "TAU_RATIO")) "AT8/TAU5 ratio" else
    paste("Normalized", marker)
}

# Load WB data — prefers PSP_Protein_Level_Data.csv (wide format: one row per
# sample, proteins as columns, all proteins probed in every sample).
# Group mapping: NPC -> Control, PO -> PSP, POD -> PSP+
# Marker mapping: Tau5 -> TAU5, Tau(pS202/pT205) -> AT8, Synaptophysin -> SYNAPTOPHYSIN,
#               TRIM46 unchanged
# Only the explicitly configured original WB file is read.
wb_data <- tryCatch({
  wb_wide <- read.csv(WB_FILE,
                      fileEncoding = "UTF-8-BOM", check.names = FALSE)

  # First column holds sample IDs (NPC1, PO5, POD14, ...)
  names(wb_wide)[1] <- "Sample"

  wb_long <- wb_wide %>%
    pivot_longer(cols = -Sample, names_to = "Marker", values_to = "Value") %>%
    mutate(
      # Derive group from sample-ID prefix (POD must be checked before PO)
      Group = case_when(
        grepl("^NPC", Sample) ~ "Control",
        grepl("^POD", Sample) ~ "PSP+",
        grepl("^PO",  Sample) ~ "PSP",
        TRUE ~ NA_character_
      ),
      # Normalize marker names to the analysis convention
      Marker = case_when(
        Marker == "Tau5"                     ~ "TAU5",
        Marker == "Tau(pS202/pT205)"         ~ "AT8",
        Marker == "Synaptophysin"            ~ "SYNAPTOPHYSIN",
        TRUE ~ Marker  # TRIM46 unchanged
      ),
      Value = as.numeric(Value)
    ) %>%
    filter(!is.na(Group), !is.na(Value)) %>%
    dplyr::select(Sample, Group, Marker, Value)

  cat("Loaded PSP_Protein_Level_Data.csv:", nrow(wb_long), "sample-by-marker rows\n")
  cat("Samples per group:\n")
  print(table(wb_long$Group[!duplicated(wb_long$Sample)]))
  cat("Markers:", paste(sort(unique(wb_long$Marker)), collapse = ", "), "\n")
  wb_long
}, error = function(e) {
  stop("Could not read configured original WB input: ",WB_FILE,
       "\n",conditionMessage(e),call.=FALSE)
})

if (!is.null(wb_data)) {
  # --- Normalize group labels ---
  wb_data <- wb_data %>%
    mutate(
      Group = trimws(as.character(Group)),
      Group = case_when(
        Group %in% c("Control", "CTRL", "Ctrl") ~ "Control",
        Group %in% c("PSP") ~ "PSP",
        Group %in% c("PSP+", "PSP +", "PSPplus", "PSP Plus") ~ "PSP+",
        TRUE ~ Group
      ),
      Group = factor(Group, levels = c("Control", "PSP", "PSP+")),
      Value = as.numeric(Value)
    )

  if (any(is.na(wb_data$Group))) {
    cat("WARNING: Unrecognized WB sample prefixes in configured original input.\n")
    cat("Unique Group values:", paste(unique(as.character(wb_data$Group)), collapse = ", "), "\n")
  }

  # --- WB QC summary table ---
  wb_qc <- wb_data %>%
    filter(!is.na(Group), !is.na(Value)) %>%
    group_by(Marker, Group) %>%
    summarise(
      N = n(),
      Mean = round(mean(Value, na.rm = TRUE), 4),
      SD = round(sd(Value, na.rm = TRUE), 4),
      Median = round(median(Value, na.rm = TRUE), 4),
      Min = round(min(Value, na.rm = TRUE), 4),
      Max = round(max(Value, na.rm = TRUE), 4),
      .groups = "drop"
    )
  write_csv(wb_qc, file.path(OUT, "tables", "WB_Group_Summary_QC.csv"))
  cat("\nSaved: WB_Group_Summary_QC.csv\n")
  print(wb_qc)

  # --- WB results tables ---
  wb_anova_table <- NULL
  wb_pairwise_table <- NULL

  wb_markers <- unique(wb_data$Marker)

  cat("\nProcessing Part 12: Running Western Blot Welch's ANOVA Analyses...\n")

  for (marker_name in wb_markers) {

    df_marker_raw <- wb_data %>%
      filter(Marker == marker_name) %>%
      filter(Group %in% c("Control", "PSP", "PSP+")) %>%
      filter(is.finite(Value) & Value >= 0) %>%
      mutate(Group = factor(Group, levels = c("Control", "PSP", "PSP+")))

    if (length(unique(df_marker_raw$Group)) == 3 && nrow(df_marker_raw) >= 5) {

      cat(paste0("\n========================================================\n",
                 "WELCH'S ANOVA STATISTICAL OUTPUT FOR MARKER: ", marker_name,
                 "\n========================================================\n"))

      # --- WELCH'S ANOVA ---
      welch_res <- oneway.test(Value ~ Group, data = df_marker_raw, var.equal = FALSE)
      welch_p   <- welch_res$p.value

      cat("--- WELCH'S ONE-WAY ANOVA (unequal variances) ---\n")
      print(welch_res)

      wb_anova_table <- bind_rows(wb_anova_table, data.frame(
        Marker = marker_name,
        Statistic = welch_res$statistic,
        df1 = welch_res$parameter[1],
        df2 = round(welch_res$parameter[2], 2),
        PValue = welch_p,
        PValue_Formatted = format_pval(welch_p),
        stringsAsFactors = FALSE
      ))

      # --- EXPLICIT PAIRWISE WELCH T-TESTS + Holm correction ---
      # More robust than pairwise.t.test matrix extraction
      wb_pairs <- list(
        c("Control", "PSP"),
        c("Control", "PSP+"),
        c("PSP", "PSP+")
      )

      pairwise_results <- data.frame(
        Marker = character(),
        Comparison = character(),
        Group1 = character(),
        Group2 = character(),
        N1 = integer(),
        N2 = integer(),
        Mean1 = numeric(),
        Mean2 = numeric(),
        t_statistic = numeric(),
        df = numeric(),
        PValue_Raw = numeric(),
        PValue_Holm = numeric(),
        PValue_Formatted = character(),
        stringsAsFactors = FALSE
      )

      raw_pvals <- numeric(3)
      for (i in seq_along(wb_pairs)) {
        pair <- wb_pairs[[i]]
        x <- df_marker_raw %>% filter(Group == pair[1]) %>% pull(Value)
        y <- df_marker_raw %>% filter(Group == pair[2]) %>% pull(Value)
        x <- x[!is.na(x)]
        y <- y[!is.na(y)]

        if (length(x) >= 2 && length(y) >= 2) {
          tt <- t.test(x, y, var.equal = FALSE)
          raw_pvals[i] <- tt$p.value

          pairwise_results <- bind_rows(pairwise_results, data.frame(
            Marker = marker_name,
            Comparison = paste(pair[1], "vs", pair[2]),
            Group1 = pair[1], Group2 = pair[2],
            N1 = length(x), N2 = length(y),
            Mean1 = round(mean(x), 4), Mean2 = round(mean(y), 4),
            t_statistic = round(tt$statistic, 3),
            df = round(tt$parameter, 2),
            PValue_Raw = tt$p.value,
            PValue_Holm = NA_real_,  # filled after Holm correction
            PValue_Formatted = NA_character_,
            stringsAsFactors = FALSE
          ))
        } else {
          raw_pvals[i] <- NA
          pairwise_results <- bind_rows(pairwise_results, data.frame(
            Marker = marker_name,
            Comparison = paste(pair[1], "vs", pair[2]),
            Group1 = pair[1], Group2 = pair[2],
            N1 = length(x), N2 = length(y),
            Mean1 = NA, Mean2 = NA,
            t_statistic = NA, df = NA,
            PValue_Raw = NA,
            PValue_Holm = NA,
            PValue_Formatted = "NA",
            stringsAsFactors = FALSE
          ))
        }
      }

      # Apply Holm correction
      adj_pvals <- p.adjust(raw_pvals, method = "holm")
      pairwise_results$PValue_Holm <- adj_pvals
      pairwise_results$PValue_Formatted <- sapply(adj_pvals, format_pval)

      # Extract for brackets
      p_ctrl_vs_psp      <- adj_pvals[1]
      p_ctrl_vs_psp_plus <- adj_pvals[2]
      p_psp_vs_psp_plus  <- adj_pvals[3]

      cat("\n--- PAIRWISE WELCH T-TESTS (Holm-Adjusted) ---\n")
      print(pairwise_results)

      wb_pairwise_table <- bind_rows(wb_pairwise_table, pairwise_results)

      # --- TWO-GROUP WELCH T-TEST (Control vs Pooled PSP) ---
      df_marker_2g <- df_marker_raw %>%
        mutate(Analysis_Group = case_when(
          Group == "Control" ~ "Control",
          Group %in% c("PSP", "PSP+") ~ "PSP spectrum",
          TRUE ~ NA_character_
        ))

      t_test_res <- t.test(Value ~ Analysis_Group, data = df_marker_2g)

      cat("\n--- WELCH TWO-SAMPLE T-TEST (Control vs Pooled PSP) ---\n")
      print(t_test_res)

      t_stat <- t_test_res$statistic
      t_df   <- t_test_res$parameter
      t_p    <- t_test_res$p.value

      # --- PLOTS: same print-size style as the IF figures ---------------
      # 3-group brackets: Holm-adjusted pairwise Welch t-tests (p_adj).
      # 2-group bracket: Welch two-sample t-test (single test, p).
      wb_panel <- tibble::tibble(Key=marker_name,Strip=marker_name,
                                 Ytitle=wb_y_label(marker_name))
      wb_d3 <- df_marker_raw %>% dplyr::transmute(
        Key=marker_name,G=factor(as.character(Group),levels=GRP3),Value,Case=Sample)
      wb_ann3 <- tibble::tibble(
        Key=marker_name,
        Contrast=c("Control - PSP","Control - PSP+","PSP - PSP+"),
        P=c(p_ctrl_vs_psp,p_ctrl_vs_psp_plus,p_psp_vs_psp_plus))
      fig3 <- box_figure(wb_d3,wb_panel,wb_ann3,GRP3,colors_wb_3g,
                         p_prefix="adj. p",show_strip=FALSE,
                         title=if(isTRUE(publication_setting("SHOW_PLOT_TEXT",FALSE)))
                           paste0(marker_name,"; Welch ANOVA p = ",format_pval(welch_p)) else NULL)
      save_plot(fig3$grob,"WB",paste0("WB_3Group_Boxplot_",marker_name),
                width=fig3$width,height=fig3$height)
      wb_d2 <- df_marker_2g %>% dplyr::transmute(
        Key=marker_name,G=factor(Analysis_Group,levels=GRP2),Value,Case=Sample)
      wb_ann2 <- tibble::tibble(Key=marker_name,Contrast="Control - PSP spectrum",P=t_p)
      fig2 <- box_figure(wb_d2,wb_panel,wb_ann2,GRP2,colors_wb_2g,
                         p_prefix="p",show_strip=FALSE,
                         title=if(isTRUE(publication_setting("SHOW_PLOT_TEXT",FALSE)))
                           paste0(marker_name,"; Welch t(",round(t_df,1),") = ",round(t_stat,2)) else NULL)
      save_plot(fig2$grob,"WB",paste0("WB_2Group_Boxplot_",marker_name),
                width=fig2$width,height=fig2$height)

      cat(paste0("Saved Plots: WB_3Group_Boxplot_", marker_name, ".png & WB_2Group_Boxplot_", marker_name, ".png\n"))
    }
  }

  # --- Save WB results tables ---
  if (!is.null(wb_anova_table)) {
    write_csv(wb_anova_table, file.path(OUT, "tables", "WB_Welch_ANOVA_Results.csv"))
    cat("\nSaved: WB_Welch_ANOVA_Results.csv\n")
  }
  if (!is.null(wb_pairwise_table)) {
    write_csv(wb_pairwise_table, file.path(OUT, "tables", "WB_Pairwise_PostHoc_Results.csv"))
    cat("Saved: WB_Pairwise_PostHoc_Results.csv\n")
  }
}

# ============================================================================

  wb_data
}

# Sensitivity benchmark: standardized mean difference required
# under normal, equal-variance populations. No new package required.
mde_d <- function(n1, n2, target=.80, alpha=.05) {

  if(length(n1)!=1L || length(n2)!=1L)
    return(NA_real_)

  if(!all(is.finite(c(n1,n2))) || n1<2 || n2<2)
    return(NA_real_)

  df <- n1+n2-2
  critical_t <- stats::qt(1-alpha/2, df)

  power_difference <- function(d) {
    ncp <- d/sqrt(1/n1+1/n2)

    stats::pt(-critical_t, df, ncp=ncp) +
      stats::pt(critical_t, df, ncp=ncp, lower.tail=FALSE) -
      target
  }

  stats::uniroot(
    power_difference,
    interval=c(0,50),
    tol=1e-8
  )$root
}

# ---------------------------------------------------------------------------
# WB rank-based sensitivity analysis: Kruskal-Wallis omnibus, exact
# permutation Wilcoxon rank-sum pairwise tests (Holm within marker), and the
# pooled Control vs PSP-spectrum Wilcoxon test, shown beside the Welch results.
# ---------------------------------------------------------------------------
wb_rank_sensitivity <- function(wb) {
  rows <- list()
  for(m in unique(wb$Marker)) {
    d <- wb %>% dplyr::filter(Marker==m,is.finite(Value),Value>=0) %>%
      dplyr::mutate(Group=factor(as.character(Group),levels=GRP3))
    if(dplyr::n_distinct(d$Group)<3) next
    kw <- stats::kruskal.test(Value~Group,data=d)
    wa <- stats::oneway.test(Value~Group,data=d,var.equal=FALSE)
    rows[[length(rows)+1L]] <- tibble::tibble(
      Marker=m,Grouping="3group",Test="Omnibus",Contrast="Control vs PSP vs PSP+",
      N_A=NA_integer_,N_B=NA_integer_,Rank_statistic=unname(kw$statistic),
      P_rank=kw$p.value,Rank_method="Kruskal-Wallis",Cliffs_delta=NA_real_,
      P_welch=wa$p.value,Welch_method="Welch one-way ANOVA")
    pr <- list()
    for(pair in list(c("Control","PSP"),c("Control","PSP+"),c("PSP","PSP+"))) {
      x <- d$Value[d$Group==pair[1]]; y <- d$Value[d$Group==pair[2]]
      rt <- group_rank_test(x,y,ACTIVE_CONFIG$MIN_CASES)
      wt <- if(length(x)>=2&&length(y)>=2) stats::t.test(x,y)$p.value else NA_real_
      pr[[length(pr)+1L]] <- tibble::tibble(
        Marker=m,Grouping="3group",Test="Pairwise",Contrast=paste(pair,collapse=" - "),
        N_A=length(x),N_B=length(y),Rank_statistic=NA_real_,P_rank=rt$P,
        Rank_method=rt$Method,Cliffs_delta=rt$Cliffs_delta,P_welch=wt,
        Welch_method="Welch t-test")
    }
    pr <- dplyr::bind_rows(pr) %>% dplyr::mutate(
      P_rank_Holm=p.adjust(P_rank,"holm"),P_welch_Holm=p.adjust(P_welch,"holm"))
    rows[[length(rows)+1L]] <- pr
    x <- d$Value[d$Group=="Control"]; y <- d$Value[d$Group!="Control"]
    rt <- group_rank_test(x,y,ACTIVE_CONFIG$MIN_CASES)
    rows[[length(rows)+1L]] <- tibble::tibble(
      Marker=m,Grouping="2group",Test="Pooled",Contrast="Control - PSP spectrum",
      N_A=length(x),N_B=length(y),Rank_statistic=NA_real_,P_rank=rt$P,
      Rank_method=rt$Method,Cliffs_delta=rt$Cliffs_delta,
      P_welch=stats::t.test(x,y)$p.value,Welch_method="Welch t-test")
  }
  if(!length(rows)) return(invisible(NULL))
  out <- dplyr::bind_rows(rows) %>%
    dplyr::mutate(P_rank_Holm=dplyr::coalesce(P_rank_Holm,P_rank),
                  P_welch_Holm=dplyr::coalesce(P_welch_Holm,P_welch)) %>%
    dplyr::group_by(Contrast) %>%
    dplyr::mutate(BH_Q_rank_across_markers=bh(P_rank),
                  BH_Q_welch_across_markers=bh(P_welch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Same_conclusion_at_0.05=(P_rank_Holm<.05)==(P_welch_Holm<.05))
  write_table(out,"WB_rank_sensitivity")
  invisible(out)
}

# ---------------------------------------------------------------------------
# Sample sizes needed to detect the OBSERVED effects (two-sided).
#   Group tests (IF, WB): rank-based n from Cliff's delta via Noether (1987),
#     plus a Welch/t-test n from Cohen's d (pooled SD) for WB.
#   MoCA: Spearman n via Fisher z with the Bonett-Wright (2000) variance
#     factor (1 + rho^2/2); |rho| is capped at 0.95.
#   Alpha scenarios: nominal 0.05 and Bonferroni across the analysis family
#     (a conservative stand-in for BH control).
# Observed effects from small samples are inflated (winner's curse), so the
# required n is a lower bound. Planning benchmarks are appended.
# ---------------------------------------------------------------------------
n_noether <- function(delta,alpha,power) {
  # Equal allocation; P(X>Y) = (delta+1)/2. Returns n per group.
  if(!is.finite(delta) || abs(delta)<1e-8) return(NA_real_)
  pxy <- (abs(delta)+1)/2
  z <- stats::qnorm(1-alpha/2)+stats::qnorm(power)
  max(2,ceiling(z^2/(6*(pxy-.5)^2)))
}
n_t_test <- function(d,alpha,power) {
  if(!is.finite(d) || abs(d)<1e-8) return(NA_real_)
  if(stats::power.t.test(n=2,delta=abs(d),sd=1,sig.level=alpha)$power>=power) return(2)
  n <- tryCatch(stats::power.t.test(delta=abs(d),sd=1,sig.level=alpha,power=power)$n,
                error=function(e) NA_real_)
  if(is.finite(n)) ceiling(n) else NA_real_
}
n_spearman <- function(rho,alpha,power,cap=.95) {
  if(!is.finite(rho) || abs(rho)<1e-8) return(NA_real_)
  r <- min(abs(rho),cap)
  z <- stats::qnorm(1-alpha/2)+stats::qnorm(power)
  ceiling(3+(1+r^2/2)*(z/atanh(r))^2)
}
cohen_d <- function(x,y) {
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  if(length(x)<2||length(y)<2) return(NA_real_)
  s <- sqrt(((length(x)-1)*stats::var(x)+(length(y)-1)*stats::var(y))/(length(x)+length(y)-2))
  if(!is.finite(s)||s<=0) NA_real_ else (mean(x)-mean(y))/s
}
cliff_delta <- function(x,y) {
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  if(!length(x)||!length(y)) return(NA_real_)
  mean(outer(x,y,">"))-mean(outer(x,y,"<"))
}
add_n_columns <- function(z,fun,effect_col,family_size,per_group=TRUE) {
  targets <- publication_setting("POWER_TARGETS",c(.8,.9))
  a <- publication_setting("POWER_ALPHA",.05)
  for(pw in targets) {
    tag <- sprintf("%02.0f",100*pw)
    nom <- vapply(z[[effect_col]],fun,numeric(1),alpha=a,power=pw)
    fam <- vapply(seq_len(nrow(z)),function(i)
      fun(z[[effect_col]][i],alpha=a/max(1,family_size[i]),power=pw),numeric(1))
    if(per_group) {
      z[[paste0("N_per_group_",tag,"pct_nominal")]] <- nom
      z[[paste0("N_per_group_",tag,"pct_family_Bonferroni")]] <- fam
    } else {
      z[[paste0("N_total_",tag,"pct_nominal")]] <- nom
      z[[paste0("N_total_",tag,"pct_family_Bonferroni")]] <- fam
    }
  }
  z
}
sample_size_requirements <- function(ft,wb,cog) {
  out <- list()
  # IF: case level, every assay/class/feature/contrast.
  cas <- ft$case %>% dplyr::filter(is.finite(Value))
  if_rows <- list()
  for(grouping in c("3group","2group")) {
    lv <- if(grouping=="3group") GRP3 else GRP2
    dd <- cas %>% dplyr::mutate(G=if(grouping=="3group") as.character(Diagnosis) else
      ifelse(Diagnosis=="Control","Control","PSP spectrum"))
    for(s in dplyr::group_split(dplyr::group_by(dd,Assay,Class,Feature))) {
      for(pair in combn(lv,2,simplify=FALSE)) {
        x <- s$Value[s$G==pair[1]]; y <- s$Value[s$G==pair[2]]
        if_rows[[length(if_rows)+1L]] <- tibble::tibble(
          Analysis="IF case-level group comparison",Grouping=grouping,
          Assay=s$Assay[1],Class=s$Class[1],Feature=s$Feature[1],
          Contrast=paste(pair,collapse=" - "),Current_n_A=length(x),Current_n_B=length(y),
          Cliffs_delta=cliff_delta(x,y),Cohens_d=cohen_d(x,y),
          Test_assumed="Wilcoxon rank-sum (Noether, from Cliff's delta)")
      }
    }
  }
  z <- dplyr::bind_rows(if_rows) %>% dplyr::group_by(Grouping) %>%
    dplyr::mutate(Family_size=sum(is.finite(Cliffs_delta) & Cliffs_delta!=0)) %>% dplyr::ungroup()
  out$IF <- add_n_columns(z,n_noether,"Cliffs_delta",z$Family_size)
  # WB: rank-based and Welch/t-based n.
  if(!is.null(wb) && nrow(wb)) {
    wb_rows <- list()
    for(m in unique(wb$Marker)) for(grouping in c("3group","2group")) {
      d <- wb %>% dplyr::filter(Marker==m,is.finite(Value),Value>=0) %>%
        dplyr::mutate(G=if(grouping=="3group") as.character(Group) else
          ifelse(Group=="Control","Control","PSP spectrum"))
      lv <- if(grouping=="3group") GRP3 else GRP2
      for(pair in combn(lv,2,simplify=FALSE)) {
        x <- d$Value[d$G==pair[1]]; y <- d$Value[d$G==pair[2]]
        wb_rows[[length(wb_rows)+1L]] <- tibble::tibble(
          Analysis="Western blot group comparison",Grouping=grouping,Assay="WB",Class=m,
          Feature="Protein",Contrast=paste(pair,collapse=" - "),
          Current_n_A=length(x),Current_n_B=length(y),
          Cliffs_delta=cliff_delta(x,y),Cohens_d=cohen_d(x,y))
      }
    }
    z <- dplyr::bind_rows(wb_rows) %>% dplyr::group_by(Grouping) %>%
      dplyr::mutate(Family_size=dplyr::n()) %>% dplyr::ungroup()
    zt <- add_n_columns(z %>% dplyr::mutate(Test_assumed="Welch/Student t-test (from Cohen's d)"),
                        n_t_test,"Cohens_d",z$Family_size)
    zr <- add_n_columns(z %>% dplyr::mutate(Test_assumed="Wilcoxon rank-sum (Noether, from Cliff's delta)"),
                        n_noether,"Cliffs_delta",z$Family_size)
    out$WB <- dplyr::bind_rows(zt,zr)
  }
  # MoCA: Spearman, PSP spectrum.
  if(!is.null(cog) && nrow(cog)) {
    z <- cog %>% dplyr::filter(is.finite(Spearman_rho)) %>%
      dplyr::transmute(Analysis="Last MoCA association (PSP spectrum)",Grouping="PSP spectrum",
                       Assay,Class,Feature,Contrast="Spearman rho",
                       Current_n_A=N_cases,Current_n_B=NA_integer_,
                       Spearman_rho,Test_assumed="Spearman (Fisher z, Bonett-Wright)")
    z$Family_size <- nrow(z)
    out$MoCA <- add_n_columns(z,n_spearman,"Spearman_rho",z$Family_size,per_group=FALSE)
  }
  # Planning benchmarks independent of the observed data.
  bench_g <- tibble::tibble(Analysis="Planning benchmark: two-group comparison",
                            Grouping="Any",Contrast="Two groups",
                            Cliffs_delta=c(.3,.5,.7,.9),Test_assumed="Wilcoxon rank-sum (Noether)",
                            Family_size=1)
  bench_r <- tibble::tibble(Analysis="Planning benchmark: correlation",Grouping="Any",
                            Contrast="Spearman rho",Spearman_rho=c(.3,.5,.7,.9),
                            Test_assumed="Spearman (Fisher z, Bonett-Wright)",Family_size=1)
  out$Bench_g <- add_n_columns(bench_g,n_noether,"Cliffs_delta",bench_g$Family_size)
  out$Bench_r <- add_n_columns(bench_r,n_spearman,"Spearman_rho",bench_r$Family_size,per_group=FALSE)
  res <- dplyr::bind_rows(out) %>% dplyr::mutate(
    Alpha_nominal=publication_setting("POWER_ALPHA",.05),
    Note=paste("Observed effects from small samples are optimistic; required n is a lower bound.",
               "Family = all tests of that analysis and grouping; Bonferroni is conservative relative to BH.",
               "|rho| capped at 0.95; n.a. when the observed effect is zero or not estimable."))
  # Additional cases needed relative to the current design (80% power, nominal).
  if("N_per_group_80pct_nominal"%in%names(res))
    res$Additional_cases_per_group_80pct_nominal <-
      pmax(0,res$N_per_group_80pct_nominal-pmin(res$Current_n_A,res$Current_n_B))
  if("N_total_80pct_nominal"%in%names(res))
    res$Additional_cases_total_80pct_nominal <- pmax(0,res$N_total_80pct_nominal-res$Current_n_A)
  write_table(res,"Power_sample_size_requirements")
  invisible(res)
}

# ---------------------------------------------------------------------------
# Sensitivity of case-level IF results to the header-only export policy.
# Re-aggregates the raw exports under the alternative policy into a separate
# folder, recomputes case-level comparisons, and compares them side by side.
# ---------------------------------------------------------------------------
empty_policy_sensitivity <- function(config,primary) {
  alt <- setdiff(c("zero","exclude"),config$EMPTY_POLICY)[1]
  scfg <- config
  for(a in names(CLASS_DEFS)) {
    target <- file.path(config$OUT,paste0("raw_aggregation_",alt),a)
    aggregate_ilastik_raw(input_dir=config[[paste0(a,"_RAW_DIR")]],output_dir=target,
                          assay=a,width_px=2000,height_px=2000,empty_policy=alt,
                          save_objects=FALSE)
    scfg[[paste0(a,"_CSV")]] <- file.path(target,"image_by_class_and_merger.csv")
  }
  TABLE_SUBDIR <<- paste0("sensitivity_empty_",alt)
  on.exit(TABLE_SUBDIR <<- "",add=TRUE)
  sres <- group_features(build_feature_tables(read_features(scfg)))
  TABLE_SUBDIR <<- ""
  keys <- c("Assay","Class","Feature","Grouping","Contrast")
  a <- primary %>% dplyr::filter(Level=="case") %>%
    dplyr::select(dplyr::all_of(keys),N_A,N_B,P,P_Holm_within_feature,BH_Q,Cliffs_delta)
  b <- sres %>% dplyr::filter(Level=="case") %>%
    dplyr::select(dplyr::all_of(keys),N_A,N_B,P,P_Holm_within_feature,BH_Q,Cliffs_delta)
  names(a)[-(1:5)] <- paste0(names(a)[-(1:5)],"_",config$EMPTY_POLICY)
  names(b)[-(1:5)] <- paste0(names(b)[-(1:5)],"_",alt)
  cmp <- dplyr::full_join(a,b,by=keys)
  pa <- cmp[[paste0("P_Holm_within_feature_",config$EMPTY_POLICY)]]
  pb <- cmp[[paste0("P_Holm_within_feature_",alt)]]
  da <- cmp[[paste0("Cliffs_delta_",config$EMPTY_POLICY)]]
  db <- cmp[[paste0("Cliffs_delta_",alt)]]
  cmp$Same_significance_Holm_0.05 <- ifelse(is.finite(pa)&is.finite(pb),(pa<.05)==(pb<.05),NA)
  cmp$Same_direction <- ifelse(is.finite(da)&is.finite(db)&da!=0&db!=0,sign(da)==sign(db),NA)
  cmp$Primary_policy <- config$EMPTY_POLICY; cmp$Alternative_policy <- alt
  write_table(cmp,"IF_sensitivity_empty_policy")
  invisible(cmp)
}

run_feature_study <- function(config=CONFIG) {
  # Check only original inputs. Aggregations and statistics are always rebuilt.
  required_inputs <- path.expand(c(config$AT8_RAW_DIR,config$TRIM46_RAW_DIR,
                                  config$WB_FILE,config$COG_FILE))
  if(any(!file.exists(required_inputs)))
    stop("Original input missing: ",
         paste(required_inputs[!file.exists(required_inputs)],collapse=", "))
  root <- path.expand(config$OUT)
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  # tempfile supplies a collision-resistant run ID without deleting any output.
  run_dir <- tempfile(paste0("run_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_"),
                      tmpdir=root)
  dir.create(run_dir,recursive=TRUE,showWarnings=FALSE)
  config$OUT <- run_dir
  config$AT8_CSV <- file.path(run_dir,"raw_aggregation","AT8",
                            "image_by_class_and_merger.csv")
  config$TRIM46_CSV <- file.path(run_dir,"raw_aggregation","TRIM46",
                               "image_by_class_and_merger.csv")
  ACTIVE_CONFIG <<- config
  OUT <<- path.expand(config$OUT)
  WB_FILE <<- path.expand(config$WB_FILE)
  INPUT <<- dirname(WB_FILE)
  output_dir <<- file.path(OUT,"figures","WB")
  PLOT_INDEX <<- list()
  for(p in c(OUT,file.path(OUT,c("tables","qc","figures")),
             file.path(OUT,"figures",c("IF_case","IF_image","cognition","WB"))))
    dir.create(p,recursive=TRUE,showWarnings=FALSE)
  writeLines("INCOMPLETE",file.path(OUT,"RUN_STATUS.txt"))
  message("New run folder: ",OUT)
  dput(config,file.path(OUT,"qc","Run_config.R"))
  registry <- dplyr::bind_rows(lapply(names(CLASS_DEFS),function(a)
    tibble::tibble(Assay=a,Class=CLASS_DEFS[[a]],
                   Trusted=CLASS_DEFS[[a]]%in%TRUSTED)))
  write_table(registry,"Class_registry",qc=TRUE)
  for(a in names(CLASS_DEFS)) {
    aggregate_ilastik_raw(
      input_dir=config[[paste0(a,"_RAW_DIR")]],
      output_dir=dirname(config[[paste0(a,"_CSV")]]),
      assay=a,width_px=2000,height_px=2000,
      empty_policy=config$EMPTY_POLICY,save_objects=TRUE)
  }
  set.seed(42)
  input <- read_features(config)
  ft <- build_feature_tables(input)
  results <- group_features(ft)
  image_lmm_results <- image_level_lmm(ft)   # NEW: true image-level mixed-model analysis
  
  # Independent-case counts are used for BOTH summary levels.
  # Family size matches the grouping-specific IF screening family.
  designs <- results %>%
    dplyr::group_by(Grouping) %>%
    dplyr::mutate(N_family_tests=dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      Level, Assay, Class, Feature, Grouping, Contrast,
      N_A, N_B, N_family_tests
    ) %>%
    dplyr::filter(N_A>=4, N_B>=4) %>%
    dplyr::distinct()
  
  power_results <- dplyr::bind_rows(
    designs %>%
      dplyr::mutate(
        Scenario="Nominal alpha",
        Alpha=.05
      ),
    
    designs %>%
      dplyr::mutate(
        Scenario="Whole IF family Bonferroni benchmark",
        Alpha=.05/N_family_tests
      )
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      MDE_d_80=mde_d(N_A, N_B, target=.80, alpha=Alpha),
      MDE_d_90=mde_d(N_A, N_B, target=.90, alpha=Alpha),
      Model="Normal equal-variance t-test sensitivity benchmark"
    ) %>%
    dplyr::ungroup()
  
  write_table(power_results, "IF_power_sensitivity")
  plot_features(ft, dplyr::bind_rows(results, image_lmm_results))
  wb <- run_retained_WB()
  if(is.null(wb))stop("Western-blot data did not load. Correct CONFIG$WB_FILE.")
  if(!is.null(wb)) {
    dup <- wb %>% dplyr::count(Sample,Marker) %>% dplyr::filter(n>1)
    write_table(dup,"WB_duplicate_sample_marker_rows",qc=TRUE)
    if(nrow(dup))stop("WB sample-marker duplicates.")
    pooled <- dplyr::bind_rows(lapply(unique(wb$Marker),function(m) {
      d <- wb %>% dplyr::filter(Marker==m,is.finite(Value),Value>=0)
      a <- d$Value[d$Group=="Control"];b <- d$Value[d$Group!="Control"]
      tt <- if(length(a)>=2&&length(b)>=2)t.test(a,b)else NULL
      tibble::tibble(
        Marker=m,N_control=length(a),N_spectrum=length(b),
        Mean_difference_control_minus_spectrum=if(!is.null(tt))unname(tt$estimate[1]-tt$estimate[2])else NA_real_,
        CI_low=if(!is.null(tt))tt$conf.int[1]else NA_real_,
        CI_high=if(!is.null(tt))tt$conf.int[2]else NA_real_,
        P=if(!is.null(tt))tt$p.value else NA_real_)
    })) %>% dplyr::mutate(BH_Q_across_markers=bh(P))
    write_table(pooled,"WB_2group_Welch_tests")
    pp <- file.path(OUT,"tables","WB_Pairwise_PostHoc_Results.csv")
    if(file.exists(pp))readr::write_csv(
      readr::read_csv(pp,show_col_types=FALSE) %>%
        dplyr::group_by(Comparison) %>% dplyr::mutate(BH_Q_across_markers=bh(PValue_Raw)) %>%
        dplyr::ungroup(),pp)
  }
  wb_rank <- wb_rank_sensitivity(wb)
  cog <- run_feature_cognition(ft,wb,config)
  power_n <- sample_size_requirements(ft,wb,cog)
  empty_sens <- if(isTRUE(config$RUN_EMPTY_POLICY_SENSITIVITY))
    empty_policy_sensitivity(config,results) else NULL
  plot_index <- if(length(PLOT_INDEX))dplyr::bind_rows(PLOT_INDEX)else
    tibble::tibble(File=character(),Category=character())
  plot_index <- plot_index %>% dplyr::distinct(File,.keep_all=TRUE)
  write_table(plot_index %>% dplyr::mutate(File=substring(File,nchar(OUT)+2)),
              "Figure_manifest",qc=TRUE)
  paths <- c(input$paths,WB_FILE,path.expand(config$COG_FILE))
  paths <- paths[file.exists(paths)]
  write_table(tibble::tibble(File=paths,MD5=unname(tools::md5sum(paths))),"Input_checksums",qc=TRUE)
  write_table(FEATURES_V6,"Feature_dictionary")
  capture.output(sessionInfo(),file=file.path(OUT,"qc","R_sessionInfo.txt"))
  writeLines(c(
    "COMPLETE",
    "Final PSP pipeline: ilastik feature/distribution study; exploratory case-only cognition.",
    "No AT8-TRIM46 association models are executed.",
    "Case summaries use feature-appropriate pooling; image inference uses case-random-intercept mixed models.",
    "Image Detection uses a binomial case-random-intercept GLMM (tables only; not plotted at field level).",
    paste("Header-only exports policy:",config$EMPTY_POLICY,
          if(isTRUE(config$RUN_EMPTY_POLICY_SENSITIVITY)) "(alternative policy in tables/IF_sensitivity_empty_policy.csv)" else ""),
    paste("Morphology converted to microns at",config$MICRONS_PER_PIXEL,"um/pixel; object size in um^2."),
    "Figures: PNG only; WB, Trusted AT8/TRIM46 IF (case and image), Diffuse AT8 vs Last MoCA.",
    if(isTRUE(config$PLOT_SIGNIFICANT_ONLY)) paste("IF panels shown only when a bracket p <",config$PLOT_ALPHA,
      "; see qc/Figure_panel_selection.csv. All results are in tables/.") else "All IF panels shown.",
    "3-group brackets: Holm-adjusted p (p_adj) within feature/marker; 2-group brackets: unadjusted p.",
    "WB rank-based sensitivity: tables/WB_rank_sensitivity.csv.",
    "Sample-size requirements: tables/Power_sample_size_requirements.csv.",
    "Every aggregation and statistical table was rebuilt from original inputs in this run.",
    "Rank-sum tests compare distributions, not necessarily medians.",
    "Median-difference intervals are descriptive case-bootstrap intervals, not rank-test inversions.",
    "BH families combine both IF summary levels and all endpoints/contrasts within each grouping.",
    "Image fields and pixels are not independent biological replicates.",
    "Morphology/intensity endpoints are conditional on available detected objects.",
    "WB statistics retained (Welch); figures use the shared print-size style.",
    "Area denominator is full 2000 x 2000 image, not tissue area.",
    "Classification, staining/acquisition, brain-bank and sampling confounding need external review."
  ),file.path(OUT,"RUN_STATUS.txt"))
  message("Completed final PSP feature study: ",OUT)
  invisible(list(out=OUT,config=config,features=ft,comparisons=results,
                 image_comparisons=image_lmm_results,cognition=cog,wb=wb,
                 wb_rank_sensitivity=wb_rank,power=power_n,empty_policy_sensitivity=empty_sens))
}

# Safe for RStudio Source or Run All: no terminal-only Usage error.
result <- if(!identical(getOption("psp.features.autorun"),FALSE))
  run_feature_study(CONFIG) else NULL
list(config=CONFIG,run=run_feature_study,result=result,
     environment=environment())
},envir=new.env(parent=baseenv()))

# Output location of this run.
if(!is.null(FINAL_PSP$result)) message("Results: ", FINAL_PSP$result$out)

pw <- read.csv(file.path(FINAL_PSP$result$out, "tables", "Power_sample_size_requirements.csv"))

# WB: per-group n (t-test rows)
subset(pw, Assay == "WB" & grepl("t-test", Test_assumed),
       c(Class, Grouping, Contrast, Cohens_d,
         N_per_group_80pct_nominal, N_per_group_90pct_nominal))

# IF: trusted classes only
subset(pw, Analysis == "IF case-level group comparison" & grepl("^Trusted", Class),
       c(Class, Feature, Grouping, Contrast, Cliffs_delta,
         N_per_group_80pct_nominal, N_per_group_90pct_nominal))

# MoCA: diffuse AT8, total n
subset(pw, grepl("MoCA", Analysis) & Class == "Diffuse AT8",
       c(Feature, Spearman_rho, N_total_80pct_nominal, N_total_90pct_nominal))

sel <- read.csv(file.path(FINAL_PSP$result$out, "qc", "Figure_panel_selection.csv"))
shown <- unique(sel$Feature[sel$Level == "case" & sel$Plotted])
subset(pw, Analysis == "IF case-level group comparison" & grepl("^Trusted", Class) & Feature %in% shown,
       c(Class, Feature, Grouping, Contrast, Cliffs_delta,
         N_per_group_80pct_nominal, N_per_group_90pct_nominal))