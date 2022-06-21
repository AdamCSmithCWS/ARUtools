#' Get accoustic complexity values
#'
#' Wrapper for soundecology package
#'
#' @param filename File name
#' @param fl file location
#' @param mf minimum frequency for complexity
#' @param maxf max freqency for complexity
#' @param units_ Units for tuneR to read wave file. Defaults to "samples"
#'
#' @return Returns a data frame with accoustic indices.
#' @export
getvals <- function(filename,fl , mf = NA, maxf = NA, units_ = "samples") {
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Packages \"soundecology\" and \"tuneR\" must be installed to use this function.",
      call. = FALSE
    )
  }
  cat(glue::glue("Running file named {filename}\n"))
  file_location <- ifelse(fl=="", filename, glue::glue("{fl}/{filename}"))
  wave_ex <- tuneR::readWave(file_location, units = units_)

  complexity <- soundecology::acoustic_complexity(wave_ex, min_freq = mf, max_freq = maxf)
  bioindex <- soundecology::bioacoustic_index(wave_ex)
  result <- soundecology::acoustic_diversity(wave_ex)
  df_out <- tibble::tibble(file = filename) %>% bind_cols(as_tibble(complexity[1:4])) %>%
    bind_cols(as_tibble(bioindex)) %>% as_tibble(result[1:4])
  df_out
}
