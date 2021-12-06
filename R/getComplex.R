#' Get accoustic complexity values
#'
#' @param filename File name
#' @param fl file location
#' @param mf minimum frequency for complexity
#' @param maxf max freqency for complexity
#' @param units_
#'
#' @return
#' @export
#'
#' @examples
getvals <- function(filename,fl , mf = NA, maxf = NA, units_ = "samples") {
  cat(glue::glue("Running file named {filename}\n"))
  file_location <- ifelse(fl=="", filename, glue::glue("{fl}/{filename}"))
  wave_ex <- tuneR::readWave(file_location, units = units_)

  complexity <- soundecology::acoustic_complexity(wave_ex, min_freq = mf, max_freq = maxf)
  bioindex <- soundecology::bioacoustic_index(wave_ex)
  result <- soundecology::acoustic_diversity(wave_ex)
  df_out <- tibble(file = filename) %>% bind_cols(as_tibble(complexity[1:4])) %>%
    bind_cols(as_tibble(bioindex)) %>% as_tibble(result[1:4])
  df_out
}
