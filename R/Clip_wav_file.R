#' Clip wave to length from given start time in recording
#'
#' @param seg
#' @param length_clip
#' @param out_dir
#'
#' @return
#' @export
#'
#' @examples
format_clip_wave <- function(seg, length_clip, out_dir){
  if(length_clip==300){
    file.copy(from = seg$filename,
              to =  glue::glue("{out_dir}/{seg$SunRisSet}/{seg$WildTraxName}.wav"))
  } else{
    in_wav <-
      tuneR::readWave(
        seg$filename, from = seg$StartTime,
        to = seg$StartTime + length_clip,
        units = 'seconds')
    tuneR::writeWave(in_wav,
                     glue::glue("{out_dir}/{seg$SunRisSet}/{seg$WildTraxName}.wav"))
    return(TRUE)
  }
}
