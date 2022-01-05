#' Run the model function
#'
#' @param df Data frame to use with ARU information and selection probabilities
#' @param N Number of samples per stratum
#' @param os Oversample proportion
#' @param seed random number seed
#' @param strat_ Stratum ID. This is the column grts runs on
#' @param selprob_id Selection probability column
#' @param x Column in df for x (I've been using doy)
#' @param y Column in df for y -time of day or time to sunrise/sunset
#'
#' @return A sampling run from grts
#' @export
#'
fun_aru_samp <- function(df, N, os, seed, strat_, selprob_id, x, y) {

    arus <- df %>%
      dplyr::select({{ strat_ }}) %>%
      dplyr::distinct() %>% # morningChorus
      .[[strat_]]
    # print(arus)
    if(packageVersion("spsurvey")<5){
    Stratdsgn <- vector(mode = "list", length = length(arus))
    names(Stratdsgn) <- arus
    for (a in arus) {
      Stratdsgn[[a]] <- list(
        panel = c(PanelOne = N),
        over = N * os,
        seltype = "Continuous"
      )
    }


    set.seed(seed)

    samp <- spsurvey::grts(
      design = Stratdsgn,
      DesignID = "ARU_Sample",
      type.frame = "finite",
      stratum_var = strat_, # "ARU_ID",
      # type.frame = "area",
      src.frame = "att.frame", # "shapefile",
      # in.shape="output/Sample_Frame_w_Legacy_LCC",
      att.frame = df, # morningChorus,
      mdcaty = selprob_id, # "psel_tod",
      xcoord = x, #' doy',
      startlev = 1,
      id = FileID,
      ycoord = y, # "min_to_Sunrise",
      shapefile = F
    )
    return(samp)
  }
  if(packageVersion("spsurvey")>=5){
  sf_df <- df %>%
      sf::st_as_sf(coords = c(paste0(x),paste0(y)), crs = 3395)

  # browser()

  Stratdsgn <- rep(N, length(arus))

  n_os <-  round(N*os)
  names(Stratdsgn) <- names(n_os) <- arus



  set.seed(seed)
  samp <- spsurvey::grts(sframe = sf_df,n_over = n_os,
                         n_base = Stratdsgn,
                         stratum_var = strat_,
                         # caty_n = stratum_var,
                         aux_var =  selprob_id)



  return(samp)
  }
}
