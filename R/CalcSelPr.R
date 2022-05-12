#' Simulate selection probabilities for GRTS
#'
#' @param min  Vector of minutes to sunrise or sunset
#' @param mean_log_min Mean (on log scale) of density for minutes to sun event
#' @param sdlog_min Standard deviation of minutes portion of density
#' @param doy Vector of minutes to sunrise or sunset (negative is before)
#' @param mean_doy Mean of density for days to sun event
#' @param sd_doy Standard deviation of days portion of density
#' @param return_dat Logical - return just the data or print basic plots
#' @param log_ Logical - to use log of densities (creates softer patterns)
#' @param fun Function to use for densities. Can be "lognorm", "norm", or "cauchy"
#'
#' @return Returns either a data frame with selection probabilities or plots of selection probabiliies.
#' @export
gen_dens_sel_simulation <- function( min, mean_min, sd_min, doy, mean_doy, sd_doy, return_dat =F, log_=F, fun = 'norm',
                                     off=NULL ) {
  min_fun <- switch (fun,
    "lognorm" = function(x, m, sd, log) dlnorm(x+off,log(m+off), sd, log),
    "norm" = dnorm,
    "cauchy"=dcauchy
  )

  dens_min <- min_fun(min, mean_min, sd_min, log = log_)
  dens_doy <- dnorm(doy, mean_doy, sd_doy, log=log_)
  all <- expand.grid(doy = doy, min = min) |>
    dplyr::mutate(
      psel_tod = min_fun(round(min,0), mean_min, sd_min, log = log_)/max(abs(dens_min)),
      psel_doy = dnorm(doy,mean= mean_doy, sd = sd_doy, log=log_)/max(abs(dens_doy)),
      psel = dplyr::case_when(log_~ exp( psel_tod + psel_doy),
                       !log_~ psel_tod * psel_doy),
      psel_scaled = psel/max(psel))

  if(isTRUE(return_dat))(return(all))

  p1 <- ggplot(all, aes(doy, psel_doy)) + geom_line()
  p2 <- ggplot(all, aes(min, psel_tod)) + geom_line()
  p3 <- ggplot(all,
               aes(doy,
                   min,
                   fill =
                     psel_scaled
               )
  ) +
    geom_tile() +
    scale_fill_viridis_c()


  (p1 + p2) / p3


}



#' Calculate Inclusion Probabilities
#'
#' @param .data Data frame with Date, times, and ARU IDs
#' @param ARU_ID_col ARU id column. Should not be quoted.
#' @param min_col Minutes column. Should not be quoted.
#' @param day_col Day column. Should not be quoted.
#' @param parms list of parameters. See defaults for examples.
#'                 Should include min_range, doy_range, mean_min, sd_min,
#'                mean_doy, sd_doy, off, log_, fun.
#'
#' @return   Returns .data with selection probability columns
#' @export
calc_sel_pr <- function(.data,ARU_ID_col, min_col, day_col, parms = list(min_range = c(-60, 300),
                                                                         doy_range = c(150, 180),
                                                                         mean_min = 30, sd_min = 30,
                                                              mean_doy = 0, sd_doy = 10,off=0,
                                                              log_ = FALSE, fun = "norm")){

  list2env(parms, envir = environment())
  min_fun <- switch (fun,
                     "lognorm" = function(x, m, sd, log) dlnorm(x+off,log(m+off), sd, log),
                     "norm" = dnorm,
                     "cauchy"=dcauchy
  )
  # min_range <- range(pull(.data,{{min_col}}) )
  # doy_range <- range(pull(.data,{{min_col}}) )
  dens_min <- min_fun(seq(min_range[[1]], min_range[[2]]), mean_min, sd_min, log = log_)
  dens_doy <- dnorm(seq(doy_range[[1]], doy_range[[2]]), mean_doy, sd_doy, log=log_)
  # browser()
  .data %>%
    dplyr::filter({{day_col}} >= doy_range[[1]] &
             {{day_col}} <= doy_range[[2]] &
             {{min_col}}>= min_range[[1]] &
             {{min_col}}<= min_range[[2]]
             ) %>%
    dplyr::mutate(psel_tod = min_fun(round({{min_col}},0), mean_min, sd_min, log = log_)/max(abs(dens_min)),
           psel_doy = dnorm({{day_col}},mean= mean_doy, sd = sd_doy, log=log_)/max(abs(dens_doy)),
           psel = dplyr::case_when(log_~ exp( psel_tod + psel_doy),
                            !log_~ psel_tod * psel_doy),
           psel_scaled = psel/max(psel)) %>%
    dplyr::group_by({{ARU_ID_col}}) %>%
    dplyr::mutate(psel_std = psel / max(psel)) %>%
    dplyr::ungroup()



}




 # gen_dens_sel_simulation(min =  seq(-60,60*4 ),
 #             mean_min =  -30,
 #             sd_min = 120,
 #             doy =  seq(152, 210),
 #             mean_doy = 170,sd_doy = 200, log_ = F,
 #             return_dat = F, fun = "norm", off=60
 #             )
#  test <- gen_dens_sel(min =  seq(-31,120 ),
#               mean_log_min =  log(250),
#               sdlog_min = 0.94,
#               doy =  seq(152, 210),
#               mean_doy = 170,sd_doy = 200, return_dat = T
#  )
#
#
#
# sigma_min <- 50
# sigma_doy <-  75
# cor_m_doy <- 0.8
#
# cov_dm <- sigma_doy*sigma_min*cor_m_doy
# Sigma <- matrix(c(sigma_doy^2, cov_dm, cov_dm, sigma_min^2), ncol=2)
#
# test$mat <- mvtnorm::dmvnorm(test[,1:2], mean = c(180, -4), sigma = Sigma)
# ggplot(test,
#        aes(doy,
#            min,
#            fill =
#              mat
#        )
# ) +
#   geom_tile() +
#   scale_fill_viridis_c()

