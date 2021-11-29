library(testthat)
library(ARUtools)

library(ggplot2)
library(dplyr)
library(patchwork)

test_check("ARUtools")


file.location <- "//WRIV02DTSTDNT1/RecordStor20172019/BetweenRivers_2019"
d <-
  readr::read_rds(
    "C:/Users/hoped/Documents/Local_Workspace/Projects/AtlasNorth/AudioSamples/rds/2019-11-08_ARU_ExclusionDataTable.rds"
    )

morningChorus <- d %>%
  filter(Sample_Group == "Dawn"&doy <188 )

minutes <- seq(0, 7*60)
days_ <- seq(152, 189)
d_sel <- gen_dens_sel(min =minutes, mean_log_min = log(100), sdlog_min = 0.6,
                      doy = days_, mean_doy = 170, sd_doy = 170
                        )

morningChorus <- d %>%
  filter(Sample_Group == "Dawn"&doy <188 ) %>%
  mutate(psel_tod = ifelse(excluded == "Included",
                           dlnorm(round(min_to_Sunrise,0)+60, log(100), 0.6) / sum(d_sel$data$psel_tod),
                           0),
         psel_doy = dnorm(doy,mean= 170, sd = 200)/sum(d_sel$data$psel_doy),
         psel = psel_tod * psel_doy,
         sixdayweek = (doy - 152) %/% 6,
         ARU_week_ID = paste0(ARU_ID, "_", sixdayweek)) %>%
  group_by(ARU_ID) %>%
  mutate(psel_std = psel / sum(psel)) %>%
  ungroup

ggplot(morningChorus, aes(doy, min_to_Sunrise, colour = psel_std)) + geom_point() +
  scale_colour_viridis_c()


mc_doy <- fun_aru_samp(df = morningChorus[morningChorus$psel_std>0,],
                       N = 12, os =0.2,
                       seed = 5735779,
                       strat_ = "ARU_ID",
                       selprob_id = "psel_tod", x = 'doy', y = 'min_to_Sunrise'
)
res <- mc_doy$sites_base
resall  <- res %>% sf::st_drop_geometry() %>% dplyr::select(siteID:FileID) %>% left_join(x= morningChorus[morningChorus$psel_std>0,])

ggplot( morningChorus[morningChorus$psel_std>0,],
        aes(doy,
            min_to_Sunrise
            )) + geom_point(alpha =0.2) +
  geom_point(colour='red', data = resall[!is.na(resall$siteID),])

resall[!is.na(resall$siteID),] %>% ggplot(aes(min_to_Sunrise)) + geom_density() +
  geom_density(colour = 'red', data = morningChorus[morningChorus$psel_std>0,])

resall[!is.na(resall$siteID),] %>% ggplot(aes(doy)) + geom_density() +
  geom_density(colour = 'red', data = morningChorus[morningChorus$psel_std>0,])


ggplot( morningChorus[morningChorus$psel_std>0,],
        aes(doy,
            min_to_Sunrise, z= psel_std
        )) + stat_contour(breaks = c(0.025,0.975) ) +
  geom_point(colour='red', data = resall[!is.na(resall$siteID),])
