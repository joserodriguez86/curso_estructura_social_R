eph_419 <- eph_ind_419 %>%
  left_join(eph_hog_419, by=c("CODUSU", "NRO_HOGAR"))

eph_mdp <- subset(eph_419, AGLOMERADO == 34)
eph_mdp <- eph_419[eph_419$AGLOMERADO == 34, ]


table(eph_419$go)


eph_ind_215$cso_agg_factor <- factor(eph_ind_215$cso_agg, labels = c("DIREC",
                                                                     "PROF",
                                                                     "PPE",
                                                                     "TECN",
                                                                     "PPA",
                                                                     "EAV",
                                                                     "TEA",
                                                                     "OCAL",
                                                                     "ONCAL",
                                                                     "TMARG",
                                                                     "EDOM"))
table(eph_ind_215$cso_agg_factor)
