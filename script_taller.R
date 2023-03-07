# Ejemplo de instalación y activación de paquetes----------

install.packages("tidyverse")

library(tidyverse)

# Objetos-----------

objeto <- "Yo soy un objeto"

numeros <- 5625

Palabras <- "¡Hola amigos!"

vector <- c(4,5,6,7,8)


# Factor-----------

clases_sociales <- c("alta", "media", "baja", "baja", "media", "media", "baja")
clases_factor <- factor(clases_sociales, levels = c("baja", "media", "alta"))

table(clases_factor)


# Apertura de bases-----------

eph_03_22 <- read.csv2("bases/usu_individual_T322.txt", header = T)

nrow(eph_03_22)


# Paquete EPH---------
install.packages("eph")

library(eph)

eph_ind_419 <- get_microdata(year = 2019, trimester = 4, type = "individual")

eph_ind_419 <- organize_labels(eph_ind_419, type = 'individual')

attributes(eph_ind_419$CH04)

eph_hog_419 <- get_microdata(year = 2019, trimester = 4, type = "hogar",
                             vars = c("CODUSU", "NRO_HOGAR", "II7", "II1", "IX_TOT"))


eph_419 <- eph_ind_419 %>%
  left_join(eph_hog_419, by=c("CODUSU", "NRO_HOGAR"))

#formas de filtrado
eph_mdp1 <- subset(eph_419, AGLOMERADO == 34)

eph_mdp2 <- eph_419[eph_419$AGLOMERADO == 34, ]

eph_419_mdp <- eph_419 %>%
  filter(AGLOMERADO == 34)


# Ponderación ----------

eph_419_mdp %>%
  tally(PONDERA)


# Esquemas de clase---------

table(eph_419$ESTADO)

table(eph_419$PP04D_COD)

table(eph_419$CAT_OCUP)


#Separación de dígitos CNO

eph_419$cno <- str_pad(eph_419$PP04D_COD, 5, pad = "0")

eph_419$cno12 <- str_sub(eph_419$cno, 1, 2) #señala que parta de la posición 1 hasta 2 del código
eph_419$cno3 <- str_sub(eph_419$cno, 3, 3) #señala que parta de la posición 3 a la 3 del código
eph_419$cno4 <- str_sub(eph_419$cno, 4, 4)
eph_419$cno5 <- str_sub(eph_419$cno, 5, 5)

eph_419$cno12 <- as.numeric(eph_419$cno12)
eph_419$cno3 <- as.numeric(eph_419$cno3)
eph_419$cno4 <- as.numeric(eph_419$cno4)
eph_419$cno5 <- as.numeric(eph_419$cno5)


#Creación GO

eph_419 <- eph_419 %>%
  mutate(go = case_when((cno12 >= 0 & cno12 <=4) | (cno12 == 7) ~ 1,

                        (cno12 >= 5 & cno12 <=6) ~ 2,

                        (cno5 == 1) & ((cno12 >= 10 & cno12 <= 20) |
                                         (cno12 == 32) | (cno12 >= 34 & cno12 <= 40) |
                                         (cno12 >= 42 & cno12 <= 47) | (cno12 >= 49 & cno12 <= 54) |
                                         (cno12 >= 60 & cno12 <= 64) | (cno12 >= 70 & cno12 <= 92)) ~ 3,

                        (cno5 > 4) & (cno12 == 11 | cno12 == 42 | cno12 == 43 |
                                        cno12 == 50 | cno12 == 70 | cno12 == 81 | cno12 == 91) ~ 3,

                        (cno5 == 2) & ((cno12 >= 10 & cno12 <= 32) | (cno12 >= 34 & cno12 <= 54) |
                                         (cno12 >= 56 & cno12 <= 92)) ~ 4,

                        (cno5 == 1) & ((cno12 == 30 | cno12 == 31 | cno12 == 41 | cno12 == 48) |
                                         (cno12 >= 56 & cno12 <= 58) | (cno12 == 65)) ~ 4,

                        (cno5 == 3) & (cno12 == 40 | cno12 == 42 | cno12 == 43 | cno12 == 45 |
                                         cno12 == 91) ~ 4,

                        (cno5 == 4) & (cno12 == 42 | cno12 == 43 | cno12 == 45) ~ 4,

                        (cno5 > 4) & (cno12 == 40 | cno12 == 41 | cno12 == 44 | cno12 == 45 |
                                        cno12 == 46 | cno12 == 51 | cno12 == 92) ~ 4,

                        (cno5 == 3) & ((cno12 >= 10 & cno12 <= 32) | (cno12 == 35) | (cno12 == 41) |
                                         (cno12 == 54) | (cno12 == 81)) ~ 5,

                        (cno5 == 4) & ((cno12 >= 10 & cno12 <= 11) | (cno12 >= 30 & cno12 <= 32) |
                                         (cno12 == 35)) ~ 5,

                        (cno5 > 4) & ((cno12 == 10) | (cno12 >= 20 & cno12 <= 32) | (cno12 == 35)) ~ 5,

                        (cno5 == 1 | cno5 == 2) & (cno12 == 55) ~ 6,

                        (cno5 == 3) & ((cno12 == 34) | (cno12 == 36) | (cno12 == 44) |
                                         (cno12 >= 46 & cno12 <= 53) | (cno12 >= 55 & cno12 <= 80) |
                                         (cno12 == 82) | (cno12 == 90) | (cno12 == 92)) ~ 6,

                        (cno5 == 4) & ((cno12 == 44) | (cno12 == 49) | (cno12 == 53) | (cno12 == 57)) ~ 6,

                        (cno5 > 4) & ((cno12 == 34) | (cno12 >= 47 & cno12 <= 49) | (cno12 >= 52 & cno12 <= 54) | (cno12 >= 57 & cno12 <= 65) | (cno12 >= 71 & cno12 <= 80) | (cno12 >= 82 & cno12 <= 90)) ~ 6,

                        (cno12 == 33)   ~ 7,

                        (cno5 == 4 | cno5 == 9) & ((cno12 == 20) | (cno12 == 34) |
                                                     (cno12 >= 36 & cno12 <= 41) | (cno12 >= 46 & cno12 <= 48) |
                                                     (cno12 >= 50 & cno12 <= 52) | (cno12 == 54) | (cno12 == 56) |
                                                     (cno12 >= 58 & cno12 <= 92)) ~ 7,

                        (cno5 >= 4) & (cno12 == 55) ~ 8

                        ,(cno12 == 99) | (is.na(cno12)) ~ 9))


table(eph_419$go)


#Recodificación sector de actividad y tamaño

eph_419 <- eph_419 %>%
  mutate(sector_act = car::recode(eph_419$PP04A, "1=2; 2:3=1"))

eph_419 <- eph_419 %>%
  mutate(tamano = case_when((PP04C > 0 & PP04C <= 5) | (PP04C == 99 & PP04C99 == 1) ~ 1,
                            (PP04C > 5 & PP04C < 99) | (PP04C == 99 & PP04C99 >= 2) ~ 2,
                            PP04C == 0 | PP04C99 == 0 ~ NA_real_
  ))


#CSO desagregado

eph_419 <- eph_419 %>%
  mutate(cso_desag = case_when(go==1 & CAT_OCUP==1 & tamano==2 ~ 1,
                               go==1 & CAT_OCUP==1 & tamano==1 ~ 13,
                               go==1 & CAT_OCUP==3 & sector_act==1 & tamano==2 ~ 2,
                               go==1 & CAT_OCUP==3 & sector_act==1 & is.na(tamano) ~ 2,
                               go==1 & CAT_OCUP==3 & sector_act==1 & tamano==1 ~ 11,
                               go==1 & CAT_OCUP==3 & sector_act==2 ~ 1,
                               go==1 & CAT_OCUP==2 ~ 14,
                               go==1 & CAT_OCUP==4 ~ 14,
                               go==1 & CAT_OCUP==NA ~ 14,

                               go==2 & CAT_OCUP==1 & tamano==2 ~ 9,
                               go==2 & CAT_OCUP==1 & tamano==1  ~ 13,
                               go==2 & CAT_OCUP==3 & sector_act==1 & tamano==2 ~ 10,
                               go==2 & CAT_OCUP==3 & sector_act==1 & is.na(tamano) ~ 10,
                               go==2 & CAT_OCUP==3 & sector_act==1 & tamano==1  ~ 11,
                               go==2 & CAT_OCUP==3 & sector_act==2  ~ 12,
                               go==2 & CAT_OCUP==2  ~ 14,
                               go==2 & CAT_OCUP==4  ~ 14,
                               go==2 & CAT_OCUP==NA     ~ 14,

                               go==3 & CAT_OCUP==1 & tamano==2 ~ 3,
                               go==3 & CAT_OCUP==1 & tamano==1 ~ 4,
                               go==3 & CAT_OCUP==3 & sector_act==1 & tamano==2 ~ 6,
                               go==3 & CAT_OCUP==3 & sector_act==1 & is.na(tamano) ~ 6,
                               go==3 & CAT_OCUP==3 & sector_act==1 & tamano==1  ~ 7,
                               go==3 & CAT_OCUP==3 & sector_act==2 ~ 8,
                               go==3 & CAT_OCUP==2 ~ 5,
                               go==3 & CAT_OCUP==4 ~ 5,
                               go==3 & CAT_OCUP==NA ~ 5,

                               go==4 & CAT_OCUP==1 & tamano==2  ~ 9,
                               go==4 & CAT_OCUP==1 & tamano==1  ~ 13,
                               go==4 & CAT_OCUP==3 & sector_act==1 & tamano==2  ~ 10,
                               go==4 & CAT_OCUP==3 & sector_act==1 & is.na(tamano)  ~ 10,
                               go==4 & CAT_OCUP==3 & sector_act==1 & tamano==1  ~ 11,
                               go==4 & CAT_OCUP==3 & sector_act==2 ~ 12,
                               go==4 & CAT_OCUP==2  ~ 14,
                               go==4 & CAT_OCUP==4  ~ 14,
                               go==4 & CAT_OCUP== NA ~ 14,

                               go==5 & CAT_OCUP==1 & tamano==2 ~ 9,
                               go==5 & CAT_OCUP==1 & tamano==1 ~ 13,
                               go==5 & CAT_OCUP==3 & sector_act==1 & tamano==2 ~ 15,
                               go==5 & CAT_OCUP==3 & sector_act==1 & is.na(tamano) ~ 15,
                               go==5 & CAT_OCUP==3 & sector_act==1 & tamano==1 ~ 16,
                               go==5 & CAT_OCUP==3 & sector_act==2  ~ 17,
                               go==5 & CAT_OCUP==2  ~ 14,
                               go==5 & CAT_OCUP==4 ~ 14,
                               go==5 & CAT_OCUP== NA ~ 14,

                               go==6 & CAT_OCUP==1 & tamano==2  ~ 9,
                               go==6 & CAT_OCUP==1 & tamano==1  ~ 13,
                               go==6 & CAT_OCUP==3 & sector_act==1 & tamano==2 ~ 19,
                               go==6 & CAT_OCUP==3 & sector_act==1 & is.na(tamano)  ~ 19,
                               go==6 & CAT_OCUP==3 & sector_act==1 & tamano==1 ~ 20,
                               go==6 & CAT_OCUP==3 & sector_act==2 ~ 21,
                               go==6 & CAT_OCUP==2  ~ 18,
                               go==6 & CAT_OCUP==4  ~ 18,
                               go==6 & CAT_OCUP== NA    ~ 18,

                               go==7 & CAT_OCUP==1 & tamano==2  ~ 25,
                               go==7 & CAT_OCUP==1 & tamano==1  ~ 25,
                               go==7 & CAT_OCUP==3 & sector_act==1 & tamano==2  ~ 22,
                               go==7 & CAT_OCUP==3 & sector_act==1 & is.na(tamano) ~ 22,
                               go==7 & CAT_OCUP==3 & sector_act==1 & tamano==1  ~ 23,
                               go==7 & CAT_OCUP==3 & sector_act==2  ~ 24,
                               go==7 & CAT_OCUP==4  ~ 25,
                               go==7 & CAT_OCUP==2  ~ 25,
                               go==7 & CAT_OCUP== NA    ~ 25,

                               go==8 ~ 26,
                               go==9   ~ 27))


#CSO agregado

eph_419 <- eph_419 %>%
  mutate(cso_agg = case_when(cso_desag <=2 ~ 1,
                             cso_desag >=3 & cso_desag <=8 ~ 2,
                             cso_desag ==9 ~ 3,
                             cso_desag >=10 & cso_desag <=12 ~ 4,
                             cso_desag >=13 & cso_desag <=14 ~ 5,
                             cso_desag >=15 & cso_desag <=17 ~ 6,
                             cso_desag ==18 ~ 7,
                             cso_desag >=19 & cso_desag <=21 ~ 8,
                             cso_desag >=22 & cso_desag <=24 ~ 9,
                             cso_desag ==25 ~ 10,
                             cso_desag ==26 ~ 11,
                             cso_desag ==27 | is.na(cso_desag) ~ NA_real_))


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


# Esquema final

eph_419 <- eph_419 %>%
  mutate(clase6 = case_when(cso_agg == 1 ~ 1,
                            (cso_agg == 2 & CAT_OCUP == 2) | cso_agg == 3 | cso_agg == 5 ~ 2,
                            (cso_agg == 2 & CAT_OCUP >= 3) | cso_agg == 4 | cso_agg == 6 ~ 3,
                            cso_agg == 7 ~ 4,
                            cso_agg == 8 | cso_agg == 9 ~ 5,
                            cso_agg == 10 | cso_agg == 11 ~ 6))

eph_419$clase6_factor <- factor(eph_419$clase6, labels = c("Clase alta",
                                                           "Clase media - autónoma",
                                                           "Clase media -asalariada",
                                                           "Clase obrera - autónoma",
                                                           "Clase obrera - asalariada",
                                                           "Clase obrera - trabajadores marginales"))



