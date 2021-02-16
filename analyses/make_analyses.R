#' @header *********************************************************************
#' @dataset (01) BARTONOVA 2016
#' @header *********************************************************************

# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "bartonova_2016.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "bartonova_2016")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (02) CLEARLY 2016
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "clearly_2016.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:10, 15))
dataset <- as_ordinal(dataset, cols = c(11, 12))


# Run Analysis ----

run_analysis(dataset, name = "clearly_2016")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (03) RIBERA 2001
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "ribera_2001.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:10)
dataset <- as_categorical(dataset, cols = 11:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "ribera_2001")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (04) PAKEMAN 2011
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "pakeman_2011.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(3:4, 23:28))
dataset <- as_ordinal(dataset, cols = 5:22)


# Run Analysis ----

run_analysis(dataset, name = "pakeman_2011")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (05) GONCALVES 2014
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "goncalves_2014.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_categorical(dataset, cols = 1)
dataset <- as_numerical(dataset, cols = 2:6)


# Run Analysis ----

run_analysis(dataset, name = "goncalves_2014")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (06) DIAZ 2008
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "diaz_2008.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_ordinal(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "diaz_2008")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (07) NORTHSEATRAITS
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "north_sea_traits.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(4:5, 7:11, 13:14))
dataset <- as_categorical(dataset, cols = c(1:3, 6))
dataset <- as_ordinal(dataset, cols = 12)


# Run Analysis ----

run_analysis(dataset, name = "north_sea_traits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (08) NZ INSECT
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "nz_insect.csv"),
  header    = TRUE
)


# Traits Preparation and Categorization ----

rownames(dataset) <- dataset$"ID_sp"
dataset <- dataset[ , -c(1:5)]

dataset <- as_ordinal(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "nz_insect")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (09) KRASNOV 2015
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "krasnov_2015.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:6)


# Run Analysis ----

run_analysis(dataset, name = "krasnov_2015")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (10) USDA PLANTS                                                    NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "usda_plant_traits.csv"),
  header    = TRUE
)


# Traits Preparation and Categorization ----

dataset <- dataset[ , -c(1:4)]
rownames(dataset) <- dataset[ , 1]
dataset <- dataset[ , -1]

dataset <- as_numerical(dataset, cols = 10:18)
dataset <- as_categorical(dataset, cols = 19:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "usda_plant_traits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (11) PALM TRAITS                                                    NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "palm_traits.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- dataset[ , -c(1:4)]

dataset <- as_numerical(dataset, cols = c(7:8, 11:19))
dataset <- as_categorical(dataset, cols = c(9, 20:ncol(dataset)))


# Run Analysis ----

run_analysis(dataset, name = "palm_traits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (12) FRESHWATER FISHES                                              NOT RUN
#' @header *********************************************************************



# Read Dataset ----

load(here::here("data", "fresh_fish_trait.RData"))
dataset <- freshfishtrait
rm(list = "freshfishtrait")


# Remove lines with too much NA (i.e. more than 5) ----

dataset <- dataset[rowSums(is.na(dataset)) < 5, ]


# Run Analysis ----

run_analysis(dataset, name = "fresh_fish_trait")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (13) BIRDS                                                          NOT RUN
#' @header *********************************************************************



# Read Dataset ----

load(here::here("data", "birds_trait.RData"))
dataset <- birdstrait
rm(list = "birdstrait")


# Run Analysis ----

run_analysis(dataset, name = "birds_trait")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (14) MAMMALS                                                        NOT RUN
#' @header *********************************************************************



# Read Dataset ----

load(here::here("data", "mammals_trait.RData"))
dataset <- mammalstrait
rm(list = "mammalstrait")


# Run Analysis ----

run_analysis(dataset, name = "mammals_trait")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (15) CORAL                                                          NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "coral_traits.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(2:5, 13))
dataset <- as_categorical(dataset, cols = c(1, 6:12))

dataset <- dataset[ , -10]
dataset <- dataset[ , -13]


# Run Analysis ----

run_analysis(dataset, name = "coral_traits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (16) BACTERIA                                                       NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "bacteria.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_categorical(dataset, cols = 1)


# Run Analysis ----

run_analysis(dataset, name = "bacteria")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (17) EALLONARDO 2013                                                NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "eallonardo_2013.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 6:11)
dataset <- as_categorical(dataset, cols = 1:5)


# Run Analysis ----

run_analysis(dataset, name = "eallonardo_2013")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (18) YATES 2014                                                     NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "yates_2014.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:11)
dataset <- as_ordinal(dataset, cols = 11)


# Run Analysis ----

run_analysis(dataset, name = "yates_2014")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (19) JELIAZKOV 2013                                                 NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "jeliazkov_2013.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_ordinal(dataset, cols = 1:89)


# Run Analysis ----

run_analysis(dataset, name = "jeliazkov_2013")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (20) GIBB 2015                                                      NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "gibb_2015.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:10)


# Run Analysis ----

run_analysis(dataset, name = "gibb_2015")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (21) THERMAL VENT FAUNA                                             NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "thermal_fauna.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_ordinal(dataset, cols = c(1, 3:8, 13:16))


# Run Analysis ----

run_analysis(dataset, name = "thermal_fauna")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (22) PLANTS ALPS                                                    NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "plant_alps.csv"),
  header    = TRUE,
  row.names = 1
)


# Remove traits with too much NA (> 60 %) ----

dataset <- dataset[ , (colSums(is.na(dataset)) < nrow(dataset) * 0.6)]


# Remove line with too much NA (> 50%) ----

dataset <- dataset[(rowSums(is.na(dataset)) < ncol(dataset) * 0.5), ]


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(18, 28:30))
dataset <- as_ordinal(dataset, cols = c(1:3, 5, 7, 10, 13, 15, 17, 20:27, 31:33), level = FALSE)
dataset <- as_ordinal(dataset, cols = c(4, 6, 8, 9, 11:12, 14, 16, 19))


# Run Analysis ----

run_analysis(dataset, name = "plant_alps")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (23) CHONDRICHTYENS                                                 NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "chondrichtyens_traits.csv"),
  header    = TRUE,
  row.names = 1
)


# Remove traits with too much NA ----

dataset <- dataset[ , (colSums(is.na(dataset)) < 672)]


# Remove line with too much NA ----

dataset <- dataset[(rowSums(is.na(dataset)) < 7), ]


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:7, 11:14))
dataset <- as_categorical(dataset, cols = 8:10)


# Run Analysis ----

run_analysis(dataset, name = "chondrichtyens_traits")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (24) PHYTOPLANKTON                                                  NOT RUN
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "phyto_plankton.csv"),
  header    = TRUE,
  row.names = 1
)


# Remove traits with too much NA ----

dataset <- dataset[ , (colSums(is.na(dataset)) < nrow(dataset) * 0.6)]


# Remove line with too much NA ----

dataset <- dataset[(rowSums(is.na(dataset)) < ncol(dataset) * 0.5), ]


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:3, 5:7))
dataset <- as_categorical(dataset, cols = c(4, 8:15))


# Run Analysis ----

run_analysis(dataset, name = "phyto_plankton")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (25) VILLEGER 2012                                                 
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "villeger_2012.csv"),
  header    = TRUE,
  row.names = 1
)


# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "villeger_2012")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (26) CARVALHO 2015                                                 
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "carvalho_2015.csv"),
  header    = TRUE,
  row.names = 1
)

# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 1)
dataset <- as_categorical(dataset, cols = 2:ncol(dataset))


# Run Analysis ----

run_analysis(dataset, name = "carvalho_2015")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (27) CHARBONNIER 2016                                                 
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "charbonnier_2016.csv"),
  header    = TRUE,
  row.names = 1
)

# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 7:10)
dataset <- as_categorical(dataset, cols = 1:6)


# Run Analysis ----

run_analysis(dataset, name = "charbonnier_2016")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (28) CHMURA 2016                                                 
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "chmura_2016.csv"),
  header    = TRUE,
  row.names = 1
)

# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = c(1:3, 5:ncol(dataset)))
dataset <- as_categorical(dataset, cols = 4)


# Run Analysis ----

run_analysis(dataset, name = "chmura_2016")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (29) FRIED 2012                                                 
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "fried_2012.csv"),
  header    = TRUE,
  row.names = 1
)

# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 4:6)
dataset <- as_categorical(dataset, cols = 1:3)
dataset <- as_ordinal(dataset, cols = 7:ncol(dataset))

# Run Analysis ----

run_analysis(dataset, name = "fried_2012")
rm(list = "dataset")



#' @header *********************************************************************
#' @dataset (30) PAVOINE 2011                                                 
#' @header *********************************************************************



# Read Dataset ----

dataset <- read.csv2(
  file      = here::here("data", "pavoine_2011.csv"),
  header    = TRUE,
  row.names = 1
)

# Traits Preparation and Categorization ----

dataset <- as_numerical(dataset, cols = 8:11)
dataset <- as_categorical(dataset, cols = 1:7)
dataset <- as_ordinal(dataset, cols = 12:14)

# Run Analysis ----

run_analysis(dataset, name = "pavoine_2011")
rm(list = "dataset")

