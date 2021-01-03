library(dplyr) # for data wrangling
library(meta) # for meta-analysis

# loading datasets and converting into factors
distancing_dataset <- read.csv("C:/Users/Louie Florendo Dy/Downloads/Projects/COVID 2019 Research/Models/Lancet Systematic Review on NPIs/lancet-meta-analysis-verification/distancing.csv") %>%
  mutate(respirator = as.factor(respirator))#,
         #distancing = as.factor(distancing))
colnames(distancing_dataset)[1] <- "study"

face_mask_dataset <- read.csv("C:/Users/Louie Florendo Dy/Downloads/Projects/COVID 2019 Research/Models/Lancet Systematic Review on NPIs/lancet-meta-analysis-verification/face_mask.csv") %>%
  mutate(respirator = as.factor(respirator))
colnames(face_mask_dataset)[1] <- "study"

eye_protection_dataset <- read.csv("C:/Users/Louie Florendo Dy/Downloads/Projects/COVID 2019 Research/Models/Lancet Systematic Review on NPIs/lancet-meta-analysis-verification/eye_protection.csv") %>%
  mutate(respirator = as.factor(respirator))
colnames(eye_protection_dataset)[1] <- "study"

# meta-analysis

# distancing subgroup distance
ma.distancing.distance <- metabin(ne, Ne, nc, Nc,
                                  studlab = paste(study),
                                  data = distancing_dataset,
                                  sm = "RR",
                                  comb.fixed = FALSE,
                                  comb.random = TRUE,
                                  byvar = distance,
                                  bylab = "Distance",
                                  overall = TRUE,
                                  overall.hetstat = TRUE,
                                  prediction = TRUE)

pdf(file = "ma_distancing_distance.pdf", width = 11, height = 13)
ma_distancing_distance <- forest(ma.distancing.distance,
                                 title = "Effect of Physical Distancing By Distance Subgroup",
                                 lab.e = "Further Dist.",
                                 lab.c = "Shorter Dist.",
                                 leftcols = c("studlab", "distance", "event.e", "n.e", "event.c", "n.c"))
dev.off()

# distancing subgroup respirator
ma.distancing.respirator <- metabin(ne, Ne, nc, Nc,
                                    studlab = paste(study),
                                    data = distancing_dataset,
                                    sm = "RR",
                                    comb.fixed = FALSE,
                                    comb.random = TRUE,
                                    byvar = respirator,
                                    bylab = "N95 Mask",
                                    overall = TRUE,
                                    overall.hetstat = TRUE,
                                    prediction = TRUE)

pdf(file = "ma_distancing_respirator.pdf", width = 11, height = 13)
ma_distancing_respirator <- forest(ma.distancing.respirator,
                                   title = "Effect of Physical Distancing By N95 Subgroup",
                                   lab.e = "Further Dist.",
                                   lab.c = "Shorter Dist.",
                                   leftcols = c("studlab", "respirator", "event.e", "n.e", "event.c", "n.c"))
dev.off()

# face mask subgroup respirator
ma.mask.respirator <- metabin(ne, Ne, nc, Nc,
                              studlab = paste(study),
                              data = face_mask_dataset,
                              sm = "RR",
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              byvar = respirator,
                              bylab = "N95 Mask",
                              overall = TRUE,
                              overall.hetstat = TRUE,
                              prediction = TRUE)

pdf(file = "ma_mask_respirator.pdf", width = 11, height = 13)
ma_mask_respirator <- forest(ma.mask.respirator,
                             title = "Effect of Face Mask By N95 Subgroup",
                             lab.e = "Face Mask",
                             lab.c = "No Face Mask",
                             leftcols = c("studlab", "respirator", "event.e", "n.e", "event.c", "n.c"))
dev.off()

# face shield subgroup respirator
ma.shield.respirator <- metabin(ne, Ne, nc, Nc,
                                studlab = paste(study),
                                data = eye_protection_dataset,
                                sm = "RR",
                                comb.fixed = FALSE,
                                comb.random = TRUE,
                                byvar = respirator,
                                bylab = "N95 Mask",
                                overall = TRUE,
                                overall.hetstat = TRUE,
                                prediction = TRUE)

pdf(file = "ma_shield_respirator.pdf", width = 11, height = 13)
ma_shield_respirator <- forest(ma.shield.respirator,
                               title = "Effect of Face Shield By N95 Subgroup",
                               lab.e = "Face Shield",
                               lab.c = "No Face Shield",
                               leftcols = c("studlab", "respirator", "event.e", "n.e", "event.c", "n.c"))
dev.off()