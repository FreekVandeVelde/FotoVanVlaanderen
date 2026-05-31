#######################################
# Heranalyse data Foto van Vlaanderen #
#######################################

#------------------------------------------------------------------------------
#Freek Van de Velde
#2026-05-30
#------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Packages laden en instellingen goed zetten
# -----------------------------------------------------------------------------

#set working directory#
setwd("C:/Users/u0039016/OneDrive - KU Leuven/Documents/DATA/onderzoek/mcl/FotoVanVlaanderen")

library(dplyr)
library(psych)
library(MASS)
library(partykit)
library(randomForest)
library(glmnet)
library(effects)
library(car)
library(grid)

citation("dplyr")
citation("psych")
citation("car")
citation("MASS")
citation("glmnet")
citation("partykit")
citation("randomForest")
citation("grid")

rm(list = ls())
options(scipen = 10)
set.seed(1979)
options(max.print = 10000)

# -----------------------------------------------------------------------------
# Dataset inlezen en datavoorbewerking
# -----------------------------------------------------------------------------


FVV <- read.csv("PROF252792_VRT Foto van Vlaanderen 2025_datafile.csv", sep=";", stringsAsFactors = TRUE)
str(FVV)
describe(FVV)

#check opleidingsniveau
table(FVV$qprofxresp)

#Gender als factor met string-based levels
FVV$gender <- as.factor(ifelse(FVV$gender == 1, "man", "vrouw"))
table(FVV$gender)
FVV$geslacht <- FVV$gender #correcter, want binair in deze dataset

#Categorische leeftijdsintervalvariabele
table(FVV$dumleeftijd)
FVV <- FVV %>%
mutate(
  leeftijd_cat = factor(
    dumleeftijd,
    levels = c(1, 2, 3, 4, 5, 6, 7),
    labels = c(
      "12-17",
      "18-24",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65+"
    ),
    ordered = FALSE
  )
)
FVV$leeftijd_cat <- relevel(FVV$leeftijd_cat, ref = "35-44")

#Eerst even de miscoderingen door verkeerde spelling rechtzetten#
FVV <- FVV %>%
mutate(
  open_herkomst = paste(qr1_98_other, qr2_98_other, sep = " "),
  
  is_oosteuropa_correctie = grepl(
    "albanees|alban|kosov|bosn",
    open_herkomst,
    ignore.case = TRUE
  ),
  
  is_mena_correctie = grepl(
    "aghantan|afghan|lebanes",
    open_herkomst,
    ignore.case = TRUE
  ),
  
  herkomstregio_oosteuropa = if_else(
    is_oosteuropa_correctie,
    1,
    herkomstregio_oosteuropa
  ),
  
  herkomstregio_noordafrika_middenoosten = if_else(
    is_mena_correctie,
    1,
    herkomstregio_noordafrika_middenoosten
  ),
  
  herkomstregio_westeuropa = if_else(
    is_mena_correctie,
    0,
    herkomstregio_westeuropa
  ),
  
  herkomstregio_andere = if_else(
    is_oosteuropa_correctie | is_mena_correctie,
    0,
    herkomstregio_andere
  )
)

#Nieuwe variabele voor macro-regio Herkomst:

FVV <- FVV %>%
  mutate(
    Herkomst = case_when(
      
      # Pure Belg
      herkomstregio_belgië == 1 &
        herkomstregio_westeuropa == 0 &
        herkomstregio_zuideuropa == 0 &
        herkomstregio_oosteuropa == 0 &
        herkomstregio_noordafrika_middenoosten == 0 &
        herkomstregio_andere == 0 &
        herkomstregio_onbekend == 0 ~ "Belg",
      
      # Prioriteitsvolgorde
      herkomstregio_noordafrika_middenoosten == 1 ~ "NoordAfrika-MiddenOosten",
      
      herkomstregio_oosteuropa == 1 ~ "Oost-Europees",
      
      herkomstregio_zuideuropa == 1 ~ "Zuid-Europees",
      
      herkomstregio_andere == 1 ~ "Andere",
      
      herkomstregio_westeuropa == 1 ~ "West-Europees",
      
      TRUE ~ "Unknown"
    ),
    
    Herkomst = factor(
      Herkomst,
      levels = c(
        "Belg",
        "West-Europees",
        "Oost-Europees",
        "Zuid-Europees",
        "NoordAfrika-MiddenOosten",
        "Andere",
        "Unknown"
      )
    )
  )


table(FVV$Herkomst)

#extra check#
table(FVV$dumnieuwevlam) #aantal mensen van buitenlandse origine n=310
which(is.na(FVV$dumnieuwevlam))
dplyr::select(FVV, dumnieuwevlam, Herkomst, gender, leeftijd_cat, qprofxresp, qr1_98_other, qr2_98_other)[which(is.na(FVV$dumnieuwevlam)),]

# -----------------------------------------------------------------------------
# COMPOSIETVARIABELE AANMAKEN
# -----------------------------------------------------------------------------
# Doel:
# - Alle items herschalen naar 0–100
# - 100 = meest tolerant / egalitair / inclusief
# - Niet-inhoudelijke antwoorden worden NA

# Helper functions encapsulate the three scale types used in the survey.

rescale_q10 <- function(x, reverse = FALSE) {
  # 1–7 agreement.  99 = don't know, 100 = prefer not to say  → NA
  x[x %in% c(99, 100)] <- NA
  if (reverse) (7 - x) / 6 * 100 else (x - 1) / 6 * 100
}

rescale_q11 <- function(x) {
  # 3-option scenarios.  1 = no problem (tolerant) → 100
  #                      2 = not OK (intolerant)   →   0
  #                      3 = no opinion            →  50  (neutral midpoint)
  #                      99 = prefer not to say    →  NA
  x[x == 99] <- NA
  dplyr::case_when(
    x == 1 ~ 100,
    x == 2 ~   0,
    x == 3 ~  50,
    TRUE   ~ NA_real_
  )
}

rescale_q12 <- function(x, reverse = FALSE) {
  # 1–3 scale between two opposing statements.
  if (reverse) (3 - x) / 2 * 100 else (x - 1) / 2 * 100
}

# Apply rescaling — each item annotated with its direction.
FVV <- FVV %>%
  mutate(
    # --- LGBTQ items -------------------------------------------------------
    # Q10 (1–7 agreement)
    t_q10_1  = rescale_q10(q10grid_1),                  # OK that LGB couples adopt
    t_q10_2  = rescale_q10(q10grid_2),                  # LGBTQI+ can be who they are
    t_q10_3  = rescale_q10(q10grid_3, reverse = TRUE),  # only 2 genders        — reverse
    t_q10_4  = rescale_q10(q10grid_4, reverse = TRUE),  # problem w/ gay friend — reverse
    t_q10_5  = rescale_q10(q10grid_5, reverse = TRUE),  # problem w/ trans frnd — reverse
    t_q10_6  = rescale_q10(q10grid_6, reverse = TRUE),  # ban sex-change surg.  — reverse
    
    # Q11 (no problem / not OK / no opinion)
    t_q11_1  = rescale_q11(q11grid_1),                  # sex-ed at school
    t_q11_2  = rescale_q11(q11grid_2),                  # classmate w/ two papas/mamas
    t_q11_3  = rescale_q11(q11grid_3),                  # die/hen pronouns
    t_q11_4  = rescale_q11(q11grid_4),                  # Pride parade
    t_q11_5  = rescale_q11(q11grid_5),                  # colleague who was a woman
    
    # Q12 (1–3) — LGBTQ-relevant items
    t_q12_5  = rescale_q12(q12grid_5),                  # trans attention   (3=good)
    t_q12_7  = rescale_q12(q12grid_7, reverse = TRUE),  # LGBTQI+ media     (1=too little, tolerant)
    
    # --- Gender-role items (Q12) ------------------------------------------
    t_q12_3  = rescale_q12(q12grid_3, reverse = TRUE),  # breadwinners       (1=both, egalitarian)
    t_q12_4  = rescale_q12(q12grid_4, reverse = TRUE),  # childcare/housework (1=share, egalitarian)
    t_q12_12 = rescale_q12(q12grid_12),                 # relationship power  (3=equal)
    t_q12_13 = rescale_q12(q12grid_13, reverse = TRUE),  # feminism            (1=positive)
    t_q11_11 = rescale_q12(q12grid_11, reverse = TRUE) # divorce     (1=always ok)
  )


# COMPOSITE SCORES

# A respondent's composite = mean of the rescaled items in that domain.
# Require at least 70 % of items answered, else the composite is NA.

lgbtq_vars  <- c("t_q10_1", "t_q10_2", "t_q10_3", "t_q10_4", "t_q10_5", "t_q10_6",
                 "t_q11_1", "t_q11_2", "t_q11_3", "t_q11_4", "t_q11_5",
                 "t_q12_5", "t_q12_7")

gender_vars <- c("t_q12_3", "t_q12_4", "t_q12_12", "t_q12_13", "t_q11_11")

row_mean_if_enough <- function(data, vars, min_frac = 0.70) {
  m <- as.matrix(data[, vars])
  answered <- rowSums(!is.na(m))
  applicable <- rowSums(!is.na(m) | is.na(m))  # default: all selected items
  
  out <- rowMeans(m, na.rm = TRUE)
  out[answered / length(vars) < min_frac] <- NA_real_
  out
}

FVV$lgbtq_composite  <- row_mean_if_enough(FVV, lgbtq_vars)
FVV$gender_composite <- row_mean_if_enough(FVV, gender_vars)

# OVERKOEPELENDE DIVERSITY / GENDER-LGBTQ COMPOSIET

diversity_vars <- c(
  
  # LGBTQ attitudes (Q10)
  "t_q10_1",
  "t_q10_2",
  "t_q10_3",
  "t_q10_4",
  "t_q10_5",
  "t_q10_6",
  
  # Concrete scenario's (Q11)
  "t_q11_1",
  "t_q11_2",
  "t_q11_3",
  "t_q11_4",
  
  # Alleen 18+ maar inhoudelijk sterk relevant
  "t_q11_5",
  
  # Gender / transgender attitudes (Q12)
  "t_q12_3",
  "t_q12_4",
  "t_q12_5",
  "t_q12_7",
  "t_q12_12",
  "t_q12_13",
  "t_q11_11"
)


# Functie: gemiddelde indien voldoende antwoorden

row_mean_if_enough <- function(data, vars, min_frac = 0.70) {
  
  m <- as.matrix(data[, vars])
  
  answered <- rowSums(!is.na(m))
  
  out <- rowMeans(m, na.rm = TRUE)
  
  out[answered / length(vars) < min_frac] <- NA_real_
  
  out
}

# Composietscore

FVV$diversity_gender_lgbtq <- row_mean_if_enough(
  FVV,
  diversity_vars,
  min_frac = 0.70
)

#Check interne consistentie#

diversity_gender_lgbtq_vars <- c(
  "t_q10_1",
  "t_q10_2",
  "t_q10_3",
  "t_q10_4",
  "t_q10_5",
  "t_q10_6",
  "t_q11_1",
  "t_q11_2",
  "t_q11_3",
  "t_q11_4",
  "t_q11_5",
  "t_q12_3",
  "t_q12_4",
  "t_q12_5",
  "t_q12_7",
  "t_q12_12",
  "t_q12_13"
)


psych::alpha(FVV[, diversity_gender_lgbtq_vars], check.keys = TRUE)


# -----------------------------------------------------------------------------
# VERIFICATION — reproduce the group-level means from the analysis report
# -----------------------------------------------------------------------------
FVV %>%
  group_by(Herkomst) %>%
  summarise(
    n            = n(),
    lgbtq_mean   = round(mean(lgbtq_composite,  na.rm = TRUE), 1),
    gender_mean  = round(mean(gender_composite, na.rm = TRUE), 1),
    diversitygenderlgbtq_mean = round(mean(diversity_gender_lgbtq, na.rm = TRUE), 1),
    .groups      = "drop"
  )

#-------------------------------------------------------------------------------
#INFERENTIEEL-STATISTISCHE ANALYSE
#------------------------------------------------------------------------------

#Kortere labels voor ctree

FVV <- FVV %>%
  mutate(
    HerkomstRegio = factor(
      Herkomst,
      levels = c(
        "Belg",
        "West-Europees",
        "Oost-Europees",
        "Zuid-Europees",
        "NoordAfrika-MiddenOosten",
        "Andere",
        "Unknown"
      ),
      labels = c(
        "Belg",
        "W-Eur",
        "O-Eur",
        "Z-Eur",
        "MENA",
        "Ander",
        "Onbekend"
      )
    )
  )

table(FVV$Herkomst, FVV$HerkomstRegio)


summary(lm(FVV$qprofxresp ~ Herkomst, data=FVV)) #model zonder multivariate controle
boxplot(qprofxresp ~ Herkomst, data=FVV, ylab="Opleidingsniveau", las=1)

table(FVV$gender, FVV$Herkomst)

table(FVV$leeftijd_cat)

summary(FVV$diversity_gender_lgbtq)

summary(lm_FVV <- lm(FVV$diversity_gender_lgbtq ~ Herkomst + geslacht + leeftijd_cat + qprofxresp, data=FVV))

plot(allEffects(lm_FVV), colors="black", main="", ylab="Gendertolerantie")

plot(effect("HerkomstRegio", lm(FVV$diversity_gender_lgbtq ~ HerkomstRegio + geslacht + leeftijd_cat + qprofxresp, data=FVV)), colors="black", main="", xlab="Herkomstregio", ylab="Gendertolerantie")

dev.off()

#tests 
vif(lm_FVV) #multicollineariteit
#png("DiagnostischePlots.png", width = 2400, height = 2400, res = 300)
#par(mfrow = c(2, 2),
   # mar = c(4, 4, 2, 1))
#plot(lm_FVV)

par(mfrow = c(2,2)) #lineariteit, homoskedasticiteit
plot(lm_FVV)
dev.off()

par(mfrow = c(1,1))
influencePlot(lm_FVV)

summary(step_lm_FVV <- stepAIC(lm_FVV, direction="both"))

droplevels(dplyr::select(FVV, diversity_gender_lgbtq, Herkomst, gender, leeftijd_cat, qprofxresp))[1811,]
droplevels(dplyr::select(FVV, diversity_gender_lgbtq, Herkomst, gender, leeftijd_cat, qprofxresp))[1521,]


#Conditional Inference Tree-----

FVV_ctree <- FVV %>%
  filter(
    !is.na(diversity_gender_lgbtq),
    !is.na(HerkomstRegio),
    !is.na(geslacht),
    !is.na(leeftijd_cat),
    !is.na(qprofxresp)
  ) %>%
  mutate(
    herkomstregio = droplevels(as.factor(HerkomstRegio)),
    geslacht = droplevels(as.factor(geslacht)),
    leeftijd_cat = droplevels(as.factor(leeftijd_cat)),
    diversity_gender_lgbtq = as.numeric(diversity_gender_lgbtq),
    opleidingsniveau = droplevels(as.factor(qprofxresp))
  )

ct_FVV <- ctree(
  diversity_gender_lgbtq ~ HerkomstRegio + geslacht + leeftijd_cat + opleidingsniveau,
  control = ctree_control(
    mincriterion = 0.95,
    minsplit = 20,
    maxdepth = 3
  ),
  data = FVV_ctree
)

#mincriterion → stricter split significance
#minsplit → minimum observations before splitting
#maxdepth → maximum tree depth

plot(ct_FVV, gp = gpar(fontsize = 9))

#with crossvalidation

train_index <- sample(1:nrow(FVV_ctree), 0.8 * nrow(FVV_ctree))

train_data <- FVV_ctree[train_index, ]
test_data  <- FVV_ctree[-train_index, ]

ct_FVV_crossv <- ctree(
    diversity_gender_lgbtq ~ herkomstregio + geslacht + leeftijd_cat + opleidingsniveau,
    control = ctree_control(
      mincriterion = 0.95,
      minsplit = 20,
      maxdepth = 3
    ),
    data = train_data
  )

plot(ct_FVV_crossv, gp = gpar(fontsize = 5))

# Predictions
pred <- predict(ct_FVV_crossv, newdata = test_data)

# Accuracy
cor(pred, test_data$diversity_gender_lgbtq, use = "complete.obs")

#Random forest-----

rf_FVV <- randomForest(
  diversity_gender_lgbtq ~ herkomstregio + geslacht + leeftijd_cat + opleidingsniveau,
  data = FVV_ctree,
  ntree = 5000,
  importance = TRUE
)

print(rf_FVV)

importance(rf_FVV)

varImpPlot(rf_FVV)

varImpPlot(rf_FVV, type = 1, main = "Variabele-belang (%IncMSE)")

#Lasso Regression-----

# 1. Analyse-dataset maken zonder missings
FVV_lasso <- FVV %>%
  filter(
    !is.na(diversity_gender_lgbtq),
    !is.na(leeftijd_cat),
    !is.na(gender),
    !is.na(HerkomstRegio),
    !is.na(qprofxresp)
  ) %>%
  mutate(
    leeftijd_cat = as.factor(leeftijd_cat),
    gender = as.factor(gender),
    Herkomst = as.factor(Herkomst),
    Opleidingsniveau = as.factor(qprofxresp)
  )

# 2. Design matrix maken

x <- model.matrix(
  diversity_gender_lgbtq ~ leeftijd_cat + gender + Herkomst + Opleidingsniveau,
  data = FVV_lasso
)[, -1]

# 3. Uitkomstvariabele
y <- FVV_lasso$diversity_gender_lgbtq

# 4. Cross-validated LASSO

lasso_FVV <- cv.glmnet(
  x = x,
  y = y,
  alpha = 1,          
  family = "gaussian",
  standardize = TRUE
)

# 5. Plot cross-validatie
plot(lasso_FVV)

# 6. Beste lambda's
lasso_FVV$lambda.min
lasso_FVV$lambda.1se

# 7. Coëfficiënten bij lambda.min
coef(lasso_FVV, s = "lambda.min")

# 8. Conservatievere coëfficiënten bij lambda.1se
coef(lasso_FVV, s = "lambda.1se")

#Wie valt onder missingness?

missingness_tabel_leeftijd <- rbind(table(droplevels(filter(FVV, !is.na(diversity_gender_lgbtq)))$leeftijd_cat), table(FVV$leeftijd_cat))
rownames(missingness_tabel_leeftijd) <- c("missing", "volledige dataset")
missingness_tabel_leeftijd

missingness_tabel_herkomst <- rbind(table(droplevels(filter(FVV, !is.na(diversity_gender_lgbtq)))$HerkomstRegio), table(FVV$HerkomstRegio))
rownames(missingness_tabel_herkomst) <- c("missing", "volledige dataset")
missingness_tabel_herkomst

missingness_tabel_opleidingsniveau <- rbind(table(droplevels(filter(FVV, !is.na(diversity_gender_lgbtq)))$qprofxresp), table(FVV$qprofxresp))
rownames(missingness_tabel_opleidingsniveau) <- c("missing", "volledige dataset")
missingness_tabel_opleidingsniveau

missingness_tabel_geslacht <- rbind(table(droplevels(filter(FVV, !is.na(diversity_gender_lgbtq)))$geslacht), table(FVV$geslacht))
rownames(missingness_tabel_geslacht) <- c("missing", "volledige dataset")
missingness_tabel_geslacht

with(droplevels(filter(FVV, !is.na(diversity_gender_lgbtq))), table(leeftijd_cat, geslacht))
with(FVV, table(leeftijd_cat, geslacht))

#------------------------------------------------------------------------------
#Moslimlanden
#------------------------------------------------------------------------------
#Veranderen de regressiecoëfficiënten als de landen met een moslimmeerderheid (bijv. Sierra Leone, Indonesië ...) bij de MENA-groep geteld worden? #

levels(FVV$qr1_98_other)

FVV <- FVV %>%
  mutate(
    moslimregio = case_when(
      
      HerkomstRegio == "MENA" ~ "ja",
      
      grepl(
        "aghantan|afghan|albanees|alban|indones|iran|iraq|iraakees|lebanes|marok|sierra",
        qr1_98_other,
        ignore.case = TRUE
      ) ~ "ja",
      
      TRUE ~ "nee"
    ),
    
    moslimregio = factor(
      moslimregio,
      levels = c("nee", "ja")
    )
  )

table(FVV$moslimregio, FVV$HerkomstRegio)

FVV$HerkomstRegioPlus <- factor(ifelse(FVV$moslimregio == "ja" | FVV$HerkomstRegio == "MENA", "MENAplus", as.character(FVV$HerkomstRegio)), levels=c("Belg", "W-Eur","Z-Eur","O-Eur", "MENAplus", "Ander", "Onbekend"))

table(FVV$HerkomstRegio, FVV$HerkomstRegioPlus)

summary(moslim_lm <- lm(FVV$diversity_gender_lgbtq ~ HerkomstRegioPlus + gender + leeftijd_cat + qprofxresp, data=FVV))

vif(moslim_lm) #multicollineariteit
#png("DiagnostischePlots.png", width = 2400, height = 2400, res = 300)
#par(mfrow = c(2, 2),
# mar = c(4, 4, 2, 1))
#plot(lm_FVV)

par(mfrow = c(2,2)) #lineariteit, homoskedasticiteit
plot(moslim_lm)
dev.off()

par(mfrow = c(1,1))
influencePlot(moslim_lm)

#------------------------------------------------------------------------------
#END
#------------------------------------------------------------------------------
