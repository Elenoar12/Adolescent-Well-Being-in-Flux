# read packages
library(dplyr)
library(lme4)
library(lmerTest)
library(purrr)
library(broom.mixed)
library(performance)
library(sjPlot)
library(parameters)


## PREPARATION ------------------------------------------------------------

df <- read.csv("hbsc_mlvl_data_per_sy.csv")

df <- df %>%
  mutate(
    country = factor(countryname),
    wave    = as.numeric(surveyyear),
    c_wave  = wave - mean(wave, na.rm = TRUE),        # centered wave
    c_wave2 = c_wave^2,                               # centered wave squared
    ct      = interaction(country, wave, drop = TRUE),# country × wave Cluster
    HB      = factor(Label),                          # Health-Behavior-Profile
    HB      = relevel(HB, ref = "Low risk"),          # reference profile
    SES     = scale(fas),                                    # SES-Proxy
    sex     = factor(sex),                            
    age    = scale(age),
    Gini_c    = scale(gini_disp),
    HDI_c     = scale(hdi),
    lifesat = scale(lifesat),
    ache = scale(ache),
    feeling=scale(feeling),
    health=scale(health)
  )

## DATA ANALYSIS -------------------------------------------------

outcomes <-c("lifesat","ache","feeling","health")

## Step 0 and 1 
results <- list()
for (y in outcomes) {
  
  message("Running models for: ", y)
  
  # Model formulas
  f_null      <- as.formula(paste0(y, " ~ 1 + (1 | country) + (1 | country:wave)")) # Step 0 get ICC
  f_null2     <- as.formula(paste0(y, " ~ HB + c_wave + (1 | country) + (1 | country:wave)")) # Step 0 get ICC with HB + wave (decreases the between-country-variance?)
  f_linear    <- as.formula(paste0(y, " ~ HB + age + sex + SES + c_wave + (1 | country) + (1 | country:wave)")) # Step 1: test linear effect on outcome but without random slope
  f_quadratic <- as.formula(paste0(y, " ~ HB + age + sex + SES + c_wave + c_wave2 + (1 | country) + (1 | country:wave)")) # Step 1: test quadratic effect on outcome but without random slope
  
  # Fit models
  m_null      <- try(lmer(f_null,      data = df), silent=FALSE)
  m_null2     <- try(lmer(f_null2,     data = df), silent=FALSE)
  m_linear    <- try(lmer(f_linear,    data = df, REML = FALSE), silent=FALSE)
  m_quadratic <- try(lmer(f_quadratic, data = df, REML = FALSE), silent=FALSE)
  
  # Store everything
  results[[y]] <- list(
    null      = m_null,
    null2     = m_null2,
    linear    = m_linear,
    quadratic = m_quadratic,
    icc_null      = icc(m_null),
    icc_null2     = icc(m_null2),
    anova_lin_quad = anova(m_linear, m_quadratic)
  )
  
  # Print model tables to console (no HTML output)
  print(
    tab_model(
      m_null, m_null2, m_linear, m_quadratic,
      show.icc = TRUE,
      show.re.var = TRUE,
      dv.labels = c(
        paste0(y, " – Null"),
        paste0(y, " – Null + HB"),
        paste0(y, " – Linear"),
        paste0(y, " – Quadratic")
      )
    )
  )
  
  # Optional: print ICCs and model comparison
  message("ICC Null:\n"); print(results[[y]]$icc_null)
  message("ICC Null+HB:\n"); print(results[[y]]$icc_null2)
  message("Linear vs Quadratic:\n"); print(results[[y]]$anova_lin_quad)
}



## Step 2: Change of the effect of hb-profile over time (does adding hb x wave improve fit over the main effect of hb only)?

### LIFESAT (linear)
lifesat_noint <- lmer(
  lifesat ~ HB + c_wave + age + sex + SES + 
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

lifesat_waveint <- lmer(
  lifesat ~ HB * c_wave + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

lifesat_lrt <- anova(lifesat_noint, lifesat_waveint)
lifesat_lrt

### ACHE (linear)
ache_noint <- lmer(
  ache ~ HB + c_wave + age + sex + SES + 
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

ache_waveint <- lmer(
  ache ~ HB * c_wave + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

ache_lrt <- anova(ache_noint, ache_waveint)
ache_lrt

### FEELING (quadratic)
feeling_noint <- lmer(
  feeling ~ HB + c_wave + c_wave2 + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

feeling_waveint <- lmer(
  feeling ~ HB * c_wave + c_wave2 + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

feeling_lrt <- anova(feeling_noint, feeling_waveint)
feeling_lrt

### HEALTH (quadratic)
health_noint <- lmer(
  health ~ HB + c_wave + c_wave2 + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

health_waveint <- lmer(
  health ~ HB * c_wave + c_wave2 + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)

health_lrt <- anova(health_noint, health_waveint)
health_lrt


#save(lifesat_noint, lifesat_waveint,
     #ache_noint, ache_waveint,
     #feeling_noint, feeling_waveint,
     #health_noint, health_waveint,
     #lifesat_lrt, ache_lrt, feeling_lrt, health_lrt,
     #file = "waveint_models.RData")

## Step 3: Does the hb-effect vary between countries (random slope for hb)?
lifesat_HBcountry <- lmer(
  lifesat ~ HB * c_wave + c_wave2 + age + sex + SES +
    (1 + HB | country) + (1 | ct),
  data = df, REML = FALSE
)

ache_HBcountry <- lmer(
  ache ~ HB * c_wave + c_wave2 + age + sex + SES +
    (1 + HB | country) + (1 | ct),
  data = df, REML = FALSE
)

feeling_HBcountry <- lmer(
  feeling ~ HB * c_wave + c_wave2 + age + sex + SES +
    (1 + HB | country) + (1 | ct),
  data = df, REML = FALSE
)

health_HBcountry <- lmer(
  health ~ HB * c_wave + c_wave2 + age + sex + SES +
    (1 + HB | country) + (1 | ct),
  data = df, REML = FALSE
)

## GINI
health_gini <- lmer(
  health ~ HB * c_wave + c_wave2 +
    Gini_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(health_gini)


feeling_gini <- lmer(
  feeling ~ HB * c_wave + c_wave2 +
    Gini_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(feeling_gini)

ache_gini <- lmer(
  ache ~ HB * c_wave + 
    Gini_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(ache_gini)

lifesat_gini <- lmer(
  lifesat ~ HB * c_wave +
    Gini_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(lifesat_gini)

## HDI
health_HDI <- lmer(
  health ~ HB * c_wave + c_wave2 +
    HDI_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(health_HDI)


feeling_HDI <- lmer(
  feeling ~ HB * c_wave + c_wave2 +
    HDI_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(feeling_HDI)

ache_HDI <- lmer(
  ache ~ HB * c_wave + 
    HDI_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(ache_HDI)

lifesat_HDI <- lmer(
  lifesat ~ HB * c_wave +
    HDI_c*HB + age + sex + SES +
    (1 | country) + (1 | ct),
  data = df, REML = FALSE
)
summary(lifesat_HDI)

#save(lifesat_gini, lifesat_HDI,
     #ache_gini, ache_HDI,
     #feeling_gini, feeling_HDI,
     #health_gini, health_HDI,
     #file = "countryind_models.RData")
