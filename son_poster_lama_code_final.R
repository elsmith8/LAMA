library(tidyverse)
library(data.table)
library(RSocrata)
library(getPass)
library(janitor)
library(gtsummary)
library(clipr)
library(oddsratio)
library(ggplot2)
library(ggpubr)
library(scales)

options(scipen = 999)

### Acquire data - Log into API
d <- read.socrata("https://health.data.ny.gov/resource/4ny4-j5zv.json?hospital_county='Erie'",
                    email = getPass(msg = "Email: ", noblank = FALSE, forcemask = FALSE),
                    password = getPass(msg = "Password: ", noblank = FALSE, forcemask = FALSE)) 

### Data prep
df <- d %>%
  # Filter out kids
  filter(!age_group == '0 to 17') %>%
  # Make the dependent variable lama
  mutate(lama = ifelse(patient_disposition == 'Left Against Medical Advice',1,0)) %>%
  # Factor independent variables
  mutate(factor_age_group = relevel(factor(age_group), ref = '18 to 29')) %>%
  mutate(factor_gender = relevel(factor(gender), ref = 'M')) %>%
  mutate(factor_race = relevel(factor(race), ref = 'Other Race')) %>%
  mutate(factor_toa = relevel(factor(type_of_admission), ref = 'Urgent')) %>%
  mutate(factor_pmt = relevel(factor(payment_typology_1), ref = 'Department of Corrections')) %>%
  mutate(factor_mdc = relevel(factor(apr_mdc_code), ref = '24')) %>%
  # Select final dataset variables
  select(lama,age_group,gender,race,type_of_admission,
         payment_typology_1,apr_mdc_code,patient_disposition,
         factor_age_group:factor_mdc)

### Overall LAMA rate
overall_lama_rate <- round(sum(df$lama) / nrow(df),3)

#### Visualizations

# Overall
lama_rate <- df %>%
  group_by(lama) %>%
  summarize(n = length(lama)) %>%
  mutate(lama = ifelse(lama == 1,'LAMA','Other')) %>%
  mutate(prcnt = round(n / sum(n),3)*100) 

labs <- paste0(lama_rate$lama, " (", lama_rate$prcnt, "%)")
ggpie(lama_rate, "prcnt", label = labs,
      fill = "lama", color = "white",
      palette = c("#ed8c32","#325ea8"),
      legend = 'right') +
  guides(fill=guide_legend(title="Disposition")) +
  ggtitle('LAMA Rate') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Age
lama_rate_age <-df %>%
  group_by(age_group) %>%
  summarize(n = length(lama),
            n_lama = sum(lama)) %>%
  mutate(prcnt = percent(round(n_lama / n,3))) %>%
  mutate(lbl = ifelse(age_group == '30 to 49','shade','noshade'))

ggbarplot(lama_rate_age,'age_group','prcnt',
          label = TRUE,
          fill = 'lbl',
          palette = c("#325ea8","#ed8c32")) +
  ggtitle('LAMA Rate by Age Group') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = .5)) +
  ylab('') +
  xlab('') +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_line(color = 'white')) +
  rremove('legend')
        
# Race
lama_rate_race <-df %>%
  group_by(race) %>%
  summarize(n = length(lama),
            n_lama = sum(lama)) %>%
  mutate(prcnt = percent(round(n_lama / n,3))) %>%
  mutate(lbl = ifelse(race == 'Black/African American','shade','noshade'))

ggbarplot(lama_rate_race,'race','prcnt',
          label = TRUE,
          fill = 'lbl',
          palette = c("#325ea8","#ed8c32")) +
  ggtitle('LAMA Rate by Race') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = .5)) +
  ylab('') +
  xlab('') +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_line(color = 'white')) +
  rremove('legend')

# Payment
lama_rate_pmt <-df %>%
  group_by(payment_typology_1) %>%
  summarize(n = length(lama),
            n_lama = sum(lama)) %>%
  mutate(prcnt = percent(round(n_lama / n,2))) %>%
  filter(n_lama > 60) %>%
  mutate(lbl = ifelse(payment_typology_1 == 'Medicaid','shade',
               ifelse(payment_typology_1 == 'Self-Pay','shade','noshade')))

ggbarplot(lama_rate_pmt,'payment_typology_1','prcnt',
          label = TRUE,
          fill = "lbl",
          palette = c("#325ea8","#ed8c32")) +
  ggtitle('LAMA Rate by Primary Payer') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = .5)) +
  ylab('') +
  xlab('') +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_line(color = 'white')) +
  rremove('legend')

# Dx
lama_rate_dx <-df %>%
  group_by(apr_mdc_code) %>%
  summarize(n = length(lama),
            n_lama = sum(lama)) %>%
  mutate(prcnt_lama = round(n_lama / n,3)) %>%
  mutate(prcnt_admits = round(n / nrow(df),3)) %>%
  mutate(color_key = ifelse(apr_mdc_code == 20,'Substance Use Disorders','Other'))
  
ggscatter(lama_rate_dx,'prcnt_lama','prcnt_admits',color = 'color_key',
          palette = c("#325ea8","#ed8c32"),legend.title = 'MDC', size = 3) +
  ggtitle('LAMA Rate by Major Diagnostic Category') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = .5)) +
  xlab('Percent of Discharges') +
  ylab('LAMA Rate') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)
  
### Descriptive statistics and univariable regressions
  # Descriptive statistics
  desc_stats <- df %>%
    select(age_group,gender,race,type_of_admission,payment_typology_1,apr_mdc_code) %>%
    mutate(apr_mdc_code = as.character(apr_mdc_code)) %>%
    pivot_longer(cols = 1:6,
                 names_to = 'variable',
                 values_to = 'value') %>%
    group_by(variable,value) %>%
    summarize(n = length(value)) %>%
    group_by(variable) %>%
    mutate(total = sum(n)) %>%
    mutate(prcnt = round(n / total,2)) %>%
    mutate(key = ifelse(variable == 'apr_mdc_code','mdc',
                 ifelse(variable == 'payment_typology_1','pmt',
                 ifelse(variable == 'type_of_admission','toa',variable))))

  # LAMA rates
  lama_rates <- df %>%
    select(lama,age_group,gender,race,type_of_admission,payment_typology_1,apr_mdc_code) %>%
    mutate(apr_mdc_code = as.character(apr_mdc_code)) %>%
    pivot_longer(cols = 2:7,
                 names_to = 'variable',
                 values_to = 'value') %>%
    group_by(variable,value) %>%
    summarize(n = length(value),
              lama_n = sum(lama)) %>%
    mutate(lama = round(lama_n / n,2)) %>%
    mutate(key = ifelse(variable == 'apr_mdc_code','mdc',
                 ifelse(variable == 'payment_typology_1','pmt',
                 ifelse(variable == 'type_of_admission','toa',variable)))) %>%
    select(key,value,lama) %>%
    data.frame() %>%
    rename('lama_rate' = 4)

  # Univariable models
  models <- data.frame(c('lama ~ factor_age_group',
                         'lama ~ factor_gender',
                         'lama ~ factor_race',
                         'lama ~ factor_toa',
                         'lama ~ factor_pmt',
                         'lama ~ factor_mdc')) %>%
    rename('model_vars' = 1)

  for(i in models$model_vars){
    model_name <- paste0('uni_',word(i,3,sep = ' '))
    
    uni <- glm(i, family = binomial, data = df)
    
    assign(model_name,uni)
    
  }
  
    # Clean up
  rm(i,model_name,models,uni)
  gc()
  
### OMNIBUS TESTS
  
  ## Run Wald Chi Square test on each univariable regression to see which variables are significant
  library(aod)
  
  # Age Group
  summary(uni_factor_age_group)
  wald.test(Sigma = vcov(uni_factor_age_group), b = coef(uni_factor_age_group), Terms = 2:4)
  # Gender
  summary(uni_factor_gender)
  wald.test(Sigma = vcov(uni_factor_gender), b = coef(uni_factor_gender), Terms = 2:2)
  # Race
  summary(uni_factor_race)
  wald.test(Sigma = vcov(uni_factor_race), b = coef(uni_factor_race), Terms = 2:3)
  # Type of Admit
  summary(uni_factor_toa)
  wald.test(Sigma = vcov(uni_factor_toa), b = coef(uni_factor_toa), Terms = 2:4)
  # Payment Typology 1
  summary(uni_factor_pmt)
  wald.test(Sigma = vcov(uni_factor_pmt), b = coef(uni_factor_pmt), Terms = 2:9)
  # MDC
  summary(uni_factor_mdc)
  wald.test(Sigma = vcov(uni_factor_mdc), b = coef(uni_factor_mdc), Terms = 2:24)

### Likelihood Ratio Test
  library(epiDisplay) # Don't call this package until after data prep is done, as it
  # masks the select function from dplyr
  
  ## Null model
  rq1_glm_null <- glm(lama ~ 1, family = binomial, data = df)
  
  models <- list(uni_factor_age_group,uni_factor_gender,uni_factor_mdc,uni_factor_pmt,uni_factor_race,uni_factor_toa)
  
  model_names <- data.frame(c(1,2,3,4,5,6),
                         c('factor_age_group','factor_gender','factor_mdc','factor_pmt','factor_race','factor_toa')) %>%
    rename('model_num' = 1,'model_var' = 2)
  
  model_seq <- 1
  
  for(i in models){
    
    lr_results <- lrtest(rq1_glm_null,i)
    
    model_name <- paste0('lr_',as.character(subset(model_names, subset = model_num == model_seq)[2]))
    
    assign(model_name,lr_results)
    
    model_seq <- model_seq + 1
    
  }
  
  # Cleanup
  rm(model_seq,model_name,lr_results,i)
  
  # P-values
  model_names <- c('factor_age_group','factor_gender','factor_mdc','factor_pmt','factor_race','factor_toa')
                       
  omnibus_pv <- list(lr_factor_age_group[6],lr_factor_gender[6],lr_factor_mdc[6],lr_factor_pmt[6],
                         lr_factor_race[6],lr_factor_toa[6])
  
  names(omnibus_pv) <- model_names
  
  omnibus_pv
  
  ### Detach and unload the epiDisplay and MASS packages so we can use dplyr again.
  detach("package:epiDisplay", unload = TRUE)
  detach("package:MASS", unload = TRUE)
  
  library(dplyr)
  
  ### Get the odds ratios, confidence intervals, and p-values for univariates

  or_glm(data = df, model = uni_factor_age_group)  
  
  # Age
  orci_uni_age <- or_glm(data = df, model = uni_factor_age_group) %>%
    select(-increment)
    
  pv_uni_age <- as.data.frame(coef(summary(uni_factor_age_group))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  uni_results_age <- orci_uni_age %>%
    left_join(pv_uni_age, by = 'predictor')
    
  # Gender
  orci_uni_gender <- or_glm(data = df, model = uni_factor_gender) %>%
    select(-increment)
  
  pv_uni_gender <- as.data.frame(coef(summary(uni_factor_gender))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  uni_results_gender <- orci_uni_gender %>%
    left_join(pv_uni_gender, by = 'predictor')
  
  # Race
  orci_uni_race <- or_glm(data = df, model = uni_factor_race) %>%
    select(-increment)
  
  pv_uni_race <- as.data.frame(coef(summary(uni_factor_race))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  uni_results_race <- orci_uni_race %>%
    left_join(pv_uni_race, by = 'predictor')
  
  # Type of Admission
  orci_uni_toa <- or_glm(data = df, model = uni_factor_toa) %>%
    select(-increment)
  
  pv_uni_toa <- as.data.frame(coef(summary(uni_factor_toa))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  uni_results_toa <- orci_uni_toa %>%
    left_join(pv_uni_toa, by = 'predictor')
  
  # Payment Typology 1
  orci_uni_pmt <- or_glm(data = df, model = uni_factor_pmt) %>%
    select(-increment)
  
  pv_uni_pmt <- as.data.frame(coef(summary(uni_factor_pmt))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  uni_results_pmt <- orci_uni_pmt %>%
    left_join(pv_uni_pmt, by = 'predictor')
  
  # APR MDC - or_glm is long-running so use alternative function from base R
  or_uni_mdc <- as.data.frame(exp(coef(uni_factor_mdc))) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)') %>%
    rename('oddsratio' = 2)
    
  ci_uni_mdc <- as.data.frame(exp(confint(uni_factor_mdc))) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)') %>%
    rename('ci_low (2.5)' = 2, 'ci_high (97.5)' = 3)
  
  orci_uni_mdc <- or_uni_mdc %>%
    left_join(ci_uni_mdc, by = 'predictor')
    
  pv_uni_mdc <- as.data.frame(coef(summary(uni_factor_mdc))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  uni_results_mdc <- orci_uni_mdc %>%
    left_join(pv_uni_mdc, by = 'predictor')
  
  results_uni <- rbind(uni_results_age,uni_results_gender,uni_results_race,uni_results_toa,uni_results_pmt,uni_results_mdc) %>%
    mutate(predictor = str_replace_all(predictor,"factor_",""))
  
  names(results_uni)[2:5] <- c('uni_or','uni_ci_low','uni_ci_high','uni_pv')

  ### Multivariable logistic regression
  
  full_model <- glm(lama ~ factor_pmt + factor_age_group + factor_mdc + factor_gender + factor_race + factor_toa, family = binomial, data = df)
  summary(full_model)

  # Results
 
  ## Full model
  or_full_model <- as.data.frame(exp(coef(full_model))) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)') %>%
    rename('oddsratio' = 2)
  
  ci_full_model <- as.data.frame(exp(confint(full_model))) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)') %>%
    rename('ci_low (2.5)' = 2, 'ci_high (97.5)' = 3)
  
  orci_full_model <- or_full_model %>%
    left_join(ci_full_model, by = 'predictor')
  
  pv_full_model <- as.data.frame(coef(summary(full_model))[,4]) %>%
    rename('pvalue' = 1) %>%
    mutate(pvalue = round(pvalue,4)) %>%
    rownames_to_column(var = 'predictor') %>%
    filter(!predictor == '(Intercept)')
  
  results_full_model <- orci_full_model %>%
    left_join(pv_full_model, by = 'predictor') %>%
    mutate(predictor = str_replace_all(predictor,"factor_",""))
  
  names(results_full_model)[2:5] <- c('full_or','full_ci_low','full_ci_high','full_pv')
  
  ### REPORT - table 
  rpt <- desc_stats %>%
    left_join(lama_rates, by = c('variable','value','key')) %>%
    mutate(key = paste0(key,value)) %>%
    left_join(results_uni, by = c('key' = 'predictor')) %>%
    left_join(results_full_model, by = c('key' = 'predictor'))
  
    # Copy/ paste to Excel
  clipr::write_clip(rpt)
  
  #### MDC labels
  mdc <- d %>%
    select(apr_mdc_code,apr_mdc_description) %>%
    distinct() %>%
    arrange(apr_mdc_code)
  
  ### Garbage clean up/ memory release
  gc()
  