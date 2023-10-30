##### Preprocessing script of switzerland data & largest CVD data from Kaggle #####

# Load packages
library(tidyverse)
setwd("C:/Users/catdu/OneDrive/DTU/11. semester/SpecialCourse")


# Database data ----------------------------------------------------------------

# Variable names
varNames <- c(V1 = "id",        V2 = "ccf",       V3 = "age",       V4 = "sex",       
              V5 = "painloc",   V6 = "painexer",  V7 = "relrest",   V8 = "pncaden",  
              V9 = "cp",        V10 = "trestbps", V11 = "htn",      V12 = "chol",    
              V13 = "smoke",    V14 = "cigs",     V15 = "years",    V16 = "fbs",     
              V17 = "dm",       V18 = "famhist",  V19 = "restecg",  V20 = "ekgmo", 
              V21 = "ekgday",   V22 = "ekgyr",    V23 = "dig",      V24 = "prop",     
              V25 = "nitr",     V26 = "pro",      V27 = "diuretic", V28 = "proto",   
              V29 = "thaldur",  V30 = "thaltime", V31 = "met",      V32 = "thalac",  
              V33 = "thalrest", V34 = "tpeakbps", V35 = "tpeakbpd", V36 = "dummy",   
              V37 = "trestbpd", V38 = "exang",    V39 = "xhypo",    V40 = "oldpeak", 
              V41 = "slope",    V42 = "rldv5",    V43 = "rldv5e",   V44 = "ca",       
              V45 = "restckm2", V46 = "exerckm",  V47 = "restef",   V48 = "restwm",  
              V49 = "exeref",   V50 = "exerwm",   V51 = "thal",     V52 = "thalsev", 
              V53 = "thalpul",  V54 = "earlope",  V55 = "cmo",      V56 = "cday",    
              V57 = "cyr",      V58 = "num",      V59 = "lmt",      V60 = "ladprox", 
              V61 = "laddist",  V62 = "diag",     V63 = "cxmain",   V64 = "ramus",    
              V65 = "om1",      V66 = "om2",      V67 = "rcaprox",  V68 = "rcadist", 
              V69 = "lvx1",     V70 = "lvx2",     V71 = "lvx3",     V72 = "lvx4",    
              V73 = "lvf",      V74 = "cathef",   V75 = "junk",     V76 = "name")


# Switzerland ------------------------------------------------------------------
switzerland <-  read.csv("switzerland.txt", 
                         row.names = NULL, 
                         sep = "\n", 
                         header = F)

# Split dataframe into vector
switzerland <- switzerland %>%
  pluck("V1") %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  na_if(-9) # replace -9 with NA

# Split the vector into a dataframe based on number of variables
switzerland <- as.data.frame(matrix(switzerland, 
                                    ncol = length(varNames),  
                                    byrow = TRUE), 
                             stringsAsFactors = FALSE)

switzerland <- as_tibble(switzerland) %>%
  plyr::rename(all_of(varNames)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(datasetOrig = "switzerland",
         sex = ifelse(sex == 0, 3, 2), 
         sex = ifelse(sex == 3, 1, 2),
         gender = ifelse(sex == 1, "female", "male"))
  



# Hungarian data ---------------------------------------------------------------
# Load Hungarian data
hungarian <-  read.csv("hungarian.txt", 
                       row.names = NULL, 
                       sep = "\n", 
                       header = F)
str(hungarian)

# Split list of vectors into one vector
hungarian <- hungarian %>%
  pluck("V1") %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  na_if(-9) # replace -9 with NA

# Split the vector into a dataframe based on number of variables
hungarian <-  as.data.frame(matrix(hungarian,
                                   ncol = length(varNames),  
                                   byrow = TRUE), 
                            stringsAsFactors = FALSE)

hungarian <- as_tibble(hungarian) %>%
  plyr::rename(all_of(varNames)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(datasetOrig = "hungarian",
         sex = ifelse(sex == 0, 3, 2), 
         sex = ifelse(sex == 3, 1, 2),
         gender = ifelse(sex == 1, "female", "male"))

hungarian


# longBeach data ---------------------------------------------------------------
# Load longBeach data
longBeach <-  read.csv("long-beach-va.txt", 
                       row.names = NULL, 
                       sep = "\n", 
                       header = F)
str(longBeach)

# Split list of vectors into one vector
longBeach <- longBeach %>%
  pluck("V1") %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  na_if(-9) # replace -9 with NA

# Split the vector into a dataframe based on number of variables
longBeach <-  as.data.frame(matrix(longBeach,
                                   ncol = length(varNames),  
                                   byrow = TRUE), 
                            stringsAsFactors = FALSE)

longBeach <- as_tibble(longBeach) %>%
  plyr::rename(all_of(varNames)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(datasetOrig = "longBeach",
         sex = ifelse(sex == 0, 3, 2), 
         sex = ifelse(sex == 3, 1, 2),
         gender = ifelse(sex == 1, "female", "male"))

longBeach




# Gather/joining datasets -------------------------------------------------
# OBS: Makes no sense to make wider or longer since each row is one subject ID

# Binding of rows since varNames are equal
combdata <- switzerland %>% 
  bind_rows(hungarian) %>%
  bind_rows(longBeach) %>%
  mutate(smoke = ifelse(cigs > 0, 0, 1),
         num = ifelse(num != 0, 1, 0), # create smoke variable and present or not for CVD in num(diagnosis) 
         id = as.integer(id),
         age = as.integer(age),
         sex = as.integer(sex),
         fbs = ifelse(fbs == 0, 2, 1),
         fbsFactor = ifelse(fbs == 1, "true", "false"),
         cpFactor = as.factor(cp)) %>% 
  dplyr::rename(cvdPresent = "num") %>%
  dplyr::select(id, age, sex, cp, trestbps, chol, smoke, fbs, cvdPresent, datasetOrig, gender, fbsFactor, cpFactor) %>%
  filter(between(trestbps, 
                 mean(trestbps, na.rm=TRUE) - (3 * sd(trestbps, na.rm=TRUE)),
                 mean(trestbps, na.rm=TRUE) + (3 * sd(trestbps, na.rm=TRUE))))

str(combdata)






# Load large CVD dataset --------------------------------------------------
heartdisease <-  read.csv("cardio_train.csv",
                          row.names = NULL,
                          sep = ";",
                          header = T)
str(heartdisease)

# Rename columns to match with the other datasets
heartdisease <- heartdisease %>%
  tibble() %>%
  dplyr::rename(sex = "gender", chol = "cholesterol", cvdPresent = cardio) %>% # Normal level of chol (binary)
  mutate_if(is.character, as.numeric) %>%
  rowwise() %>%
  mutate(chol = case_when(
    chol == 1 ~ ceiling(runif(1, min = 0, max = 200)), # normal level
    chol == 2 ~ ceiling(runif(1, min = 200, max = 239)), # above normal level
    chol == 3 ~ ceiling(runif(1, min = 240, max = max(combdata$chol, na.rm = TRUE))) # well above normal
  )) #https://my.clevelandclinic.org/health/articles/11920-cholesterol-numbers-what-do-they-mean

# Create columns
heartdisease <- heartdisease %>%
  dplyr::select(-c(gluc, smoke, active)) %>%
  mutate(age = ceiling(age/365),
         bmi = round(weight/((height/100)^2), 1),
         ap_mean = round(ap_lo + 1/3*(ap_hi - ap_lo), 1), # mean systolic blood pressure 
         gender = ifelse(sex == 1, "female", "male"),
         datasetOrig = "CVD",
         alco = as.factor(alco))  




# Select variables (reduce data) ------------------------------------------
# Sort data - extract 'important' variables according to studies
# https://www-sciencedirect-com.proxy.findit.cvt.dk/science/article/pii/S2352914821000745
# https://www.kaggle.com/datasets/aavigan/switzerland-clinic-heart-disease-dataset/code
# https://github.com/nyuvis/datasets/blob/master/heart/heart-disease.names
# https://link-springer-com.proxy.findit.cvt.dk/article/10.1186/1475-2840-12-24
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9219571/

hungarian <- hungarian %>%
  mutate(smoke = ifelse(cigs > 0, 1, 0),
         num = ifelse(num != 0, 1, 0), # create smoke variable and present or not for CVD in num(diagnosis) 
         id = as.integer(id),
         age = as.integer(age),
         sex = as.integer(sex),
         fbs = ifelse(fbs == 0, 2, 1),
         fbsFactor = ifelse(fbs == 1, "true", "false"),
         cpFactor = as.factor(cp)) %>% 
  dplyr::rename(cvdPresent = "num") %>%
  dplyr::select(id, age, sex, cp, trestbps, chol, smoke, fbs, cvdPresent, gender, fbsFactor, cpFactor) %>%
  filter(between(trestbps, 
                 mean(trestbps, na.rm=TRUE) - (3 * sd(trestbps, na.rm=TRUE)),
                 mean(trestbps, na.rm=TRUE) + (3 * sd(trestbps, na.rm=TRUE))))

switzerland <- switzerland %>%
  mutate(smoke = ifelse(cigs > 0, 1, 0),
         num = ifelse(num != 0, 1, 0), # create smoke variable and present or not for CVD in num(diagnosis) 
         id = as.integer(id),
         age = as.integer(age),
         sex = as.integer(sex),
         fbs = ifelse(fbs == 0, 2, 1),
         fbsFactor = ifelse(fbs == 1, "true", "false"),
         cpFactor = as.factor(cp)) %>% 
  dplyr::rename(cvdPresent = "num") %>%
  dplyr::select(id, age, sex, cp, trestbps, chol, smoke, fbs, cvdPresent, gender, fbsFactor, cpFactor) %>%
  filter(between(trestbps, 
                 mean(trestbps, na.rm=TRUE) - (3 * sd(trestbps, na.rm=TRUE)),
                 mean(trestbps, na.rm=TRUE) + (3 * sd(trestbps, na.rm=TRUE))))

longBeach <- longBeach %>%
  mutate(smoke = ifelse(cigs > 0, 1, 0),
         num = ifelse(num != 0, 1, 0), # create smoke variable and present or not for CVD in num(diagnosis) 
         id = as.integer(id),
         age = as.integer(age),
         sex = as.integer(sex),
         fbs = ifelse(fbs == 0, 2, 1),
         fbsFactor = ifelse(fbs == 1, "true", "false"),
         cpFactor = as.factor(cp)) %>% 
  dplyr::rename(cvdPresent = "num") %>%
  dplyr::select(id, age, sex, cp, trestbps, chol, smoke, fbs, cvdPresent, gender, fbsFactor, cpFactor) %>%
  filter(between(trestbps, 
                 mean(trestbps, na.rm=TRUE) - (3 * sd(trestbps, na.rm=TRUE)),
                 mean(trestbps, na.rm=TRUE) + (3 * sd(trestbps, na.rm=TRUE))))



# Combine the 2 datasets with the large CVD data
largeCombData <- combdata %>%
  bind_rows(heartdisease) %>%
  arrange(age) %>%
  mutate(id = as.integer(id),
         age = as.integer(age),
         sex = as.integer(sex))
largeCombData






# Are the data realistic? ------------------------------------------------------
# boxplots to show that we need removal of data

# Systolic bp are at around 120 mmHg 'normal'
largeCombDataClean <- largeCombData %>%
  filter(ap_hi >= 90 & ap_hi <= 250,
         ap_lo >= 40 & ap_lo <= 160, 
         height > 120 & height < 210,
         weight > 30,
         ap_hi > ap_lo) %>%
  dplyr::select(where(function(x) any(!is.na(x)))) # remove column with only NA is

# Difference in columns after removal of 'outliers'
namesBefore = names(largeCombData)
namesAfter = names(largeCombDataClean)
cat("Removed columns when combined with large CVD data:\n", 
    namesBefore[!namesBefore %in% namesAfter])


#   -----------------------------------------------------------------------
# Now we should select things to show via shiny application
saveRDS(hungarian, "HungarianData.rds")
saveRDS(switzerland, "SwitzerlandData.rds")
saveRDS(longBeach, "LongBeachData.rds")
saveRDS(combdata, "SwitzHungLongBeachData.rds")
saveRDS(largeCombDataClean, "mainData.rds")





# 1. #3  (age)   age in years    
# 2. #4  (sex)   sex (1 = male; 0 = female)    
# 3. #9  (cp)    
#                chest pain type
#                -- Value 1: typical angina
#                -- Value 2: atypical angina
#                -- Value 3: non-anginal pain
#                -- Value 4: asymptomatic
# 4. #10 (trestbps)  resting blood pressure (in mm Hg on admission to the hospital)
# 5. #12 (chol)      serum cholestoral in mg/dl
# 6. #16 (fbs)       (fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)
# Maybe famhist?
# 14. #58 (num)      (the predicted attribute)
#                     diagnosis of heart disease (angiographic disease status)
#                     -- Value 0: < 50% diameter narrowing
#                     -- Value 1: > 50% diameter narrowing
#                     (in any major vessel: attributes 59 through 68 are vessels)
# 

