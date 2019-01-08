library(dplyr)

AD_data <- read.csv("printads.csv", header = TRUE)

#Creating quadratic term for page number

AD_data <- AD_data %>% mutate(page_num_sqr = page_num^2)

#poisson regression model for picture fixation count

Pic_fix_reg <- glm(pic_fix ~ page_num + page_num_sqr + as.factor(page_pos) + brand_size + pic_size
                   , family = poisson(link = "log"), data = AD_data) 

recoeff_pic_fix <- exp(coef(Pic_fix_reg))

summary(Pic_fix_reg)

#poisson regression model for brand fixation count

brand_fix_reg <- glm(brand_fix ~ page_num + page_num_sqr + as.factor(page_pos) + brand_size + pic_size
                     , family = poisson(link = "log"), data = AD_data) 

recoeff_brand_fix <- exp(coef(brand_fix_reg))

summary(brand_fix_reg)

#binomial regression model for recall accuracy

recall_accu_reg <- glm(recall_accu ~ pic_fix + brand_fix + page_num + page_num_sqr + as.factor(page_pos)
                       , family = binomial(link = "logit"), data = AD_data)

regcoeff_recall_accu <- exp(coef(recall_accu_reg))

summary(recall_accu_reg)

#extracting each brands ad design parameters

Brand_parameters <- AD_data %>% select(c("brand", "page_num", "page_num_sqr", "page_pos")) %>%
  distinct() %>%
  mutate(pic_fix = unique(predict.glm(Pic_fix_reg, AD_data, type = "response"))) %>% #adding pic fix estimate
  mutate(brand_fix = unique(predict.glm(brand_fix_reg, AD_data, type = "response"))) #adding brand fix estimate

#Brand recall accuracy estimates

Brand_parameters$recall_accuracy_est <- predict.glm(recall_accu_reg, Brand_parameters, type = "response")
