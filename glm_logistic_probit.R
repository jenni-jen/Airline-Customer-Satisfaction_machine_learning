library(caTools)
library(caret)
library(lmtest)
library(pscl)
library(ResourceSelection)
library(stargazer)
library(ggplot2)
library(tidyverse)
library(broom)
library(pROC)
theme_set(theme_classic())

airline = read.csv('./Data/airline_processed.csv')

# ========== train test split ==========
# test size = 0.2
set.seed(1234)
sample = sample.split(airline$satisfaction, SplitRatio = 0.8)
airline_train = subset(airline, sample == TRUE)
airline_test = subset(airline, sample == FALSE)
write.csv(airline_train, file = './Data/airline_train.csv', row.names = F)
write.csv(airline_test, file = './Data/airline_test.csv', row.names = F)

# ========== logistic regression ==========
# fit full model
fit_full = glm(satisfaction ~ ., data = airline_train, family = binomial)
summary(fit_full)

# fit baseline model
fit_baseline = glm(satisfaction ~ 1, data = airline_train, family = binomial)
summary(fit_baseline)

# stepwise selection: backward elimination
step(fit_full, scope=list(lower = formula(fit_baseline), upper = ~ .^2 ), scale=1, trace=T, 
     direction="both")

# consider interactions
fit_final = glm(satisfaction ~ . + seat_comfort:flight_time_convenient + class:seat_comfort, 
                data = airline_train, family = binomial)
summary(fit_final)

# compare fit_full & fit_final
stargazer(fit_full, fit_final, type = 'html', style = 'ajps', out = './Code_outputs/logistic.html')

# goodness of fit: likelihood ratio test
lrtest(fit_final, fit_full)

# goodness of fit: pseudo R2
pR2(fit_full)  # look for 'McFadden'
pR2(fit_final)

# goodness of fit: Hosmer-Lemeshow test
train_pred_prob = predict(fit_final, newdata = airline_train, type = 'response')
rm_na = is.na(train_pred_prob)
train_pred_prob = as.vector(train_pred_prob[which(rm_na == F)])
train_pred = ifelse(train_pred_prob > 0.5, 1, 0)
train_obs = airline_train$satisfaction[which(rm_na == F)]
hoslem.test(train_obs, train_pred)

# diagnostics: plot the standardized residuals
model_data = augment(fit_final) %>% 
  mutate(Satisfaction = ifelse(satisfaction == 1, 'Satisfied', 'Dissatisfied'))

ggplot(model_data, aes(x = .std.resid, colour = Satisfaction, fill = Satisfaction)) + 
  geom_histogram(position = 'stack', bins = 25, alpha = 0.5) +
  labs(x = 'Standardized Residuals', y = 'Count') +
  theme_bw()
ggsave('./Figures/logistic_std_resid.png', width = 5, height = 3, dpi = 300)

ggplot(model_data, aes(sample = .std.resid)) + 
  stat_qq(alpha = 0.6, colour = 'grey') +
  stat_qq_line(alpha = 0.6, lwd = 0.8) +
  labs(x = 'Theoretical', y = 'Sample') +
  theme_bw()
ggsave('./Figures/logistic_std_resid_qq.png', width = 5, height = 3, dpi = 300)

# performance on the test set
test_pred_prob = predict(fit_final, newdata = airline_test, type = 'response')
test_pred = ifelse(test_pred_prob > 0.5, 1, 0)
test_obs = airline_test$satisfaction

confusionMatrix(
  data = as.factor(test_pred), reference = as.factor(test_obs), 
  positive = '1', mode = 'sens_spec')

# draw the ROC curve
png('./Figures/logistic_roc.png', width = 10, height = 10, units = 'cm', res = 300)
par(pty="s")
logis_roc = roc(response=test_obs, predictor=test_pred_prob)
plot.roc(logis_roc, lwd=3, print.auc=T, auc.polygon=T, auc.polygon.col='linen', 
         print.thres=T, grid = T)
plot.roc(smooth(logis_roc), add=TRUE, col='firebrick')
dev.off()

# ========== probit regression ==========
# fit probit model
model_probit = glm(satisfaction ~ ., data = airline_train, 
                   family = binomial(link = 'probit'))
summary(model_probit)
stargazer(model_probit, type = 'html', style = 'ajps', out = './Code_outputs/probit.html')

# goodness of fit: likelihood ratio test
lrtest(model_probit)

# goodness of fit: pseudo R2
pR2(model_probit)  # look for 'McFadden'

# goodness of fit: Hosmer-Lemeshow test
probit_train_pred_prob = predict(model_probit, newdata = airline_train, type = 'response')
probit_rm_na = is.na(probit_train_pred_prob)
probit_train_pred_prob = as.vector(probit_train_pred_prob[which(probit_rm_na == F)])
probit_train_pred = ifelse(probit_train_pred_prob > 0.5, 1, 0)
probit_train_obs = airline_train$satisfaction[which(probit_rm_na == F)]
hoslem.test(probit_train_obs, probit_train_pred)

# performance on the test set
probit_test_pred_prob = predict(model_probit, newdata = airline_test, type = 'response')
probit_test_pred = ifelse(probit_test_pred_prob > 0.5, 1, 0)
probit_test_obs = airline_test$satisfaction

confusionMatrix(
  data = as.factor(probit_test_pred), reference = as.factor(probit_test_obs), 
  positive = '1', mode = 'sens_spec')

# draw the ROC curve
png('./Figures/probit_roc.png', width = 10, height = 10, units = 'cm', res = 300)
par(pty="s")
probit_roc = roc(response=probit_test_obs, predictor=probit_test_pred_prob)
plot.roc(probit_roc, lwd=3, print.auc=T, auc.polygon=T, auc.polygon.col='linen', 
         print.thres=T, grid = T)
plot.roc(smooth(probit_roc), add=TRUE, col='firebrick')
dev.off()

