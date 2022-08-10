##### This script is based on the following posts, I recommended that you read this resources before modifying or implementing this script.#####
# 1) Tidymodels: tidy machine learning in R, Rebecca Barter (www.rebeccabarter.com/blog/2020-03-25_machine_learning/)
# 2) Classification with Tidymodels, Workflow and Recipes, Jan Kirenz (www.kirenz.com/post/2021-02-17-r-classification-tidymodels/)
# 3) Dials, Tune, and Parsnip: Tidymodels' Way to Create and Tune Model Parameters, Yu-En Hsu (https://towardsdatascience.com/dials-tune-and-parsnip-tidymodels-way-to-create-and-tune-model-parameters-c97ba31d6173)

################################################################################################################################################

library(tidyverse)
library(tidymodels)
library(gt)
library(stringr)
library(visdat)
library(skimr)
library(rsample)
library(ggplot2)
library(workflows)
library(recipes)
library(parsnip)
library(ranger)
library(xgboost)
library(keras)
library(GGally)
library(purrr)
library(dplyr)
library(yardstick)
library(scales)
library(tune)
library(kknn)
library(vip)
library(discrim)
library(dials)
library(naivebayes)

## Import and clean data #######################################################################################################################

mT<-read.csv(file('ML_nuclusters_211021_v2.csv'),head=TRUE,sep=',')
Mt<-as.data.frame(mT[,12:30])
MtT<-rowSums(Mt)
Mt<-Mt/MtT
Mt<-cbind(mT$Nu.Cluster,Mt)
GDGTn<-c("Type","GDGT0","GDGT1","GDGT2","GDGT3","Cren","Cren'","IIIa","IIIa'","IIIb","IIIb'","IIa","IIa'","IIb","IIb'","IIc","IIc'","Ia","Ib","Ic")
colnames(Mt)<-GDGTn

#Turn the data into a tibble
GDGTs<-as_tibble(Mt)
GDGTs<-
  GDGTs %>%
  dplyr::mutate(Type=as.factor(Type))

## Split the data ##############################################################################################################################

set.seed(123)

data_split<-initial_split(GDGTs,prop = 3/4,strata = Type)

train_data<-training(data_split)
test_data<-testing(data_split)

## Create recipe ###############################################################################################################################

GDGT_rec <-
  recipe(Type ~ ., 
         data = train_data) %>%
  step_normalize(all_numeric())

## Create Validation set #######################################################################################################################

set.seed(100)

cv_folds<-
  vfold_cv(train_data,v=10,strata = Type)

## Model building ##############################################################################################################################

# Null Model -- This model is intended to be an uninformative baseline to compare the other models against 

null_spec <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

# Logistic Regression

log_spec <- 
  logistic_reg(penalty = tune(),mixture = tune()) %>%  #Use tune() as a placeholder for the hyperparameters so we can tune them later
  set_engine(engine = "glm") %>%  
  set_mode("classification") 

# Random Forest

rf_spec <- 
  rand_forest(mtry = tune(),trees=tune(),min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# XGBoost

xgb_spec <- 
  parsnip::boost_tree(trees = 1000, tree_depth = tune(),min_n = tune(),loss_reduction = tune(),sample_size = tune(),mtry = tune(),learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

# K Nearest Neighbor

knn_spec <- 
  nearest_neighbor(neighbors=tune(),weight_func=tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification") 

#Naive Bayes

nb_spec <- 
  naive_Bayes(smoothness = tune(),Laplace = tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes")

## Tune Hyperparameters ########################################################################################################################

lg_param<-parameters(penalty(),mixture())
rf_param<-parameters(trees(),min_n(),finalize(mtry(),select(GDGTs_new,-Type)))
knn_param<-parameters(neighbors(),weight_func())
nb_param<-parameters(smoothness(),Laplace())

lg_grid<-grid_regular(lg_param,levels = 5)
rf_grid<-grid_regular(rf_param,levels = 5)
knn_grid<-grid_regular(knn_param,levels = 5)
nb_grid<-grid_regular(nb_param,levels = 5)

# Had to use a latin hypercube to tune XGBoost
xgb_grid<- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size=sample_prop(),
  finalize(mtry(),select(GDGTs_new,-Type)),
  learn_rate(),
  size = 30
)


## Create tune Workflow ########################################################################################################################
#This is so we can test different hyperparameter values and pick the best performing ones

# Null Model

null_workflow <- 
  workflow() %>%
  add_recipe(GDGT_rec) %>%
  add_model(null_spec)

# Logistic Regression

log_wflow <- 
  workflow() %>% 
  add_recipe(GDGT_rec) %>%   
  add_model(log_spec)

# Random Forest

rf_wflow <-
  workflow() %>%
  add_recipe(GDGT_rec) %>% 
  add_model(rf_spec) 

# XGBoost

xgb_wflow <-
  workflow() %>%
  add_recipe(GDGT_rec) %>% 
  add_model(xgb_spec)

# K Nearest Neighbor

knn_wflow <-
  workflow() %>%
  add_recipe(GDGT_rec) %>% 
  add_model(knn_spec)

# Naive Bayes

nb_wflow <- 
  workflow() %>%
  add_recipe(GDGT_rec) %>%
  add_model(nb_spec)

## Pick Hyperparameters ########################################################################################################################
#Running this section required the use of an HPC for me to run in a reasonable amount of time, I don't recommend it being run in a personal
#computer as is in this script. The outputs from the HPC run were saved as RDS environments and later imported to my computer.

 # set.seed(5432)
 # 
 # tune_rf<-tune_grid(
 #   rf_wflow,
 #   cv_folds,
 #   grid = rf_grid,
 #   control = control_resamples(save_pred = TRUE)
 # )
 # saveRDS(tune_rf,file('tune_rf_GDGTs.RDS'))
 # 
 # set.seed(5432)
 # 
 # tune_xgb<-tune_grid(
 #   xgb_wflow,
 #   cv_folds,
 #   grid = xgb_grid,
 #   control = control_resamples(save_pred = TRUE)
 # )
 # saveRDS(tune_xgb,file('tune_xgb_GDGTs.RDS'))
 # 
 # set.seed(5432)
 # 
 # tune_knn<-tune_grid(
 #   knn_wflow,
 #   cv_folds,
 #   grid = knn_grid,
 #   control = control_resamples(save_pred = TRUE)
 # )
 # saveRDS(tune_knn,file('tune_knn_GDGTs.RDS'))
 # 
 # set.seed(5432)
 # 
 # tune_nb<-tune_grid(
 #   nb_wflow,
 #   cv_folds,
 #   grid = nb_grid,
 #   control = control_resamples(save_pred = TRUE)
 # )
 # saveRDS(tune_nb,file('tune_nb_GDGTs.RDS'))

#Get the outputs
tune_rf<-readRDS('tune_rf_GDGTs.RDS')
tune_xgb<-readRDS('tune_xgb.RDS')
tune_knn<-readRDS('tune_knn_GDGTs.RDS')
tune_nb<-readRDS('tune_nb_GDGTs.RDS')

## Best Hyperparameters #######################################################################################################################

rf_best <- tune_rf %>%
  select_best(metric = "roc_auc")

xgb_best <- tune_xgb %>%
  select_best(metric = "roc_auc")

knn_best <- tune_knn %>%
  select_best(metric = "roc_auc")

nb_best <- tune_nb %>%
  select_best(metric = "roc_auc")

## Finalize workflows #########################################################################################################################
#Apply the best performing hyperparameters to the models

final_rf <- finalize_workflow(rf_wflow,rf_best)
final_xgb <- finalize_workflow(xgb_wflow,xgb_best)
final_knn <- finalize_workflow(knn_wflow,knn_best)
final_nb <- finalize_workflow(nb_wflow,nb_best)

## Run models to compare ######################################################################################################################

null_res<-
  null_workflow %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = yardstick::metric_set(
             recall, precision, f_meas, 
             accuracy, kap,
             roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

rf_res<-
  final_rf %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

xgb_res<-
  final_xgb %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

knn_res<-
  final_knn %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

nb_res<-
  final_nb %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

## Compare models #############################################################################################################################

null_metrics <-
  null_res %>%
  collect_metrics(summarise = TRUE) %>%
  mutate(model ="Null")

rf_metrics <-
  rf_res %>%
  collect_metrics(summarise = TRUE) %>%
  mutate(model="Random Forest")

xgb_metrics <-
  xgb_res %>%
  collect_metrics(summarise = TRUE) %>%
  mutate(model="XGBoost")

knn_metrics <-
  knn_res %>%
  collect_metrics(summarise = TRUE) %>%
  mutate(model="KNN")

nb_metrics <-
  nb_res %>%
  collect_metrics(summarise = TRUE) %>%
  mutate(model="Naive Bayes")

model_compare <-bind_rows(null_metrics,rf_metrics,xgb_metrics,knn_metrics,nb_metrics)

model_comp <-
  model_compare %>%
  select(model,.metric, mean, std_err) %>%
  pivot_wider(names_from = .metric,values_from = c(mean,std_err))


#Show mean F1-Score
model_comp %>%
  arrange(mean_f_meas) %>%
  mutate(model = fct_reorder(model,mean_f_meas)) %>%
  ggplot(aes(model,mean_f_meas,fill=model))+
  geom_col()+
  coord_flip()+
  scale_fill_brewer(palette="Blues")+
  geom_text(size=3,aes(label = round(mean_f_meas,2),y=mean_f_meas + 0.08),vjust =1)

#Show mean area under the curve (auc)
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>%
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(
    size = 3,
    aes(label = round(mean_roc_auc, 2), y = mean_roc_auc + 0.08),
    vjust = 1
  )

#Show mean Accuracy
model_comp %>%
  arrange(mean_accuracy) %>%
  mutate(model = fct_reorder(model,mean_accuracy)) %>%
  ggplot(aes(model,mean_accuracy,fill=model))+
  geom_col()+
  coord_flip()+
  scale_fill_brewer(palette="Blues")+
  geom_text(size=3,aes(label = round(mean_accuracy,2),y=mean_f_meas + 0.08),vjust =1)

#Select max value of metric
model_comp %>% slice_max(mean_roc_auc)

#Collect predictions

null_pred <- 
  null_res %>%
  collect_predictions()

null_pred %>% 
  conf_mat(Type, .pred_class) 

rf_pred <- 
  rf_res %>%
  collect_predictions()

rf_pred %>% 
  conf_mat(Type, .pred_class) 

xgb_pred <- 
  xgb_res %>%
  collect_predictions()

xgb_pred %>% 
  conf_mat(Type, .pred_class) 

knn_pred <- 
  knn_res %>%
  collect_predictions()

knn_pred %>% 
  conf_mat(Type, .pred_class) 

nb_pred <- 
  nb_res %>%
  collect_predictions()

nb_pred %>% 
  conf_mat(Type, .pred_class) 

## Evaluate on test set #########################################################################################################################

last_fit_rf <-last_fit(final_rf,
                        split=data_split,
                        metrics = metric_set(
                          recall, precision, f_meas, 
                          accuracy, kap,
                          roc_auc, sens, spec)
)

last_fit_rf %>%
  collect_metrics()

rf_f_pred <- 
  last_fit_rf %>%
  collect_predictions()

rf_f_pred %>% 
  conf_mat(Type, .pred_class) 

##Access important variables ####################################################################################################################

#pdf("variables_v1.pdf")
last_fit_rf %>%
  extract_fit_parsnip(".workflow",1) %>%
  vip(num_features = 19)
#dev.off()

##Confusion matrix

last_fit_rf %>%
  collect_predictions() %>% 
  conf_mat(Type, .pred_class) %>% 
  autoplot(type = "heatmap")

## Fit the model with the full dataset ##########################################################################################################

final_model<-fit(final_rf,GDGTs)

saveRDS(final_model,file('Final_RF_model.RDS'))

#Model is now ready to use, see example file