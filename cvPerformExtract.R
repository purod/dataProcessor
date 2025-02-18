# extract model performance from cross-validation
cvPerformExtract <- function(cvDir, target, targetype="r2MSE", nsamples, target_cens=NULL){
  ## reading in crossvalidaiton file
  dirfold <- list.files(cvDir, pattern = "fold", full.names = TRUE)
  pb <- progress_estimated(length(dirfold))
  
  allres <- purrr::map_df( dirfold,
                           function(dirf) {
                             if(grepl("cv_Repeat", dirf)){
                               repeatn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\1", dirf)
                               foldn <- sub(".*cv_Repeat_(.*)_fold_(.*)", "\\2", dirf)
                             }else{
                               repeatn = 1
                               foldn = sub(".*cv_fold_(.*)", "\\1", dirf)
                             }
                             ensloc <- file.path(dirf, "ensemble.txt.proto")
                             if (file.exists(ensloc)) {
                               # load in the model
                               ens <- fsReadModel(ensloc)
                               # performance on training folds
                               # Define a named list of functions
                               traindf <- fread(file.path(dirf, "training.csv"), data.table = F)
                               extract_train <- list(
                                 normalMSS = function() r2Extract(ens, df=traindf, target, nSamples, seed = 89),
                                 normalRSS = function() r2Extract(ens, df=traindf, target, nSamples, method = "rss", seed = 89),
                                 r2MSE = function() r2MSELastLayer(ens, df=traindf, target),
                                 adjustR2  = function() adjr2Extract(ens, df=traindf, target, nSamples),
                                 binary    = function() aucExtract(ens, df=traindf, target, nSamples),
                                 time      = function() cindexExtract(ens, df=traindf, target, target_cens)
                               )
                               testdf <- fread(file.path(dirf, "testing.csv"), data.table = F) 
                              extract_test <- list(
                                 normalMSS = function() r2Extract(ens, df=testdf, target, nSamples, seed = 89),
                                 normalRSS = function() r2Extract(ens, df=testdf, target, nSamples, method = "rss", seed = 89),
                                 r2MSE = function() r2MSELastLayer(ens, df=testdf, target),
                                 adjustR2  = function() adjr2Extract(ens, df=testdf, target, nSamples),
                                 binary    = function() aucExtract(ens, df=testdf, target, nSamples),
                                 time      = function() cindexExtract(ens, df=testdf, target, target_cens)
                               )                     
                               # Check if the targetype is valid and execute the corresponding function
                               if (targetype %in% names(extract_train)) {
                                 trainrs <- extract_train[[targetype]]()
                                 testrs <- extract_test[[targetype]]()
                               } else {
                                 stop("Invalid targetype provided.")
                               }
                               
                               pb$tick()$print()
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               target, repeatn, foldn, trainrs, testrs))
                            }else{
                               message("ensemble.txt.proto missing in ", dirf)
                               pb$tick()$print()
                               return( tribble(~Target, ~Repeat, ~Fold, ~TrainPerf, ~TestPerf, 
                                               target, repeatn, foldn, list(), list()))
                            }
                          })

  cvSummary = list()
  # convert into dataframe
  cvSummary$train <- do.call(rbind, allres$TrainPerf) 
  storage.mode(cvSummary$train) = "numeric"
  cvSummary$train <- cvSummary$train %>% rbind(.,colMeans(.)) %>% 
    rbind(., unlist(apply(., 2, median)))
  
  cvSummary$test <- do.call(rbind, allres$TestPerf) 
  storage.mode(cvSummary$test) = "numeric"
  cvSummary$test <- cvSummary$test %>% rbind(.,colMeans(.)) %>% 
    rbind(., unlist(apply(., 2, median)))
  
  return(cvSummary)
}
