
remove_batch_effects <- function(expr_matrix, 
                                 batch_cat = NULL,
                                 batch_cont = NULL, 
                                 bio_covariates = NULL, 
                                 method = c("limma", "combat")) {
  #' Remove batch effects from normalized gene expression data
  #'
  #' @param expr_matrix       A normalized and variance-stabilized gene expression matrix (genes x samples)
  #' @param batch_cat         Data frame of categorical batch variables (e.g., batch ID, sequencing center)
  #' @param batch_cont        Data frame of continuous batch variables (e.g., RIN, library size)
  #' @param bio_covariates    Data frame of biological covariates to preserve (e.g., diagnosis, sex)
  #' @param method            Method for batch correction: "limma" (default) or "combat"
  #' 
  #' @return Batch-corrected expression matrix

  method <- match.arg(method)

  # 1. Combine and clean batch variables
  batch_vars <- NULL
  if (!is.null(batch_cat) || !is.null(batch_cont)) {
    batch_vars <- data.frame(batch_cat, batch_cont) %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      dplyr::mutate_if(is.factor, as.numeric) %>%
      as.matrix()
  }

  # 2. Format biological covariates (to preserve during correction)
  design_matrix <- NULL
  if (!is.null(bio_covariates)) {
    design_matrix <- bio_covariates %>%
      dplyr::mutate_if(is.character, as.factor) %>%
      as.matrix()
  }

  # 3. Perform batch correction using selected method
  if (method == "limma") {
    expr_corrected <- limma::removeBatchEffect(
      x = expr_matrix,
      covariates = batch_vars,
      design = design_matrix
    )
  } else if (method == "combat") {
    if (is.null(batch_vars)) {
      stop("ComBat method requires at least one batch variable.")
    }
    expr_corrected <- expr_matrix
    for (i in seq_len(ncol(batch_vars))) {
      expr_corrected <- sva::ComBat(
        dat = expr_corrected,
        batch = batch_vars[, i],
        mod = design_matrix,
        par.prior = TRUE,
        prior.plots = FALSE
      )
    }
  }

  return(expr_corrected)
}



remove_batch_effects <- function(expr_matrix, 
                                 batch_vars = NULL,
                                 bio_covariates = NULL, 
                                 method = c("limma", "combat")) {
  #' Remove batch effects from normalized expression data
  #'
  #' @param expr_matrix       Normalized and variance-stabilized expression matrix (genes x samples)
  #' @param batch_vars         Data frame of batch variables (e.g., batch ID)
  #' @param bio_covariates    Data frame of biological covariates to preserve (e.g., condition, sex)
  #' @param method            "limma" (default) or "combat"
  #'
  #' @return Batch-corrected expression matrix

  method <- match.arg(method)
  expr_corrected <- expr_matrix
  sample_names <- colnames(expr_matrix)

  # -- Format biological covariates into design matrix
  design_matrix <- if (!is.null(bio_covariates)) {
  model.matrix(~ ., data = bio_covariates %>%
                 dplyr::mutate_if(is.character, as.factor))
  } else {
   model.matrix(~ 1, data = data.frame(dummy = rep(1, ncol(expr_matrix))))
  }

  batch_vars <- batch_vars %>%
        dplyr::mutate_if(is.character, as.factor)

  # -- Prepare batch covariates
  if (method == "limma") {

    batch_matrix <- NULL
    if (!is.null(batch_vars)) {
      cat_vars <- batch_vars %>% dplyr::select(where(is.factor))
      num_vars <- batch_vars %>% dplyr::select(where(is.numeric))
      dummy_vars <- if (ncol(cat_vars) > 0) model.matrix(~ ., data = cat_vars)[, -1, drop = FALSE] else NULL
      batch_matrix <- cbind(dummy_vars, num_vars)
    }

    expr_corrected <- limma::removeBatchEffect(
      x = expr_matrix,
      covariates = batch_matrix,
      design = design_matrix
    )
  } 

  if (method == "combat"){

    expr_corrected <- expr_matrix
    for (batch_name in colnames(batch_vars)) {
      batch_vector <- batch_vars[[batch_name]]
      expr_corrected <- sva::ComBat(
        dat = expr_corrected,
        batch = batch_vector,
        mod = design_matrix,
        par.prior = TRUE,
        prior.plots = FALSE
      )
    }
  }

  colnames(expr_corrected) <- sample_names
  return(expr_corrected)
}
