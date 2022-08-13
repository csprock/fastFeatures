
#' Sample column names and row indices
#'
#' @param feature_columns character vector containing names to sample from
#' @param nrows_ the number of rows 
#' @param column_proportion numeric specifying the fraction of columns to include in the sample
#' @param record_proportion numeric specifying the fraction of rows in include in the sample
#' 
#' @family sampling
#' @return list
#' \describe{
#'  \item{random_rows}{Vector containing row indices}
#'  \item{random_columns}{Vector containing feature names}
#' }
sample_indices <- function(feature_columns, nrows_, column_proportion, record_proportion) {

        random_columns <- sample(x = feature_columns,
                                size = round(x = column_proportion*length(feature_columns), digits = 0),
                                replace = FALSE)

        random_rows   <- sample(x = 1:nrows_,
                                size = round(x = nrows_*record_proportion,digits = 0),
                                replace = TRUE)

    return(list(random_rows=random_rows, random_columns=random_columns))
}


#' Sample rows and columns of a matrix or data.frame
#' 
#' @param df data.frame
#' @param target_column str 
#' @param feature_columns character vector containing names to sample from
#' @param column_proportion numeric specifying the fraction of columns to include in the sample
#' @param record_proportion numeric specifying the fraction of rows in include in the sample
#' @family sampling
#' 
#' @return list
#' \describe{
#'  \item{x}{Subset of original data.frame containing sampled rows and features}
#'  \item{y}{vector containing target values corresponding to the rows of x}
#' }
sample_matrix <- function(df, target_column, feature_columns, column_proportion, record_proportion) {

    sample_idx <- sample_indices(
        feature_columns=feature_columns,
        nrows_=dim(df)[1],
        record_proportion=record_proportion,
        column_proportion=column_proportion
    )

    output <- list(
        x=df[sample_idx$random_rows, sample_idx$random_columns],
        y=df[sample_idx$random_rows, target_column]
    )
    return(output)
}



#' Compute the variable inclusion probability
#'
#' Computes the variable inclusion probability from bootstrapped lasso runs. 
#'
#' Computes the variable inclusion probability as the fraction of non-zero coefficients 
#' out of all bootstrapped lasso runs. 
#' 
#' @importFrom stats aggregate
#' @param results list containing coefficients of glmnet model from each bootstrapped run
#' 
#' @return a data.frame containg the variable name and its probability of inclusion
compute_results <- function(results) {names
    results <- do.call(rbind, results)
    results[,1] <- ifelse(results[,1] > 0, 1, 0)
    results <- aggregate(results[,1], by=list(rownames(results)), FUN=mean)
    colnames(results) <- c("Variable", "Conditional Variable Inclusion Probability")
    return(results)
}