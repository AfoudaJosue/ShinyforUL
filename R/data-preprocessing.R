#' Function to normalize a variable by shift the scale so that it starts at O,
#' and then compress it so that it ends on 1.
#'
#' @details THis function takes two arguments, a vector x of numeric values,
#' and the logical na.rm and return a vector of normalized values
#'
#' @param x A vector of numeric values
#' @param na.rm A boolean value TRUE or FALSE. The default is set to TRUE
#'
#' @return a vector contains normalized values
#' @author Josué AFOUDA
#' @export
#'
#' @examples
#' # Normalize a single vector
#' normalize(iris$Sepal.Length)
#' # Normalize all variables of a dataframe
#' normalize(mtcars)
normalize <- function(x, na.rm = TRUE) {
  resultat <- (x - min(x, na.rm)) / (max(x, na.rm) - min(x, na.rm))
  return(resultat)
}


#' Function to standardize a variable. After being standardized,
#' the variable has zero mean and unit standard deviation.
#'
#' @details THis function takes one argument, a vector x of numeric values.
#' and return a vector of z-scores that represent the number of standard deviations
#' above or below the mean that a specific observation falls.
#'
#' @param x A vector of numeric values
#'
#' @return a vector contains standard scores a.k.a z-scores
#' @author Josué AFOUDA
#' @export
#'
#' @examples
#' # Example usage of standardize
#' standardize(iris$Sepal.Length)
#' standardize(mtcars)
standardize <- function(x, na.rm = TRUE) {
  resultat <- data.frame(scale(x))
  return(resultat)
}


#' This function "cleans" up a dataframe so that it can be used by the application
#'
#' @details For the application to work well, the input dataframe data should
#' contain only numeric variables with no missing values.
#' This function acts as a safeguard in case the user does not clean up his dataframe.
#' Qualitative variables are removed and only non-missing values are kept
#' in the final dataframe.
#'
#' @param data A dataframe
#'
#' @return a dataframe that can be used by the application
#' @author Josué AFOUDA
#' @export
cleaning_df <- function(data) {
  df_clean <- data[, !sapply(data, is.character)]
  df_clean <- df_clean[complete.cases(df_clean), ]
  return(df_clean)
}
