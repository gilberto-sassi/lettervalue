#' Summary Using Letter Value
#'
#' Compute the resume measures (location and scale) using letter values.
#'
#' In this summary, we present the trimean, median, F-spread, F-pseudo sigma, F-pseudo variance e outliers values.
#'
#' @details This function returns the measures of location and scale as presented at Understanding Robust and Exploratory Data Analysis by Hoaglin, Mosteller and Tukey published in 1983.
#'
#' This is a generic method for the class "lv".
#'
#' @param object an object \code{lv}.
#' @param ... further arguments passed to or from other methods.
#' @param coef Length of the whiskers as multiple of IQR. Defaults to 1.
#' @return A \code{tibble} object with the following columns:
#' \describe{
#' \item{trimean}{resistant measure to small changes in the dataset for location.}
#' \item{median}{resistant measure to small changes in the datase for location.}
#' \item{f_spread}{resistant measure to small changes in the dataset for scale.}
#' \item{f_pesudo_sigma}{resistant measure to small changes in the dataset for location. For a normal distribution, this measure is equal to populational statndard deviation.}
#' \item{f_pseudo_variance}{squared valued of \code{f_pseudo_sigma}.}
#' \item{outliers}{values outside whiskers.}
#' }
#' @examples
#' lv_obj <- letter_value(rivers)
#' summary.lv(lv_obj)
#'
#' @export summary.lv
#' @export
summary.lv <- function(object, ..., coef = 1.5) {
    # letter values data frame
    df_lv <- object$lv_data_frame

    # trimean
    trimean <- 0.25 * df_lv$lv_lower[2] + 0.5 * df_lv$lv_lower[1] + 0.25 * df_lv$lv_upper[2]

    # median
    median <- df_lv$lv_lower[1]

    # F-spread
    f_spread <- df_lv$lv_upper[2] - df_lv$lv_lower[2]

    # F-Pseudo sigma
    f_pseudo_sigma <- f_spread / 1.379

    # F-Pseudo variance
    f_pseudo_variance <- f_pseudo_sigma^2

    # outliers
    lower_bound <- df_lv$lv_lower[2] - coef * f_spread
    upper_bound <- df_lv$lv_upper[2] + coef * f_spread

    points <- vector("double")
    if (min(object$sample) < lower_bound) {
        points <- c(points, object$sample[object$sample < lower_bound])
    }
    if (max(object$sample) > upper_bound) {
        points <- c(points, object$sample[object$sample > upper_bound])
    }

    if (length(points) > 0) {
        outliers <- points
    } else {
        outliers <- "No outliers"
    }

    tibble::tibble(
        trimean = trimean,
        median = median,
        f_spread = f_spread,
        f_pseudo_sigma = f_pseudo_sigma,
        f_pseudo_variance = f_pseudo_variance,
        outliers = list(outliers)
    )
}
