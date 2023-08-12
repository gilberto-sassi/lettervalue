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
#' @param x an object \code{lv}.
#' @param ... further arguments passed to or from other methods.
#' @examples
#' lv_obj <- letter_value(rivers)
#' summary.lv(lv_obj)
#'
#' @export summary.lv
#' @export
summary.lv <- function(x, ...) {
    # letter values data frame
    df_lv <- x$lv_data_frame

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
    lower_bound <- df_lv$lv_lower[2] - f_spread
    upper_bound <- df_lv$lv_upper[2] + f_spread

    points <- vector("double")
    if (min(x$sample) < lower_bound) {
        points <- c(points, x$sample[x$sample < lower_bound])
    }
    if (max(x$sample) > upper_bound) {
        points <- c(points, x$sample[x$sample > upper_bound])
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