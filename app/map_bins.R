#' Calculate Map Bins with Factor Labels
#'
#' @description Instead of returning numeric categories to be used for mapping,
#'     this function provides custom defined labels based on a number of principled
#'     approaches to calculating map bins.
#'
#' @usage create_factor_bins(.data, var, new_var, style, classes, zero_class = TRUE, bin_labs)
#' 
#' @param .data A \code{sf} object
#' @param var Variable with numeric data to be binned
#' @param new_var Optional; name of new variable to store bin labels
#' @param style One of the following approaches to pass to \code{classInt::classIntervals}:
#'     "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", 
#'     "fisher", "jenks", "dpih", or "headtails"
#' @param classes The number of classes to return including an optional zero class.
#' @param zero_class Should all zero values be included in their own class?
#' @param bin_labs A character vector with an equal number of elements to \code{classes}
#'     if \code{zero_class} is \code{FALSE} or one less number of elements to \code{classes}
#'     if \code{zero_class} is \code{TRUE}. For example, if you have the character vector
#'     \code{c("Low", "Moderate", "High")} for this argument, you would set \code{classes} to
#'     \code{3} if \code{zero_class} was \code{FALSE}. If \code{zero_class} was \code{TRUE},
#'     you would set \code{classes} to {4} (the three specified plus the automatically created
#'     \code{"Zero"} class).
#' @param output Either \code{"data"} or \code{"breaks"}; see return below
#'     
#' @return If \code{output} is \code{"data}" - a \code{sf} object or a \code{data.frame} with 
#'     the new factor column created. If \code{output} is \code{"data}" - the breaks values (
#'     not including the zero category).

## function
create_factor_bins <- function(.data, var, new_var, style, classes, zero_class = TRUE, bin_labs,
                               output = c("data", "breaks")){
  
  # validate parameters
  ## output is correctly specified
  if (length(output) != 1){
    stop("The argument for 'output' must either be 'data' or 'breaks'.")
  }
  
  if (output %in% c("data", "breaks") == FALSE){
    stop("The argument for 'output' must either be 'data' or 'breaks'.")
  }
  
  ## style is valid
  if(style %in% c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
                  "bclust", "fisher", "jenks", "dpih", "headtails") == FALSE){
    stop("Pick one of the following approaches to calculating breaks - fixed, sd, equal, pretty, quantile, kmeans, hclust, bclust, fisher, jenks, dpih, or headtails.")
  }
  
  ## zero_class is logical
  if (is.logical(zero_class) == FALSE){
    stop("The 'zero_class' argument must be either TRUE or FALSE.")
  }
  
  ## classes match bin_labs
  if (zero_class == TRUE){
    if (classes != length(bin_labs)+1){
      stop("If you are specifying a zero bin, the number of bin_labs elements should one less than your number of classes.")
    }
    
    classes <- classes-1
    
  } else if (zero_class == FALSE){
    if (classes != length(bin_labs)){
      stop("If you are not specifying a zero bin, the number of bin_labs elements should be equal to the number of classes.")
    }
  }
  
  # save parameters to list
  paramList <- as.list(match.call())
  
  # quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }
  
  varQ <- rlang::quo_name(rlang::enquo(var))
  
  if (missing(new_var)) {
    
    new_var <- rlang::quo(!! rlang::sym(paste0(varQ,"_bin")))
    
  } else {
   
    if (!is.character(paramList$new_var)) {
      new_var <- rlang::enquo(new_var)
    } else if (is.character(paramList$new_var)) {
      new_var <- rlang::quo(!! rlang::sym(new_var))
    }
    
  }
  
  new_varQ <- rlang::quo_name(rlang::enquo(new_var))
  
  # validate parameters
  ## input variable is numeric
  if (is.numeric(.data[[varQ]]) == FALSE){
    stop("Input variable for the 'var' argument must be numeric.")
  }
  
  # optionally subset
  if (zero_class == TRUE){
    a <- dplyr::filter(.data, {{var}} > 0)
    b <- dplyr::filter(.data, {{var}} == 0 | is.na({{var}}) == TRUE)  
  } else if (zero_class == FALSE){
    a <- .data
  }
  
  # create breaks and categories (zero data)
  if (zero_class == TRUE){
    b <- dplyr::mutate(b, {{new_var}} := ifelse({{var}} == 0, "Zero", NA))
    b <- dplyr::mutate(b, {{new_var}} := as.factor({{new_var}})) 
  }
  
  # calculate breaks and categories (valid data)
  ## breaks
  breaks <- classInt::classIntervals(a[[varQ]], n = classes, style = style)
  
  if (output == "data"){
    
    categories <- cut(a[[varQ]], breaks = c(breaks$brks), include.lowest = TRUE, dig.lab = 10)
    
    ## create new variable
    a <- dplyr::mutate(a, {{new_var}} := categories)
    levels(a[[new_varQ]]) <- bin_labs
    
    # combine
    if (zero_class == TRUE){
      a <- rbind(a,b)
      a[[new_varQ]] <- relevel(a[[new_varQ]], "Zero")
    }
    
  } else if (output == "breaks"){
    a <- breaks
  }
  
  ## custom return for Tim
  a %>%
    select(id, !!new_varQ) -> a

  # return output
  return(a)
  
}

# ## sample data
# df <- data.frame(
#   id = c(1:100),
#   flu_b = c(sample(10:75, 75, replace = TRUE), rep(0, 15), rep(NA, 10)),
#   dummy_var = c(sample(1:20, 100, replace = TRUE))
# )
# 
# ## samples
# create_factor_bins(df, var = flu_b, new_var = bins, style = "quantile", 
#                    classes = 4, zero_class = TRUE, 
#                    bin_labs = c("Low", "Moderate", "High"),
#                    output = "data")
# 
# create_factor_bins(df, var = "flu_b", new_var = "bins", style = "quantile", 
#                    classes = 5, zero_class = TRUE, 
#                    bin_labs = c("Low", "Moderate", "Higher", "Highest"),
#                    output = "data")
# 
# create_factor_bins(df, var = flu_b, new_var = bins, style = "quantile", 
#                    classes = 3, zero_class = FALSE, 
#                    bin_labs = c("Low", "Moderate", "High"),
#                    output = "data")
# 
# create_factor_bins(df, var = flu_b, new_var = bins, style = "quantile", 
#                    classes = 3, zero_class = FALSE, 
#                    bin_labs = c("Low", "Moderate", "High"),
#                    output = "breaks")
# 
# create_factor_bins(df, var = flu_b, style = "quantile", 
#                    classes = 3, zero_class = FALSE, 
#                    bin_labs = c("Low", "Moderate", "High"),
#                    output = "data")
