#' Calculate Fit Statistics
#'
#' @param y A vector with the observable values in the dataset.
#' @param P A matrix with the the predicted values.
#' @param r A vector with naive predictions (optional).
#' @param statistics A list of statistics to be calculated. 
#' @return A matrix with the selected statistics.
#' @author Patrick C. Silva
#' @details The user can specify the following statistics: RMSE, MAD, MAPE, MEAPE, RMSLE, and MRAE.
#'  If "mrae" is specified by the user, a vector r must be provided. Otherwise, MRAE. will be not calculated and
#'  a warning messenger will be shown (The remaining statistics will be calculated in any case).
#'  If P or y contain negative numbers, RMSLE will be not calculated and a warning messenger will be 
#'  shown.
#' @examples
#' x <- rpois(100, lambda = 100)
#' Y <- 4 + 3 * x + rnorm(100)
#' P <- matrix(predict(lm(Y ~ x)), ncol = 1)
#' r <- sample(min(Y):max(Y), 100, replace = TRUE)
#' FitStats(y = Y, P = P, r = r)
#' @rdname FitStats
#' @import stats
#' @export
FitStats <- function(y, P, r = NULL, 
                          statistics = c("rmse", "mad", "mape", "meape", "rmsle", "mrae")){
   # If y=0, it adds a small number to not generate Inf in the next 
   #  in the next steps.
   y[y==0 & !is.na(y)] <- y[y==0 & !is.na(y)]+0.01
   
   # Calculate the error term
   error <- apply(P, MARGIN = 2, function(x) abs(x-y))
   
   # Calculate the percentage of error
   perc_error <- (error/abs(y)) * 100
   
   # Select the observations in y that do not have NA in error
   yn <- y[complete.cases(error)]
   
   # Select the observation in P that do not have NA in error
   Pn <- matrix(P[complete.cases(error),], ncol = ncol(P))
   
   # Keep only the complete cases of error and perc_error (Non-NAs)
   e <- matrix(error[complete.cases(error),], ncol = ncol(error))
   a <- matrix(perc_error[complete.cases(perc_error),], ncol = ncol(perc_error))
   
   # Generate the number of non-NAs rows in e 
   n <- nrow(e)
   
   # Generate a matrix to store the results
   results <- matrix(1:ncol(Pn), nrow = ncol(Pn), ncol = 1)
   # Rename first matrix rows
   rownames(results) <- colnames(P)
   
   # Calculate the fit statistics
   # Calculate RMSE
   if("rmse" %in% statistics){
      rmse <- apply(e, MARGIN = 2, function(x) sqrt(sum(x^2)/n))
      results <- cbind(results, RMSE = round(rmse, 4))
   }
   # Calculate MAD
   if("mad" %in% statistics){
      mad <- apply(e, MARGIN = 2, median)
      results <- cbind(results, MAD = round(mad, 4))
   }
   # Calculate MAPE
   if("mape" %in% statistics){
      mape <- apply(a, MARGIN = 2, function(x) sum(x)/n)
      results <- cbind(results, MAPE = round(mape, 4))
   }
   # Calculate MEAPE
   if("meape" %in% statistics){
      meape <- apply(a, MARGIN = 2, median)
      results <- cbind(results, MEAPE = round(meape, 4))
   }
   # Calculate RMSLE
   if("rmsle" %in% statistics){
      rmsle <- apply(Pn, MARGIN = 2, function(x) sqrt(sum((log(x+1)-log(yn+1))^2)/n))
      results <- cbind(results, RMSLE = round(rmsle, 4))
      # Test if P or y contain negative numbers and generate a warning messeger
      neg_numbers <- apply(Pn, MARGIN = 2, function(x) (min(x)<0 | min(yn)<0))
      if(TRUE %in% neg_numbers){
         warning("NaNs introduced because P or x contain negative numbers")
      }
   }
   if("mrae" %in% statistics){
      if(is.null(r)){
         results[,-1]
         warning("r must be specified when mrae is selected.")
      }else if (!is.null(r)){
         # If r=0, it adds a small number to not generate Inf in the next 
         #  in the next steps.
         r[r==0 & !is.na(r)] <- r[r==0 & !is.na(r)]+0.01  
         # Calculate b
         b <- abs(r-y)
         # Keep only the observations that are not NA's in error:
         bn <- b[complete.cases(error)]
         # Calculate MRAE
         mrae <- apply(e, MARGIN = 2, function(x) median(x/bn))
         results <- cbind(results, MRAE = round(mrae, 4))
      }
   }
   results[,-1]
}
