#main author: Kévin Allan Sales Rodrigues
#' Summarizing Linear Model Fits With Greek Letters
#'
#' @description summary method with Greek letters for class "lm".
#'
#' @inheritParams stats::summary.lm
#' @inheritDotParams stats::summary.lm
#'
#' @return The function is like summary.lm but with Greek
#' letters in output.
#'
#'
#' @details
#'
#' It is recommended that the font size of the R console be increased for
#' better visualization of the symbols,
#' as some of the symbols are quite small.
#'
#'
#' @seealso See \code{\link[stats]{summary.lm}} for more details.
#'
#' @importFrom stats summary.lm
#' @author Kévin Allan Sales Rodrigues.
#'
#' @examples
#'
#' \dontrun{
#' Same example as summary.lm but
#' with Greek letters
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' lm.D90 <- lm(weight ~ group - 1) # omitting intercept
#' coef(lm.D90)  # the bare coefficients
#' sld90 <- greekLetters::summary_greek(lm.D90 <- lm(weight ~ group -1))  # omitting intercept
#' greekLetters::print.summary_greek(sld90)
#'  }
#'

##############
#' @export summary_greek
#' @export
summary_greek = function(object, correlation = FALSE, symbolic.cor = FALSE, ...){

 ss= stats::summary.lm(object, correlation = FALSE, symbolic.cor = FALSE, ...)

 colnames(ss$coefficients) = c(greek$beta, greek$sigma, "t value", "Pr(>|t|)")

 class(ss) = "summary_greek"
 return(ss)

}

