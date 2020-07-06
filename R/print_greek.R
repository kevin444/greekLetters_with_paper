#main author: Kévin Allan Sales Rodrigues
#' Print Summary for Linear Model Fits With Greek Letters
#'
#' @description print summary method with Greek letters for class "lm".
#'
#' @inheritParams base::print
#' @inheritDotParams base::print
#' @inheritParams stats::summary.lm
#' @inheritDotParams stats::summary.lm
#'
#' @param concise logical.
#'
#' @return The function is like print.summary.lm but with Greek
#' letters in output.
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
#' @importFrom stats quantile
#' @importFrom stats printCoefmat
#' @importFrom stats naprint
#' @importFrom stats symnum
#' @importFrom stats pf
#'
#' @author Kévin Allan Sales Rodrigues.
#'
#' @examples
#'
#' \dontrun{
#' #Same example as summary.lm and print.summary.lm from stat packages but with Greek letters.
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
#' }
#'
#' @export print.summary_greek
#' @export
print.summary_greek <-
  function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor,
            signif.stars = getOption("show.signif.stars"), concise = FALSE, ...)
  {
    cat("\nCall:", if(!concise) "\n" else " ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        if (!concise) "\n\n", sep = "")
    cat("Statistical Model of form: y = X", greek$beta, " + ", greek$epsilon, sep = "", collapse = "\n\n")
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    if (!concise) {
      cat(if (!is.null(x$weights) && diff(range(x$weights)))
        "Weighted ", "Residuals:\n", sep = "")
    }
    if (rdf > 5L) {
      nam <- c("Min", greekLetters::greeks("Q_1"), "Median", greekLetters::greeks("Q_3"), "Max")
      rq <- if (length(dim(resid)) == 2L)
        structure(apply(t(resid), 1L, stats::quantile), dimnames = list(nam,
                                                                 dimnames(resid)[[2L]]))
      else {
        zz <- zapsmall(stats::quantile(resid), digits + 1L)
        structure(zz, names = nam)
      }
      if (!concise) print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
      print(resid, digits = digits, ...)
    }
    else {
      cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
      cat("\n")
    }
    if (length(x$aliased) == 0L) {
      cat("\nNo Coefficients\n")
    }
    else {
      if (nsingular <- df[3L] - df[1L])
        cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n",
            sep = "")
      else { cat("\n"); if (!concise) cat("Coefficients:\n")  }
      coefs <- x$coefficients
      if (!is.null(aliased <- x$aliased) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn,
                                                                colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      stats::printCoefmat(coefs, digits = digits, signif.stars = signif.stars, signif.legend = (!concise),
                   na.print = "NA", eps.Pvalue = if (!concise) .Machine$double.eps else 1e-4, ...)
    }
    cat("\nResidual standard error:", format(signif(x$sigma,
                                                    digits)), "on", rdf, "degrees of freedom")
    cat("\n")
    if (nzchar(mess <- stats::naprint(x$na.action)))
      cat("  (", mess, ")\n", sep = "")
    if (!is.null(x$fstatistic)) {
      cat("Multiple ", greekLetters::greeks("R^2"),":",  formatC(x$r.squared, digits = digits))
      cat(",\tAdjusted ", greekLetters::greeks("R^2"),":", formatC(x$adj.r.squared,
                                             digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L],
                                                                                         digits = digits), "on", x$fstatistic[2L], "and",
          x$fstatistic[3L], "DF,  p-value:", format.pval(stats::pf(x$fstatistic[1L],
                                                            x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),
                                                         digits = digits, if (!concise) .Machine$double.eps else 1e-4))
      cat("\n")
    }
    correl <- x$correlation
    if (!is.null(correl)) {
      p <- NCOL(correl)
      if (p > 1L) {
        cat("\nCorrelation of Coefficients:\n")
        if (is.logical(symbolic.cor) && symbolic.cor) {
          print(stats::symnum(correl, abbr.colnames = NULL))
        }
        else {
          correl <- format(round(correl, 2), nsmall = 2,
                           digits = digits)
          correl[!lower.tri(correl)] <- ""
          print(correl[-1, -p, drop = FALSE], quote = FALSE)
        }
      }
    }
    cat("\n")
    invisible(x)
  }
