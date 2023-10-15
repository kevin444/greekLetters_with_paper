#main author: Kévin Allan Sales Rodrigues
#' Function that returns strings with Greek letters
#'
#' Function to obtain strings with Greek letters and mathematical symbols
#' with or without subscripts and superscripts.
#'
#'
#' @param greekletter is a string that contains the Greek letter name.
#'
#'
#' @returns Returns the desired Greek letter or mathematical symbol in string format.
#'
#' @details
#'
#' It is recommended that the font size of the R console be increased for
#' better visualization of the symbols,
#' as some of the symbols are quite small.
#'
#' The subscripts and supersripts are restricted to numbers 0 to 9.
#'
#' For users working with Rgui this function don't accept subscripts and just accept superscripts 1, 2 and 3. For more details run the commands:
#'
#' symbols printed as a "rectangle" don't work in RGui, but these symbols work normally in RStudio.
#'
#' To see names of symbols use \code{\link{print_greeks}}.
#'
#' @importFrom stringr str_extract
#' @importFrom assertthat assert_that
#' @export greeks
#' @author Kévin Allan Sales Rodrigues.
#'
#' @examples
#' \donttest{
#' # Linear regression
#' paste("y", " = X", greeks("beta"), " + ", greeks("epsilon"), sep ="")
#'
#' # pi constant
#' paste(greeks("pi"), greeks("almostEqual"), "3.14")
#'
#' # Expected value
#' paste("E[X] = ", greeks("integral"), "xf(x)dx", sep = "")
#'
#' greeks("sigma^2")
#'
#' greeks("Delta^5")
#'
#' greeks("yourname^9")
#'
#' # testing statistical hypothesis
#' paste(greeks("H_0"),":", greeks("mu"), " = 0")
#' paste("versus", greeks("H_1"), ":", greeks("notEqual"), " 0" )
#'
#' # returns the Greek letters
#' paste("The Greek letters", greeks("alpha"), greeks("beta"), "...", greeks("omega"), ".", sep=", ")
#' }
#'

##############
# all Greek letters and mathematical symbols names
# greek_vector
#
# all Greek letters and mathematical symbols names
# greek

greeks = function(greekletter){

  greek_vector = c( # lowercase Greek letters
    alpha='\u03b1', beta='\u03b2', gamma='\u03b3', delta='\u03b4', epsilon='\u03b5', zeta='\u03b6',
    eta='\u03b7', theta='\u03b8', iota='\u03b9', kappa='\u03ba', lambda='\u03bb', mu='\u03bc',
    nu='\u03bd', xi='\u03be', omicron='\u03bf', pi='\u03c0', rho='\u03c1', sigma='\u03c3', tau='\u03c4',
    upsilon='\u03c5', phi='\u03c6', chi='\u03c7', psi='\u03c8', omega='\u03c9',
    # uppercase Greek letters
    Alpha='\u0391', Beta='\u0392', Gamma='\u0393', Delta='\u0394', Epsilon='\u0395', Zeta='\u0396',
    Eta='\u0397', Theta='\u0398', Iota='\u0399', Kappa='\u039a', Lambda='\u039b', Mu='\u039c',
    Nu='\u039d', Xi='\u039e', Omicron='\u039f', Pi='\u03a0', Rho='\u03a1', Sigma='\u03a3', Tau='\u03a4',
    Upsilon='\u03a5', Phi='\u03a6', Chi='\u03a7', Psi='\u03a8', Omega='\u03a9',
    # mathematical symbols
    infinity ='\u221e', leftrightarrow ='\u21d4', forall='\u2200', exist ='\u2203', notexist ='\u2204',
    emptyset ='\u2205', elementof='\u2208', notelementof='\u2209', proportional='\u221d',
    asymptoticallyEqual='\u2243', notasymptoticallyEqual='\u2244', approxEqual='\u2245', almostEqual='\u2248',
    leq='\u2264', geq='\u2265', muchless='\u226a', muchgreater='\u226b', leftarrow='\u21d0', rightarrow='\u21d2',
    equal='\uff1d', notEqual='\u2260', integral='\u222b', doubleintegral='\u222c', tripleintegral='\u222d',
    logicalAnd='\u2227', logicalOr='\u2228', intersection='\u2229', union='\u222a')

  #checking encoding, UTF-8 is required for correct printing
  #> Encoding(greek_vector)
  #[1] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[11] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[21] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[31] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[41] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[51] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[61] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"
  #[71] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"


  greek = list( # lowercase Greek letters
    alpha='\u03b1', beta='\u03b2', gamma='\u03b3', delta='\u03b4', epsilon='\u03b5', zeta='\u03b6',
    eta='\u03b7', theta='\u03b8', iota='\u03b9', kappa='\u03ba', lambda='\u03bb', mu='\u03bc',
    nu='\u03bd', xi='\u03be', omicron='\u03bf', pi='\u03c0', rho='\u03c1', sigma='\u03c3', tau='\u03c4',
    upsilon='\u03c5', phi='\u03c6', chi='\u03c7', psi='\u03c8', omega='\u03c9',
    # uppercase Greek letters
    Alpha='\u0391', Beta='\u0392', Gamma='\u0393', Delta='\u0394', Epsilon='\u0395', Zeta='\u0396',
    Eta='\u0397', Theta='\u0398', Iota='\u0399', Kappa='\u039a', Lambda='\u039b', Mu='\u039c',
    Nu='\u039d', Xi='\u039e', Omicron='\u039f', Pi='\u03a0', Rho='\u03a1', Sigma='\u03a3', Tau='\u03a4',
    Upsilon='\u03a5', Phi='\u03a6', Chi='\u03a7', Psi='\u03a8', Omega='\u03a9',
    # mathematical symbols
    infinity ='\u221e', leftrightarrow ='\u21d4', forall='\u2200', exist ='\u2203', notexist ='\u2204',
    emptyset ='\u2205', elementof='\u2208', notelementof='\u2209', proportional='\u221d',
    asymptoticallyEqual='\u2243', notasymptoticallyEqual='\u2244', approxEqual='\u2245', almostEqual='\u2248',
    leq='\u2264', geq='\u2265', muchless='\u226a', muchgreater='\u226b', leftarrow='\u21d0', rightarrow='\u21d2',
    equal='\uff1d', notEqual='\u2260', integral='\u222b', doubleintegral='\u222c', tripleintegral='\u222d',
    logicalAnd='\u2227', logicalOr='\u2228', intersection='\u2229', union='\u222a')

  #checking encoding, UTF-8 is required for correct printing
  # for (i in greek) {
  #   print(Encoding(i))
  # }
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"
  # [1] "UTF-8"

  superscriptSymbols = c(
    sup0 = '\u2070',
    sup1 = '\u00B9',
    sup2 = '\u00B2',
    sup3 = '\u00B3',
    sup4 = '\u2074',
    sup5 = '\u2075',
    sup6 = '\u2076',
    sup7 = '\u2077',
    sup8 = '\u2078',
    sup9 = '\u2079')

  #checking encoding, UTF-8 is required for correct printing
  # > Encoding(superscriptSymbols)
  # [1] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"

  subscriptSymbols = c(
    sub0 = '\u2080',
    sub1 = '\u2081',
    sub2 = '\u2082',
    sub3 = '\u2083',
    sub4 = '\u2084',
    sub5 = '\u2085',
    sub6 = '\u2086',
    sub7 = '\u2087',
    sub8 = '\u2088',
    sub9 = '\u2089')


  assertthat::validate_that( is.character(greekletter), msg = "Your argument isn't a string, please insert a string.")

  letter = stringr::str_extract(greekletter, "[a-zA-Z]+") #extract Greek letter

  if(letter %in% names(greek)){ letter = greek_vector[letter]}

  subscript = stringr::str_extract(greekletter, "_[0-9]") #extract subscript, otherwise NA
  subscript = substr(subscript, 2, 2)

  superscript = stringr::str_extract(greekletter, "\\^[0-9]") #extract superscript, otherwise NA
  superscript = substr(superscript, 2, 2)


  return(enc2utf8(paste(letter, if(!is.na(subscript)){subscriptSymbols[paste("sub", subscript, sep="")]},
                        if(!is.na(superscript)){superscriptSymbols[paste("sup", superscript, sep="")]}, sep="")))


  invisible()
}

#main author: Kévin Allan Sales Rodrigues
#' Print all Greek letters and mathematical symbols names
#'
#' Function to Print all Greek letters and mathematical symbols names.
#'
#'
#' @export
#'
#' @returns Print all Greek letters and mathematical symbols names.
#'
#' @details
#'
#' It is recommended that the font size of the R console be increased for
#' better visualization of the symbols,
#' as some of the symbols are quite small.
#'
#' The subscripts and supersripts are restricted to numbers 0 to 9.
#'
#' Symbols printed as a "rectangle" don't work in RGui, but these symbols work normally in RStudio.
#'
#'
#' @author Kévin Allan Sales Rodrigues.
#'
#'
#' @examples
#' \donttest{
#' #all Greek letters and mathematical symbols names
#' print_greeks()
#'}
#'

print_greeks = function(){

  greek_vector = c( # lowercase Greek letters
    alpha='\u03b1', beta='\u03b2', gamma='\u03b3', delta='\u03b4', epsilon='\u03b5', zeta='\u03b6',
    eta='\u03b7', theta='\u03b8', iota='\u03b9', kappa='\u03ba', lambda='\u03bb', mu='\u03bc',
    nu='\u03bd', xi='\u03be', omicron='\u03bf', pi='\u03c0', rho='\u03c1', sigma='\u03c3', tau='\u03c4',
    upsilon='\u03c5', phi='\u03c6', chi='\u03c7', psi='\u03c8', omega='\u03c9',
    # uppercase Greek letters
    Alpha='\u0391', Beta='\u0392', Gamma='\u0393', Delta='\u0394', Epsilon='\u0395', Zeta='\u0396',
    Eta='\u0397', Theta='\u0398', Iota='\u0399', Kappa='\u039a', Lambda='\u039b', Mu='\u039c',
    Nu='\u039d', Xi='\u039e', Omicron='\u039f', Pi='\u03a0', Rho='\u03a1', Sigma='\u03a3', Tau='\u03a4',
    Upsilon='\u03a5', Phi='\u03a6', Chi='\u03a7', Psi='\u03a8', Omega='\u03a9',
    # mathematical symbols
    infinity ='\u221e', leftrightarrow ='\u21d4', forall='\u2200', exist ='\u2203', notexist ='\u2204',
    emptyset ='\u2205', elementof='\u2208', notelementof='\u2209', proportional='\u221d',
    asymptoticallyEqual='\u2243', notasymptoticallyEqual='\u2244', approxEqual='\u2245', almostEqual='\u2248',
    leq='\u2264', geq='\u2265', muchless='\u226a', muchgreater='\u226b', leftarrow='\u21d0', rightarrow='\u21d2',
    equal='\uff1d', notEqual='\u2260', integral='\u222b', doubleintegral='\u222c', tripleintegral='\u222d',
    logicalAnd='\u2227', logicalOr='\u2228', intersection='\u2229', union='\u222a')

  print(greek_vector)
  }

##functions loaded when packages is loaded
#.onLoad = function(libname, pkgname){


#checking encoding, UTF-8 is required for correct printing
# > Encoding(subscriptSymbols)
# [1] "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8" "UTF-8"

