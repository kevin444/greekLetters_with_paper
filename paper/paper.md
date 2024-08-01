---
title: 'greekLetters: Routines for Writing Greek Letters and Mathematical Symbols on the RStudio and RGui'
tags:
  - R
  - mathematical symbols
  - statistical notation
  - unicode
authors:
  - name: Kévin Allan Sales Rodrigues
    affiliation: 1
    orcid: 0000-0003-4925-5883
affiliations:
 - name: Department of Statistics, University of São Paulo, São Paulo, Brazil
   index: 1
date: 01 August 2024
bibliography: paper.bib
---

# Summary
The R [@R] ecosystem lacks a dedicated, streamlined package for incorporating Greek letters and mathematical symbols into text outputs seamlessly. The greekLetters package addresses this need, offering a comprehensive toolkit for displaying Greek letters and various mathematical symbols in RStudio and RGui environments. Designed for ease of use, the package facilitates the inclusion of Greek letters and math equations in RStudio and RGui, enhancing the clarity and presentation of statistical notation. The package ensures compatibility across operating systems by encoding characters in UTF-8. Additionally, it supports the creation of summary functions that incorporate the functional form of fitted models with Greek letters, bridging the gap between statistical theory and practice. The package’s simplicity and accessibility make it an essential tool for enhancing the presentation and understanding of statistical concepts in R.

# Statement of need
Incorporating Greek letters and mathematical symbols in R outputs is essential for clear and accurate statistical notation, particularly in educational and professional settings. However, R lacks a package that fills this gap. Existing solutions are limited in scope and functionality, especially in environments like RGui where Unicode support is partial. The greekLetters package [@greekLetters] fills this gap by providing functions to display Greek letters and mathematical symbols consistently across RStudio and RGui. This capability is crucial for creating clear and professional statistical outputs, enhancing the communication and understanding of statistical models and results.

# Examples

Here are some straightforward examples showcasing its utility.

1. To denote the approximation of Pi, you can use:

```
    # pi constant
    paste(greeks("pi"), greeks("almostEqual"), "3.14")
    
    # output
    # "y = Xβ + ε"
```

2. The linear regression equation, in matrix form, can be elegantly displayed using Greek letters for the coefficients and error term:

```
    # Linear regression
    paste("y", " = X", greeks("beta"), " + ", greeks("epsilon"), sep ="")
    
    # output
    # "π ≈ 3.14"   
```


3. The expected value of a random variable X can be represented as:

```
    # Expected value
    paste("E[X] = ", greeks("integral"), "xf(x)dx", sep = "")
    
    # output
    # "E[X] = ∫xf(x)dx"
```

4. The notation for testing a statistical hypothesis can be shown as:

```
    # testing statistical hypothesis
    paste(greeks("H_0"), ":", greeks("mu"), " = 0")
    paste("versus", greeks("H_1"), ":", greeks("mu"), greeks("notEqual"), " 0" )
    
    # output
    # "H<sub>0</sub> : μ  = 0"
    # "versus H~1~ : μ ≠ 0"
```

By using the greekLetters package, these examples demonstrate how to effectively incorporate Greek letters and mathematical symbols into R outputs, enhancing the clarity of R statistical outputs.

# Package Availability

The greekLetters package is hosted on the official CRAN (The Comprehensive R Archive Network) repository, ensuring its reliability and easy access for all R users. The package can be found and downloaded via the following link: [https://cran.r-project.org/package=greekLetters](https://cran.r-project.org/package=greekLetters). Being available on CRAN ensures that the package has undergone rigorous quality and compliance checks, guaranteeing its compatibility with different versions of R and various operating systems. Additionally, its presence on CRAN facilitates the installation and updating of the package directly through R using simple commands like:

```
    install.packages("greekLetters")
```

This accessibility and ongoing support make greekLetters a valuable tool for anyone needing to incorporate Greek letters and mathematical symbols into their R outputs.



# Testing and documentation
The greekLetters package undergoes rigorous testing to ensure its robustness and reliability across different operating systems and R environments. Each function is tested to verify the accurate rendering of Greek letters and mathematical symbols, with particular attention to the compatibility of Unicode characters. Extensive documentation accompanies the package, featuring detailed descriptions and examples for each function. This thorough documentation aids users in effectively utilizing the package's capabilities, ensuring they can integrate Greek letters and mathematical symbols into their R outputs with ease. The package's comprehensive testing and documentation guarantee a reliable and user-friendly experience.


# References
