# General-Election-Predictor
2019 GE Predictor

Simple R code to take predicted vote shares for Cons, Lab, LDs, and Brexit and change assumptions about connection to constituency Brexit vote.

Nov 1st 2019: Added new Additive Model which uses simple Uniform National Swing assumption for Conservatives, Labour, and Liberal Democrats. Brexit Party are modeled as uniform across country (could be changed!). Parameters can be set to shift support for each party while maintaining average poll levels according to Brexit vote in constituency. Vote is censored at zero and one hundred percent so major changes to Brexit parameters may push dataset averages away from polling averages. 

Shiny application responding to Shiny code here is available at https://benwansell.shinyapps.io/General-Election-Predictor/
