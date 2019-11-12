# General-Election-Predictor
2019 GE Predictor

Simple R code to take predicted vote shares for Cons, Lab, LDs, and Brexit and change assumptions about connection to constituency Brexit vote.

Nov 1st 2019: Added new Additive Model which uses simple Uniform National Swing assumption for Conservatives, Labour, and Liberal Democrats. Brexit Party are modeled as uniform across country (could be changed!). Parameters can be set to shift support for each party while maintaining average poll levels according to Brexit vote in constituency. Vote is censored at zero and one hundred percent so major changes to Brexit parameters may push dataset averages away from polling averages. 

Shiny application responding to Shiny code here is available at https://benwansell.shinyapps.io/General-Election-Predictor/ (currently offline due to overuse!)

Nov 4th 2019: Added Chris Hanretty's collection of Best for Britain polling data. Added BfB data to GE predictor to compare UNS to BfB. 

Nov 5th 2019: Added code to predict Best for Britain's recommendation for tactical voting.

Nov 12th. Added a set of scenarios for each Leave and Remain 'alliance'. Note all data is now being pulled in from parlitools.
