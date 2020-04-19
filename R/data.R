#====================================== mtcars ===================================================

#'@title Motor Trend Car Road Tests
#'
#'@description The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models).
#'
#'@format  data frame with 32 observations on 11 (numeric) variables.
#'\describe{
#'\item{mpg}{Miles/(US) gallon}
#'\item{cyl}{Number of cylinders}
#'\item{disp}{Displacement (cu.in.)}
#'\item{hp}{Gross horsepower}
#'\item{drat}{Rear axle ratio}
#'\item{wt}{Weight (1000 lbs)}
#'\item{qsec}{1/4 mile time}
#'\item{vs}{Engine (0 = V-shaped, 1 = straight)}
#'\item{am}{Transmission (0 = automatic, 1 = manual)}
#'\item{gear}{Number of forward gears}
#'\item{carb}{Number of carburetors}
#'}
#'
#'@note Henderson and Velleman (1981) comment in a footnote to Table 1: ‘Hocking [original transcriber]'s noncrucial coding of the Mazda's rotary engine as a straight six-cylinder engine and the Porsche's flat engine as a V engine, as well as the inclusion of the diesel Mercedes 240D, have been retained to enable direct comparisons to be made with previous analyses.
#'
#'@source Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411.

"mtcars"


#====================================== vocab ===================================================

#'@title Vocabulary and Education
#'
#'@description The Vocab data frame has 30,351 rows and 4 columns. The observations are respondents to U.S. General Social Surveys, 1972-2016.
#'
#'@format  This data frame contains the following columns:
#'\describe{
#'\item{year}{Year of the survey.}
#'\item{sex}{Sex of the respondent, Female or Male.}
#'\item{education}{Education, in years.}
#'\item{vocabulary}{Vocabulary test score: number correct on a 10-word test.}
#'}
#'
#'@source HNational Opinion Research Center General Social Survey. GSS Cumulative Datafile 1972-2016, originally downloaded from \code{\link{http://gss.norc.org/}}.
#'
#'@references
#'Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
#'
#'Fox, J. and Weisberg, S. (2019) An R Companion to Applied Regression, Third Edition, Sage.

"Vocab"


#====================================== MplsStops ===================================================

#'@title Minneapolis Police Department 2017 Stop Data
#'
#'@description Results of nearly all stops made by the Minneapolis Police Department for the year 2017.
#'
#'@format A data frame with 51857 observations on the following 14 variables.
#'\describe{
#'\item{idNum}{character vector of incident identifiers.}
#'\item{date}{a POSIXlt date variable giving the date and time of the stop.}
#'\item{problem}{a factor with levels suspicious for suspicious vehicle or person stops and traffic for traffic stops.}
#'\item{citationIssued}{a factor with levels no yes indicating if a citation was issued.}
#'\item{personSearch}{a factor with levels no yes indicating if a citation was issued.}
#'\item{vehicleSearch}{a factor with levels no or yes indicating if a vehicle was searched.}
#'\item{preRace}{a factor with levels white, black, east african, latino, native american, asian, other, unknown for the officer's assessment of race of the person stopped before speaking with the person stopped.}
#'\item{race}{a factor with levels white, black, east african, latino, native american, asian, other, unknown, officer's determination of race after the incident.}
#'\item{gender}{a factor with levels female, male, unknown, gender of person stopped.}
#'\item{lat}{latitude of the location of the incident, somewhat rounded.}
#'\item{long}{latitude of the location of the incident, somewhat rounded.}
#'\item{policePrecinct}{Minneapolis Police Precinct number.}
#'\item{neighborhood}{a factor with 84 levels giving the name of the Minneapolis neighborhood of the incident.}
#'\item{MDC}{a factor with levels mdc for data collected via in-vehicle computer, and other for data submitted by officers not in a vehicle, either on foot, bicycle or horseback. Several of the variables above were recorded only in-vehicle.}
#'}
#'
#'@details A few stops have been deleted, either because thesu location data was missing, or a few very rare categories were also removed. Demographics are available for 84 of Minneaolis' 87 neighborhoods. The remaining 3 presumably have no housing.
#'@source These are public data obtained from <http://opendata.minneapolismn.gov/datasets/police-stop-data>. A few more fields, and more data, are available at the original source.
#'
#'@examples

"MplsStops"
