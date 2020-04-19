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
