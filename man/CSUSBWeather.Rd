\name{CSUSBWeather}
\alias{CSUSBWeather}
\title{Retrieve the daily weather at CSUSB}
\value{
  A data.frame of today's weather.
}
\description{
  Retrieves the weather at CSUSB for today.
}
\examples{
today <- CSUSBWeather()
par(mfrow = c(2, 1))
plot(today$times, today$temp.raw, type = 'l')
max.time <- today$times[today$temp.raw == max(today$temp.raw)]
max.time <- max.time[length(max.time)]
max.temp <- max(today$temp.raw)
points(max.time, max.temp, col = 'red', pch = 16, cex = 1.5)
plot(today$times, today$hum.pct, type = 'l')
}
\author{
  Kevin Middleton
}

