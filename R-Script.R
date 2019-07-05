library(ggplot2)
library(purrr)
library(readr)

# pdf export
# library(grid)
# library(gridGraphics)

setwd("D:/workspace/master-thesis")

printPdf <- function(graphic) {
  pdffile <- sprintf("thesis/pics/MainSequencePoster4.pdf")
  pdf(pdffile,
      paper = "special",
      width = 6.0,
      height = 6.0)
  pushViewport(viewport(layout = grid.layout(1, 1)))
  print(graphic, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  dev.off()
}

chi19s2 <-
  read_delim(
    "D:/workspace/master-thesis/data-analysis_previous-studies/chi19s2.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

chi19s3 <-
  read_delim(
    "D:/workspace/master-thesis/data-analysis_previous-studies/chi19s3.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

ac2 <-
  read_delim(
    "D:/workspace/master-thesis/data-analysis_previous-studies/ac2.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

filterByIdeaSubmits <-
  function(csvData, timerValue, minIdeaSubmits) {
    columnName <- paste(timerValue, "-ideaSubmitTimers", sep = "")
    filteredData <- csvData[csvData[, columnName] >= minIdeaSubmits, ]
    return(filteredData)
  }

getMedianRequests <- function (csvData, timerValue) {
  columnName <- paste(timerValue, "-inspirationRequestTimers", sep = "")
  return(median(data.matrix(csvData[, columnName])))
}


getHistogram <- function(csvData, ideaSubmitMinimum, timerValue, maxBuckets = 12) {
  data <- filterByIdeaSubmits(csvData, timerValue, ideaSubmitMinimum)
  
  columnName <-
    paste(timerValue, "-inspirationRequestTimers", sep = "")
  datavector <- data.matrix(data[, columnName])
  hist(
    ifelse(datavector >= maxBuckets, maxBuckets, datavector),
    breaks = c(0:maxBuckets),
    xlim = c(0, maxBuckets),
    xlab = "Inspiration requests",
    xaxt = "n",
    ylab = "Participants",
    cex.lab = 1.5,
    cex.main = 1.5,
    #cex.axis = 1,
    col = "slateblue1",
    main = deparse(substitute(csvData))
    
  )
  axis(1, at = 1:maxBuckets - 0.5, labels = c(0:(maxBuckets-2), paste(">", maxBuckets-1, sep = "")))
  abline(v = getMedianRequests(data, timerValue) + 0.5,
         col = "blue",
         lwd = 2)
}

getTypes <-
  function(csvData,
           timerValue,
           ideaSubmitMinimum,
           ideaSubmitTimer = timerValue,
           seeker) {
    data <- filterByIdeaSubmits(csvData, ideaSubmitTimer, ideaSubmitMinimum)
    
    median <- getMedianRequests(data, timerValue)
    
    columnName <-
      paste(timerValue, "-inspirationRequestTimers", sep = "")

    if (seeker) {
      filteredData <- data[data[, columnName] > median, ]
    } else {
      # filteredData <- data[data[, columnName] <= median, ]
      filteredData <- data[data[, columnName] <= 2, ]
    }
    return(filteredData$"workerId")
  }

plotSuccessRates <- function(csvData, timerEndValue, falsePositive = TRUE) {
  getSuccessRate <-
    function(timerValueBefore,
             timerValueAfter,
             seeker) {
      ideatorsBefore <-
        getTypes(
          csvData,
          timerValue = timerValueBefore,
          # ideaSubmitTimer = timerValueBefore,
          # ideaSubmitMinimum = 1,
          ideaSubmitTimer = timerValueAfter,
          ideaSubmitMinimum = 3,
          seeker = seeker
        )
      ideatorsAfter <-
        getTypes(
          csvData,
          timerValue = timerValueAfter,
          ideaSubmitTimer = timerValueAfter,
          ideaSubmitMinimum = 3,
          seeker = seeker
        )
      
      if (falsePositive) {
        # false-positive: 
        rate <-
          length(setdiff(ideatorsBefore, ideatorsAfter)) / length(ideatorsBefore)
        return(rate)
      } else {
        # false-negative
        rate <-
          length(setdiff(ideatorsAfter, ideatorsBefore)) / length(ideatorsAfter)
        return(rate)
      }
    }
  
  getSeekerSuccessRateComparedToEnd <- function(timerValue) {
    return(
      getSuccessRate(
        timerValueBefore = timerValue,
        timerValueAfter = timerEndValue,
        seeker = TRUE
      )
    )
  }
  
  getAvoiderSuccessRateComparedToEnd <- function(timerValue) {
    return(
      getSuccessRate(
        timerValueBefore = timerValue,
        timerValueAfter = timerEndValue,
        seeker = FALSE
      )
    )
  }
  
  timerValues <- seq(from = 120, to = timerEndValue, by = 60)
  seekerSuccessRates <-
    as.numeric(map(timerValues, getSeekerSuccessRateComparedToEnd))
  avoiderSuccessRates <-
    as.numeric(map(timerValues, getAvoiderSuccessRateComparedToEnd))
  #qplot(timerValues, seekerSuccessRates, geom="line")
  #qplot(timerValues, avoiderSuccessRates, geom="line")
  
  plot(
    timerValues,
    seekerSuccessRates,
    col = "red",
    type = "l",
    main = deparse(substitute(csvData)),
    xlab = "Seconds",
    ylab = "Failure rate"
  )
  lines(timerValues, avoiderSuccessRates, col = "blue", )
  legend(
    x = "topright",
    legend = c("Seeker", "Avoider"),
    col = c("red", "blue"),
    pch = 15
  )
  
}
par(mfrow = c(2, 2))
plotSuccessRates(chi19s2, 900)
plotSuccessRates(chi19s3, 1500)
plotSuccessRates(ac2, 1200)
plot.new()
par(mfrow = c(2, 2))
plotSuccessRates(chi19s2, 900, falsePositive = FALSE)
plotSuccessRates(chi19s3, 1500, falsePositive = FALSE)
plotSuccessRates(ac2, 1200, falsePositive = FALSE)
plot.new()
par(mfrow = c(2, 2))
getHistogram(csvData = chi19s2,
             ideaSubmitMinimum = 3,
             maxBuckets = 15,
             timerValue = 900)
getHistogram(csvData = chi19s3,
             ideaSubmitMinimum = 3,
             timerValue = 1500)
getHistogram(csvData = ac2,
             ideaSubmitMinimum = 3,
             timerValue = 1200)

length(getTypes(csvData = chi19s3,
         ideaSubmitMinimum = 3,
         timerValue = 1500,
         seeker = TRUE))

#################################################
############### PDF GENERIERUNG #################
#################################################
if (FALSE) {
  
pdf("thesis/pics/prestudy/histograms_inspiration-requests_fulltime_min3submits.pdf", width = 9, height = 3)
par(mfrow = c(1, 3))
getHistogram(csvData = chi19s2, 
             ideaSubmitMinimum = 3,
             maxBuckets = 8,
             timerValue = 900)
getHistogram(csvData = chi19s3,
             ideaSubmitMinimum = 3,
             timerValue = 1500)
getHistogram(csvData = ac2,
             ideaSubmitMinimum = 3,
             timerValue = 1200)
dev.off()

pdf("thesis/pics/prestudy/sucessRates_min3submits.pdf", width = 9, height = 3)
par(mfrow = c(1, 3))
plotSuccessRates(chi19s2, 900)
plotSuccessRates(chi19s3, 1500)
plotSuccessRates(ac2, 1200)
dev.off()

}
#################################################

filteredData <- filterByIdeaSubmits(chi19s3, 1500, 3)
datavector <- data.matrix(filteredData[, "1500-ideaSubmitTimers"])
p4 <- ggplot(data = filteredData, aes(x = datavector)) +
  geom_histogram(fill = "red", bins = 8, breaks = c(0:7)) +
  ylab("Number of Participants") +
  scale_x_continuous(name = "Number of Inspiration Requests", c(0:7))

p4

# printPdf(p4)
