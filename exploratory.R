# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate"))}
require("lubridate")


calldata = data.frame()
for (file in list.files("original-data")){
    calldata <- rbind(calldata, read.csv(file.path(".","original-data", file)))
}

names(calldata) <- unname(sapply(names(calldata), function(name) gsub(pattern = "[.]", replacement = "", x = name)))

isEDT <- sapply(calldata$StartTime, function(row) grepl(pattern = "EDT", x = row))
isEST <- sapply(calldata$StartTime, function(row) grepl(pattern = "EST", x = row))

calldata$StartTimeParsed[isEDT] <- as.POSIXct(calldata$StartTime[isEDT],format="%a %b %d %H:%M:%S EDT %Y",tz="EDT")
calldata$StartTimeParsed[isEST] <- as.POSIXct(calldata$StartTime[isEST],format="%a %b %d %H:%M:%S EST %Y",tz="EST")
calldata$StartTimeParsed = as.POSIXct(calldata$StartTimeParsed,origin="1970-01-01")

rm(isEDT)
rm(isEST)


# calls frequency by hour
barplot(table(hour(calldata$StartTimeParsed)))

# calls frequency by weekday
barplot(table(wday(calldata$StartTimeParsed)))

#hold time hist
hist(calldata$HoldTime)
hist(calldata$TalkTime)

# grouping by calls
callbynumber <- data.frame(aggregate(calldata$OriginatorDirectoryNumber, by=list(calldata$OriginatorDirectoryNumber), FUN=length))
names(callbynumber) <- c("phonenumber", "calls")

hist(callbynumber$calls)

# comparing groups (fake groups right now)
set.seed(1311)
controlgroup = data.frame(sample(callbynumber$calls, 300, replace = FALSE))
names(controlgroup) <- c("calls")
controlgroup$group <- "controlgroup"
shapiro.test(controlgroup$calls) # p is less than .05 so ... not gaussian dist. maybe we remove outliers but that seems like valuable data

rejectedpilot = data.frame(sample(callbynumber$calls, 300, replace = FALSE))
names(rejectedpilot) <- c("calls")
rejectedpilot$group <- "rejectedpilot"
shapiro.test(rejectedpilot$group)


acceptedpilot = data.frame(sample(callbynumber$calls, 300, replace = FALSE))
names(acceptedpilot) <- c("calls")
acceptedpilot$group <- "acceptedpilot"
shapiro.test(acceptedpilot$calls)

testtable <- rbind(controlgroup, rejectedpilot)
testtable <- rbind(testtable, acceptedpilot)

boxplot(testtable$calls~testtable$group, col= rainbow(3))

kruskal.test(testtable$calls~testtable$group) # seems correct, there should be no difference

#sanity check
controlgroup$calls = 1
controlgroup$group = "other"
testtablesanity <- rbind(testtable, controlgroup)

kruskal.test(testtablesanity$calls~testtablesanity$group)


