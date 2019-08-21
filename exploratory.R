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

callbynumber <- data.frame(aggregate(calldata$OriginatorDirectoryNumber, by=list(calldata$OriginatorDirectoryNumber), FUN=length))
names(callbynumber) <- c("phonenumber", "calls")

hist(callbynumber$calls)

