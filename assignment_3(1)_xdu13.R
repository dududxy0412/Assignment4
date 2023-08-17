# # ===================================================
# GBA464: RFM analysis on CDNOW data
# Author: Yufeng Huang
# Description: Lab on functions and loops
# Data: CDNOW customer data (this time full data)
# Source: provided by Professor Bruce Hardie on
#   http://www.brucehardie.com/datasets/CDNOW_sample.zip
# ===================================================

# ====== CLEAR EVERYTHING ======
rm(list = ls())

# ====== READ TRIAL DATA =======

url <- 'https://dl.dropboxusercontent.com/s/xxfloksp0968mgu/CDNOW_sample.txt'
if (!file.exists('CDNOW_sample.txt')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'CDNOW_sample.txt')
}
df.raw <- read.fwf('CDNOW_sample.txt', width = c(6, 5, 9, 3, 8), stringsAsFactors = F)  # load data

# ====== Section 2: loading the data ======

df.raw[[1]] <- NULL # drop old id
names(df.raw) <- c("id", "date", "qty", "expd")

# a) generate year and month
date <- as.character(df.raw$date)
year <- substr(date, start = 1, stop = 4)
month <-substr(date,start=5, stop = 6)
df <- cbind(df.raw,year,month)

# b) aggregate into monthly data with number of trips, total quantity, and total expenditure

df.qty <- aggregate(x = list(qty = df$qty),
                        by = list(id = df$id,
                                  year = df$year,
                                  month = df$month),
                        FUN = sum)

df.expd <- aggregate(x = list(
                               expd = df$expd),
                      by = list(id = df$id,
                                year = df$year,
                                month = df$month),
                      FUN = sum)

df.trip <- aggregate(x = list(trip= df$qty),
                     by = list(id = df$id,
                               year = df$year,
                               month = df$month),
                     FUN = length)

# c) generate a table of year-months, merge, replace no trip to zero.
# Hint: how do you deal with year-months with no trip? These periods are not in the original data,
#   but you might need to have these periods when you calcualte RFM, right?
# Consider expanding the time frame using expand.grid() but you do not have to.
df.ymonth <- merge(df.trip,df.qty,by = c("year","month","id"))
df.ymonth <- merge(df.ymonth,df.expd,by = c("year","month","id"))
DATE <- expand.grid(year = seq(1997, 1998, 1), month = seq(01, 12, 1),
                       id = seq(1, 1000, 1))
DATE<- DATE[DATE$year != 1998 | DATE$month <= 6, ]

DATE$month <- as.numeric(DATE$month)
df.ymonth$month <- as.numeric(df.ymonth$month)


DATE$month <- sprintf("%02d", DATE$month)
df.ymonth$month <- sprintf("%02d", df.ymonth$month)


df <- merge(DATE, df.ymonth, by = c("year", "month", "id"), all = TRUE)
df[is.na(df)] <-0

# now we should have the dataset we need;
#   double check to make sure that every consumer is in every period (18 months in total)


# ====== Section 3.1: recency ======
# use repetition statement, such as a "for-loop", to generate a recency measure for each consumer 
#   in each period. Hint: if you get stuck here, take a look at Example 3 when we talked about "for-loops"
#   call it df$recency

# ... (previous code remains unchanged)

df$recency <- NA
df$year <- as.numeric(as.character(df$year))
df$month <- as.numeric(as.character(df$month))
last_purchase_year <- rep(NA_real_, max(df$id))
last_purchase_month <- rep(NA_real_, max(df$id))
for (i in 1:nrow(df)) {
    current_id <- df$id[i]
    if (df$year[i] == 1997 && df$month[i] == 1 || is.na(last_purchase_year[current_id])) {
        df$recency[i] <- NA
    } else {
        df$recency[i] <- (df$year[i] - last_purchase_year[current_id]) * 12 +
            df$month[i] - last_purchase_month[current_id]
    }
    if (df$trip[i] > 0) {
        last_purchase_year[current_id] <- df$year[i]
        last_purchase_month[current_id] <- df$month[i]
    }
}
head(df)

# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be six quarters in the 1.5-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency
get_previous_quarter <- function(year, month) {
    if (month <= 3) {
        return(list(year=year-1, start_month=10, end_month=12))
    } else if (month <= 6) {
        return(list(year=year, start_month=1, end_month=3))
    } else if (month <= 9) {
        return(list(year=year, start_month=4, end_month=6))
    } else {
        return(list(year=year, start_month=7, end_month=9))
    }
}

for (i in 1:nrow(df)) {
    current_id <- df$id[i]
    prev_quarter <- get_previous_quarter(df$year[i], df$month[i])
    
    df$frequency[i] <- sum(df$trip[df$id == current_id & 
                                         df$year == prev_quarter$year & 
                                         df$month >= prev_quarter$start_month & 
                                         df$month <= prev_quarter$end_month])
}
head(df)

# ====== Section 3.3: monetary value ======
# average monthly expenditure in the months with trips (i.e. when expenditure is nonzero)
#   for each individual in each month, find the average expenditure from the start of the sample to 
#   the PAST MONTH. Call this df$monvalue

df$monvalue <- NA

cumulative_expenditure <- rep(0, length(unique(df$id)))
names(cumulative_expenditure) <- unique(df$id)

cumulative_months_with_trips <- rep(0, length(unique(df$id)))
names(cumulative_months_with_trips) <- unique(df$id)

for (i in 1:nrow(df)) {
    current_id <- df$id[i]

    if (df$trip[i] > 0) {
        cumulative_expenditure[current_id] <- cumulative_expenditure[current_id] + df$expd[i]
        cumulative_months_with_trips[current_id] <- cumulative_months_with_trips[current_id] + 1
    }

    if (cumulative_months_with_trips[current_id] > 0) {
        df$monvalue[i] <- cumulative_expenditure[current_id] / cumulative_months_with_trips[current_id]
    }
}

for (id in unique(df$id)) {
    first_index <- which(df$id == id)[1]
    df$monvalue[first_index] <- NA
}

summary(df)
# ====== Section 4: Targeting using RFM ======
# now combine these and construct an RFM index
#   You only need to run this section.

b1 <- -0.05
b2 <- 3.5
b3 <- 0.05

df$index <- b1*df$recency + b2*df$frequency + b3*df$monvalue


# ====== using the RFM index (Optional, not graded) =======
# validation: check whether the RFM index predict customer purchase patterns
# Order your sample (still defined by keys of consumer-year-month) based on the RFM index. 
#   Split your sample into 10 groups. The first group is top 10% in terms of
#   the RFM index; second group is 10%-20%, etc.
# Make a bar plot on the expected per-month expenditure that these consumers generate and comment on 
#   whether the RFM index help you segment which set of customers are "more valuable"

df$rank <- rank(df$index, na.last = "keep", ties.method = "first")
df$decile <- as.integer(cut(df$rank, breaks = 10, labels = FALSE))
if (!("decile" %in% names(df))) {
    stop("Decile column doesn't exist in the dataframe!")
}

if (!("expd" %in% names(df))) {
    stop("Expenditure column doesn't exist in the dataframe!")
}

avg_spending_per_decile <- aggregate(expd ~ decile, data = df, FUN = mean, na.rm = TRUE)
install.packages("ggplot2")
library(ggplot2)
ggplot(avg_spending_per_decile, aes(x = as.factor(decile), y = expd)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Average Spending per Decile",
         x = "Decile",
         y = "Average Spending") +
    theme_minimal()








