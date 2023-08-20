# ===================================
# GBA464: Assignment 4
# Twitch and Steam: Part I
# Instructor: Yufeng Huang
# ===================================

# Instructions:
#   1. This is a team assignment. Your team hands in one copy of R code by email to r.programming.simon@gmail.com
#   2. Please attempt all questions. Please make sure the code runs. If you have attempted all questions and the 
#       code runs, you will get full marks. 
#   3. You will need to make assumptions when you process the data. Be critical and convince yourselves that your 
#       choices make sense. These choices do not affect your grades. But this is a good place to learn when to stand
#       behind your decisions and when to acknowledge that something do not make sense. 
#   4. You can use ChatGPT or other language models to help you code.

# Specific coding instructions
#   5. Load your libraries here. Do not load them later in the code. Do not install.packages in the code (install them separately)
#   6. Datasets are large. So consider using data.table. Also, the data are large, so try to avoid looping over many observations. 
#   7. Have fun!


# Load necessary libraries
#TASK
#1) date变时区 把time dicard掉
#2）string提取信息：positive ratings and the average rating number???
#3) price-date data cleaning 
#4) average daily number of players, average price, total number of price changes, number of ratings and the fraction of positive reviews
#5) mean sd
#6) paid games: 9 games with the highest average daily players. 


#############################
# Step 1: clean and understand Steam data
#############################
# ====== CLEAR EVERYTHING ======
rm(list = ls())
library(httr)
library(data.table)

# We start with two data sets: Twitch and Steam. In this step we first work on cleaning Steam data
# There are three files. game_attributes, game_players, and game_price_changes
#   The primary key for game_attributes is $app_id. This is app-level data on their attributes.
#   The primary key for game_players is $app_id and $date. This is daily data for the number of players in the game
#   The primary key for game_price_changes is $app_id and $price_change_date. This is a record of the price-changing
#   occasions and the new prices at those occasions. 

download_from_dropbox <- function(url, destfile) {
    dropbox_url <- gsub("www.dropbox.com", "dl.dropboxusercontent.com", url)
    GET(dropbox_url, write_disk(destfile, overwrite = TRUE))
}

url1 <- "https://www.dropbox.com/scl/fi/n7vtdpym7sfsks3fnmrlz/game_attributes.csv?rlkey=ni7qq0507k81v0gwozn3j2wmv&dl=0"
url2 <- "https://www.dropbox.com/scl/fi/x4k0b5vd9n7e6q4vvo3tj/game_players.csv?rlkey=jgp95ncmiq6871cigl2rek0iw&dl=0"
url3 <- "https://www.dropbox.com/scl/fi/wlnm74032ytl8sfz0fy2d/game_price_changes.csv?rlkey=iaon3h6zajbr22jbiijic9rmh&dl=0"
url4 <- "https://www.dropbox.com/scl/fi/16r20vxsr2ykctml2woeh/twitch_profiles.csv?rlkey=cl4wvpq0cjmizjonf6zy28wzc&dl=0"
url5 <- "https://www.dropbox.com/scl/fi/0wrzc8i8wglqtifsgitl7/twitch_streams.csv?rlkey=w409e8kug8buisgo5a8i47p4t&dl=0"

destfile1 <- "game_attributes.csv"
destfile2 <- "game_players.csv"
destfile3 <- "game_price_changes.csv"
destfile4 <- "twitch_profiles.csv"
destfile5 <- "twitch_streams.csv"

download_from_dropbox(url1, destfile1)
download_from_dropbox(url2, destfile2)
download_from_dropbox(url3, destfile3)
download_from_dropbox(url4, destfile4)
download_from_dropbox(url5, destfile5)

game_attributes_data <- read.csv(destfile1)
game_players_data <- read.csv(destfile2)
game_price_changes_data <- read.csv(destfile3)
twitch_profiles_data <- read.csv(destfile4)
twitch_streams_data <- read.csv(destfile5)

# We should clean game_attributes data. $release_date is in some weird format. Make it a date variable. Discard the time 
#   aspect. $rating_text contains useful information. We want the percentage of positive ratings and the average rating number
#   in two separate variables. For all data, keep games with the type "game" and with non-missing release date, rating, and reviews.

game_attributes_clean <- subset(game_attributes_data, 
                                app_type == "Game")

game_attributes_clean <- game_attributes_clean[
    complete.cases(game_attributes_clean) & game_attributes_clean$release_date != "" & game_attributes_clean$rating_text != "" & game_attributes_clean$game_desc != "", ]

game_attributes_clean$release_ymd <- 0
game_attributes_clean$release_ymd <- as.Date(strptime(game_attributes_clean$release_date, format = "%d %B %Y"))


game_attributes_clean$pos_percentage <- sub("^(\\d+\\.\\d+)% of.*", "\\1%", 
                                            game_attributes_clean$rating_text) #extract percentage data of posotive ratings from rating_text

game_attributes_clean$user_review_counts <- sub("^[0-9.]+% of the ([0-9,]+) user reviews are positive.*", 
                                                "\\1", game_attributes_clean$rating_text) # extract how many reviews are postive from rating_text

# We need to clean the price change data to get daily prices. For a game-date combination, if the game_price_changes 
#   data has an observation on that day, the $price variable in that data measures the price on that day. If there is no 
#   price data on a day, we know that price did not change on that day, so its takes the previous value. For example, for 
#   game 730 (Counter-Strike: Global Offensive). The price was changed to $9.99 on 2012-11-01. Then it was changed again 
#   to $11.24 on 2012-11-22. This means that before the change on 2012-11-22, the price stayed at $9.99. 
#   By the end of this step, you should have a price data at the level of game-date. There should be no gap in dates unless 
#   for specific reasons that lead to missing data. You should then be able to merge the players data with the price data.
# Convert datasets to data.tables explicitly
library(data.table)
setDT(game_attributes_clean)
setDT(game_players_data)
setDT(game_price_changes_data)

game_price_changes_data$price_change_date <- as.Date(game_price_changes_data$price_change_date)

game_price_changes_data <- game_price_changes_data[!is.na(price_change_date)]

dates_list <- game_price_changes_data[, .(date_seq = seq(min(price_change_date), max(price_change_date), by = "days")), by = app_id]

daily_prices <- merge(dates_list, game_price_changes_data, by.x = c("app_id", "date_seq"), by.y = c("app_id", "price_change_date"), all.x = TRUE)

setorder(daily_prices, app_id, date_seq) 

daily_prices[, price := nafill(price, type="locf"), by=app_id]

game_players_data$date <- as.Date(game_players_data$date)
game_players_price_data <- merge(game_players_data,daily_prices,by.x = c("app_id", "date"), by.y = c("app_id", "date_seq"), all.x = TRUE)
game_players_price_data



# Finally, produce summary statistics at the game level: for each game, first compute the average daily number of players, 
#   average price, total number of price changes, number of ratings and the fraction of positive reviews, Then summarize the 
#   mean and standard deviation of these variables across games. Also, find all paid games (i.e., games with an average price 
#   above zero), and among those paid games, find 9 games with the highest average daily players. Produce summary statistics 
#   for each of these 9 games. Print these summary stats in the console. Optionally, export the summary stats into tables. 

avg_daily_players <- aggregate(player_count ~ app_id, data = game_players_price_data, FUN = mean, na.rm = TRUE)
average_price <- aggregate(price ~ app_id, data = game_players_price_data, FUN = mean, na.rm = TRUE)
price_changes <- aggregate(x = list(changes_time = game_price_changes_data$price_change_date), by = list(app_id = game_price_changes_data$app_id), FUN = length)

summary_statistics <- merge(avg_daily_players,average_price,by = "app_id")
summary_statistics <- merge(summary_statistics,price_changes,by = "app_id")
summary_statistics <- merge(summary_statistics , game_attributes_clean[, c("app_id", "pos_percentage", "user_review_counts")], by = "app_id")

# 4. Compute global mean and standard deviation of these values
summary_stats <- data.frame(
    means = c(
        mean(avg_daily_players$player_count, na.rm = TRUE),
        mean(average_price$price, na.rm = TRUE),
        mean(price_changes$changes_time, na.rm = TRUE),
        mean(game_attributes_clean$pos_percentage, na.rm = TRUE),
        mean(game_attributes_clean$user_review_counts, na.rm = TRUE)
    ),
    sds = c(
        sd(avg_daily_players$player_count, na.rm = TRUE),
        sd(average_price$price, na.rm = TRUE),
        sd(price_changes$changes_time, na.rm = TRUE),
        sd(game_attributes_clean$pos_percentage, na.rm = TRUE),
        sd(game_attributes_clean$user_review_counts, na.rm = TRUE)
    )
)

# 5. Filter out free games and retrieve the 9 games with the highest average daily players
paid_games <- summary_statistics[summary_statistics$price > 0,]
top9_games <- paid_games[order(-paid_games$player_count), ][1:9, ]

# 6. Print top 9 games with their details
print(top9_games)


#############################
# Step 2: how well associated between prices and the number of players?
#############################

# When there is a price discount, the number of players might increase if people buy the game and play it. We will look into 
#   this hypothesis. 
#   We start by looking at the 9 games. For each of these 9 games, plot the trajectory of prices and the number of players 
#   over time. Use log scale for the number of players but not for price (put them on two different axes). Do you see a correlation? 
#   Then, calculate the correlation coefficient between the log number of players and price. Then, in the graph title, mark the game 
#   and the correlation coefficient between the log number of players and price. Do you see a negative correlation, in that the higher
#   the price, the lower the number of players?
#   Finally, compute the correlation between log players and price for each game, and summarize the average and the distribution of this 
#   correlation. Or you can plot a histogram. Do you still find negative correlation?


#1) plot(time~price)
#2) log(number of players)
#3) correlation(log number of players and price) mark the game 
#   and the correlation coefficient between the log number of players and price. Xinyi
#4) mean(correlation), mean(distribution) and 画个直方图 Mengxi

top9_games_merge <- merge(
    daily_prices,
    game_players_price_data,
    by.x = c("app_id", "date_seq"), by.y = c("app_id", "date"),
    all.x = TRUE
)

top9_games_subset <- subset(top9_games_merge, app_id %in% top9_games$app_id)
top9_games_subset <- top9_games_subset[, c("app_id", "date_seq", "price.x", "player_count")]
top9_games_subset <- na.omit(top9_games_subset)

#plot: player_count & price.x

unique_app_ids <- unique(top9_games_subset$app_id)
num_apps <- length(unique_app_ids)

# Set up a layout matrix for 9 plots, 3 columns by 3 rows
layout(matrix(1:num_apps, ncol = 3))

# Loop through each app_id and plot the two lines
for (app in unique_app_ids) {
    app_subset <- subset(top9_games_subset, app_id == app)
    
    # Plot price vs. date_seq
    plot(price.x ~ date_seq, data = app_subset, type = 'l', xlab = 'Date', ylab = 'Price', main = paste('App ID:', app), col = "red")
    
    # Overlay player_count vs. date_seq
    par(new = TRUE)
    plot(player_count ~ date_seq, data = app_subset, type = 'l', xlab = '', ylab = '', axes = FALSE, col = "blue")
    axis(side = 4)  # Add an axis to the right side
}

# Reset graphics parameters
par(new = FALSE)


#3）log的图
# Pre-computing log of player counts and correlations
unique_app_ids <- unique(top9_games_subset$app_id)
top9_games_subset$log_player_count <- log(top9_games_subset$player_count)

# Set up a layout matrix for 9 plots, 3 columns by 3 rows
correlations <- sapply(unique_app_ids, function(app) {
    app_data <- subset(top9_games_subset, app_id == app)
    cor_value <- cor(app_data$price.x, app_data$log_player_count, use = "complete.obs")
    if(is.na(cor_value)) {
        return(0)
    } else {
        return(round(cor_value, 3))
    }
}, USE.NAMES = TRUE)

layout(matrix(1:9, ncol = 3))

# Loop through each app_id and plot the two lines
for (i in 1:length(unique_app_ids)) {
    
    # Subset data for the current app
    app_subset <- subset(top9_games_subset, app_id == unique_app_ids[i])
    
    # Define title
    title_text <- paste("App ID:", unique_app_ids[i])
    
    # Plot price vs. date_seq
    plot(app_subset$date_seq, app_subset$price.x, type = 'l', col = "red", ylim = range(app_subset$price.x),
         xlab = 'Date', ylab = 'Price', main = title_text)
    
    mtext(paste("Corr:", correlations[i]), side=3)
    # Overlay log of player counts on the same graph but different axis
    par(new = TRUE)
    plot(app_subset$date_seq, app_subset$log_player_count, type = 'l', col = "blue", 
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ylim = range(app_subset$log_player_count))
    
    # Resetting par to default for next iteration
    par(new = FALSE)
}
correlations
average_correlation <- mean(correlations, na.rm = TRUE)
print(average_correlation)

cor_values <- as.numeric(correlations[correlations != "Not Available"])
print(cor_values)
hist(cor_values, main="Distribution of Correlation Values", xlab="Correlation", breaks=10, col="lightblue", border="black", las=1, cex.axis=0.8)


# Now, perform a related (but different) exercise on the number of ratings and the average number of players. Note that we do 
#   not see ratings on each day for each game. We simply see an average rating. So we cannot look at how rating changes and how that's
#   associated with the number of players. But at least we can still look at the scatter plot between the average rating and the 
#   log(average number of players) for each game. In the scatter plot, one observation is a game. Fit a line or curve between the two 
#   variables. Also write down the correlation coefficient in the graph title. What can you conclude here?
# Step 1: Remove NA values from avg_daily_players$player_count
avg_daily_players <- avg_daily_players[!is.na(avg_daily_players$player_count), ]

# Step 2: Log Transformation
avg_daily_players$log_avg_daily_players <- log(avg_daily_players$player_count)

# Step 3: Merge Data Frames
merge_player_rating <- merge(avg_daily_players, game_attributes_clean, by = "app_id")

# Step 4: Check for Infinite or NA values after merging & handle them
merge_player_rating <- merge_player_rating[!is.na(merge_player_rating$log_avg_daily_players) & 
                                               !is.na(merge_player_rating$pos_percentage) & 
                                               !is.infinite(merge_player_rating$log_avg_daily_players) & 
                                               !is.infinite(merge_player_rating$pos_percentage), ]

# Determine the finite range for y-axis values
typeof(merge_player_rating$pos_percentage)
merge_player_rating$pos_percentage <- as.numeric(sub("%", "", merge_player_rating$pos_percentage)) / 100

# Scatter Plot with adjusted ylim values
plot(merge_player_rating$log_avg_daily_players, merge_player_rating$pos_percentage, 
     main="Average Rating vs Log(Avg. Number of Players)", 
     xlab="Log(Average Number of Players)", 
     ylab="Average Rating", 
     pch=19, col="blue",cex=0.2)
fit <- lm(merge_player_rating$pos_percentage ~ merge_player_rating$log_avg_daily_players, data=merge_player_rating)
abline(fit, col="red") # adds a linear regression line in red color
#6) make a conclusion

#Conclusion: There is a positive correlation between the log of average player numbers and the average rating being plotted.
#This means that as average rating increases, the average number of players tends to increase as well.

#############################
# Step 3: now load the Twitch streamer data
#############################

# now read Twitch data. There are two files. 
#   Data twitch_profiles has the key $streamer. This is a sample of about 10,000 streamers. 
#   Data twitch_streams' key should be each observation, which is a stream. 
#   A stream is broadcasted by $streamer on a $date, but notice that a streamer on a date can have multiple streams. 
#   Variable $viewers records how many viewers are simultaneously present in the stream. $duration is the number of hours of the stream. 
#   $unique_viewers is the number of viewers who ever appeared. $followers is the number of followers of the streamer at the time of the 
#   stream. $stream_title is the title of the stream. And $games is a list of all games that are broadcasted in the same stream. 

# task:total amount of viewing time for each game on each day
#1) 分开games: expand.grid()
#2）aggregate(viewer*duration ~ stream + game) #???用啥function
#3) aggregate(sum(streamers) ~ game + date)
#4) number of streamers broadcasting game j/number of followers #???


# Now, let us organize the data such that we count the total amount of viewing time for each game on each day. To do this, we have to process
#   the data in a few steps:
#   1. restructure the data into stream (the original level of observation)-game level. That is, we should put different 
#   games in different observations for each stream. 
#   2. for each stream, multiply the number of viewers (not the unique viewer) by the stream duration to get the viewing time (unit is viewer-hour). 
#   Then divide viewing time by the number of games. Here, we're assuming that each game gets equal amount of exposure. 
#   3. sum up the total viewing time for each game on each day, across streams. Also count how many streamers broadcasted the game on each day.
#   Finally, because some streamers are "big" in that they attract many viewers, whereas other streamers are small. Compute a follower-weighted 
#   measure of the number of streamers by summing up all streamers who broadcast the game by their followers (sum followers for those who broadcast game j)

library(dplyr)
library(tidyr)
twitch_streams_data <- twitch_streams_data %>%
    mutate(games = strsplit(games, ",\\s*")) %>%
    unnest(games)

twitch_streams_data$viewing_time <- twitch_streams_data$duration * twitch_streams_data$viewers

viewing_games <- aggregate(
    viewing_time ~ games + date,
    data = twitch_streams_data,
    FUN = sum
)

viewing_games$divided_viewing_time <- viewing_games$viewing_time / length(viewing_games$games)

streamer_count <- twitch_streams_data %>%
    group_by(games, date) %>%
    summarise(streamer_count = n_distinct(streamer))

sum_followers <- twitch_streams_data %>%
    group_by(games) %>%
    summarise(total_followers = sum(followers))

#############################
# Step 4: Is there an association between Twitch streaming and video game playing?
#############################

# Now we're ready to examine whether Twitch broadcasts are associated with the number of players in games
# Now we're ready to examine whether Twitch broadcasts are associated with the number of players in games
#   To begin with, merge the Twitch data on the daily total viewing hours and the number of streamers. 
#  Drop games that do not exist in both data, 
# but keep all dates for games that exist in both data even if 
#  that date does not have streams. 

library(data.table)
viewing_games <- data.table(viewing_games)
streamer_count <- data.table(streamer_count)
sum_followers <- data.table(sum_followers)

twitch_viewing <- merge(viewing_games, streamer_count, by = c("games", "date"))
twitch_viewing <- merge(twitch_viewing, sum_followers, by = "games")
twitch_viewing <- twitch_viewing[complete.cases(twitch_viewing)]

twitch_viewing[, date := as.Date(date)]
twitch_viewing <- twitch_viewing[date > as.Date("2017-01-01")]
all_dates <- seq(min(twitch_viewing$date), max(twitch_viewing$date), by = "days")

merged_data <- twitch_viewing[CJ(date = all_dates), on = .(date), nomatch = 0]

merged_data[is.na(viewing_time), viewing_time := 0]
merged_data[is.na(streamer_count), streamer_count := 0]
merged_data[is.na(total_followers), total_followers := 0]

filtered_data <- merged_data[total_followers != 0]
colnames(filtered_data)[1] <- "title"
colnames(filtered_data)[2] <- "date"


# Now examine the correlation between the number of streamers who broadcast the game and the number of players 
#   First, for each of the top-9 paid games we obtain from step 2, plot the number of players and the number of streamers. 
#   Separately, plot the number of players against the log viewing hours + 1 (so if viewing_hours = 0, log(viewing_hours + 1) 
#   is not NaN). In both cases, title the graph with the correlation coefficient between the two covariates. 
#   Alternatively, you can plot viewing hours, players, and number of streamers against time, just like what we did in step 2.


# Given from the prior code context
top9_games_game_title <- merge(top9_games, game_attributes_clean, by = "app_id", all.x = TRUE)
top9_games_after17 <- subset(top9_games_subset, date_seq > as.Date("2017-01-01"))
colnames(top9_games_after17)[2] <- "date"

top9_games_after17_title <- merge(top9_games_after17, top9_games_game_title, by = "app_id", all.x = TRUE)

# Merge Twitch data with game data
colnames(viewing_games)[1] <- "title"
top9_games_twitch <- merge(top9_games_after17_title, filtered_data, by = c("date", "title"))
viewing_games$date <- as.Date(viewing_games$date)
top9_games_twitch1 <- merge(top9_games_twitch, viewing_games, by = c("date", "title"))

# Compute log viewing time
# Compute log for positive values, set 0 otherwise
top9_games_twitch1$log_viewing_time <- ifelse(top9_games_twitch1$viewing_time.x > 0, 
                                              log(top9_games_twitch1$viewing_time.x) + 1, 
                                              0)

# Handle potential NA values by setting them to 0 (or any other value you deem appropriate)
top9_games_twitch1$log_viewing_time[is.na(top9_games_twitch1$log_viewing_time.x)] <- 0

unique_app_ids <- unique(top9_games_subset$app_id)
num_apps <- length(unique_app_ids)

# Set up a layout matrix for 9 plots, 3 columns by 3 rows
layout(matrix(1:num_apps, ncol = 3))

# Loop through each app_id and plot the two lines
for (app in unique_app_ids) {
    app_data <- subset(top9_games_twitch1, app_id == app)
    
    
    if (nrow(app_data) > 0) {
        plot(streamer_count~ date, data = app_data, type = 'l', xlab = 'Date', ylab = 'Twitch Profile', main = paste('App ID:', app), col = "red")
        par(new = TRUE)
        plot(player_count.x ~ date, data = app_data, type = 'l', xlab = '', ylab = '', axes = FALSE, col = "blue")
        axis(side = 4, col.axis = "blue", las = 1, line = -1) 
        par(new = TRUE)
        plot(log_viewing_time ~ date, data = app_data, type = 'l', xlab = '', ylab = '', axes = FALSE, col = "green")
        axis(side = 4, col.axis = "green", las = 1, line = 2)
    } else {
        warning(paste("No valid data for App ID:", app))
    }
}

# Reset graphics parameters
par(new = FALSE)
