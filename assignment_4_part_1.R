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

# 定义一个函数，用于从Dropbox下载文件
download_from_dropbox <- function(url, destfile) {
    dropbox_url <- gsub("www.dropbox.com", "dl.dropboxusercontent.com", url)
    GET(dropbox_url, write_disk(destfile, overwrite = TRUE))
}

# 下载CSV文件
url1 <- "https://www.dropbox.com/scl/fi/n7vtdpym7sfsks3fnmrlz/game_attributes.csv?rlkey=ni7qq0507k81v0gwozn3j2wmv&dl=0"
url2 <- "https://www.dropbox.com/scl/fi/x4k0b5vd9n7e6q4vvo3tj/game_players.csv?rlkey=jgp95ncmiq6871cigl2rek0iw&dl=0"
url3 <- "https://www.dropbox.com/scl/fi/wlnm74032ytl8sfz0fy2d/game_price_changes.csv?rlkey=iaon3h6zajbr22jbiijic9rmh&dl=0"

destfile1 <- "game_attributes.csv"
destfile2 <- "game_players.csv"
destfile3 <- "game_price_changes.csv"

download_from_dropbox(url1, destfile1)
download_from_dropbox(url2, destfile2)
download_from_dropbox(url3, destfile3)

# 读取CSV文件
game_attributes_data <- read.csv(destfile1)
game_players_data <- read.csv(destfile2)
game_price_changes_data <- read.csv(destfile3)
# We start with two data sets: Twitch and Steam. In this step we first work on cleaning Steam data
# There are three files. game_attributes, game_players, and game_price_changes
#   The primary key for game_attributes is $app_id. This is app-level data on their attributes.
#   The primary key for game_players is $app_id and $date. This is daily data for the number of players in the game
#   The primary key for game_price_changes is $app_id and $price_change_date. This is a record of the price-changing
#   occasions and the new prices at those occasions. 

# We should clean game_attributes data. $release_date is in some weird format. Make it a date variable. Discard the time 
#   aspect. $rating_text contains useful information. We want the percentage of positive ratings and the average rating number
#   in two separate variables. For all data, keep games with the type "game" and with non-missing release date, rating, and reviews.

game_attributes_clean <- subset(game_attributes_data, 
                                app_type == "Game")

game_attributes_clean <- game_attributes_clean[
    complete.cases(game_attributes_clean) & game_attributes_clean$release_date != "" & game_attributes_clean$rating_text != "" & game_attributes_clean$game_desc != "", ]

game_attributes_clean$release_ymd <- 0
game_attributes_clean$release_ymd <- as.Date(strptime(game_attributes_clean$release_date, format = "%d %B %Y"))

game_attributes_clean$pos_percentage <- ifelse(grepl("^\\d+\\.\\d+% of.*", game_attributes_clean$rating_text),
                          sub("^(\\d+\\.\\d+)% of.*", "\\1%", game_attributes_clean$rating_text),
                          NA) #extract percentage data of postive ratings from rating_text

game_attributes_clean$user_review_counts <- sub("^[0-9.]+% of the ([0-9,]+) user reviews are positive.*", 
                                               "\\1", game_attributes_clean$rating_text) # extract how many reviews are postive from rating_text但是我越做越不对 总感觉不是这个意思 明天去问问

# We need to clean the price change data to get daily prices. For a game-date combination, if the game_price_changes 
#   data has an observation on that day, the $price variable in that data measures the price on that day. If there is no 
#   price data on a day, we know that price did not change on that day, so its takes the previous value. For example, for 
#   game 730 (Counter-Strike: Global Offensive). The price was changed to $9.99 on 2012-11-01. Then it was changed again 
#   to $11.24 on 2012-11-22. This means that before the change on 2012-11-22, the price stayed at $9.99. 
#   By the end of this step, you should have a price data at the level of game-date. There should be no gap in dates unless 
#   for specific reasons that lead to missing data. You should then be able to merge the players data with the price data.
# Convert datasets to data.tables explicitly
setDT(game_attributes_clean)
setDT(game_players_data)
setDT(game_price_changes_data)
#1. 确保 price_change_date 是日期类型
game_price_changes_data$price_change_date <- as.Date(game_price_changes_data$price_change_date)
# 2. 移除含有 NA 的行
game_price_changes_data <- game_price_changes_data[!is.na(price_change_date)]
# 再次为每款游戏创建日期序列
dates_list <- game_price_changes_data[, .(date_seq = seq(min(price_change_date), max(price_change_date), by = "days")), by = app_id]
# 将日期序列与实际价格数据合并
daily_prices <- merge(dates_list, game_price_changes_data, by.x = c("app_id", "date_seq"), by.y = c("app_id", "price_change_date"), all.x = TRUE)
#使用setorder按照app_id和date_seq对daily_prices进行排序
setorder(daily_prices, app_id, date_seq) 
#使用nafill函数将每个app_id的price列中的NA值替换为上一个非NA值这相当于前向填充了这些NA值
daily_prices[, price := nafill(price, type="locf"), by=app_id]



# Finally, produce summary statistics at the game level: for each game, first compute the average daily number of players, 
#   average price, total number of price changes, number of ratings and the fraction of positive reviews, Then summarize the 
#   mean and standard deviation of these variables across games. Also, find all paid games (i.e., games with an average price 
#   above zero), and among those paid games, find 9 games with the highest average daily players. Produce summary statistics 
#   for each of these 9 games. Print these summary stats in the console. Optionally, export the summary stats into tables. 




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





# Now, perform a related (but different) exercise on the number of ratings and the average number of players. Note that we do 
#   not see ratings on each day for each game. We simply see an average rating. So we cannot look at how rating changes and how that's
#   associated with the number of players. But at least we can still look at the scatter plot between the average rating and the 
#   log(average number of players) for each game. In the scatter plot, one observation is a game. Fit a line or curve between the two 
#   variables. Also write down the correlation coefficient in the graph title. What can you conclude here?



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




# Now, let us organize the data such that we count the total amount of viewing time for each game on each day. To do this, we have to process
#   the data in a few steps:
#   1. restructure the data into stream (the original level of observation)-game level. That is, we should put different 
#   games in different observations for each stream. 
#   2. for each stream, multiply the number of viewers (not the unique viewer) by the stream duration to get the viewing time (unit is viewer-hour). 
#   Then divide viewing time by the number of games. Here, we're assuming that each game gets equal amount of exposure. 
#   3. sum up the total viewing time for each game on each day, across streams. Also count how many streamers broadcasted the game on each day.
#   Finally, because some streamers are "big" in that they attract many viewers, whereas other streamers are small. Compute a follower-weighted 
#   measure of the number of streamers by summing up all streamers who broadcast the game by their followers (sum followers for those who broadcast game j)





#############################
# Step 4: Is there an association between Twitch streaming and video game playing?
#############################

# Now we're ready to examine whether Twitch broadcasts are associated with the number of players in games
#   To begin with, merge the Twitch data on the daily total viewing hours and the number of streamers. 
#   Drop games that do not exist in both data, but keep all dates for games that exist in both data even if 
#   that date does not have streams. Finally, keep observations after 2017-01-01. 
# For days when there is no stream, replace viewing hour, number of streamers, and sum of streamers' followers to zero
# Drop observations without data on the number of players





# Now examine the correlation between the number of streamers who broadcast the game and the number of players 
#   First, for each of the top-9 paid games we obtain from step 2, plot the number of players and the number of streamers. 
#   Separately, plot the number of players against the log viewing hours + 1 (so if viewing_hours = 0, log(viewing_hours + 1) 
#   is not NaN). In both cases, title the graph with the correlation coefficient between the two covariates. 
#   Alternatively, you can plot viewing hours, players, and number of streamers against time, just like what we did in step 2.





















