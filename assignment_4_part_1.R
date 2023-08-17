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

limengxi

#############################
# Step 1: clean and understand Steam data
#############################

# We start with two data sets: Twitch and Steam. In this step we first work on cleaning Steam data
# There are three files. game_attributes, game_players, and game_price_changes
#   The primary key for game_attributes is $app_id. This is app-level data on their attributes.
#   The primary key for game_players is $app_id and $date. This is daily data for the number of players in the game
#   The primary key for game_price_changes is $app_id and $price_change_date. This is a record of the price-changing
#   occasions and the new prices at those occasions. 

# We should clean game_attributes data. $release_date is in some weird format. Make it a date variable. Discard the time 
#   aspect. $rating_text contains useful information. We want the percentage of positive ratings and the average rating number
#   in two separate variables. For all data, keep games with the type "game" and with non-missing release date, rating, and reviews.

# We need to clean the price change data to get daily prices. For a game-date combination, if the game_price_changes 
#   data has an observation on that day, the $price variable in that data measures the price on that day. If there is no 
#   price data on a day, we know that price did not change on that day, so its takes the previous value. For example, for 
#   game 730 (Counter-Strike: Global Offensive). The price was changed to $9.99 on 2012-11-01. Then it was changed again 
#   to $11.24 on 2012-11-22. This means that before the change on 2012-11-22, the price stayed at $9.99. 
#   By the end of this step, you should have a price data at the level of game-date. There should be no gap in dates unless 
#   for specific reasons that lead to missing data. You should then be able to merge the players data with the price data.

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















