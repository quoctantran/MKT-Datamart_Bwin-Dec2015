# ----------------------------------------------------
# BUILDING MARKETING DATA MART
# bwin.com - Online Betting, Poker & Casino
# Created by: Minh PHAN & Daniel GOMEZ
# ----------------------------------------------------
# IESEG, MBD, Dec 2015
# ----------------------------------------------------



# ----------------------------------------------------
# STEP 1 - PREPARATION & STANDARDIZATION
# ----------------------------------------------------

# ----------------------------------------------------
# 1.1. Load R packages
# ----------------------------------------------------
# Clean-up everything
rm(list = setdiff(ls(), lsf.str()))

# Load libraries
library(haven)
library(readxl)
library(data.table)

# ----------------------------------------------------
# 1.2. Prepare file paths & Import data (haven, readxl)
# ----------------------------------------------------
path <- file.path("C:", "Users", "Administrator", "Desktop", "MKT Datamart", "SAS")

# File paths, use file.path() to auto-create path for both Win, Linux or Mac OS
pDemographics <- file.path(path, "RawDataIDemographics.sas7bdat")
pUserDaily <- file.path(path, "RawDataIIUserDailyAggregation.sas7bdat")
pPokerChip <- file.path(path, "RawDataIIIPokerChipConversions.sas7bdat")
pAppendix <- file.path(path, "codings_appendicies.xlsx")

# Import and convert raw data to data frame
Demographics <- as.data.frame(read_sas(pDemographics))
UserDaily <- as.data.frame(read_sas(pUserDaily))
PokerChip <- as.data.frame(read_sas(pPokerChip))

# Import Codings Appendices
tProductID <- as.data.table(read_excel(pAppendix, sheet = 1,
                                       col_names = TRUE, col_types = NULL))
tCountryID <- as.data.table(read_excel(pAppendix, sheet = 2,
                                       col_names = TRUE, col_types = NULL))
tLanguageID <- as.data.table(read_excel(pAppendix, sheet = 3,
                                        col_names = TRUE, col_types = NULL))
tApplicationID <- as.data.table(read_excel(pAppendix, sheet = 4,
                                           col_names = TRUE, col_types = NULL))

# ----------------------------------------------------
# 1.3. Check & remove duplicated observations
# ----------------------------------------------------
# Show number of duplicated observations, to debug
# sum(duplicated(Demographics))
# sum(duplicated(UserDaily))
# sum(duplicated(PokerChip))

# Keep only unique observations
Demographics <- unique(Demographics)
UserDaily <- unique(UserDaily)
PokerChip <- unique(PokerChip)

# ----------------------------------------------------
# 1.4. Standardize date-time data (yyyy-mm-dd)
# ----------------------------------------------------
# After this step, NA, NULL, <blank> values are converted all to NA

# Standardize date of Demographics
Demographics[, "RegDate"] <- as.Date(Demographics[, "RegDate"])
for (i in 5:10)
	Demographics[, c(i)] <- as.Date(strptime(Demographics[, c(i)], "%Y%m%d"))

# Standardize date of tUserDaily
UserDaily[, "Date"] <- as.Date(strptime(UserDaily[, "Date"], "%Y%m%d"))

# Standardize date of tPokerChip
PokerChip[, "TransDateTime"] <- as.Date(PokerChip[, "TransDateTime"])

# Period: 1 February 2005 - 30 September 2005
BeginPeriod <- as.Date("2005-02-01")
EndPeriod <- as.Date("2005-09-30")

# Convert data.frame to data.table
# This set of code can be used to reset 3 main data.tables
tDemographics <- as.data.table(Demographics)
tUserDaily <- as.data.table(UserDaily)
tPokerChip <- as.data.table(PokerChip)

# ----------------------------------------------------
# Summary current data.tables:
# ----------------------------------------------------
# tDemographics - User's demographics and registration information
# tUserDaily - User's daily transaction of 8 products
# tPokerChip - User's poker chip transaction
# tProductID - ID and product name
# tCountry - ID and country name
# tLanguage - ID and language name
# tApplicationID - ID and user application source name
# ----------------------------------------------------



# ----------------------------------------------------
# STEP 2 - CALCULATION ON EACH SINGLE DATA.TABLE
# ----------------------------------------------------

# ----------------------------------------------------
# 2.1. Work on tDemographics
# ----------------------------------------------------
# Period: 1 February 2005 - 30 September 2005
# tDemographics <- as.data.table(Demographics) # Quick reset tDemographics to debug

# ----------------------------------------------------
# 2.1.1. Clean-up and standardize columns name
# ----------------------------------------------------
# Remove users who never active during the period: FirstAct == NA
tDemographics <- tDemographics[!is.na(FirstAct)]

# Remove users who active out of the period
tDemographics <- tDemographics[FirstAct <= EndPeriod]

# Change CountryID and LanguageID to real country and language name
setnames(tDemographics, c("Country", "Language") , c("CountryID", "LanguageID"))

tDemographics <- merge(tDemographics, tCountryID, by = "CountryID", all.x = TRUE)
tDemographics <- merge(tDemographics, tLanguageID, by = "LanguageID", all.x = TRUE)

tDemographics[, c("CountryID", "LanguageID", "CountryName", "LanguageDescription") := NULL]

# Change Gender code to gender character abbreviation (i.e. M & F)
tDemographics$Gender <- as.character(tDemographics$Gender)
tDemographics[Gender == 0, Gender := "F"]
tDemographics[Gender == 1, Gender := "M"]

# Standardize names for RegDate, FirstAct, FirstPay, FirstSp, FirstCa, FirstGa, FirstPo
setnames(tDemographics,
         c("RegDate", "FirstAct", "FirstPay", "FirstSp", "FirstCa", "FirstGa", "FirstPo"),
         c("Reg_Date", "First_Act", "First_Pay", "First_SP", "First_CA", "First_GA", "First_PO"))

# ----------------------------------------------------
# 2.1.2. Calculate metrics
# ----------------------------------------------------
# Gap_Reg_Act: gap days between registration date and first active date
tDemographics[, Gap_Reg_Act := First_Act - Reg_Date]

# Gap_Reg_Pay: gap days between registration date and first pay date
tDemographics[, Gap_Reg_Pay := First_Pay - Reg_Date]

# Num_Product: number of product types were played by each customer
tDemographics[, Num_Product := 4 - (is.na(First_SP) + is.na(First_CA) +
                               is.na(First_GA) + is.na(First_PO))]

# ----------------------------------------------------
# 2.1.3. Re-arrange tDemographics
# ----------------------------------------------------
# Re-arrange columns order
setcolorder(tDemographics, c("UserID", "Country", "Language", "Gender",
                             "ApplicationID", "Reg_Date", "First_Act", "First_Pay",
                             "First_SP", "First_CA", "First_GA", "First_PO",
                             "Gap_Reg_Act", "Gap_Reg_Pay", "Num_Product"))

# Get final UserID list, to filter in other data.tables
UserList <- tDemographics[, UserID]
UserNum <- length(UserList)

# ----------------------------------------------------
# Summary tDemographics data.table family:
# ----------------------------------------------------
# tDemographics - users information and behaviours
# UserList - list of user IDs, use to filter other data.table
# ----------------------------------------------------


# ----------------------------------------------------
# 2.2. Work on tUserDaily
# ----------------------------------------------------
# Period: 1 February 2005 - 30 September 2005
# tUserDaily <- as.data.table(UserDaily) # Quick reset tUserDaily to debug

# ----------------------------------------------------
# 2.2.1. Clean-up and standardize
# ----------------------------------------------------
# Remove all UserID that are not in UserList
tUserDaily <- tUserDaily[UserID %in% UserList]

# Remove all activities that are not in the period
tUserDaily <- tUserDaily[Date >= BeginPeriod & Date <= EndPeriod]

# ----------------------------------------------------
# 2.2.2. Group product by 4 catalogues as follow:
# ----------------------------------------------------
# Catalogue "SP": Sports book fixed-odd, Sports book live-action, Supertoto
# Catalogue "PO": Poker BossMedia
# Catalogue "CA": Casino BossMedia, Casino Chartwell
# Catalogue "GA": Games VS, Games bwin
# ----------------------------------------------------
# Group to 4 catalogues: SP, PO, CA, GA
tUserDaily[ProductID %in% c(1, 2, 5), Product := "SP"]
tUserDaily[ProductID %in% c(3), Product := "PO"]
tUserDaily[ProductID %in% c(4, 8), Product := "CA"]
tUserDaily[ProductID %in% c(6, 7), Product := "GA"]
tUserDaily[, ProductID := NULL] # Delete this column

# Aggregate transaction by UserID, Product and Date
# This step will also aggregate negative values (< 0) created by accounting system correction
tUserDaily <- tUserDaily[, lapply(.SD, sum), by = c("UserID", "Product", "Date")]
tUserDaily <- tUserDaily[order(Date)]

# ----------------------------------------------------
# 2.2.3. Calculate metrics (Daily level)
# ----------------------------------------------------
# Win: if Winnings > 0 then win = 1, Loss = 0
# Loss: if Winnings <= 0 then win = 0, Loss = 1
tUserDaily[Winnings > 0, ':=' (Win = 1, Loss = 0)]
tUserDaily[Winnings <= 0, ':=' (Win = 0, Loss = 1)]

# Total_Earning: if win = 1, the gambler earns money, if Loss = 1, he losses his Stakes
tUserDaily[Win == 1, Total_Earning := Winnings] # Win
tUserDaily[Loss == 1, Total_Earning := - Stakes] # Loss

# Find the last active date of gamblers per product catalogues
# Last_SP, Last_CA, Last_GA: gambler last active day on SP/CA/GA product catalogue
# Last_PO will be calculated by tPokerChip data.table
# Note: tUserDaily has already been sorted by column Date
tLastAct <- tUserDaily[, Date[.N], by = c("UserID", "Product")]
trans_tLastAct <- dcast.data.table(tLastAct, UserID ~ Product, value.var = "V1")
setnames(trans_tLastAct, c("CA", "GA", "SP"), c("Last_CA", "Last_GA", "Last_SP"))

# Total_Active: set = 1 for each day, will be meaningful when aggregate by UserID
tUserDaily[, Total_Active := 1]

# Transpose tUserDaily to bring Product name from row to column
# Auto fill missing values (i.e. NA) by 0
trans_tUserDaily <- dcast.data.table(tUserDaily, UserID + Date ~ Product,
			                                value.var = c("Stakes", "Winnings", "Bets",
			                                              "Win", "Loss","Total_Earning",
			                                              "Total_Active"),
			                                fill = 0)

# ----------------------------------------------------
# 2.2.4. Calcualte metrics (UserID aggregation level)
# ----------------------------------------------------
# Aggregate by UserID
aggr_tUserDaily <- trans_tUserDaily[, lapply(.SD, sum), by = c("UserID")]
aggr_tUserDaily[, "Date" := NULL] # Delete this column

# Add column Last_SP, Last_CA, Last_GA to aggr_tUserDaily data.table to calculate RFM
aggr_tUserDaily <- merge(aggr_tUserDaily, trans_tLastAct, by = "UserID", all.x = TRUE)

# ----------------------------------------------------
# 2.2.4.1. Calculate metrics for catalogue SP
# ----------------------------------------------------
# Winning_Rate_SP: total win / total SP game (%)
aggr_tUserDaily[, Winning_Rate_SP := Win_SP / (Win_SP + Loss_SP) * 100]

# Avg_Earning_SP: average earning (lossing) per day
aggr_tUserDaily[, Avg_Earning_SP := Total_Earning_SP / (Win_SP + Loss_SP)]

# Calculate customer RFM values for SP catalogue (10 + 10 + 10 = 30 max)
# Recency_SP: customer recency = 10 - number of months that have passed since the last purchased
aggr_tUserDaily[, Recency_SP := 10 - (month(EndPeriod) - month(Last_SP))]

# Frequency_SP: customer frequency = numer of purchases in last 12 months (out of 10)
MaxActive_SP <- max(aggr_tUserDaily$Total_Active_SP)
aggr_tUserDaily[, Frequency_SP := 10 * Total_Active_SP / MaxActive_SP]

# Monetary_SP: customer total spending (benchmarked over 10)
MaxSpend_SP <- max(aggr_tUserDaily$Stakes_SP)
aggr_tUserDaily[, Monetary_SP := 10 * Stakes_SP / MaxSpend_SP]

# RFM_SP (basic model) = Recency_SP + Frequency_SP + Monetary_SP
aggr_tUserDaily[, RFM_SP := Recency_SP + Frequency_SP + Monetary_SP]

# Rank_SP: ranking customer by RFM_SP values
aggr_tUserDaily <- aggr_tUserDaily[order(RFM_SP, decreasing = TRUE)]
UserNum_SP <- nrow(aggr_tUserDaily)
aggr_tUserDaily[, Rank_SP := c(1:UserNum_SP)]

# ----------------------------------------------------
# 2.2.4.2. Calculate metrics for catalogue CA
# ----------------------------------------------------
# Winning_Rate_CA: total win / total CA game (%)
aggr_tUserDaily[, Winning_Rate_CA := Win_CA / (Win_CA + Loss_CA) * 100]

# Avg_Earning_CA: average earning (lossing) per day
aggr_tUserDaily[, Avg_Earning_CA := Total_Earning_CA / (Win_CA + Loss_CA)]

# Calculate customer RFM values for CA catalogue (10 + 10 + 10 = 30 max)
# Recency_CA: customer recency = 10 - number of months that have passed since the last purchased
aggr_tUserDaily[, Recency_CA := 10 - (month(EndPeriod) - month(Last_CA))]

# Frequency_CA: customer frequency = numer of purchases in last 12 months (out of 10)
MaxActive_CA <- max(aggr_tUserDaily$Total_Active_CA)
aggr_tUserDaily[, Frequency_CA := 10 * Total_Active_CA / MaxActive_CA]

# Monetary_CA: customer total spending (benchmarked over 10)
MaxSpend_CA <- max(aggr_tUserDaily$Stakes_CA)
aggr_tUserDaily[, Monetary_CA := 10 * Stakes_CA / MaxSpend_CA]

# RFM_CA (basic model) = Recency_CA + Frequency_CA + Monetary_CA
aggr_tUserDaily[, RFM_CA := Recency_CA + Frequency_CA + Monetary_CA]

# Rank_CA: ranking customer by RFM_CA values
aggr_tUserDaily <- aggr_tUserDaily[order(RFM_CA, decreasing = TRUE)]
UserNum_CA <- nrow(aggr_tUserDaily)
aggr_tUserDaily[, Rank_CA := c(1:UserNum_CA)]

# ----------------------------------------------------
# 2.2.4.3. Calculate metrics for catalogue GA
# ----------------------------------------------------
# Winning_Rate_GA: total win / total GA game (%)
aggr_tUserDaily[, Winning_Rate_GA := Win_GA / (Win_GA + Loss_GA) * 100]

# Avg_Earning_GA: average earning (lossing) per day
aggr_tUserDaily[, Avg_Earning_GA := Total_Earning_GA / (Win_GA + Loss_GA)]

# Calculate customer RFM values for GA catalogue (10 + 10 + 10 = 30 max)
# Recency_GA: customer recency = 10 - number of months that have passed since the last purchased
aggr_tUserDaily[, Recency_GA := 10 - (month(EndPeriod) - month(Last_GA))]

# Frequency_GA: customer frequency = numer of purchases in last 12 months (out of 10)
MaxActive_GA <- max(aggr_tUserDaily$Total_Active_GA)
aggr_tUserDaily[, Frequency_GA := 10 * Total_Active_GA / MaxActive_GA]

# Monetary_GA: customer total spending (benchmarked over 10)
MaxSpend_GA <- max(aggr_tUserDaily$Stakes_GA)
aggr_tUserDaily[, Monetary_GA := 10 * Stakes_GA / MaxSpend_GA]

# RFM_GA (basic model) = Recency_GA + Frequency_GA + Monetary_GA
aggr_tUserDaily[, RFM_GA := Recency_GA + Frequency_GA + Monetary_GA]

# Rank_GA: ranking customer by RFM_GA values
aggr_tUserDaily <- aggr_tUserDaily[order(RFM_GA, decreasing = TRUE)]
UserNum_GA <- nrow(aggr_tUserDaily)
aggr_tUserDaily[, Rank_GA := c(1:UserNum_GA)]

# ----------------------------------------------------
# Summary tUserDaily data.table family:
# ----------------------------------------------------
# tUserDaily - detail activities of gamblers
# trans_tUserDaily - transpose version of tUserDaily
# aggr_tUserDaily - aggregate by UserID
# ----------------------------------------------------


# ----------------------------------------------------
# 2.3. Work on tPokerChip
# ----------------------------------------------------
# Period: 1 February 2005 - 30 September 2005
# tPokerChip <- as.data.table(PokerChip) # Quick reset tPokerChip to debug

# ----------------------------------------------------
# 2.3.1. Clean-up and standardize
# ----------------------------------------------------
# Remove UserID not in UserList
tPokerChip <- tPokerChip[UserID %in% UserList]

# Change column name and sort by Date
setnames(tPokerChip, "TransDateTime", "Date")
tPokerChip <- tPokerChip[order(Date)]

# Remove transaction not in the period
tPokerChip <- tPokerChip[Date >= BeginPeriod & Date <= EndPeriod]

# Add transaction type description column
tPokerChip[TransType == 24, Type := "Buy_PO"]
tPokerChip[TransType == 124, Type := "Sell_PO"]

# ----------------------------------------------------
# 2.3.2. Aggregate and transpose tPokerChip
# ----------------------------------------------------
# Aggregate transaction by UserID, Type and Date
tPokerChip <- tPokerChip[, lapply(.SD, sum), by = c("UserID", "Date", "Type")]

# Transpose tPokerChip to bring transaction type (buy / sell)  from row to column
# Auto fill NA value by 0
trans_tPokerChip <- dcast.data.table(tPokerChip, UserID + Date ~ Type,
                                     value.var = c("TransAmount"), fill = 0)

# ----------------------------------------------------
# 2.3.3. Calculate metrics (Daily level)
# ----------------------------------------------------
# Total_Earning_PO: assume that gambler wins money if sell more poker chips than buy it in a day
trans_tPokerChip[, Total_Earning_PO := Sell_PO - Buy_PO]

# Last_PO: last active day in poker
tLastAct_PO <- trans_tPokerChip[, Date[.N], by = c("UserID")]
setnames(tLastAct_PO, "V1", "Last_PO")

# Win_PO: if Total_Earning_PO > 0 then Win_PO = 1
# Loss_PO: if Total_Earning_PO <= 0 then Loss_PO = 1
trans_tPokerChip[Total_Earning_PO > 0, ':=' (Win_PO = 1, Loss_PO = 0)]
trans_tPokerChip[Total_Earning_PO <= 0, ':=' (Win_PO = 0, Loss_PO = 1)]

# Total_Active_PO: set = 1 for each day, will be meaningful when aggregate by UserID
trans_tPokerChip[, Total_Active_PO := 1]

# ----------------------------------------------------
# 2.3.4. Calculate metrics (UserID aggregation level)
# ----------------------------------------------------
# Aggregate by UserID
aggr_tPokerChip <- trans_tPokerChip[, lapply(.SD, sum), by = c("UserID")]
aggr_tPokerChip[, "Date" := NULL] # Delete this column

# Winning_Rate_PO: total win / total PO game (%)
aggr_tPokerChip[, Winning_Rate_PO := Win_PO / (Win_PO + Loss_PO) * 100]

# Avg_Earning_PO: average earning (lossing) per day
aggr_tPokerChip[, Avg_Earning_PO := Total_Earning_PO / (Win_PO + Loss_PO)]

# Last_PO: merge tLastAct_PO to calculate customer RFM values
aggr_tPokerChip <- merge(aggr_tPokerChip, tLastAct_PO, by = "UserID", all.x = TRUE)

# Calculate customer RFM values for PO catalogue (10 + 10 + 10 = 30 max)
# Recency_PO: customer recency = 10 - number of months that have passed since the last purchased
aggr_tPokerChip[, Recency_PO := 10 - (month(EndPeriod) - month(Last_PO))]

# Frequency_PO: customer frequency = numer of purchases in last 12 months (out of 10)
MaxActive_PO <- max(aggr_tPokerChip$Total_Active_PO)
aggr_tPokerChip[, Frequency_PO := 10 * Total_Active_PO / MaxActive_PO]

# Monetary_PO: customer total spending (benchmarked over 10)
MaxSpend_PO <- max(aggr_tPokerChip$Buy_PO)
aggr_tPokerChip[, Monetary_PO := 10 * Buy_PO / MaxSpend_PO]

# RFM_PO (basic model) = Recency_PO + Frequency_PO + Monetary_PO
aggr_tPokerChip[, RFM_PO := Recency_PO + Frequency_PO + Monetary_PO]

# Rank_PO: ranking customer by RFM_PO values
aggr_tPokerChip <- aggr_tPokerChip[order(RFM_PO, decreasing = TRUE)]
UserNum_PO <- nrow(aggr_tPokerChip)
aggr_tPokerChip[, Rank_PO := c(1:UserNum_PO)]

# ----------------------------------------------------
# Summary tPokerChip data.table family:
# ----------------------------------------------------
# tPokerChip - poker chip transaction in detail
# trans_tPokerChip - transpose version of tPokerChip
# aggr_tPokerChip - aggregate by UserID of trans_tPokerChip
# tLastAct_PO - last active day of poker gambler
# ----------------------------------------------------



# ----------------------------------------------------
# STEP 3 - CALCULATION SOME GENERAL METRICS
# ----------------------------------------------------

# ----------------------------------------------------
# 3.1. Build consolidated data mart of daily activities
# ----------------------------------------------------
# Create daily data mart, including all activities of SP, CA, GA and PO
dailydm <- merge(trans_tUserDaily[, c("UserID", "Date"), with = FALSE],
                 trans_tPokerChip[, c("UserID", "Date"), with = FALSE],
                 by = c("UserID", "Date"), all = TRUE)

# ----------------------------------------------------
# 3.2. Calculate some general customer metrics
# ----------------------------------------------------
# Total_Active: aggregate to count total active days to update in final datamart
dailydm[, Total_Active := 1]
totaldm <- dailydm[, lapply(.SD, sum), by = c("UserID")]
totaldm[, c("Date") := NULL] # Don't need these columns in yearly level

# Min_Bet, Max_Bet, Mean_Bet: some metrics about customer betting behaviors
# Avg_Bet_Amount: estimate average betting amount per activities
temp <- tUserDaily
temp[, Avg_Bet_Amount := Stakes / Bets]
temp <- temp[order(UserID, Product, Avg_Bet_Amount)]

# Max_Bet: gambler's maximum one-time betting amount on overal SP, CA and GA (not PO)
# Min_Bet: gambler's minimum one-time betting amount on overal SP, CA and GA (not PO)
# Avg_Bet: gambler's average one-time betting amount on overal SP, CA and GA (not PO)
temp[, ':=' (Max_Bet = max(Avg_Bet_Amount, na.rm = TRUE),
                      Min_Bet = min(Avg_Bet_Amount, na.rm = TRUE),
                      Avg_Bet = mean(Avg_Bet_Amount, na.rm = TRUE)),
              by = c("UserID")]

# Keep only unique Max_Bet, Min_Bet, Avg_Bet per UserID
temp <- temp[, lapply(.SD, max), by = c("UserID")]

# Merge with the final result
totaldm <- merge(totaldm, temp[, c('UserID', "Max_Bet", "Min_Bet", "Avg_Bet"), with = FALSE],
                 by = "UserID", all.x = TRUE)

# ----------------------------------------------------
# Summary detail activities data mart family:
# ----------------------------------------------------
# dailydm - consolidate daily activities of 4 catalogues
# totaldm - aggregate dailydm by UserID, calculate Max / Min / Average betting
# ----------------------------------------------------



# ----------------------------------------------------
# STEP 4 - FINAL DATA MART COMBINATION AND CALCULATION
# ----------------------------------------------------
# tDemographics / tUserDaily / tPokerChip
# trans_tUserDaily / trans_tPokerChip
# aggr_tUserDaily / aggr_tPokerChip
# totaldm

# ----------------------------------------------------
# 3.1. Put everything together
# ----------------------------------------------------
# Left join tDemographics with aggr_tUserDaily and aggr_tPokerChip into 1 data mart
dm <- merge(tDemographics, aggr_tUserDaily, by = "UserID", all.x = TRUE)
dm <- merge(dm, aggr_tPokerChip, by = "UserID", all.x = TRUE)

# Left join dm with totaldm to bring predicting leaving month into data mart
# Total_Active: included in totaldm
dm <- merge(dm, totaldm, by = "UserID", all.x = TRUE)

# Last_Act: last active date of a gambler for all SP, PO, CA and GA
dm[, Last_Act := pmax(Last_SP, Last_PO, Last_CA, Last_GA, na.rm = TRUE)]

# Fill all NA values by 0, except those columns which contain date-time
Namesdm <- names(dm)
ExceptCol <- c("UserID", "Country", "Language", "Gender", "ApplicationID",
               "Reg_Date", "First_Pay",
               "First_Act", "First_SP", "First_CA", "First_GA", "First_PO",
               "Last_Act", "Last_SP", "Last_CA", "Last_GA", "Last_PO",
               "Leaving_Month")

Namesdm <- setdiff(Namesdm, ExceptCol)
for (i in Namesdm)
  set(dm, which(is.na(dm[[i]])), i, 0)

# ----------------------------------------------------
# 3.2. Calculate RFM metrics (total)
# ----------------------------------------------------
# Total_Earning: total earning of gamblers
dm[, Total_Earning := Total_Earning_SP + Total_Earning_CA + Total_Earning_GA + Total_Earning_PO]

# Winning_Rate: general winning rate of gamblers (%)
dm[, Winning_Rate := 100 * (Win_SP + Win_CA + Win_GA + Win_PO) / 
                    (Win_SP + Win_CA + Win_GA + Win_PO + Loss_SP + Loss_CA + Loss_GA + Loss_PO)]

# Avg_Earning_Day: average earning (lossing) amount of gamblers per day
dm[, Avg_Earning_Day := Total_Earning /
                    (Win_SP + Win_CA + Win_GA + Win_PO + Loss_SP + Loss_CA + Loss_GA + Loss_PO)]

# Calculate customer total RFM values (10 + 10 + 10 = 30 max)
# Recency: customer recency = 10 - number of months that have passed since the last purchased
dm[, Recency := 10 - (month(EndPeriod) - month(Last_Act))]

# Frequency: customer frequency = numer of purchases in last 12 months (out of 10)
MaxActive <- max(dm$Total_Active)
dm[, Frequency := 10 * Total_Active / MaxActive]

# Total_Spend: total spending of a gambler
dm[, Total_Spend := Stakes_SP + Stakes_CA + Stakes_GA + Buy_PO]

# Monetary: customer total spending (benchmarked over 10)
MaxSpend <- max(dm$Total_Spend)
dm[, Monetary := 10 * Total_Spend / MaxSpend]

# RFM (basic model) = Recency + Frequency + Monetary (out of 30)
dm[, RFM := Recency + Frequency + Monetary]

# Rank: ranking customer by RFM values
dm <- dm[order(RFM, decreasing = TRUE)]
UserNum <- nrow(dm)
dm[, Rank := c(1:UserNum)]

# ----------------------------------------------------
# 3.3. Re-arrange columns order by product catalogues
# ----------------------------------------------------
# Set datamart columns order
setcolorder(dm, c("UserID", "Country", "Language", "Gender", "ApplicationID",
                  "Reg_Date", "First_Pay",
                  
                  "First_Act", "First_SP", "First_CA", "First_GA", "First_PO",
                  "Last_Act", "Last_SP", "Last_CA", "Last_GA", "Last_PO",
                  "Gap_Reg_Act", "Gap_Reg_Pay", "Num_Product",
                  "Max_Bet", "Min_Bet", "Avg_Bet",
                  
                  "Total_Earning", "Total_Spend", "Total_Active", "Winning_Rate", "Avg_Earning_Day",
                  "Recency", "Frequency", "Monetary", "RFM", "Rank",
                  
                  "Stakes_SP", "Winnings_SP", "Bets_SP", "Win_SP", "Loss_SP",
                  "Total_Earning_SP", "Total_Active_SP", "Winning_Rate_SP", "Avg_Earning_SP",
                  "Recency_SP", "Frequency_SP", "Monetary_SP", "RFM_SP", "Rank_SP",
                  
                  "Stakes_CA", "Winnings_CA", "Bets_CA", "Win_CA", "Loss_CA",
                  "Total_Earning_CA", "Total_Active_CA", "Winning_Rate_CA", "Avg_Earning_CA",
                  "Recency_CA", "Frequency_CA", "Monetary_CA", "RFM_CA", "Rank_CA",
                  
                  "Stakes_GA", "Winnings_GA", "Bets_GA", "Win_GA", "Loss_GA",
                  "Total_Earning_GA", "Total_Active_GA", "Winning_Rate_GA", "Avg_Earning_GA",
                  "Recency_GA", "Frequency_GA", "Monetary_GA", "RFM_GA", "Rank_GA",
                  
                  "Buy_PO", "Sell_PO", "Win_PO", "Loss_PO",
                  "Total_Earning_PO", "Total_Active_PO", "Winning_Rate_PO", "Avg_Earning_PO",
                  "Recency_PO", "Frequency_PO", "Monetary_PO", "RFM_PO", "Rank_PO"))



# ----------------------------------------------------
# STEP 5 - OUTPUTING TO CSV FILE
# ----------------------------------------------------
write.csv(dm, file = file.path(path, "mkt_datamart.csv"), row.names = FALSE, na = "")
# write.csv2(dm, file = file.path(path, "mkt_datamart2.csv"), row.names = FALSE, na = "")


# ----------------------------------------------------
# Updated on 31 Dec 2015
# ----------------------------------------------------