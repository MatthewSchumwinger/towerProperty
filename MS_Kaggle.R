#########################################################
#                                                       #
#                towerProperty <- WOOT!                 #
#                                                       #
###-- Matt Schumwinger STATS 202 Kaggle contest code --##

library(reshape2)
setwdsetwd("~/Documents/towerProperty")
subscriptions = read.csv('subscriptions.csv',colClasses='character')
subscriptionsLong = melt(subscriptions,id=c('account.id','season'))
subscriptionsWide = dcast(subscriptionsLong, account.id~season+variable,value.var="value")
account = read.csv('account.csv')
wide = merge(account,subscriptionsWide,by="account.id",all=TRUE)
## HW1
tickets = read.csv('tickets.csv')
ticketsLong = melt(tickets, id=c('account.id', 'season', 'set'))
ticketsWide = dcast(ticketsLong, account.id~season+set+variable, value.var="value")
wide = merge(wide, ticketsWide, by="account.id", all=TRUE)
dim(wide)
### test set
test = read.csv('test.csv')
names(wide)

## create and write submission 10-10-14
entry = merge(test, wide, by = "account.id")
#entry = entry[, c(1,300)]
entry = entry[, c(1,156)] # select 2013-14 total variable
names(entry)[2] = "total"
table(entry$total)
entry[is.na(entry)] <- 1 # convert NA to zeroes
write.csv(entry, paste("submissions/MS_sub_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""),
          row.names = FALSE)
# resulting public score: 0.67798



######## ----- OLD BELOW -------- ######


## create and write submission OLD 10-5-14
#entry = merge(test, wide, by = "account.id")
#entry = entry[, c(1,300)]
#entry = entry[, c(1,156)] # select 2013-14 total variable
#names(entry)[2] = "total"
#table(entry$total)
#entry[is.na(entry)] <- 0 # convert NA to zeroes
#write.csv(entry, paste("MS_sub_", format(Sys.time(), "%b_%d_%Y"),".csv", sep=""),
          row.names = FALSE)
# resulting public score 0.10342








####
test = read.csv('test.csv')
names(wide)
table(wide[, 295])
table(is.na(wide[, 295]))

# tag accounts who bought tickets in the 13-14 season
# using price level for each of the 6 sets
set1 = !is.na(wide[, 270]) 
set2 = !is.na(wide[, 275]) 
set3 = !is.na(wide[, 280]) 
set4 = !is.na(wide[, 285]) 
set5 = !is.na(wide[, 290]) 
set6 = !is.na(wide[, 295]) 

pred = cbind(set1, set2, set3, set4, set5, set6)
buy = apply(pred, 1, any)
buy = buy + 0 #converts from logical to numerical
id = as.character(wide$account.id)
pred = cbind(pred, buy, id)
pred = pred[, c(7,8)]
pred = as.data.frame(pred)
names(pred) = c("buy", "account.id")
entry = merge(test, pred, by = "account.id", all.x = TRUE)
names(entry) = c("account.id", "total")
table(entry$total)  


## maybe I'm grabbing the wrong variables, see this:
table(wide[, 163])
# find total subscrptions for the 13-14 season [, 156]
table(wide[, 156])
table(is.na(wide[, 156]))



## for Kaggle
## Create a formula for a model with a large number of variables:
xnam <- paste0("x", 1:25)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))