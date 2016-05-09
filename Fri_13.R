# Friday the 13th

rm(list=ls())

# Sequence of Months: non-leap and leap year
Jan <- Mar <- May <- Jul <- Aug <- Oct <- Dec <- 31
Feb <- 28; FebL <- 29
Apr <- Jun <- Sep <- Nov <- 30 
M.seq <- c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
ML.seq <- M.seq
ML.seq[2] <- FebL

# Initialize data frames to hold index weekdays for beginning of each month
# Both non leap and leap years
M1.index <- M1L.index <- as.data.frame(matrix(rep(NA, 12 * 7), nrow=12, ncol=7))
M1.index[1,] <- M1L.index[1,] <- 1:7
names(M1.index) <- names(M1L.index) <- paste0("d", 1:7)
row.names(M1.index) <- row.names(M1L.index) <- c("Jan", "Feb", "Mar", "Apr",
                                                 "May", "Jun", "Jul", "Aug",
                                                 "Sep", "Oct", "Nov", "Dec")

# Loop through each month and each possible Jan 1 weekday to determine
# which weekdays go with the first day of the subsequent months
for (i in 2:12) {
  for (j in 1:7) {
    M1.index[i, j] <- (M.seq[i-1] + M1.index[i-1, j]) %% 7
    M1L.index[i, j] <- (ML.seq[i-1] + M1L.index[i-1, j]) %% 7
  }
}

# Add 1 to the index after Jan to recover a 1:7 index (instead of 0:6) after
# the modulo operation
M1.index[2:12, ] <- M1.index[2:12, ] + 1
M1L.index[2:12, ] <- M1L.index[2:12, ] + 1

# Print the indices of the months' beginning weekdays
M1.index
M1L.index

# A Friday the 13th can only occur in months that begin on a Sunday;
# count the number of months that begin on Sun == 1
(Fri.13 <- apply(M1.index, 2, function(x) sum(x==1)))
(Fri.13.L <- apply(M1L.index, 2, function (x) sum(x==1)))
