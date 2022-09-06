setwd("~/Documents/GitHub/wifi-traffic-localization/data/raw_csv")

traffic <- read.csv("videocall.csv", stringsAsFactors = T)
# Filter non-802.11 packets
traffic <- subset(traffic, traffic$Protocol == "802.11") 

unique(traffic$Protocol)

# Initialize time
epoch <- traffic$Time[1]
end_of_time <- traffic$Time[nrow(traffic)]
interval <- 15

# compute number of rows
tot_time <- end_of_time - epoch
traffic_rows <- floor(tot_time/interval)

# Create empty dataframe with column names
dataset <- data.frame(matrix(ncol = 12, nrow = traffic_rows))

names <- c("mean_pl", "var_pl", "mean_iartime", "var_iartime", 
           "max_length", "min_length", "num_qosd", "num_qosnull", 
           "num_other", "num_up", "num_down", "type_of_traffic")
colnames(dataset) <- names

dataset$type_of_traffic <- "Videocall"

start_interval <- epoch
for(i in 1:traffic_rows) {
  traffic_interval <- subset(traffic, traffic$Time >= start_interval & traffic$Time < start_interval + interval)
  nrows <- nrow(traffic_interval)

  if(nrows != 0) {
    dataset[i,]$mean_pl <- mean(traffic_interval$Length)
    dataset[i,]$var_pl <- var(traffic_interval$Length)
    dataset[i,]$max_length <- max(traffic_interval$Length)
    dataset[i,]$min_length <- min(traffic_interval$Length)
    
    qosdata_rows <- nrow(traffic_interval[grep('QoS Data', traffic_interval$Info),])
    qosnull_rows <-  nrow(traffic_interval[grep('QoS Null function', traffic_interval$Info),])
    other_rows <- nrows - qosdata_rows - qosnull_rows
    
    dataset[i,]$num_qosd <- qosdata_rows
    dataset[i,]$num_qosnull <- qosnull_rows
    dataset[i,]$num_other <- other_rows
    
    rec_rows <- nrow(traffic_interval[grep('a4:42:3b:d2:f7:08|IntelCor_d2:f7:08', traffic_interval$Destination),])
    sent_rows <- nrow(traffic_interval[grep('a4:42:3b:d2:f7:08|IntelCor_d2:f7:08', traffic_interval$Source),])  
    
    dataset[i,]$num_down <- rec_rows
    dataset[i,]$num_up <- sent_rows
    
    if(nrows != 1) {
      iar <- vector(mode = "numeric", length = (nrows - 1))
      for(j in 1:(nrows - 1))
        iar[j] <- traffic_interval$Time[j + 1] - traffic_interval$Time[j]
      dataset[i,]$mean_iartime <- mean(iar)
      dataset[i,]$var_iartime <- var(iar)
      if(length(iar) < 2)
        dataset[i,]$var_iartime <- 0
    }
    else {
      dataset[i,]$var_pl <- 0
      dataset[i,]$mean_iartime <- 0
      dataset[i,]$var_iartime <- 0
    }
  }
  else 
    dataset[i,1:12] <- 0
  start_interval <- start_interval + interval
}

write.csv(dataset, "processed/processed_videocall.csv", row.names = FALSE)



