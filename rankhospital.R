checkState <- function(s) {
  list_states <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
  ans <- s %in% list_states
}

checkOutcome <- function(o) {
  list_outcomes <- c("heart attack", "heart failure","pneumonia")
  ans <- o %in% list_outcomes
}

rankhospital <- function(state, outcome, num = "best") {
  
  if (!checkState(state)) stop("invalid state")
  if (!checkOutcome(outcome)) stop("invalid outcome")
  
  list_outcomes <- c("heart attack", "heart failure","pneumonia")
  
  if (outcome == list_outcomes[1]) field_col <- 11
  if (outcome == list_outcomes[2]) field_col <- 17
  if (outcome == list_outcomes[3]) field_col <- 23
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  subset_data <- data[,c(2, 7, field_col)]
  subset_data <- subset_data[complete.cases(subset_data),]
  select_data <- subset_data[subset_data$State == state,]
  select_data <- select_data[, c(1, 3)]
  select_data[, 2] <- suppressWarnings(as.numeric(select_data[, 2]))
  clean_data <- select_data[complete.cases(select_data),]
  clean_data <- clean_data[order(clean_data[2], clean_data[1]),]
  colnames(clean_data)[2] = "Rate"
  clean_data$Rank = seq.int(nrow(clean_data))
  num_rows <- nrow(clean_data)
  if (is.numeric(num)) {
    if (num_rows < num) {
      NA
    } else {
      clean_data[clean_data$Rank == num, 1]
    }
  } else {
    if (num == "best") {
      clean_data[1, 1]
    } else if (num == "worst") {
      #clean_data[nrow(clean_data),]
      ans <- clean_data[clean_data$Rate == clean_data[num_rows,2], ]
      ans[1,1]
    } else {
      stop("invalid order")
    }
  }
  
  
}