checkState <- function(s) {
  list_states <- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
   ans <- s %in% list_states
}

checkOutcome <- function(o) {
  list_outcomes <- c("heart attack", "heart failure","pneumonia")
  ans <- o %in% list_outcomes
}

best <- function(state, outcome) {
  
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
  select_data[,3] <- suppressWarnings(as.numeric(select_data[,3]))
  clean_data <- select_data[complete.cases(select_data),]
  
  ans <- clean_data[ clean_data[3] == min(clean_data[3]),]
  ans <- ans[order(ans[1]),]
  ans[1,1]
}