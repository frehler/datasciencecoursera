# Task 1
df <- read.csv("hospitalcompare/outcome-of-care-measures.csv")
head(df)
ncol(df)
nrow(df)
names(df)
df[, 11] <- as.numeric(df[, 11])
hist(df[, 11])

# Task 2
best <- function(state, outcome) {
  valid_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  
  ## Read outcome data
  df <- read.csv("hospitalcompare/outcome-of-care-measures.csv")
  
  ## Check input
  if(!state %in% df$State){
    stop("invalid state")
  } else if(!outcome %in% names(valid_outcomes)){
    stop("invalid outcome")
  }
  
  col <- valid_outcomes[outcome]
  
  df <- subset(df, State == state)
  df[, col] <- as.numeric(df[, col])
  df <- df[complete.cases(df[, col]), ]

  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(df[which.min(df[, col]), ]$Hospital.Name)
  
}

best("TX", "heart attack")

# Task 3
rankhospital <- function(state, outcome, num = "best") {
  valid_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)

  ## Read outcome data
  df <- read.csv("hospitalcompare/outcome-of-care-measures.csv")
  
  ## Check input
  if(!state %in% df$State){
    stop("invalid state")
  } else if(!outcome %in% names(valid_outcomes)){
    stop("invalid outcome")
  }
  
  col <- valid_outcomes[outcome]
  
  df <- subset(df, State == state)
  df[, col] <- as.numeric(df[, col])
  df <- df[complete.cases(df[, col]), ]
  
  df <- df[order(df[, col], df$Hospital.Name),]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(num == "best"){
    return(df$Hospital.Name[1])
  } else if (num == "worst"){
    return(tail(df$Hospital.Name, n = 1))
  } else {
    return(df$Hospital.Name[num])
  }

}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

# Task 4
rankall <- function(outcome, num = "best"){
  df <- read.csv("hospitalcompare/outcome-of-care-measures.csv")
  state <- unique(df$State)

  hospital = vector(mode = "character")
  for(s in state){
    rank <- rankhospital(s, outcome, num)
    hospital <- c(hospital, rank)
  }
  
  result <- data.frame(hospital, state)
  return(result[order(state), ])

}  
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
  
  
  