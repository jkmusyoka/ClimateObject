# summary function labels
sum_label="Sum"
mode_label="summary_mode"
count_label="summary_count"
sd_label = "summary_sd"
median_label = "summary_median"
range_label = "summary_range"
count_label = "summary_count"
min_label="summary_min"
max_label="summary_max"
mean_label="summary_mean"
running_summary_label="summary_running_summary"

# list of summary functions
summaries_list=c(sum_label, mode_label, count_label, sd_label, median_label, range_label, count_label, min_label, max_label, mean_label, running_summary_label)

summary_mode <- function(x,...) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

summary_mean <- function (x, na.rm = FALSE,...) {
  if( length(x)==0 || (na.rm && length(x[!is.na(x)])==0) ) return(NA)
  else mean(x, na.rm=na.rm,...)
}

summary_sum <- function (x, na.rm = FALSE,...) {
  sum(x, na.rm = FALSE,...)
} 

summary_count <- function(x, na.rm = FALSE,...) {
  return(length(x, na.rm = na.rm,...))
}

summary_sd <- function(x, na.rm = FALSE,...) {
  return(sd(x,...))
}

summary_max <- function (x, na.rm = FALSE,...) {
  if( length(x)==0 || (na.rm && length(x[!is.na(x)])==0) ) return(NA)
  else max(x, na.rm = na.rm)
} 

summary_min <- function (x, na.rm = FALSE,...) {
  if( length(x)==0 || (na.rm && length(x[!is.na(x)])==0) ) return(NA)
  else min(x, na.rm = na.rm)
} 

#get the range of the data
summary_range <- function(x, na.rm = TRUE, ...){
  max(x, na.rm = na.rm) - min(x, na.rm = na.rm)  
}

# median function
summary_median <- function(x, ...) {
  odd.even <- length(x)%%2
  if (odd.even == 0)(sort(x)[length(x)/2] + sort(x)[1 + length(x)/2])/2
  else sort(x)[ceiling(length(x)/2)]
}

#in progress with return and print 
summary_count <- function(x, proportions = c(120,140,160,180,200), na.rm = TRUE, ...){
  count = c()
  for (i in 1:length(proportions)){
    count[i] = sum(x <= proportions[i], na.rm = na.rm)
    return(paste("count <=", proportions[i], "is", count[i]))
  }
}

# results as percent of data (in progress)
summary_percents = function(x,data, proportions = c(120,140,160,180,200), na.rm = FALSE, ...){
  count = c()
  percent = c()
  for (i in 1:length(proportions)){
    count[i] = sum(x <= proportions[i], na.rm = na.rm)
    percent[i] = (count[i]/nrow(data))*100
    print(paste("% of data <=", proportions[i], "is", percent[i]))
  }
}

# proportion of data (in progress)
summary_proportions <- function(x, data, proportions = c(120,140,160,180,200), na.rm = FALSE, ...){
  count = c()
  proportion = c()
  for (i in 1:length(proportions)){
    count[i] = sum(x <= proportions[i], na.rm = na.rm)
    proportion[i] = (count[i]/nrow(data))
    print(paste("proportion <=", proportions[i], "is", proportion[i]))
  }
}

summary_running_summary <- function(data, total_days = 1, func = max_label, na.rm = FALSE,...) {
  h=c()
  for (i in 1:(length(data)-total_days+1)){
    h[i] <- Sum(data[i:(i+total_days-1)], na.rm = na.rm)
  }
  
  print(h)
  if(missing(func)) return(h)
  else {
    func = match.fun(func)
    return(func(h, na.rm = na.rm))
  }
}