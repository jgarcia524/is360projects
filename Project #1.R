#Project 1
#Joanne Garcia

min_val = function (x) {
  current_min = Inf
  for (i in 1:length(x)) {
    if (!is.na(x[i]) & x[i] < current_min) {
      current_min = x[i]
    }
  }
  return(current_min)
}

max_val = function (x) { 
  current_max = -Inf
  for (i in 1:length(x)) {
    if (!is.na(x[i]) & x[i] > current_max) {
      current_max = x[i]
    }
  }
  return(current_max)
}

missing_val = function(x) {
  missing = 0
  for (i in 1:length(x)) {
    if (is.na(x[i])  ) {
      missing = missing + 1
    }
  }
  return (missing)
}

mean = function(x) {
  total = 0
  n = 0
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      total = total + x[i]
      n = n +1
    }
    the_mean = total / n  
  }
  return(the_mean)
  
}

stand_dev = function(x) {
  sq_df = 0
  avg = mean(x)
  n_good = length(x) - missing(x) 
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      sq_df = sq_df + (x[i] - avg)^2
    }
  }
  var = sq_df / (n_good-1)
  std_dev = sqrt(var)
  return(std_dev)
}


median = function(x) {
  vec = sort(x)
  if (length(x) == 1) {
    med = x[1]  
  }
  else if (length(x) %% 2 == 1) {
    med = x[ceiling(length(x)/2)]
  } 
  else {
    n = ceiling(length(x)/2)
    med = (x[n] + x[n+1]) /2 
  }
  
  return (med)  
}

quart = function(x,quartile) {
  
  x = sort(x)
  med = median(x)
  
  if (quartile == 1) {
    x_half = x[x < med]
    q = median(x_half)
  }
  else if (quartile == 2) {
    q = med
  }
  else if (quartile == 3) {
    x_half = x[x > med]
    q = median(x_half)
  }
  return (q)
}

stats = function(x) {
  
  stats_list = NULL
  
  stats_list['min'] = min_val(x)
  stats_list['max'] = max_val(x)
  stats_list['missing'] = missing_val(x)
  stats_list['mean'] = mean(x)
  stats_list['std dev'] = stand_dev(x)
  stats_list['median'] = median(x)
  stats_list['quartile 1'] = quart(x,1)
  stats_list['quartile 3'] = quart(x,3)
  
  return (stats_list)
}

x = c(-5:10,NA,NA)
statistics = stats(x)
print (statistics)

