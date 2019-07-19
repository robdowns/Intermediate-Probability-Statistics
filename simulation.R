rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = TRUE) )
    { install.packages(thispackage)}
    require(thispackage, character.only = TRUE)
  }
}

numSims = 10000
numYears = 10
capacities = seq(30000,60000,5000)
profits_matrix = matrix(0, nrow = numSims, ncol = length(capacities))

for (i in 1:numSims){
  demand = rnorm(numYears, mean = 50000, sd = 12000)
  profit_totals = rep(0,length(capacities)) 
  for (j in 1:length(capacities)){
    cap = capacities[j]
    plant_cost = 16 * cap
    cap_cost = rep(cap * .4, numYears)
    cap_vector = rep(cap,10)
    revenue = rep(0,numYears)
    prod_cost = rep(0,numYears)
    for (k in 1:numYears){
      revenue[k] = min(cap_vector[k],demand[k]) * 3.7
      prod_cost[k] = min(cap_vector[k],demand[k]) * .2
    }
    profit_yearly = revenue - prod_cost - cap_cost 
    profit_total = sum(profit_yearly) - plant_cost
    profits_matrix[i,j] = profit_total
  }
}

simulated_profits = rep(0, length(capacities))

for (i in 1:length(simulated_profits)){
  simulated_profits[i] = mean(profits_matrix[,i])
}
cbind(capacities,simulated_profits)
#50,000
ci_vector = profits_matrix[,5]
ci_vector = sort(ci_vector)
lower_tail_value = ci_vector[(length(ci_vector)*.025)+1]
upper_tail_value = ci_vector[(length(ci_vector)*(1-.025))-1]

numSims = 1000

total_cost_sum = rep(0,numSims)
total_warranty_sum = rep(0,numSims)
total_devices_sum = rep(0,numSims)

for (i in 1:numSims){
  device_lifetime = 0
  total_cost = c(100)
  total_warranty = c(0)
  total_devices = c(1)
  
  while (device_lifetime < 6){
    
    time_to_failure = rgamma(1,shape=2,scale=.5)
    
    if (time_to_failure <= 1){
      total_cost = c(total_cost,0)
      total_warranty = c(total_warranty,1)
      total_devices = c(total_devices,1)
      device_lifetime = device_lifetime + time_to_failure
    }
    if (time_to_failure > 1){
      total_cost = c(total_cost,100)
      total_warranty = c(total_warranty,0)
      total_devices = c(total_devices,1)
      device_lifetime = device_lifetime + time_to_failure
    }
    
  }
  total_cost_sum[i] = sum(total_cost)
  total_warranty_sum[i] = sum(total_warranty)
  total_devices_sum[i] = sum(total_devices) - 1
}

simulated_total_cost = mean(total_cost_sum)
simulated_warranty = mean(total_warranty_sum)
simulated_total_devices = mean(total_devices_sum)


numSims = 10000
count_days_sims = numSims
cust_nums = c(0,1,2,3,4)
cust_probs = c(.15,.25,.3,.2,.1)
purchaser_flag = c(1,0)
purchaser_probs = c(.6,.4)
pref_types = c("top-loader", "extra front", "regular front")
pref_probs = c(.4,.25,.35)



for (i in 1:numSims){
  count_days = 0
  inventory = c(5,4,3)
  sum_inventory = sum(inventory)
  while (sum_inventory > 0){
    day_cust_num = sample(cust_nums, size = 1, replace = T, prob = cust_probs)
    day_purchasers = sum(sample(purchaser_flag,size = day_cust_num, replace = T, prob = purchaser_probs))
    day_orders = sample(pref_types, size = day_purchasers, replace = T, prob = pref_probs)
    if (day_purchasers > 0){
      for (j in 1:length(day_orders)){
        if (day_orders[j] == "top-loader" & inventory[1] > 0){
          inventory[1] = inventory[1] - 1
        }
        if (day_orders[j] == "extra front" & inventory[2] > 0){
          inventory[2] = inventory[2] - 1
        }
        if (day_orders[j] == "regular front" & inventory[3] > 0){
          inventory[3] = inventory[3] - 1
        }
        sum_inventory = sum(inventory)
      }
      
    }
    count_days = count_days + 1
  }
  count_days_sims[i] = count_days
}
simulated_days_to_sellout = mean(count_days_sims)