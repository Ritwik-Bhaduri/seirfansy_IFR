target_date = "01-Feb-21"
target_date_india_format = as.Date(target_date, format = "%d-%b-%y")

data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
data_state_cases = data_state[data_state["Status"] == "Confirmed", ]
data_state_cases = data_state_cases[1:which(data_state_cases["Date"] == target_date),]
data_state_deaths = data_state[data_state["Status"] == "Deceased", ]
data_state_deaths = data_state_deaths[1:which(data_state_deaths["Date"] == target_date),]
state_codes = c("AN", "AP", "AR", "AS", "BR", "CH", "CT", "DN", "DL", "GA", "GJ", "HR", "HP" ,"JK", "JH", "KA", "KL", "LA", 
                "MP", "MH", "MN", "ML", "MZ", "NL", "OR", "PY", "PB", "RJ" ,"SK" ,"TN" ,"TG", "TR", "UP", "UT", "WB", "India")


data = read.csv(url("https://api.covid19india.org/csv/latest/case_time_series.csv"))
date_final = which(data$Date_YMD == target_date_india_format)
data_train = data[1 : date_final, ]
daily_confirmed <- data_train[,"Daily.Confirmed"] ## Daily Positive
daily_recovered <- data_train[,"Daily.Recovered"] ## Daily Recovered
daily_deaths <- data_train[,"Daily.Deceased"] ##  Daily Deaths
data_country_confirmed <- data_train[ ,"Total.Confirmed"]
data_country_recovered <- data_train[ ,"Total.Recovered"]
data_country_deaths <- data_train[ ,"Total.Deceased"]

observed_cum_cases = colSums(data_state_cases[, state_codes[-length(state_codes)]])
observed_cum_cases = c(observed_cum_cases, "India" = tail(data_country_confirmed,1))
observed_cum_deaths = colSums(data_state_deaths[, state_codes[-length(state_codes)]])
observed_cum_deaths = c(observed_cum_deaths, "India" = tail(data_country_deaths,1))

observed_CFR_values = observed_cum_deaths/observed_cum_cases

sd_calculator <- function(x, n){ # x = cfr, n = observed cases
  return(sqrt(x*(1-x)/n))
}

sd_cfr = c()
for(i in 1:length(observed_CFR_values)){
  x = observed_CFR_values[i]
  n = observed_cum_cases[i]
  sd_cfr[i] = sd_calculator(x, n)
}

low_CI_cfr = observed_CFR_values - sd_cfr*1.96
upper_CI_cfr = observed_CFR_values + sd_cfr*1.96
df_cfr = data.frame(lower_CI_cfr = low_CI_cfr, mean_cfr = observed_CFR_values, upper_CI_cfr = upper_CI_cfr)
rownames(df_cfr) = state_codes
# View(df_cfr)

saveRDS(df_cfr,paste0("CFR_CI-", target_date, ".rds"))

