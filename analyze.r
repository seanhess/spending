library(tidyverse)


# input = "2020-04-29-exported_transactions.csv"
# transactions = load(input)
# data = addBudget(transactions)

analyze = function(input) {
  addBudget(load(input))
}

clean = function(frame) {
  frame[c("Date","Amount", "Description", "Category")]
}

load = function(inp) {
  read.csv(inp, stringsAsFactors=FALSE) %>%
    select(Date, Description, Category, Amount) %>%
    mutate(
      Date = as.Date(Date),
      Source = "Simple"
    )
}

loadCapitalOne = function(inp) {
  data = read.csv(inp, stringsAsFactors = FALSE)
  data[is.na(data)] <- 0
  data %>%
    mutate(
      Date = as.Date(Posted.Date),
      Amount = Credit - Debit,
      Source = "Capital One",
    ) %>%
    select(Date, Description, Category, Amount, Source) %>%
    # Remove payments, and that one collision because it was reimbursed
    filter(Category != "Payment/Credit", Description != "VALLEY COLLISION VC 1")
}


output = function(ts, name) {
  write.csv(groupBudget(ts), paste(name, "output.csv", sep="-"))
  write.csv(summary(ts), paste(name, "summary.csv", sep="-"), na="0")
}

budget = function(cat) {
  # Basics (groceries, gas, bills, small purchases)
  # Health (therapy, unexpected stuff, etc)
  # Car
  # Fun (restaurants, etc, all spending?????)
  # Spend (tickets, kid gifts)
  # Home (home improvment, rentals)
  # Ignore (money transfers)

  if (is.element(cat, basics)) {
    "Basics"
  } else if (is.element(cat, fun)) {
    "Fun"
  } else if (is.element(cat, health)) {
    "Health"
  } else if (is.element(cat, home)) {
    "Home"
  } else if (is.element(cat, spend)) {
    "Spend"
  } else if (is.element(cat, emergencies)) {
    "Emergency"
  } else if (is.element(cat, ignore)) {
    "Ignore"
  } else {
    "_"
  }
}

budgetCapOne = function(cat, desc) {
  if (grepl("AIRBNB", desc, fixed=TRUE)) {
    "Home"
  } else if (grepl("JT AUTOMOTIVE", desc, fixed=TRUE)) {
    "Emergency"
  } else if (grepl("SMITHS", desc, fixed=TRUE)) {
    "Basics"
  } else if (cat == "Dining") {
    "Fun"
  } else {
    "Spend"
  }
}



basics = c("Groceries" , "Auto Insurance" , "Gas" , "Memberships" , "Tuition & Fees" , "Gas & Fuel" ,  "Online Services" , "Cash" , "Clothing" , "Parking & Tolls" , "Public Transit" , "Bicycle", "Software", "Hair")

# FUN
fun = c("Fast Food" ,"Alcohol & Bars" ,"Restaurants" ,"Other Food & Drink" ,"Other Sports & Fitness" ,"Business Services", "Coffee & Tea", "Taxis")

# EMERGENCIES
emergencies = c("Auto Services", "Parking Tickets", "Doctor", "Care Facilities")

# IGNORE
ignore = c("Money Transfers" , "Other Income" , "Credit Card Payment" , "Interest" , "ATM Fees")

# SPEND
spend = c("Sporting Goods" ,"Books" ,"Music" ,"Tours & Cruises" ,"Random Fun" ,"Hobbies" ,"Camping" ,"Activities", "Auto Supplies")

# HOME
home = c("Repairs & Improvement", "Hotels", "Other Home", "Furnishings", "Storage")

# HEALTH
health = c("Eyes", "Pharmacies", "Other Health & Medical", "Dentist", "Health Insurance")


addBudget = function(ts) {
  mutate(ts, Budget = as.character(sapply(Category, budget)))
}

addBudgetCapOne = function(ts) {
  mutate(ts, Budget = as.character(mapply(budgetCapOne, Category, Description)))
}

groupBudget = function(ts) {
  ts[order(ts$Budget, ts$Amount),]
}

toDate = function(ds) {
  as.Date(ds)
}

addMonth = function(data) {
  mutate(data, Month = format(Date, "%Y-%m"))
}

# Summary by month by Budget
summary = function(ts) {
  ts %>%
    addMonth() %>%
    group_by(Month, Budget) %>%
    summarize(
       Total = sum(Amount)
       # n = n()
    ) %>%
    filter(Budget != "Ignore") %>%
    pivot_wider(names_from = Budget, values_from = Total, values_fill = list(Total = 0))

  # sum1 = aggregate(Amount ~ Budget, ts, sum)
  # sum2 = transform(sum1, Amount1mo = round(Amount / n))
  # sum3 = transform(sum2, Amount12mo = round(Amount / n * 12))
  # sum3
}




# TODO: total by budget
