# this is an r file


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
  clean(read.csv(inp, header=T, sep=",", colClasses=c("character", NA, NA, NA, "factor", NA, "character", "character", "factor", "factor", "character", "character")))
}

output = function(n, name, ts) {
  write.csv(groupBudget(ts), paste(name, "output.csv", sep="-"))
  write.csv(summary(n, ts), paste(name, "summary.csv", sep="-"))
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
  } else if (is.element(cat, ignore)) {
    "Ignore"
  } else if (is.element(cat, spend)) {
    "Spend"
  } else if (is.element(cat, home)) {
    "Home"
  } else if (is.element(cat, car)) {
    "Car"
  } else if (is.element(cat, health)) {
    "Health"
  } else {
    ""
  }
}


basics = c("Groceries" , "Auto Insurance" , "Gas" , "Memberships" , "Tuition & Fees" , "Gas & Fuel" , "Storage" , "Online Services" , "Cash" , "Clothing" , "Parking & Tolls" , "Public Transit" , "Bicycle")

# FUN
fun = c("Fast Food" ,"Alcohol & Bars" ,"Restaurants" ,"Other Food & Drink" ,"Other Sports & Fitness" ,"Business Services")

# IGNORE
ignore = c("Money Transfers" , "Other Income" , "Credit Card Payment" , "Interest" , "ATM Fees")

# SPEND
spend = c("Sporting Goods" ,"Books" ,"Music" ,"Tours & Cruises" ,"Random Fun" ,"Hobbies" ,"Camping" ,"Activities")

# HOME
home = c("Repairs & Improvement", "Hotels", "Other Home", "Furnishings")

# CAR
car = c("Auto Services", "Auto Supplies", "Parking Tickets")

# HEALTH
health = c("Eyes", "Other Health & Medical", "Pharmacies")


addBudget = function(ts) {
  transform(ts, Budget = sapply(Category, budget))
}

groupBudget = function(ts) {
  ts[order(ts$Budget, ts$Amount),]
}

summary = function(n, ts) {
  sum1 = aggregate(Amount ~ Budget, ts, sum)
  sum2 = transform(sum1, Amount1mo = round(Amount / n))
  sum3 = transform(sum2, Amount12mo = round(Amount / n * 12))
  sum3
}




# TODO: total by budget
