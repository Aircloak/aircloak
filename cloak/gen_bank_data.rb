# -------------------------------------------------------------------
# Parameters
# -------------------------------------------------------------------

customers_range = (11..500)


# -------------------------------------------------------------------
# General functions for generating dummy data
# -------------------------------------------------------------------

def swift(country)
  if country == "DE" then
    ["TEAMDE77", "IRVTDEFX", "ABNADEFFBER"].sample
  elsif country == "GB" then
    ["ANTSGB2L", "CLAOGB2LCIF", "SOGEGB22GBS"].sample
  elsif country == "NO" then
    ["CITINOKX", "DABANO22CLS"].sample
  elsif country == "NL" then
    ["ABNCNL2AENE", "RGVMNL2RTR2", "RGVMNL2RYBH"].sample
  end
end

def account_number(country)
  account_number = country
  (rand(10) + 18).times do
    account_number += "#{rand(9)}"
  end
  account_number
end

def gen_amount(balance)
  return [nil, balance] if balance < 0
  sign = rand < 0.98 ? -1 : 1
  amount = (rand(1100) + 1) * sign
  [amount, balance + amount]
end

def date()
  "2016-12-#{rand(31) + 1} #{rand(24)}:#{rand(60)}:#{rand(60)}"
end

def random_sentence()
  words = []
  (rand(10) + 1).times do
    word = ""; (rand(8) + 3).times{word << ((rand(2)==1?65:97) + rand(25)).chr}
    words << word
  end
  words.join(" ")
end

def description(amount)
  if rand() < 0.6 then
    return random_sentence()
  end

  if amount >= -30 and amount < 0 then
    ["Aldi sagt danke", "Edeka bedankt sich", "Rewe sagt Ciao", "Amazon"].sample
  elsif amount < -30 and amount >= -1000 then
    ["Amazon", "IKEA"].sample
  elsif amount < -1000 then
    ["Darlehen", "KFZ", "Versicherungsbeitrag", "Depot"].sample
  elsif amount > 1000 then
    ["Gehalt", "Salary", "Foobar Salary for December", "Gift"].sample
  else
    random_sentence()
  end
end


# -------------------------------------------------------------------
# State
# -------------------------------------------------------------------

customers = []
account_id = 0
transaction_id = 0


# -------------------------------------------------------------------
# Data generation
# -------------------------------------------------------------------

puts "COPY customers (id) FROM stdin;"
customers_range.each do |customer_id|
  puts "#{customer_id}"
end
puts "\\.\n"

puts "COPY accounts (id, customer_id, account_number, swift) FROM stdin;"
customers_range.each do |customer_id|
  country_code = ["DE", "DE", "DE", "DE", "GB", "NO", "NL"].sample
  accounts = []
  (rand(5) + 1).times.each do
    account_id += 1
    accounts << account_id
    puts "#{account_id}\t#{customer_id}\t#{account_number(country_code)}\t#{swift(country_code)}"
  end
  customer = {
    id: customer_id,
    accounts: accounts
  }
  customers << customer
end
puts "\\.\n"

puts "COPY transactions (id, customer_id, account_id, amount, date, description) FROM stdin;"
customers.each do |customer|
  customer[:accounts].each do |account_nr|
    income = rand(2000) + 400

    # Generate transaction for income
    transaction_id += 1
    puts "#{transaction_id}\t#{customer[:id]}\t#{account_nr}\t#{income}\t#{date()}\t#{description(income)}"

    balance = income

    rand(100).times do
      amount, balance = gen_amount(balance)

      if amount then
        transaction_id += 1
        puts "#{transaction_id}\t#{customer[:id]}\t#{account_nr}\t#{amount}\t#{date()}\t#{description(amount)}"
      end
    end
  end
end
puts "\\."
