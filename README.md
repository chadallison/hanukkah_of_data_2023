Hanukkah of Data
================

### Contents

- [Data Import](#data-import)
- [The Investigator](#the-investigator)
- [The Contractor](#the-contractor)
- [The Neighbor](#the-neighbor)

------------------------------------------------------------------------

### Data Import

``` r
customers = read_csv("data/noahs-customers.csv", show_col_types = F)
items = read_csv("data/noahs-orders_items.csv", show_col_types = F)
orders = read_csv("data/noahs-orders.csv", show_col_types = F)
products = read_csv("data/noahs-products.csv", show_col_types = F)
```

------------------------------------------------------------------------

### The Investigator

Sarah brought a cashier over. She said, “Joe here says that one of our
customers is a skilled private investigator.”

Joe nodded, “They showed me their business card, and that’s what it
said. Skilled Private Investigator. And their phone number was their
last name spelled out. I didn’t know what that meant, but apparently
before there were smartphones, people had to remember phone numbers or
write them down. If you wanted a phone number that was easy-to-remember,
you could get a number that spelled something using the letters printed
on the phone buttons: like 2 has “ABC”, and 3 “DEF”, etc. And I guess
this person had done that, so if you dialed the numbers corresponding to
the letters in their name, it would call their phone number!

“I thought that was pretty cool. But I don’t remember their name, or
anything else about them for that matter. I couldn’t even tell you if
they were male or female.”

Sarah said, “This person seems like they are skilled at investigation. I
need them to find Noah’s rug before the Hanukkah dinner. I don’t know
how to contact them, but apparently they shop here at Noah’s Market.”

She nodded at the USB drive in your hand.

“Can you find this investigator’s phone number?”

``` r
data = customers |>
  separate(name, into = c("first_name", "last_name"), sep = " ", remove = F) |>
  mutate(phone_clean = str_remove_all(phone, "-"),
         last_name = str_to_upper(last_name)) |>
  select(last_name, phone_clean) |>
  filter(nchar(last_name) == nchar(phone_clean)) |>
  mutate(split_name = strsplit(last_name, ""))

letter_values = c(A = 2, B = 2, C = 2, D = 3, E = 3, F = 3,
                  G = 4, H = 4, I = 4, J = 5, K = 5, L = 5,
                  M = 6, N = 6, O = 6, P = 7, Q = 7, R = 7,
                  S = 7, T = 8, U = 8, V = 8, W = 9, X = 9, Y = 9, Z = 9)

get_letter_value = function(letter) {
  return(letter_values[letter])
}

name_to_num = function(name) {
  letters = str_split(name, "") |> unlist()
  values = sapply(letters, get_letter_value)
  return(paste(values, collapse = ""))
}

x = data |>
  mutate(name_as_num = sapply(split_name, name_to_num)) |>
  filter(phone_clean == name_as_num) |>
  pull(name_as_num)

sprintf("solution: %s", paste0(substr(x, 1, 3), "-", substr(x, 4, 6), "-", substr(x, 7, 10)))
```

    ## [1] "solution: 826-636-2286"

``` r
rm(data, letter_values, x, get_letter_value, name_to_num)
```

------------------------------------------------------------------------

### The Contractor

Thanks to your help, Sarah called the investigator that afternoon. The
investigator went directly to the cleaners to see if they could get any
more information about the unclaimed rug.

While they were out, Sarah said, “I tried cleaning the rug myself, but
there was this snail on it that always seemed to leave a trail of slime
behind it. I spent a few hours cleaning it, and the next day the slime
trail was back.”

When the investigator returned, they said, “Apparently, this cleaner had
a special projects program, where they outsourced challenging cleaning
projects to industrious contractors. As they’re right across the street
from Noah’s, they usually talked about the project over coffee and
bagels at Noah’s before handing off the item to be cleaned. The
contractors would pick up the tab and expense it, along with their
cleaning supplies.

“So this rug was apparently one of those special projects. The claim
ticket said ‘2017 JP’. ‘2017’ is the year the item was brought in, and
‘JP’ is the initials of the contractor.

“But they stopped outsourcing a few years ago, and don’t have contact
information for any of these workers anymore.”

Sarah first seemed hopeless, and then glanced at the USB drive you had
just put back in her hand. She said, “I know it’s a long shot, but is
there any chance you could find their phone number?”

``` r
possible_customers = customers |>
  separate(name, into = c("first", "last"), sep = " ", remove = F) |>
  mutate(fi = substr(first, 1, 1), li = substr(last, 1, 1)) |>
  filter(fi == "J" & li == "P")

possible_orders = orders |>
  filter(year(shipped) == 2017)

possible_products = products |>
  filter(str_detect(desc, "Rug "))

x = possible_products |>
  inner_join(items, by = "sku") |>
  inner_join(possible_orders, by = "orderid") |>
  inner_join(possible_customers, by = "customerid") |>
  distinct(name, phone) |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 332-274-4185"

``` r
rm(possible_customers, possible_orders, possible_products, x)
```

------------------------------------------------------------------------

### The Neighbor

Sarah and the investigator were very impressed with your data skills, as
you were able to figure out the phone number of the contractor. They
called up the cleaning contractor straight away and asked about the rug.

“Oh, yeah, I did some special projects for them a few years ago. I
remember that rug unfortunately. I managed to clean one section, which
revealed a giant spider that startled me whenever I tried to work on it.

“I already had a fear of spiders before this, but this spider was so
realistic that I had a hard time making any more progress. I kept
expecting the cleaners would call for the rug, but they never did. I
felt so bad about it, I couldn’t face them, and of course they never
gave me another project.

“At last I couldn’t deal with the rug taking up my whole bathtub, so I
gave it to this guy who lived in my neighborhood. He said that he was
naturally intuitive because he was a Cancer born in the year of the
Rabbit, so maybe he was able to clean it.

“I don’t remember his name. Last time I saw him, he was leaving the
subway and carrying a bag from Noah’s. I swore I saw a spider on his
hat.”

Can you find the phone number of the person that the contractor gave the
rug to?

``` r
years_of_rabbit = c(1939, 1951, 1963, 1974, 1987, 1999, 2011)

possible_customers = customers |>
  filter(year(birthdate) %in% years_of_rabbit &
        (month(birthdate) == 6 & day(birthdate) >= 21 | month(birthdate) == 7 & day(birthdate) <= 22))

contractor_zip = customers |>
  filter(phone == "332-274-4185") |>
  pull(citystatezip) |>
  str_extract("\\d+")

x = possible_customers |>
  filter(str_detect(citystatezip, contractor_zip)) |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 917-288-9635"

``` r
rm(years_of_rabbit, possible_customers, contractor_zip, x)
```

------------------------------------------------------------------------
