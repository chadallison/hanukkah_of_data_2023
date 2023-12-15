Hanukkah of Data
================

### Contents

- [Data Import](#data-import)
- [The Investigator](#the-investigator)
- [The Contractor](#the-contractor)
- [The Neighbor](#the-neighbor)
- [The Early Bird](#the-early-bird)
- [The Cat Lady](#the-cat-lady)
- [The Bargain Hunter](#the-bargain-hunter)
- [The Meet Cute](#the-meet-cute)
- [The Collector](#the-collector)

### Speedrun Contents

- [Data Import - Speedrun](#data-import---speedrun)
- [The Investigator - Speedrun](#the-investigator---speedrun)
- [The Contractor - Speedrun](#the-contractor---speedrun)
- [The Neighbor - Speedrun](#the-neighbor---speedrun)
- [The Early Bird - Speedrun](#the-early-bird---speedrun)
- [The Cat Lady - Speedrun](#the-cat-lady---speedrun)
- [The Bargain Hunter - Speedrun](#the-bargain-hunter---speedrun)
- [The Meet Cute - Speedrun](#the-meet-cute---speedrun)
- [The Collector - Speedrun](#the-collector---speedrun)

------------------------------------------------------------------------

### Data Import

``` r
customers = read_csv("data/regular/noahs-customers.csv", show_col_types = F)
items = read_csv("data/regular/noahs-orders_items.csv", show_col_types = F)
orders = read_csv("data/regular/noahs-orders.csv", show_col_types = F)
products = read_csv("data/regular/noahs-products.csv", show_col_types = F)
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
years_of_rabbit = c(1939, 1951, 1963, 1975, 1987, 1999, 2011)

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

------------------------------------------------------------------------

### The Early Bird

The investigator called the phone number you found and left a message,
and a man soon called back:

“Wow, that was years ago! It was quite an elegant tapestry.

“It took a lot of patience, but I did manage to get the dirt out of one
section, which uncovered a superb owl. I put it up on my wall, and
sometimes at night I swear I could hear the owl hooting.

“A few weeks later my bike chain broke on the way home, and I needed to
get it fixed before work the next day. Thankfully, this woman I met on
Tinder came over at 5am with her bike chain repair kit and some pastries
from Noah’s. Apparently she liked to get up before dawn and claim the
first pastries that came out of the oven.

“I didn’t have any money or I would’ve paid her for her trouble. She
really liked the tapestry, though, so I wound up giving it to her.

“I don’t remember her name or anything else about her.”

Can you find the bicycle fixer’s phone number?

``` r
x = products |>
  filter(str_detect(sku, "BKY")) |>
  inner_join(items, by = "sku") |>
  filter(qty > 1) |>
  inner_join(orders, by = "orderid") |>
  filter(hour(shipped) == 4) |>
  distinct(customerid) |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 607-231-3605"

------------------------------------------------------------------------

### The Cat Lady

“Yes, I did have that tapestry for a little bit. I even cleaned a
blotchy section that turned out to be a friendly koala.

“But it was still really dirty, so when I was going through a Marie
Kondo phase, I decided it wasn’t sparking joy anymore.

“I listed it on Freecycle, and a woman in Staten Island came to pick it
up. She was wearing a ‘Noah’s Market’ sweatshirt, and it was just
covered in cat hair. When I suggested that a clowder of cats might ruin
such a fine tapestry, she looked at me funny. She said “I only have ten
or eleven cats, and anyway they are getting quite old now, so I doubt
they’d care about some old rug.”

“It took her 20 minutes to stuff the tapestry into some plastic bags she
brought because it was raining. I spent the evening cleaning my
apartment.”

What’s the phone number of the woman from Freecycle?

``` r
x = items |>
  filter(str_detect(sku, "PET") & qty >= 10) |>
  inner_join(orders, by = "orderid") |>
  inner_join(customers, by = "customerid") |>
  filter(str_detect(citystatezip, "Staten Island")) |>
  distinct(name, phone) |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 631-507-6048"

------------------------------------------------------------------------

### The Bargain Hunter

“Why yes, I did have that rug for a little while in my living room! My
cats can’t see a thing but they sure chased after the squirrel on it
like it was dancing in front of their noses.

“It was a nice rug and they were surely going to ruin it, so I gave it
to my cousin, who was moving into a new place that had wood floors.

“She refused to buy a new rug for herself–she said they were way too
expensive. She’s always been very frugal, and she clips every coupon and
shops every sale at Noah’s Market. In fact I like to tease her that Noah
actually loses money whenever she comes in the store.

“I think she’s been taking it too far lately though. Once the subway
fare increased, she stopped coming to visit me. And she’s really slow to
respond to my texts. I hope she remembers to invite me to the family
reunion next year.”

Can you find her cousin’s phone number?

``` r
x = items |>
  inner_join(products, by = "sku") |>
  mutate(shop_price = qty * unit_price,
         wholesale_price = qty * wholesale_cost) |>
  group_by(orderid) |>
  summarise(order_shop_price = sum(shop_price),
            order_wholesale_price = sum(wholesale_price)) |>
  filter(order_wholesale_price > order_shop_price) |>
  inner_join(orders, by = "orderid") |>
  count(customerid) |>
  slice_max(n, n = 1, with_ties = F) |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 585-838-9161"

------------------------------------------------------------------------

### The Meet Cute

“Oh that tapestry, with the colorful toucan on it! I’ll tell you what
happened to it.

“One day, I was at Noah’s Market, and I was just about to leave when
someone behind me said ‘Miss! You dropped something!’

“Well I turned around to see this cute guy holding an item I had bought.
He said, ‘I got the same thing!’ We laughed about it and wound up
swapping items because I wanted the color he got. We had a moment when
our eyes met and my heart stopped for a second. I asked him to get some
food with me and we spent the rest of the day together.

“Before long I moved into his place, but the romance faded quickly, as
he wasn’t the prince I imagined. I left abruptly one night, forgetting
the tapestry on his wall. But by then, it symbolized our love, and I
wanted nothing more to do with it. For all I know, he still has it.”

Can you figure out her ex-boyfriend’s phone number?

``` r
df = customers |>
  filter(phone == "585-838-9161") |>
  inner_join(items |>
  inner_join(orders, by = "orderid") |>
  filter(str_detect(sku, "COL")), by = "customerid") |>
  inner_join(products, by = "sku") |>
  separate(desc, into = c("item", "color"), sep = " \\(") |>
  mutate(color = str_remove_all(color, "\\)"),
         min_order = ordered - minutes(1),
         max_order = ordered + minutes(1))

df2 = items |>
  filter(str_detect(sku, "COL")) |>
  inner_join(orders, by = "orderid") |>
  inner_join(products, by = "sku") |>
  separate(desc, into = c("item", "color"), sep = " \\(") |>
  filter(item %in% df$item)

sol = data.frame()

for (i in 1:nrow(df)) {
  sol = rbind(sol, df2 |> filter(item == df$item[i] & shipped >= df$min_order[i] & shipped <= df$max_order[i]))
}

x = (sol |>
  inner_join(customers, by = "customerid") |>
  distinct(name, phone) |>
  pull(phone))[1]

sprintf("solution: %s", x)
```

    ## [1] "solution: 838-335-7157"

------------------------------------------------------------------------

### The Collector

“Oh that damned woman! She moved in, clogged my bathtub, left her
coupons all over the kitchen, and then just vanished one night without
leaving so much as a note.

Except she did leave behind that nasty carpet. I spent months cleaning
one corner, only to discover a snake hiding in the branches! I knew then
that she was never coming back, and I had to get it out of my sight.

“Well, I don’t have any storage here, and it didn’t seem right to sell
it, so I gave it to my sister. She wound up getting a newer and more
expensive carpet, so she gave it to an acquaintance of hers who collects
all sorts of junk. Apparently he owns an entire set of Noah’s
collectibles! He probably still has the carpet, even.

“My sister is away for the holidays, but I can have her call you in a
few weeks.”

The family dinner is tonight! Can you find the collector’s phone number
in time?

``` r
x = items |>
  inner_join(orders, by = "orderid") |>
  inner_join(customers, by = "customerid") |>
  inner_join(products, by = "sku") |>
  filter(str_detect(sku, "COL")) |>
  separate(desc, into = c("item", "color"), sep = " \\(") |>
  mutate(color = str_remove_all(color, "\\)")) |>
  group_by(customerid, item) |>
  summarise(n_items = n_distinct(color),
            .groups = "drop") |>
  filter(n_items == 12) |>
  distinct(customerid) |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 212-547-3518"

------------------------------------------------------------------------

### Data Import - Speedrun

``` r
customers = read_csv("data/speedrun/noahs-customers.csv", show_col_types = F)
items = read_csv("data/speedrun/noahs-orders_items.csv", show_col_types = F)
orders = read_csv("data/speedrun/noahs-orders.csv", show_col_types = F)
products = read_csv("data/speedrun/noahs-products.csv", show_col_types = F)
```

------------------------------------------------------------------------

### The Investigator - Speedrun

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

    ## [1] "solution: 767-365-7269"

------------------------------------------------------------------------

### The Contractor - Speedrun

``` r
possible_customers = customers |>
  separate(name, into = c("first", "last"), sep = " ", remove = F) |>
  mutate(fi = substr(first, 1, 1), li = substr(last, 1, 1)) |>
  filter(fi == "D" & li == "S")

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

    ## [1] "solution: 838-351-0370"

------------------------------------------------------------------------

### The Neighbor - Speedrun

``` r
years_of_goat = c(1943, 1955, 1967, 1979, 1991, 2003, 2015, 2027)

possible_customers = customers |>
  filter(year(birthdate) %in% years_of_goat &
        (month(birthdate) == 9 & day(birthdate) >= 23 | month(birthdate) == 10 & day(birthdate) <= 22))

contractor_zip = customers |>
  filter(phone == "838-351-0370") |>
  pull(citystatezip) |>
  str_extract("\\d+")

x = possible_customers |>
  filter(str_detect(citystatezip, contractor_zip)) |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 914-594-5535"

------------------------------------------------------------------------

### The Early Bird - Speedrun

``` r
x = products |>
  filter(str_detect(sku, "BKY")) |>
  inner_join(items, by = "sku") |>
  filter(qty > 1) |>
  inner_join(orders, by = "orderid") |>
  filter(hour(shipped) == 4 & minute(shipped) >= 50) |>
  distinct(customerid) |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 716-789-4433"

------------------------------------------------------------------------

### The Cat Lady - Speedrun

``` r
x = items |>
  filter(str_detect(sku, "PET") & qty >= 10) |>
  inner_join(orders, by = "orderid") |>
  inner_join(customers, by = "customerid") |>
  distinct(name, phone) |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 347-835-2358"

------------------------------------------------------------------------

### The Bargain Hunter - Speedrun

``` r
x = items |>
  inner_join(products, by = "sku") |>
  mutate(shop_price = qty * unit_price,
         wholesale_price = qty * wholesale_cost) |>
  group_by(orderid) |>
  summarise(order_shop_price = sum(shop_price),
            order_wholesale_price = sum(wholesale_price)) |>
  filter(order_wholesale_price > order_shop_price) |>
  inner_join(orders, by = "orderid") |>
  count(customerid) |>
  slice_max(n, n = 1, with_ties = F) |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 838-295-7143"

------------------------------------------------------------------------

### The Meet Cute - Speedrun

``` r
df = customers |>
  filter(phone == "838-295-7143") |>
  inner_join(orders, by = "customerid") |>
  inner_join(items, by = "orderid") |>
  inner_join(products, by = "sku") |>
  filter(str_detect(desc, "[()]")) |>
  separate(desc, into = c("item", "color"), sep = " \\(") |>
  mutate(color = str_remove_all(color, "\\)"))

possible_orders = orders |>
  filter(!customerid %in% df$customerid) |>
  inner_join(items, by = "orderid") |>
  inner_join(products, by = "sku") |>
  filter(str_detect(desc, "[()]")) |>
  separate(desc, into = c("item", "color"), sep = " \\(") |>
  mutate(color = str_remove_all(color, "\\)")) |>
  filter(item %in% df$item & as_date(ordered) %in% as_date(df$ordered))

res = data.frame()

for (i in 1:nrow(df)) {
  valid = possible_orders |>
    filter(item == df$item[i],
           ordered >= df$ordered[i] - minutes(5),
           ordered <= df$ordered[i] + minutes(5))
  
  if (nrow(valid) > 0) res = rbind(res, valid)
}

x = res |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 516-544-4187"

------------------------------------------------------------------------

### The Collector - Speedrun

``` r
x = items |>
  inner_join(orders, by = "orderid") |>
  inner_join(customers, by = "customerid") |>
  inner_join(products, by = "sku") |>
  filter(str_detect(sku, "COL")) |>
  separate(desc, into = c("item", "color"), sep = " \\(") |>
  mutate(color = str_remove_all(color, "\\)")) |>
  group_by(customerid, item) |>
  summarise(n_items = n_distinct(color),
            .groups = "drop") |>
  filter(n_items == 12) |>
  distinct(customerid) |>
  inner_join(customers, by = "customerid") |>
  pull(phone)

sprintf("solution: %s", x)
```

    ## [1] "solution: 516-638-9966"

------------------------------------------------------------------------
