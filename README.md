Hanukkah of Data
================

### Package Loading

``` r
library(tidyverse)
```

### Data Import

``` r
customers = read_csv("data/noahs-customers.csv", show_col_types = F)
items = read_csv("data/noahs-orders_items.csv", show_col_types = F)
orders = read_csv("data/noahs-orders.csv", show_col_types = F)
products = read_csv("data/noahs-products.csv", show_col_types = F)
```

### 1. The Investigator

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

get_letter_value = function(letter) {
  if (letter %in% c("A", "B", "C")) {
    return(2)
  } else if (letter %in% c("D", "E", "F")) {
    return(3)
  } else if (letter %in% c("G", "H", "I")) {
    return(4)
  } else if (letter %in% c("J", "K", "L")) {
    return(5)
  } else if (letter %in% c("M", "N", "O")) {
    return(6)
  } else if (letter %in% c("P", "Q", "R", "S")) {
    return(7)
  } else if (letter %in% c("T", "U", "V")) {
    return(8)
  } else if (letter %in% c("W", "X", "Y", "Z")) {
    return(9)
  }
}

name_to_num = function(name) {
  name_split = strsplit(name, "") |> unlist()
  values = c()
  for (letter in name_split) {
    values = c(values, get_letter_value(letter))
  }
  paste(values, collapse = "")
}

data |>
  mutate(name_as_num = sapply(split_name, name_to_num)) |>
  filter(phone_clean == name_as_num)
```

    ## # A tibble: 1 × 4
    ##   last_name  phone_clean split_name name_as_num
    ##   <chr>      <chr>       <list>     <chr>      
    ## 1 TANNENBAUM 8266362286  <chr [10]> 8266362286

------------------------------------------------------------------------
