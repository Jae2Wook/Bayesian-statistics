# ??? Start with a preliminary guess, say f.
# ??? Compute Pl(f).
# ??? Change to f* by making a random transposition of the values f assigns to two symbols.
# ??? Compute Pl(f*); if this is larger than Pl(f), accept f*.
# ??? If not, flip a Pl(f*)/Pl(f) coin; if it comes up heads, accept f*.
# ??? If the coin toss comes up tails, stay at f.

library(tidyverse)
############################## Functions
english_letters <- function(text_list, var1) {
  text_list %>%
    mutate(
      "{{ var1 }}" := case_when({{ var1 }} == "A" ~ "A", {{ var1 }} == "A" ~ "A", {{ var1 }} == "A" ~ "A",
                                {{ var1 }} == "A" ~ "A", {{ var1 }} == "¨¡" ~ "A", {{ var1 }} == "E" ~ "E",
                                {{ var1 }} == "E" ~ "E", {{ var1 }} == "E" ~ "E", {{ var1 }} == "E" ~ "E",
                                {{ var1 }} == "O" ~ "O", {{ var1 }} == "O" ~ "O", {{ var1 }} == "O" ~ "O",
                                {{ var1 }} == "¨«" ~ "O", {{ var1 }} == "I" ~ "I", {{ var1 }} == "I" ~ "I",
                                {{ var1 }} == "I" ~ "I", {{ var1 }} == "U" ~ "U", {{ var1 }} == "U" ~ "U",
                                {{ var1 }} == "C" ~ "C", {{ var1 }} == "Y" ~ "Y", {{ var1 }} == "¡¯" ~ "'",
                                # {{ var1 }} == "¡®" ~ "¡°", # {{ var1 }} == "¡¯" ~ "¡±", # {{ var1 }} == "-" ~ "???",
                                # {{ var1 }} == "(" ~ as.character(NA), {{ var1 }} == ")" ~ as.character(NA),
                                # {{ var1 }} == "*" ~ as.character(NA), {{ var1 }} == "/" ~ as.character(NA),
                                # {{ var1 }} == "=" ~ as.character(NA), # ({{ var1 }} >= "0" & {{ var1 }} <= "9") ~ as.character(NA),
                                TRUE ~ {{ var1 }}
                                )
      ) %>%
    return()
  
}

score <- function(encoded_msg = encoded_message, current_map = f, table_of_values = transition_table){
  encoded_msg %>%
    select(code) %>%
    left_join(current_map, by = "code") %>%
    rename(first = letter_guessed) %>%
    mutate(second = lead(first)) %>%
    head(-1) %>%
    left_join(transition_table, by = c("first", "second")) %>%
    # filter_all(any_vars(!is.na(.))) %>%
    summarize(sum = sum(log_p)) %>%
    pull(sum) %>%
    return()
}

new_score <- function(encoded_msg = encoded_message, current_map = new_f, table_of_values = transition_table)
  {
  score(encoded_msg, current_map, table_of_values) %>%
    return()
  }

is_done <- function(encoded_msg = encoded_message, current_map = f)
  {encoded_msg %>%
    left_join(current_map, by = "code") %>%
    mutate(match = (letter == letter_guessed)) %>%
    summarize(sum = sum(match), count = length(match)) %>%
    mutate(done = (sum == count)) %>%
    pull(done) %>%
    return()
  }

tally <- function(encoded_msg = encoded_message, current_map = f, table_of_values = transition_table)
  {encoded_msg %>%
    left_join(current_map, by = "code") %>%
    rename(first = letter_guessed) %>%
    mutate(second = lead(first)) %>%
    head(-1) %>%
    left_join(transition_table, by = c("first", "second")) %>%
    filter_all(any_vars(!is.na(.))) %>%
    summarize(count = sum(log_p == min(log_p))) %>%
    pull(count) %>%
    return()
  }

############################## Read in data

# Read in "War and Peace"
x <- readLines('C:/Users/craigaj/OneDrive - BYU-Idaho/MyFiles/Courses/MATH424/Examples-Real World/Cryptography/War and Peace Text/War_and_Peace_Cleaned.txt')

war_peace_list <-
  data.frame(x = strsplit(toupper(str_squish(gsub("[^[:alpha:][:space:]'¡¯]", "", x))),'')) %>%
  rename(letter = 1)

# Summarize "War and Peace" - Create transition matrix
transition_table <- war_peace_list %>%
  rename(first = letter) %>%
  mutate(second = lead(first)) %>%
  group_by(first, second) %>%
  summarize(n = n()) %>%
  english_letters(first) %>%
  english_letters(second) %>%
  group_by(first, second) %>%
  summarize(n = sum(n)) %>%
  # Compute conditional probabilities
  group_by(first) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  ungroup() %>%
  # Fill in missing values with zero
  pivot_wider(names_from = second, values_from = proportion, values_fill = 0) %>%
  pivot_longer(cols = -first, names_to = "second", values_to = "proportion") %>%
  filter(second != "NA") %>%
  mutate(log_p = ifelse(proportion == 0, log(1e-20), log(proportion)))

transition_table %>% head

## Create code guess
# Naive code

f <- war_peace_list %>%
  unique() %>%
  english_letters(letter) %>%
  unique() %>%
  arrange(letter) %>%
  mutate(code = row_number()) %>%
  rename(letter_guessed = letter)

# Sequential code
f <- war_peace_list %>%
  unique() %>%
  english_letters(letter) %>%
  unique() %>%
  arrange(letter) %>%
  mutate(code = row_number()) %>%
  rename(letter_guessed = letter)

# # probability-based code
# f <- war_peace_list %>%
#   group_by(letter) %>%
#   summarize(n = n()) %>%
#   english_letters(letter) %>%
#   group_by(letter) %>%
#   summarize(n = sum(n)) %>%
#   arrange(desc(n)) %>%
#   mutate(code = row_number()) %>%
#   arrange(letter) %>%
#   rename(letter_guessed = letter) %>%
#   select(-n) %>%
#   ungroup()

f %>% head

# %%%%%%%%%%%%%%%% Read in Text to Be Encoded (Hamlet, 1 Nephi, Declaration of Independence, etc.)

setwd('C:/Users/craigaj/OneDrive - BYU-Idaho/MyFiles/Courses/MATH424/Examples-Real World/Cryptography/War and Peace Text/')

x <- readLines('sample text-hamlet.txt')
x <- readLines('Declaration.txt')
x <- readLines('1_nephi_1.txt')

secret_message_list <-data.frame(x =strsplit(toupper(str_squish(gsub("[^[:alpha:][:space:]']", "", x))),'')) %>% rename(letter = 1)

# Random initial guess
secret_code <- war_peace_list %>%
  unique() %>%
  english_letters(letter) %>%
  unique() %>%
  mutate(rand = runif(n())) %>%
  arrange(rand) %>%
  mutate(code = row_number()) %>%
  arrange(letter) %>%
  select(-rand)

secret_code %>% head

# # Probability-based initial guess

# secret_code <- secret_message_list %>%

#   group_by(letter) %>%
#   summarize(n = n()) %>%
#   english_letters(letter) %>%
#   group_by(letter) %>%
#   bind_rows(war_peace_list %>% unique() %>% english_letters(letter) %>% unique %>% mutate(n = 1)) %>%
#   summarize(n = sum(n)) %>%
#   arrange(desc(n)) %>%
#   mutate(code = row_number()) %>%
#   arrange(letter) %>%
#   ungroup()

# secret_code %>% head

encoded_message <- secret_message_list %>%
  left_join(secret_code, by = "letter")

encoded_message %>% head

# Main Loop
i <- -1
while (!(is_done())) {
  i <- i + 1
  new_f <- f
  swap_vals <- sample(x = 1:length(secret_code$code), size = 2, replace = FALSE)
  x <- new_f$letter_guessed[swap_vals[1]]
  new_f$letter_guessed[swap_vals[1]] <- new_f$letter_guessed[swap_vals[2]]
  new_f$letter_guessed[swap_vals[2]] <- x
  score_value <- score()
  new_score_value <- new_score()
  c(score_value, new_score_value, tally())
  if( new_score_value > score_value | score_value + log(runif(1)) < new_score_value ) {
    f <- new_f
    
    # Debugging
    if (new_score_value <= score_value) {star <- " - rand"} else {star <- " +++"}
    to_keep <- "swap"}
  else {
    # Debugging
    to_keep <- "keep"
    star <- ""
    }
  
  # Header for output
  if(i == 0) {print(paste0(paste(secret_code %>% select(letter) %>% arrange(letter) %>% pull, sep='', collapse=""),"  |"))}
  output <- encoded_message %>%
    left_join(f, by = "code") %>%
    head(90) %>% ################################### Debugging
    select(letter_guessed)
  # OUTPUT
  
  if (i %% 100 == 0){
    print(paste0(secret_code %>% left_join(f, by = "code") %>% select(letter_guessed) %>% pull() %>% paste(sep = '', collapse = ""), "  |  ",
                 paste(output$letter_guessed, sep='', collapse=""),
                 " -- i=",i," -- ",round(score(),0)))
    }
}

print(paste0(paste(output$letter_guessed, sep='', collapse="")," -- i=",i," -- ",round(score_value,0)))