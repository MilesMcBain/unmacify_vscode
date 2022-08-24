library(curl)
library(jsonlite)
library(tidyverse)

# fetch my keybindings
curl_fetch_disk(
  "https://raw.githubusercontent.com/MilesMcBain/vsconfig/master/keybindings.json",
  "my.keybindings.json"
)

mac_os_keybindings <-
  fromJSON("./my.macos.default.keybindings.json")

my_keybindings <-
  fromJSON("./my.keybindings.json")

# In the bindings I remove, keep them as removed, but swap the key combo for MacOS default.
my_minus_bindings <-
  my_keybindings |>
  filter(grepl("^-", command))

my_minus_bindings_mac_os <-
  mac_os_keybindings |> 
  semi_join(
    my_minus_bindings |>
    mutate(
      command = gsub("^-", "", command)
    ),
    by = c("command", "when")
  ) |>
  mutate(
    old_command = command,
    command = paste0("-", command)
  )

# swap alt for cmd in the bindings I add
my_add_bindings <-
  my_keybindings |>
  filter(!grepl("^-", command)) 

my_swapped_add_bindings <-
  my_add_bindings |>
  mutate(
    command = gsub("alt", "cmd", command)
  )

# swap cmd and ctrl in the MacOS bindings I have not removed
swapped_mac_os_bindings <- 
  mac_os_keybindings |>
  anti_join(
    my_minus_bindings_mac_os,
    by = c(command = "old_command", when = "when")
  ) |>
  mutate(
    key = gsub("cmd", "xxx", key),
    key = gsub("ctrl", "yyy", key),
    key = gsub("xxx", "ctrl", key),
    key = gsub("yyy", "cmd", key)
  )

# bind them all into one config and write out
bind_rows(
  swapped_mac_os_bindings,
  select(my_minus_bindings_mac_os, -old_command),
  my_swapped_add_bindings
) |> 
  toJSON(pretty = TRUE) |>
  write_lines(
    "my.macos.keybindings.json"
  )
  