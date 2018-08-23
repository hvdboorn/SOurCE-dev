uu = function(x) unlist(unname(x))

breaks_to_labels = function(breaks)
{
  six_months = breaks%%6 == 0 & breaks > 0
  breaks[six_months] = paste0(breaks[six_months],"\n",breaks[six_months]/12," ",dutch$PLOT$year)
  str_replace(breaks, "0\\.5", dutch$PLOT$half)
}

to_title = function(title) {
  paste0(toupper(str_sub(title,1,1)),tolower(str_sub(title,2)))
}
