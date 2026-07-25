# non-standard evaluation variables, declared so R CMD check does not read them
# as undefined globals
utils::globalVariables(c(
  ":=", "all_of", "key_item", "locname", "parameter", "set_names", "tier"
))
