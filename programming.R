##https://dplyr.tidyverse.org/articles/programming.html#the-tidyselect-dsl
##https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html

##Write a function for users

##Use expression as argument
linmod <- function(x){
  var <- enexpr(x)
  eval(expr(lm(disp~!!var, data = mtcars)))
}
linmod(wt)

## Use list of expressions
#Use arguments directly
corplot2 <- function(...){
  arg <- list(...)
  # do.call(corrplot, arg) #Add arg in list
  rlang::exec(corrplot, !!!arg, order = "hclust") #add arg out of list
}
corel <- base_eff_sal %>% select(11:31) %>% cor()
corplot2(corel, type="lower")

##Use list and dots as arguments
#Allow to add arguments in function

starg2 <- function(model,  ...){
  rlang::exec(stargazer, !!!model, ...)
}
starg2(list(lm, lm, lm), type = "text")


# starg2 <- function(model, n, ...){
# expres <- rep(list(model), n)
# rlang::exec(stargazer, !!!expres, ...)
# }
# starg2(lm, 2, type = "text")

##Use string as argument
linmod <- function(x){
  form <- paste("lm(disp~", x,",data = mtcars)")
  eval(parse(text = form))
}
linmod("wt")


##Use variable in data

with2 <- function(data, expr) {
  expr <- enquo(expr)
  eval_tidy(expr, data)
}
with2(data.frame(x = 2, y = 3), .data[["x"]] + .data[["y"]])
with2(data.frame(x = 2, y = 3), x+y)



#Using ggplot2 in packages Wed Oct 21 22:58:45 2020----------

##you already know the mapping in advance
mpg_drv_summary <- function() {
  ggplot(ggplot2::mpg) + 
    geom_bar(aes(x = .data$drv)) + 
    coord_flip()
}

## column name as a character vector
col_summary <- function(df, col) {
  ggplot(df) + 
    geom_bar(aes(x = .data[[col]])) + 
    coord_flip()
}
col_summary(mpg, "drv")

##column name or expression
col_summary <- function(df, col) {
  ggplot(df) + 
    geom_bar(aes(x = {{ col }})) + 
    coord_flip()
}
col_summary(mpg, drv)



#Programming with dplyr Wed Oct 21 22:52:35 2020----------

## data-variable as expression
filter(df, {{ var }})

var_summary <- function(data, var) {
  data %>%
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
}

##data-variable as character vector
summarise(df, mean = mean(.data[[var]]))

##data-variable as function (where, starts, end)
summarise_mean <- function(data, vars) {
  data %>% summarise(n = n(), across({{ vars }}, mean))
}
mtcars %>% 
  group_by(cyl) %>% 
  summarise_mean(where(is.numeric))

##data-variable as character (for select)
mtcars %>% select(all_of(vars))

## You know data-variables
my_summary_function <- function(data) {
  data %>% 
    filter(.data$x > 0) %>% 
    group_by(.data$grp) %>% 
    summarise(y = mean(.data$y), n = n())
}

##names of variables in the output
my_summarise4 <- function(data, expr) {
  data %>% summarise(
    "mean_{{expr}}" := mean({{ expr }}),
    "sum_{{expr}}" := sum({{ expr }}),
    "n_{{expr}}" := n()
  )
}

##arbitrary number of user supplied expressions
my_summarise <- function(.data, ...) {
  .data %>%
    group_by(...) %>%
    summarise(mass = mean(mass, na.rm = TRUE), 
              height = mean(height, na.rm = TRUE))
}

## set of data-variables as vector expression
summary_vars = c(mass, height)
my_summarise <- function(data, summary_vars) {
  data %>%
    summarise(across({{ summary_vars }}, ~ mean(., na.rm = TRUE)))
}

##control the names of the output.
my_summarise <- function(data, group_var, summarise_var) {
  data %>%
    group_by(across({{ group_var }})) %>% 
    summarise(across({{ summarise_var }}, 
                     mean, .names = "mean_{.col}"))
  
## character vector of variable names, 
##and want to operate on them with a for loop
  for (var in names(mtcars)) {
    mtcars %>% count(.data[[var]]) %>% print()
  }
}

_________________________
#Use character in dplyr

mtcars %>% filter((!!as.symbol(var)) == 4)

mtcars %>% filter((!!as.name(var)) == 4)

library(rlang)
var <- 'cyl'
mtcars %>% filter((!!sym(var)) == 4)

#In ggplot use aes_string
 ggplot(kas, aes_string(x = input$varx, 
                               y = input$vary))

##With group_by
df1 <- data %>%
  group_by_at(vars(one_of(columns))) %>%
  summarize(Value = mean(value))

##Wih summarise
df %>% 
    select(-matches(drp)) %>% 
    group_by(.data[[key]]) %>% 
    summarise(total = sum(.data[[val]], na.rm = TRUE))