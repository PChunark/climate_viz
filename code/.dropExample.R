# Count a missing variable ####
## Change x to factor, Count CANNOT capture number 5 ####
tibble (x = sample(1:4, 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5)) %>% # turn x to factors
  count(x) # count can not count number 5  

## Change x to factor, Count CAN capture number 5 ####      
tibble (x = sample(1:4, 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5)) %>% # turn x to factors
  count(x, .drop = FALSE) # count can count number 5  

## Drop the missing value ####
tibble (x = sample(1:4, 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5)) %>% # turn x to factors
  group_by(x) %>% #Drop missing value
  summarize(n = n())

## DONT Drop the missing value ####
tibble (x = sample(1:4, 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5)) %>% # turn x to factors
  group_by(x, .drop = FALSE) %>% # Dont drop the missing value
  summarize(n = n())

# Count multiple missing variables ####
## Drop the missing value ####
tibble (x = sample(1:4, 100, replace = TRUE),
        y = sample(LETTERS[1:4], 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5),
         y = factor(y, levels = c("A", "B", "C", "D","Z"))) %>% 
  count(x,y) # Drop missing value

## DONT Drop the missing value ####
tibble (x = sample(1:4, 100, replace = TRUE),
        y = sample(LETTERS[1:4], 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5),
         y = factor(y, levels = c("A", "B", "C", "D","Z"))) %>% 
  count(x,y, .drop = FALSE) # DONT Drop missing value

## DONT Drop the missing value ####
tibble (x = sample(1:4, 100, replace = TRUE),
        y = sample(LETTERS[1:4], 100, replace = TRUE)) %>% 
  mutate(x = factor(x, levels=1:5),
         y = factor(y, levels = c("A", "B", "C", "D","Z"))) %>% 
  group_by(x, y, .drop =FALSE) %>% 
  summarize(n = n())
