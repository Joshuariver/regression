rm(list=ls())

x <- seq(0, 4*pi, length.out = 201)
y <- sin(x) + cos(x + x)
plot(y)



library("gramEvol")

ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos),
                op = grule('+', '-', '*'),
                var = grule(x))

grammarDef <- CreateGrammar(ruleDef)
grammarDef
## <expr> ::= <op>(<expr>, <expr>) | <func>(<expr>) | <var>
## <func> ::= `sin` | `cos`
## <op>   ::= "+" | "-" | "*"
## <var>  ::= x


set.seed(123)
GrammarRandomExpression(grammarDef, 6)


SymRegFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (mean(log(1 + abs(y - result))))
}  

set.seed(314)
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 2500, max.depth = 5)
ge
## Grammatical Evolution Search Results:
##   No. Generations:  2149 
##   Best Expression:  sin(x) + cos(x + x) 
##   Best Cost:        0

plot(y)
points(eval(ge$best$expressions), col = "red", type = "l")


x <- seq(0, 4*pi, length.out = 201)
y <- jitter(sin(x) + cos(x + x), amount = 0.2)
plot(y)


ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos),
                op = grule('+', '-', '*'),
                var = grule(x))

grammarDef <- CreateGrammar(ruleDef)
grammarDef
## <expr> ::= <op>(<expr>, <expr>) | <func>(<expr>) | <var>
## <func> ::= `sin` | `cos`
## <op>   ::= "+" | "-" | "*"
## <var>  ::= x

SymRegFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (mean(log(1 + abs(y - result))))
}

set.seed(314)
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.1, iterations = 2500, max.depth = 5)
ge
## Grammatical Evolution Search Results:
##   No. Generations:  2149 
##   Best Expression:  sin(x) + cos(x + x) 
##   Best Cost:        0.0923240003917875

plot(y)
points(eval(ge$best$expressions), col = "red", type = "l")
</var></op></func></var></expr></func></expr></expr></op></expr>

  
  planets <- c("Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus")
distance <- c(0.72, 1.00, 1.52, 5.20, 9.53, 19.10)
period <- c(0.61, 1.00, 1.84, 11.90, 29.40, 83.50)
data.frame(planets, distance, period)
##   planets distance period
## 1   Venus     0.72   0.61
## 2   Earth     1.00   1.00
## 3    Mars     1.52   1.84
## 4 Jupiter     5.20  11.90
## 5  Saturn     9.53  29.40
## 6  Uranus    19.10  83.50  


ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos, tan, log, sqrt),
                op = grule('+', '-', '*', '/', '^'),
                var = grule(distance, n),
                n = grule(1, 2, 3, 4, 5, 6, 7, 8, 9))

grammarDef <- CreateGrammar(ruleDef)
grammarDef
## <expr> ::= <op>(<expr>, <expr>) | <func>(<expr>) | <var>
## <func> ::= `sin` | `cos` | `tan` | `log` | `sqrt`
## <op>   ::= "+" | "-" | "*" | "/" | "^"
## <var>  ::= distance | <n>
## <n>    ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

SymRegFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (mean(log(1 + abs(period - result))))
}

set.seed(2)
suppressWarnings(ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.05))
ge
## Grammatical Evolution Search Results:
##   No. Generations:  42 
##   Best Expression:  sqrt(distance) * distance 
##   Best Cost:        0.0201895728693589
</n></n></var></op></func></var></expr></func></expr></expr></op></expr>