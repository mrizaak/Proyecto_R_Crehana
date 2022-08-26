# Isaac Fernandez
# Proyecto Final

#Parte 1: Limpieza de datos

## Instalar packages
install.packages("tidyverse")
library(tidyverse)

## Instlar base de datos
data("mtcars")

## ¿Cuál es el auto cuyo consumo en Km por lt es menor?
summary(mtcars)
mtcars <- mtcars %>% mutate(kpl = (mpg*0.425144))
kml_mtcars <- mtcars %>% select(c(mpg, kpl)) %>% filter(mpg >= 33.90)

## ¿Cuál es el peso en kilogramos del auto menos pesado?
mtcars <- mtcars %>% mutate(kg = (wt*0.4536*1000))
kg_mtcars <- mtcars %>% select(c(wt, kg)) %>% filter(wt <= 1.513)

## Un auto de 6 <= Cyl
cyl_mtcars <- mtcars %>% select(c(cyl)) %>% filter(cyl <= 6) %>% arrange(cyl)

# Obtén la mediana y los deciles de consumo de gasolina

median(mtcars$kpl)

quantile(mtcars$kpl, probs = seq(0,1,0.1))

# Conviene comprar transmisión atomática o manual a partir del kpl

mtcars <- mtcars %>% mutate(transmision = if_else(am == 0, "Automático", "Manual"))

mtcars %>% group_by(transmision) %>% summarize(mean(kpl))

# Auto de bajo peso

mtcars <- mtcars %>% mutate(consumo = if_else(kpl < median(kpl), "Alto", "Bajo"),
                            peso = if_else(kg < median(kg), "Bajo", "Alto"))

table(mtcars$consumo, mtcars$transmision)

# Parte 3

# Box plot

ggplot(mtcars, aes(x = transmision, y = kpl, fill = transmision)) + geom_boxplot() +
  stat_summary(fun = mean, geom = "point") + 
    labs(title = "Consumo Promedio por Litro", subtitle = "Por Transmision") +
      xlab("Transmision") + ylab("KM/L") + 
        scale_fill_discrete(name = "Transmision")
          ggsave("Box Plot.jpg")

# Diagrama de dispersion

ggplot(mtcars, aes(x = kpl, y = kg)) + geom_point() +
  labs(title = "Consumo y peso", subtitle = "Dispersion") +
    xlab("KM/L") + ylab("KG")
      ggsave("Consumo Peso.jpg")
