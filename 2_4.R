library(dplyr)
library(ggplot2)
library(dslabs)
library(ggthemes)
data("gapminder")
head(gapminder)

# Para saber si Sri Lanka tiene mayor mortandad infantil que Turquia en 2015:
gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

# Queremos saber que tanto es cierto que el mundo se divide en paises ricos
# y pobres. Podemos usar la siguiente grafica, que si muestra dos
# grupos principales.
ds_theme_set()
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point(col = "red")

# Aqui con colores por continente.
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

# Para comparar podemos usar la funcion facet_grid(), que crea una
# tabla de graficas. Antes del ~ va la dimension fila, despues la columna.
filter(gapminder, year %in% c(1962,2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(continent~year)

# Para comparar solo una dimension, le ponemos . a la otra.
filter(gapminder, year %in% c(1962,2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(.~year)

# Cuando queremos poner muchas graficas sirve usar facet_wrap en lugar de grid.
years <- c(1962, 1980, 1987, 1990, 2000, 2012)
continents <- c("Asia", "Americas")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

# Grafica de fertilidad en Mexico:
gapminder %>% filter(country == "Mexico") %>% 
  ggplot(aes(year,fertility)) +
  geom_line(col = "darkgreen")

# Grafica de fertilidad en Alemania y Corea. Si no se anade la funcion group,
# se dibuja una sola linea que cubre ambos puntos.
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, group = country)) +
  geom_line()

# Tambien se puede diferenciar con colores:
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility, group = country, col = country)) +
  geom_line() + 
  geom_point()

# Para anadir etiquetas, se tiene que crear primero el data frame con sus
# nombres y coordenadas. Ademas, hay que pedir que no se agregue una leyenda,
# pues para eso son las etiquetas.
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

# Anadir una variable de dolares por dia a nuestra base de datos
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365) 

# Histograma de ingresos diarios en 1970 (dejando fuera a los paises para
# los que no se presento dato de PIB en 1970)
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "red", fill = "orange")

# Aqui queremos tratar de hacer el histograma mas facil de interpretar. Lo
# ponemos a escala logaritmica base 2 (2,4,8,16,32...)
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray")

# Pero si hacemos esto con otra capa en la grafica (hasta abajo) queda mejor
# porque podemos ver los valores en el eje de las x como dolares por dia.
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "blue", fill = "lightblue") +
  scale_x_continuous(trans = "log2")

#Curiosidad
gapminder %>%
  filter(year == "2010") %>%
  arrange(desc(dollars_per_day)) %>%
  select(country, dollars_per_day) %>% top_n(10)

# Con esto podemos hacer una grafica para ver como se ven las diferentes
# regiones y continentes. Es mas util que el histograma.
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# La ultima linea reacomoda el nombre de las regiones hasta abajo.
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Para que sea mas facil visualizar la grafica, la podemos ordenar segun el 
# ingreso medio, en lugar de por orden alfabetico. Aqui se muestra un ejemplo
# de como usar la funcion que hace eso (reorder).
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)

# Ahora podemos ordenar las regiones en la grafica. Esta ultima version tambien
# tiene la escala transformada en log2 para que sea mas facil de leer, tiene
# los continentes presentados en diferentes colores, tiene los puntos donde esta
# cada pais y etiquetas en los ejes y el titulo.
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) +
  xlab("Region") +
  ylab("Dollars Per Day") +
  ggtitle("Income by Region in 1970")
p

# Podemos analizar el histograma en el que vaiamos dos puntos altos si lo separamos.
# Primero hay que crear un vector de los que son west.
west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America", "Australia and New Zealand")

# A la hora de graficar, mutamos el vector para que se divida en dos grupos,
# los que si estan incluidos (west) y los que no (developing).
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black", fill = "indianred2") +
  scale_x_continuous(trans = "log2") +
  facet_grid(.~group)

# Ahora podemos comparar entre occidente y el resto en 1970 y en 2010.
present_year <- 2010
gapminder %>% filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "darkblue", fill = "cyan2") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

# Como no todos los paises de 2010 estaban en 1970, tenemos que acortar la lista.
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# Ahora si, esta comparacion es completa. No hay necesidad de especificar en la 
# grafica el !is.na(gdp) porque en la lista de paises que acabamos de hacer ya
# pusimos esa condicion.
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "darkblue", fill = "cyan2") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

# Boxplot comparativa con 2010, 2 graficas.
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, col = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) +
  xlab("Region") +
  ylab("Dollars Per Day") +
  ggtitle("Income by Region 1970 vs 2010") +
  facet_grid(.~year)
p


# Boxplot comparativa con 2010, 1 grafica. El ano se tiene que convertir
# en factor porque R le asigna colores diferentes a los factores.
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = factor(year))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("Region") +
  ylab("Dollars Per Day") +
  ggtitle("Income by Region 1970 vs 2010")
p

# Estas dos curvas de densidad muestran que los paises pobres si se han
# enriquecido, pero muestra todo el mundo reunido.
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day), fill = "black") +
  scale_x_continuous(trans = "log2")
p + geom_density(fill = "grey") + facet_grid(year ~ .)

# Hay 21 paises en occidente y 87 en el resto del mundo. Son grupos desiguales.
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

# Para separar los grupos anadimos esta insturccion dentro de la grafica, que
# cambia el eje y de densidad a cuenta de paises.
aes(x = dollars_per_day, y = ..count..)

# Aqui la grafica con colores cambiados y transparentes (alpha) y ya con numero
# de paises en lugar de densidad.
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.35, bw = .75) + facet_grid(year ~ .)
# Se ve claramente que hay mas paises en desarrollo, pero han avanzado mas.

# Para verlo por region dividimos las regiones asi:
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~
      "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~
      "Sub-Saharan Africa",
    TRUE ~ "Others"))

# Las reordenamos.
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America",
                                          "East Asia", "Sub-Saharan Africa",
                                          "West")))

# Aqui e ve como han avanzado las regiones una por una.
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = .75, position = "stack") +
  facet_grid(year ~ .)
p

# Y aqui se ve su proporcion de la poblacion mundial.
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)
p


# add additional regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# Estos dos valores los graficamos. Los breaks son los puntos que aparecen
# en los ejes. Los limites son el maximo y el minimo de la grafica. La 
# transformacion le da exponencialidad a los incrementos (logit = p/100-p)
# Ejemplo: 99.9/.1 = 999 > 99/1 = 99
surv_income %>% 
  ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limits = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limits = c(.875, .9981),
                     breaks = c(.85, .90, .95, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE)