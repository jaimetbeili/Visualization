library(tidyverse)
library(dslabs)
data(heights)



# La funcion de summarize pone los datos en una tabla con las columnas y los
# titulos de columnas que yo quiera.
s <- heights %>%
  filter(sex == "Male") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

s

s$average
s$standard_deviation

heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))

quantile(heights$height, c(0, 0.5, 1))

data(murders)

# Lo siguiente no es el promedio de asesinatos en USA porque presenta nada mas
# un promedio de los promedios de cada estado.
murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

# El promedio de asesinatos en USA se calcula con la suma del total de
# asesinatos entre la suma de la poblacion total.
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

us_murder_rate %>% .$rate

us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  .$rate

# La funcion group_by() agrupa un objeto en diferentes conjuntos.
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))

# Lo siguiente nos devuelve los 6 estados de menor poblacion ordenados.
murders %>% arrange(population) %>% head()

# Lo siguiente nos devuelve los 6 estados con mas asesinatos pc ordenados.
murders %>% arrange(desc(murder_rate)) %>% head()

# Esto ordena por region y luego por asesinatos pc.
murders %>% arrange(region, murder_rate) %>% head()

# Funcion para sacar los top n estados con mas asesinatos
# (en este caso top 10).
murders %>% top_n(10, murder_rate)

# Mismo caso, pero ordenados y con el top 15.
murders %>% arrange(desc(murder_rate)) %>% top_n(15)
