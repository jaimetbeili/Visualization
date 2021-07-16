#Checar:
#http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements

#PRINCIPIOS BASICOS PARA PRESENTAR DATOS:

#La posicion y longitud son preferidos a los angulos y las areas para mostrar
#datos. Esto quiere decir que una grafica con barras es mas clara y util que
#una grafica de pastel. Y a su vez, la de pastel es mejor que la de dona.
#Posicion y longitud > angulos > areas.

#El cero es un valor importante a incluir porque no tenerlo puede distorcionar
#el resto de la grafica, sobre todo visto desde el punto de longitud.
#Cuando analizamos posicion, el cero es menos importante.

#Es importante ver que nuestras graficas no distorsionen nuestras cantidades,
#y que sean interpretables de acuerdo a la realidad. Tambien hay que ordenar
#nuestros datos de manera congruente.

library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
data("heights")

#Estas dos graficas muestran lo mismo, pero la segunda presenta los puntos con
#una desviacion aleatoria del centro para poder verlos todos.
#La funcion alpha oscurece los puntos cuando son muchos. y los transparenta
#cuando son pocos. Esto atiende al principio de mostrar todos los datos
#que sean posibles.
heights %>% ggplot(aes(sex,height, col = sex)) + geom_point() +
  labs(tag = "1.1)")
heights %>% ggplot(aes(sex,height, col = sex)) + geom_jitter(width = 0.1, alpha = 0.2) +
  labs(tag = "1.2)")

#Histograma por densidad (NO POR CUENTA) de distribucion de alturas.
#Para comparar dos graficas, es importante que tengan los mismos ejes.
#Si queremos comparar cambios horizontales acomodamos las graficas una sobre
#la otra. Para cambios verticales, una junto a la otra. Este caso es un cambio
#horizontal, pues mientras mas a la derecha estan los datos, mas altos son
#los individuos medidos.
heights %>% ggplot() +
  geom_histogram(aes(height, stat(density)), binwidth = 1, col = "black") + 
  labs(tag = "2)")+
  facet_grid(sex~.)

#Esta grafica nos muestra la misma informacion que el histograma, pero ademas
#podemos ver todos los puntos con jitter. Ambas graficas son mas utiles para
#nuestro proposito que una barra que solo muestra un numero.
heights %>% ggplot(aes(sex,height, col = sex)) + 
  geom_boxplot(width = .5) +
  geom_jitter(width = 0.1, alpha = 0.75) +
  labs(tag = "3.1)")

#p + theme(panel.background = element_rect(fill, color, size, linetype))
#Para modificar el fondo. El primer argumento es el relleno, los demas
#corresponden al contorno.
#labs reune ggtitle, xlab y ylab en una funcion. Ademas le puedo anadir un tag
#hasta arriba (tipo para numerar, que lo hice en todo lo anterior aqui).
heights %>% ggplot(aes(sex,height, col = sex), fill = "white") + 
  geom_boxplot(width = .5) +
  geom_jitter(width = 0.1, alpha = 0.75) +
  theme(panel.background = element_rect(fill = "mediumseagreen",
                              color = "black", size = 3, linetype = 3)) +
  labs(title = "Boxplot of Students Hieghts", subtitle = "With Jitter", x = "Sex", y = "Height", tag = "3.2)")

#En ocasiones tenemos que transofrmar la escala (como a logaritmos).

#Los colores predeterminados se pueden cambiar de manera manual, por ejemplo asi:
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + labs(tag = "4.1)")
p1 + scale_color_manual(values = color_blind_friendly_cols) + labs(tag = "4.2)")

#Tambien se pueden cambiar de manera automatica usando los de otro tema:
p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_economist() + labs(tag = "4.3)")
p1 + scale_color_wsj() + labs(tag = "4.4)")
p1 + scale_color_colorblind() + labs(tag = "4.5)")

#Grafica de lineas para ver los cambios.
data("gapminder")

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid = element_line(color = "lightgray"))

#Grafica de puntos que muestra el promedio de 2010 y 2015 y la diferencia entre
#esos anos.
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy, region) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country, shape = region), color = "navyblue") +
  geom_point(size = 2, color = "darkgray") +
  geom_text_repel(color = "navyblue") +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010") +
  labs(shape = "Region") +
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "lightblue"),
        axis.title = element_text(color = "red"),
        axis.text = element_text(color = "red"),
        legend.background = element_rect(fill = "lightblue"),
        legend.key = element_rect(color = "lightblue"))

data("us_contagious_diseases")
str(us_contagious_diseases)

the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate))+
  geom_text(aes(x = 1973, y = 50, label = "Vaccines Introduced"), color = "white") +
  geom_line() +
  xlab("Year") +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "navyblue") +
  theme(rect = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue"))

library(RColorBrewer)
display.brewer.all(type = "seq")
display.brewer.all(type = "div")

dat %>% mutate(state = reorder(state,rate)) %>%
  ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "gray50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("") +
  theme(panel.background = element_rect(fill = "white"))

avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)


dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")
