library(tidyverse)
library(dslabs)
data(murders)

p <- ggplot(data = murders)
print(p)
p

# Para hacer una grafica se van anadiendo capas:
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# Se pueden anadir capas a un objeto ya definido "p".
p + geom_point(aes(x = population/10^6, y = total))

# Esta capa anade etiquetas a los puntos.
p + geom_point(aes(population/10^6, total)) +
  geom_label(aes(population/10^6, total, label = abb))

# Esta capa pone las etiquetas como texto y no rectangulos.
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

# Lo mismo, pero los mueve un poco.
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# Si definimos el aes desde antes (en la funcion de ggplot), ya no tenemos
# que repetirlo en cada capa.
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# Y si necesitamos un aes difernte para algo lo podemos agregar.
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

# Estas funciones cambian la escala de x y y a logaritmicas.
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# Lo mismo, pero mas sencilla.
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

# Las primeras dos le asignan nombres a los ejes. La tercera un titulo
# a la grafica.
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in Millions (log scale)") +
  ylab("Total Number of Murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# El color se anade dentro de la capa "geom_point". Para explicarlo vamos 
# a crear una grafica que sea todo lo que ya llevamos menos esa capa.
p <- murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in Millions (log scale)") +
  ylab("Total Number of Murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# Esto le da color a todos los puntos por igual.
p + geom_point(size = 3, color = "blue")

# Esto cambia los colores segun alguna categoria.
p + geom_point(aes(col = region), size = 3)

# La capa geom_abline es para anadir una linea de tendencia. Para ponerla
# primero calculamos el intercepto de donde va a partir asi:
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# Luego lo ponemos en la grafica. abline significa una linea de intercepto
# a con una pendiente b. La pend. predeterminada es 1, el int. es 0.
p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))

# Poner la linea antes de los puntos la pasa al fondo para que no quede
# encima de los puntos. lty = 2 la hace punteada y pues color se explica solo.
p +  geom_abline(intercept = log10(r), lty = 2, color = "darkgray") +
  geom_point(aes(col = region), size = 3)

# Para cambiar a mayuscula el titulo en la leyenda y guardar todo en p.
p <- p +  geom_abline(intercept = log10(r), lty = 2, color = "darkgray") +
  geom_point(aes(col = region), size = 3) + 
  scale_color_discrete(name = "Region")

library(ggthemes)

# El paquete ggthemes te permite cargar distintos estilos. Por ejemplo:
p + theme_economist()
p + theme_economist_white()
p + theme_clean()
p + theme_excel_new()
p + theme_fivethirtyeight()
p + theme_stata()
p + theme_wsj()
p + ds_theme_set() # Este no es de ggthemes, es de dslabs.

# Este paquete lo vamos a usar en la grafica para que no se encimen
# etiquetas. Lo vamos a meter ya directo en la grafica. Replaza una
# capa que ya teniamos.
library(ggrepel)

# El proceso que tendriamos que hacer desde cero:
# Primero sacar los paquetes y datos.
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# Luego preparar los valores para la linea. Notese el cambio.
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>% .$rate

# Finalmente, la grafica.
The_Economist <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel(aes(col = region)) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in Millions (log scale)") +
  ylab("Total Number of Murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

Wall_Street_Journal <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in Millions (log scale)") +
  ylab("Total Number of Murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_wsj()

The_Economist
Wall_Street_Journal

data(heights)
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

p + geom_histogram(binwidth = 1, fill = "lightgoldenrodyellow", col = "wheat") +
  ggtitle("Male Heights Histogram") +
  xlab("Male Heights in Inches") +
  theme_economist()

p + geom_density(fill = "green3") +
  ggtitle("Male Heights Histogram") +
  xlab("Male Heights in Inches")

p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(sample = height))

p + geom_qq(col = "paleturquoise4")

params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))

p + geom_qq(dparams = params, col = "salmon") +
  geom_abline(col = "mediumspringgreen", size = 1)

heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq(col = "peru") +
  geom_abline(col = "maroon1", size = 2)
  
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "burlywood4", col = "chocolate4") +
  ggtitle("Histogram with Binwidth of 1") +
  xlab("Male Heights in Inches") +
  ylab("Count") +
  theme_economist()
p2 <- p + geom_histogram(binwidth = 2, fill = "blueviolet", col = "blue4") +
  ggtitle("Histogram with Binwidth of 2") +
  xlab("Male Heights in Inches") +
  ylab("Count") +
  theme_economist()
p3 <- p + geom_histogram(binwidth = 3, fill = "darkolivegreen1", col = "darksalmon") +
  ggtitle("Histogram with Binwidth of 3") +
  xlab("Male Heights in Inches") +
  ylab("Count") +
  theme_economist()
p4 <- p + geom_histogram(binwidth = 4, fill = "#6e00ff", col = "hotpink4") +
  ggtitle("Histogram with Binwidth of 4") +
  xlab("Male Heights in Inches") +
  ylab("Count") +
  theme_economist()

library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

heights %>% 
  ggplot(aes(height, fill = sex)) + geom_density(alpha = .2) + xlab("Height") + ylab("Density") + ggtitle("Male & Female Density Plots")

grid.arrange(Wall_Street_Journal,The_Economist)
