airtemps <- c(212, 30.3, 78, 32)
celsius1 <- (airtemps[1]-32)*5/9
celsius2 <- (airtemps[2]-32)*5/9
celsius3 <- (airtemps[3]-32)*5/9

#this converts from faranheit to  celsius



#'Convert temp from faranheit to celsius
#'
#' @param fahr temperature in farenheit
#' @author counihan
#' @keywords temperature conversion
#'
#' @return the temperature in celsius
#' @export
#'
#' @examples
fahr_to_celsius <- function(fahr) {
  celsius <- (fahr-32)*5/9
  return(celsius)
}

celsius4 <- fahr_to_celsius(airtemps[1])
celsius4
celsius1 == celsius4

celsius_to_fahr <- function(cels) {
  faranheit <- (cels*9/5)+32 
}

fahr1 <- celsius_to_fahr(celsius1)
fahr1

convert_temps <- function(fahr){
  celsius <- (fahr-32)*5/9
  kelvin<-celsius + 273.15
  list("fahr"=fahr,"celsius"=celsius,"kelvin"=kelvin)
}

temp_series <- convert_temps(seq(-100, 100, 10))

temps_df <- data.frame(convert_temps(seq(-100,100,10)))
#or library(magrittr)
#seq(-100, 100, 10) %>% convert_temps() %>% data.frame(temps_df)


library(ggplot2)

custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = 'Helvetica', color = 'gray30', size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = 'bold'),
    panel.background = ggplot2::element_blank(),
    legend.position  = 'right',
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', size = .25),
    legend.key       = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_blank()
  )
}

ggplot(temps_df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
  geom_point() +
  custom_theme(10)








