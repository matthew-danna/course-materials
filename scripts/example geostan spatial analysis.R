# Spatial analysis with geostan
# https://r-spatial.org//r/2024/08/02/geostan-introduction.html

install.packages("geostan")
library(geostan)
library(sf)

data(world, package = "spData")

# We are going to apply the Robinson map projection for the countries:
world <- st_transform(world, crs = 'ESRI:54030')

# At least a couple of the missing values can be filled in using World Bank data, so we will do that:
## https://data.worldbank.org

france <- grep("France", world$name_long)
world$gdpPercap[ france ] <- 43068
world$lifeExp[ france ] <- 82

norway <- grep("Norway", world$name_long)
world$gdpPercap[ norway ] <- 97666
world$lifeExp[ norway ] <- 82.1

# And we will also remove Antarctica:
world <- subset(world, name_long != "Antarctica")

# Mapping the variables shows the familiar geography of high-, middle-, and low-income countries 
# and a similar geography of longevity:
  
# store geometry for countries
world_geom <- st_geometry(world)

# show two maps at once, with nice font
ogpar <- par(mfrow = c(2, 1),
             mar = rep(0, 4))

# GDP per capita
pars <- map_pars(world$gdpPercap / 1e3)
plot(world_geom,
     col = pars$col,
     lwd = .2)
legend("bottomleft",
       fill = pars$pal,
       title = 'GDP per capita\n($1,000s)',
       legend = pars$lbls,
       bty = 'n'
)
rm(pars)

# life expectancy
pars <- map_pars(world$lifeExp)
plot(world_geom,
     col = pars$col,
     lwd = .2)
legend("left",
       fill = pars$pal,
       title = 'Life Expectancy',
       legend = pars$lbls,
       bty = 'n'
)

# Adjacency matrix
## remove missing values
world <- subset(world, !is.na(gdpPercap) & !is.na(lifeExp))

## leaving 162 observations
nrow(world)

A <- shape2mat(world, "B", method = "rook")

# edges with geometry
E <- edges(A, shape = world) 
graph <- st_geometry(E)

ogpar <- par(mar = rep(0, 4))
# plot countries
plot(world_geom, lwd = .1)
# add graph nodes
plot(graph, add = TRUE, type = 'p')
# add graph edges
plot(graph, add = TRUE, type = 'l')

par(ogpar)




