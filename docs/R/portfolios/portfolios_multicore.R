#### *ПАКЕТЫ и прочее* ####
#install.packages("beepr", "parallel", "PortfolioAnalytics", "PerformanceAnalytics", "zoo", "ggplot2", "cowplot", "gridExtra")
library("parallel")
library("beepr")
library("PortfolioAnalytics")
library("PerformanceAnalytics")
library("zoo")
library("ggplot2")
library("cowplot")
library("gridExtra")
#### ... функция мультиплот <3 ####

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### *ИМПОРТ* ####
returns <- read.csv2(file = "returns.csv",
                        header = TRUE, stringsAsFactors = FALSE , na.strings = "NA",
                        strip.white = TRUE)   # собственно импорт

names(returns) <- tolower(names(returns))     # имена столбцов в маленькие буквы
date.vec <- as.Date(returns$date, "%d/%m/%Y") # дату в формат даты и в отдельный вектор
for (i in 2:length(names(returns)))           # всё остальное в формат чисел на всякий случай
{
  returns[ ,i] <- as.numeric(returns[ ,i])
}

returns$date <- NULL                          # удалим даты из датафрейма

zoo <- zoo(returns, date.vec)                 # превращаем датафрейм в zoo
#head(zoo)




#### ...Cоздадим горизонты ####
# 1 год
horizon1 <- zoo[index(zoo)>"2015-01-01"]

# 3 года
horizon3 <- zoo[index(zoo)>"2013-01-01"]

# 5 лет
horizon5 <- zoo[index(zoo)>"2011-01-01"]

# 7 лет
horizon7 <- zoo[index(zoo)>"2009-01-01"]

# 9 лет
horizon9 <- zoo[index(zoo)>"2007-01-01"]

# 10лет/все года
horizon10 <- zoo[index(zoo)>"2006-01-01"]



#### *РАСЧЁТЫ* ####
#### ...Cупер функция которая считает ####
portfolio.start <- function(x, data, number){
  meanReturns <- colMeans(data) # средние 
  covMat <- cov(data)           # ковариационная матрица
  port <- PortfolioAnalytics::portfolio.spec(assets = names(data)) # создаётся портфель
  port <- PortfolioAnalytics::add.constraint(port, type = "box", min = 0.01, max = 1.01) # ограничения по весам
  port <- PortfolioAnalytics::add.constraint(portfolio = port, type = "full_investment") # ограничения по средствам
  # путём перебора создаются наборы весов 
  rportfolios <- PortfolioAnalytics::random_portfolios(portfolio = port, permutations = number, rp_method = "sample")
  # считаются риски
   sd <- apply(rportfolios, 1, function(x){
    return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
  })
  # считаются доходности
  means <- apply(rportfolios, 1, function(x){
    return(x %*% meanReturns) # c
  })
  # Sharpe Ratio, вдруг понадобится
  sr <- means / sd
  # создаётся дата фрейм на выдачу
  a.data <- data.frame(means)
  a.data["sd"] <- sd
  a.data["sr"] <- sr
  return(a.data)
}

#### ...немножко параллельности для скорости ####
iterations = 500000 # количество итераций, далее время (обратить внимание что функция вызывается 6 раз)
#  время на Intel Core i7-4770HQ
#  6*100    -     7 сек ;fork - 7
#  6*1000   -    46 cек ;fork - 81
#  6*2000   -    91 сек ;
#  6*10000  -   456 cек ;
#  6*500000 - 38506 сек ;
# VVVVVVVVVVVVVVVVVVVV всё что отмечено выделить и запустить, как досчитает, проорёт
  beg = Sys.time()                                                                  # отмечено
  M <- detectCores()                                                                # отмечено
  cl <- makeCluster(M, outfile = "install_log")                                     # отмечено
  hor1 <- clusterCall(cl, portfolio.start, data = horizon1, number = iterations )   # отмечено
  hor3 <- clusterCall(cl, portfolio.start, data = horizon3, number = iterations )   # отмечено
  hor5 <- clusterCall(cl, portfolio.start, data = horizon5, number = iterations )   # отмечено
  hor7 <- clusterCall(cl, portfolio.start, data = horizon7, number = iterations )   # отмечено
  hor9 <- clusterCall(cl, portfolio.start, data = horizon9, number = iterations )   # отмечено
  hor10 <- clusterCall(cl, portfolio.start, data = horizon10, number = iterations ) # отмечено
  stopCluster(cl)                                                                   # отмечено
  td = as.numeric(Sys.time() - beg, "secs")                                         # отмечено
  beep(9)                                                                           # отмечено
  td                                                                                # отмечено
# 

# тут сохраним, что насчитали кластеры в требуемую форму <3
hor1 <- hor1[[1]]
hor3 <- hor3[[1]]
hor5 <- hor5[[1]]
hor7 <- hor7[[1]]
hor9 <- hor9[[1]]
hor10 <- hor10[[1]]

#### *СОХРАНИМ ДАННЫЕ* ####
savedata <- function(){ # cохранит дата фреймы по которым строилось
  write.csv2(hor1, file = "hor1_38000.csv")
  write.csv2(hor3, file = "hor3_38000.csv")
  write.csv2(hor5, file = "hor5_38000.csv")
  write.csv2(hor7, file = "hor7_38000.csv")
  write.csv2(hor9, file = "hor9_38000.csv")
  write.csv2(hor10, file = "hor10_38000.csv")
}
savedata()

saveplots <- function(){ # сохранит все графики в файлы, путь указывать в path VVVVVVVVVVVV
  ggplot2::ggsave("g1.png", plot = g1, path = "/", dpi = 1200)
  ggplot2::ggsave("g3.png", plot = g3, path = "/", dpi = 1200)
  ggplot2::ggsave("g5.png", plot = g5, path = "/", dpi = 1200)
  ggplot2::ggsave("g7.png", plot = g7, path = "/", dpi = 1200)
  ggplot2::ggsave("g9.png", plot = g9, path = "/", dpi = 1200)
  ggplot2::ggsave("g10.png", plot = g10, path = "/", dpi = 1200)
}
saveplots()
#### *ГРАФИКИ* ####
#### ...Создание графиков ####

serialplot <- function(x, color, xscale, yscale, path){ 
  # функция строит и сразу сохраняет графики в path, 
  # потом строит графики на сетке 2х3 и возвращает этот график
  ### ПАРАМЕТРЫ ###
  # color: или sr, или NULL (тогда будут чёрного цвета)
  # xscale/yscale: ограничение по осям, например c(0,8)
  # path: путь куда сохранятся графики
  
#...1 годичный горизонт  
g1 <- ggplot(data = hor1, aes_string(x = "sd", y = "means", color = color)) + 
  geom_point(size = 0.5, alpha = 0.5, shape = 21) +
  scale_colour_gradient2(low = "#430355",
                         mid = "#21998A",
                         high = "magenta",
                         midpoint = 0.21) +
  scale_x_continuous(limits = xscale) +
  scale_y_continuous(limits = yscale) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Горизонт инвестирования - 1 год") + 
  labs(color="Sharpe\nRatio") +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 2))

g1
#...3 годичный горизонт 
g3 <- ggplot(data = hor3, aes_string(x = "sd", y = "means", color = color)) + 
  geom_point(size = 0.5, alpha = 0.5, shape = 21) +
  scale_x_continuous(limits = xscale) +
  scale_y_continuous(limits = yscale) +
  scale_colour_gradient2(low = "magenta",
                         mid = "steelblue",
                         high = "salmon",
                         midpoint = 0.21) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Горизонт инвестирования - 3 года") + 
  labs(color="Sharpe\nRatio") +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 2))


#...5 годичный горизонт 
g5 <- ggplot(data = hor5, aes_string(x = "sd", y = "means", color = color)) + 
  geom_point(size = 0.5, alpha = 0.5, shape = 21) +
  scale_x_continuous(limits = xscale) +
  scale_y_continuous(limits = yscale) +
  scale_colour_gradient2(low = "magenta",
                         mid = "steelblue",
                         high = "salmon",
                         midpoint = 0.21) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Горизонт инвестирования - 5 лет") + 
  labs(color="Sharpe\nRatio") +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 2))


#...7 годичный горизонт 
g7 <- ggplot(data = hor7, aes_string(x = "sd", y = "means", color = color)) + 
  geom_point(size = 0.5, alpha = 0.5, shape = 21) +
  scale_x_continuous(limits = xscale) +
  scale_y_continuous(limits = yscale) +
  scale_colour_gradient2(low = "magenta",
                         mid = "steelblue",
                         high = "salmon",
                         midpoint = 0.21) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Горизонт инвестирования - 7 лет") + 
  labs(color="Sharpe\nRatio") +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 2))


#...9 годичный горизонт 
g9 <- ggplot(data = hor9, aes_string(x = "sd", y = "means", color = color)) + 
  geom_point(size = 0.5, alpha = 0.5, shape = 21) +
  scale_x_continuous(limits = xscale) +
  scale_y_continuous(limits = yscale) +
  scale_colour_gradient2(low = "magenta",
                         mid = "steelblue",
                         high = "salmon",
                         midpoint = 0.21) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Горизонт инвестирования - 9 лет") +
  labs(color="Sharpe\nRatio") +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 2))


#...10 годичный горизонт 
g10 <- ggplot(data = hor10, aes_string(x = "sd", y = "means", color = color)) + 
  geom_point(size = 0.5, alpha = 0.5, shape = 21) +
  scale_x_continuous(limits = xscale) +
  scale_y_continuous(limits = yscale) +
  scale_colour_gradient2(low = "magenta",
                         mid = "steelblue",
                         high = "salmon",
                         midpoint = 0.21) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Горизонт инвестирования - 10 лет") + 
  labs(color="Sharpe\nRatio") +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.ticks = element_line(size = 2))
# пускай сразу сохраняет
ggplot2::ggsave("g1.png", plot = g1, path = path, dpi = 1200)
ggplot2::ggsave("g3.png", plot = g3, path = path, dpi = 1200)
ggplot2::ggsave("g5.png", plot = g5, path = path, dpi = 1200)
ggplot2::ggsave("g7.png", plot = g7, path = path, dpi = 1200)
ggplot2::ggsave("g9.png", plot = g9, path = path, dpi = 1200)
ggplot2::ggsave("g10.png", plot = g10, path = path, dpi = 1200)

# строит сетку из шести графиков
mult <- plot_grid(g1, g3, g5, g7, g9, g10, ncol = 2, nrow = 3)
return(mult) # возвращает графики на сетке 2*3
}

ppp <- serialplot(color = "sr", xscale = NULL, yscale = NULL, path ="/")
ppp



mult <- plot_grid(g1, g3, g5, g7, g9, g10, ncol = 2, nrow = 3) ### это чисто посмотреть, сохранение ниже
mult
#### функция перестроения графиков со сменой размеров шрифтов ####
mplot <- function(x, size, dot){ # чтоб быстренько поменять параметры графиков если что
  g1 <- ggplot(data = hor1, aes(x = sd, y = means)) + 
    geom_point(size = dot, alpha = 0.5, col = "steelblue", shape = 16) +
    scale_x_continuous(limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    xlab("") +
    ylab("Средняя Доходность") +
    ggtitle("Горизонт инвестирования - 1 год") + 
    theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = size, face = "bold"),
          axis.title.y = element_text(size = size),
          axis.title.x = element_text(size = size),
          axis.text = element_text(size = size),
          axis.ticks = element_line(size = 2))
  
  
  #### ...3 годичный горизонт ####
  g3 <- ggplot(data = hor3, aes(x = sd, y = means)) + 
    geom_point(size = dot, alpha = 0.5, col = "steelblue", shape = 16) +
    scale_x_continuous(limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    xlab("") +
    ylab("") +
    ggtitle("Горизонт инвестирования - 3 года") + 
    theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = size, face = "bold"),
          axis.title.y = element_text(size = size),
          axis.title.x = element_text(size = size),
          axis.text = element_text(size = size),
          axis.ticks = element_line(size = 2))
  
  
  #### ...5 годичный горизонт ####
  g5 <- ggplot(data = hor5, aes(x = sd, y = means)) + 
    geom_point(size = dot, alpha = 0.5, col = "steelblue", shape = 16) +
    scale_x_continuous(limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    xlab("") +
    ylab("Средняя Доходность") +
    ggtitle("Горизонт инвестирования - 5 лет") + 
    theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = size, face = "bold"),
          axis.title.y = element_text(size = size),
          axis.title.x = element_text(size = size),
          axis.text = element_text(size = size),
          axis.ticks = element_line(size = 2))
  
  
  #### ...7 годичный горизонт ####
  g7 <- ggplot(data = hor7, aes(x = sd, y = means)) + 
    geom_point(size = dot, alpha = 0.5, col = "steelblue", shape = 16) +
    scale_x_continuous(limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    xlab("") +
    ylab("") +
    ggtitle("Горизонт инвестирования - 7 лет") + 
    theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = size, face = "bold"),
          axis.title.y = element_text(size = size),
          axis.title.x = element_text(size = size),
          axis.text = element_text(size = size),
          axis.ticks = element_line(size = 2))
  
  
  #### ...9 годичный горизонт ####
  g9 <- ggplot(data = hor9, aes(x = sd, y = means)) + 
    geom_point(size = dot, alpha = 0.5, col = "steelblue", shape = 16) +
    scale_x_continuous(limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    xlab("Стандартное Отклонение") +
    ylab("Средняя Доходность") +
    ggtitle("Горизонт инвестирования - 9 лет") + 
    theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = size, face = "bold"),
          axis.title.y = element_text(size = size),
          axis.title.x = element_text(size = size),
          axis.text = element_text(size = size),
          axis.ticks = element_line(size = 2))
  
  
  #### ...10 годичный горизонт ####
  g10 <- ggplot(data = hor10, aes(x = sd, y = means)) + 
    geom_point(size = dot, alpha = 0.5, col = "steelblue", shape = 16) +
    scale_x_continuous(limits = c(0, 8)) +
    scale_y_continuous(limits = c(0, 4.5)) +
    xlab("Стандартное Отклонение") +
    ylab("") +
    ggtitle("Горизонт инвестирования - 10 лет") + 
    theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = size, face = "bold"),
          axis.title.y = element_text(size = size),
          axis.title.x = element_text(size = size),
          axis.text = element_text(size = size),
          axis.ticks = element_line(size = 2))
  mult <- plot_grid(g1, g3, g5, g7, g9, g10, ncol = 2, nrow = 3)
  return(mult)
}

finplot <- mplot(size = 40, dot = 1) # size = 40 был подобран для сохранения через export -> png 2880, keepaspectratio
finplot









#### .... пример ПРОСТОЙ ГГПЛОТ для экспорта ####
ug <- ggplot(data = hor1, aes(x = sd, y = means, col = sr)) + 
  geom_point(size = 0.1, alpha = 0.5, shape = 16) +
 # geom_point(aes(x = a.data$a.means, y = a.data$a.sd),size = 10, alpha = 0.5, col = "red", shape = 16) +
  xlab("Стандартное Отклонение") +
  ylab("Средняя Доходность") +
  ggtitle("Random Portfolios") + 
  #labs(color="Sharpe\nRatio") +
  scale_colour_gradient2(low = "magenta",
                         mid = "steelblue",
                         high = "salmon",
                         midpoint = 0.21) +
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.ticks = element_line(size = 2))
ug


#### .... примерГГПЛОТ ####

library(ggplot2)
g <- ggplot(data = data, aes(x = z.sd, y = z.means)) + 
  geom_point(size = 1, alpha = 1, col = "steelblue", shape = 16) +
  geom_point(aes(x = a.sd, y = a.means),size = 1, alpha = 0.3, col = "red", shape = 3) +
  xlab("Standard Deviation") +
  ylab("Mean Return") +
  ggtitle("Random Portfolios") + 
  #labs(color="Sharpe\nRatio") +
  scale_colour_gradient2(low = "#430355",
                         mid = "#21998A",
                         high = "#FAE726",
                         midpoint = 0.21) +
  theme(panel.background = element_rect(fill = "white", colour = '#434343'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size =  unit(0.5, "in"))
g






#### .... пример график с плотностью <3 ####

hor1["group"] <- 1
hor3["group"] <- 3
hor5["group"] <- 5
hor7["group"] <- 7
hor9["group"] <- 9
hor10["group"] <- 10
new.df <- rbind(hor1, hor3, hor5, hor7, hor9, hor10)
new.df <- rbind(hor1, hor10)
rm(p1,p2,p3, theme0)
p1 <- ggplot(new.df, aes(x = sd, y = means, colour = factor(group))) +
  geom_point(size = 0.5, alpha = 0.1, shape = 21) +
  scale_x_continuous(c(0, 16)) +
  scale_y_continuous(c(0, 4)) +
  #xlab("Стандартное Отклонение") +
  #ylab("Средняя Доходность") +
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "points"))
p1

theme0 <- function(...) theme( legend.position = "none",
                               panel.background = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.spacing = unit(0, "null"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.length = unit(0, "null"),
                               axis.ticks.margin = unit(0, "null"),
                               panel.border=element_rect(color = NA), ...)

p2 <- ggplot(new.df, aes(x = sd, colour = factor(group), fill = factor(group))) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(breaks = NULL, c(0,16)) +
  scale_y_continuous(breaks = NULL, c(0,4)) +
  theme_bw() +
  theme0(plot.margin = unit(c(1, 0, -0.2, 2.2),"lines"))
p2

p3 <- ggplot(new.df, aes(x = means, colour = factor(group), fill = factor(group))) + 
  geom_density(alpha = 0.5) + 
  coord_flip()  + 
  scale_x_continuous(labels = NULL,breaks = NULL, c(0,16)) +
  scale_y_continuous(labels = NULL,breaks = NULL, c(0,4)) +
  theme_bw() +
  theme0(plot.margin = unit(c(0, 1, 1.2, -0.27), "lines"))
p3  

g <- grid.arrange(arrangeGrob(p2, ncol = 2, widths = c(3,1)),
             arrangeGrob(p1, p3, ncol = 2, widths = c(3,1)),
             heights = c(1,3))
ggplot2::ggsave("g.png", plot = g, path = "/", dpi = 1200)
