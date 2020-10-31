#Создание основной итоговой таблицы
tab_out <- function(count, path_in1, path_in2){
  in1 <- read.table(file = path_in1, head = T)
  in2 <- read.table(file = path_in2, head = T)

  
  value <- sample(100, nrow(in1))  #цена товаров
  value_shop <- value - 30 #цена покупки товаров
  value_unsale <- 25 #цена списания
  revenue = c(0, 0, 0, 0, 0, 0, 0)
  realize = 0
  supply = 0
  equability = c(0, 0, 0, 0, 0, 0, 0)
  
  for (i in 1:ncol(in1)){   #расчет выручки, реализации, поставок и равн. продаж
    for (j in  1:nrow(in1)){
      if (is.integer(in1[j, i]) || is.double(in1[i, j])){
        revenue[i - 1] <- revenue[i - 1] + in2[j, i]*value[j]
        equability[i - 1] <- equability[i - 1] + in2[j, i]
        realize <- realize + in2[j, i]
        supply <- supply + in1[j, i]
      }
    }
  }
  
  
  
  unsale <- supply - realize
  revenue <- sum(revenue)
  profit <- revenue - value_shop * supply - unsale * value_unsale
  equability <- round(sd(equability), 2)
              
  res.tab_week$Выручка[count] <- revenue
  res.tab_week$Реализация[count] <- realize
  res.tab_week$Списание[count] <- unsale
  res.tab_week$Прибыль[count] <- profit
  res.tab_week$sd[count] <- equability

  return (res.tab_week)
}

table <- rep(0, 12)

res.tab_week = data.frame(Выручка = table, Прибыль = table, Реализация = table, 
                                 Списание = table, sd = table)

#Вызовы функции
res.tab_week <- (tab_out(1, path_in1 = "in1.txt", path_in2 = "out1.txt"))
res.tab_week <- (tab_out(2, path_in1 = "in2.txt", path_in2 = "out2.txt"))
res.tab_week <- (tab_out(3, path_in1 = "in3.txt", path_in2 = "out3.txt"))
res.tab_week <- (tab_out(4, path_in1 = "in4.txt", path_in2 = "out4.txt"))
res.tab_week <- (tab_out(5, path_in1 = "in5.txt", path_in2 = "out5.txt"))
res.tab_week <- (tab_out(6, path_in1 = "in6.txt", path_in2 = "out6.txt"))
res.tab_week <- (tab_out(7, path_in1 = "in7.txt", path_in2 = "out7.txt"))
res.tab_week <- (tab_out(8, path_in1 = "in8.txt", path_in2 = "out8.txt"))
res.tab_week <- (tab_out(9, path_in1 = "in9.txt", path_in2 = "out9.txt"))
res.tab_week <- (tab_out(10, path_in1 = "in10.txt", path_in2 = "out10.txt"))

#Подсчет итоговых значений
res.tab_week$Выручка[11] <- sum(res.tab_week$Выручка)
res.tab_week$Реализация[11] <- sum(res.tab_week$Реализация)
res.tab_week$Списание[11] <- sum(res.tab_week$Списание)
res.tab_week$Прибыль[11] <- sum(res.tab_week$Прибыль)
res.tab_week$sd[11] <- sum(res.tab_week$sd)

#Подсчет средних значений
res.tab_week$Выручка[12] <- round(mean(res.tab_week$Выручка[1:10]))
res.tab_week$Реализация[12] <- round(mean(res.tab_week$Реализация[1:10]))
res.tab_week$Списание[12] <- round(mean(res.tab_week$Списание[1:10]))
res.tab_week$Прибыль[12] <- round(mean(res.tab_week$Прибыль[1:10]))
res.tab_week$sd[12] <- round(mean(res.tab_week$sd[1:10]))

print(res.tab_week)

write.table(res.tab_week, file = "C:/Market/out1.csv", sep = ";", dec = ',')


#=========================================================

#Графики
#Расчет параметров для магазинов

keys <- function(file1 = "", file2 = "", var = 1, product = 1){
  
  in1 <- read.table(file = file1, head = T)
  in2 <- read.table(file = file2, head = T)
  sale <- rep(0, ncol(in1) - 1)
  supply <- rep(0, ncol(in1) - 1)
  
  for (i in 1:ncol(in2) - 1){
    sale[i] <- in2[product, i + 1]
    supply[i] <- in1[product, i + 1]
  }
  supply <- as.integer(supply)
  sale <- as.integer(sale)
  unsale <- supply - sale
  
  len_x = seq(1:(ncol(in2) - 1))
  
  revenue <- value[1] * sale
  
  profit <- revenue - value_shop * supply - unsale * value_unsale
  rentab <- profit / revenue
  
  if (var == 1)
    return(sale)
  if (var == 2)
    return(revenue)
  if (var == 3)
    return(profit)
  if (var == 4)
    return(unsale)
  if (var == 5)
    return(rentab)
}


#Расчет параметров для 1 магазина
sale <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 1)
revenue <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 2)
profit <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 3)
unsale <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 4)
rentab <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 5)  

graphs <- function(y, fg, main, ylab, sub = ""){ #Функция для вывода графика
  plot(x = len_x, 
       y = y, 
       type = "l", 
       fg = fg,
       main = main,
       sub = sub,
       xlab = "Дни недели", 
       ylab = ylab,
       col = "black")
}

lin_gr <- function(y, fg, main, ylab){ #Функция для вывода доп.графика
  lines(x = len_x, 
      y = y, 
      type = "l", 
      fg = fg,
      main = main,
      xlab = "Дни недели", 
      ylab = ylab,
      col = "green")
}

#Вызов функции для 1 магазина

graphs(y = sale, fg = "black", main = "График эффективности работы 1 магазина", ylab =  "Объем продаж" )
graphs(y = revenue, fg = "red", main = "График эффективности работы 1 магазина", ylab =  "Выручка" )
graphs(y = profit, fg = "yellow", main = "График эффективности работы 1 магазина", ylab =  "Прибыль" )
graphs(y = unsale, fg = "green", main = "График эффективности работы 1 магазина", ylab =  "Списание" )
graphs(y = rentab, fg = "orange", main = "График эффективности работы 1 магазина", ylab =  "Рентабельность" )


#Расчет параметров для 2 магазина
sale <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 1)
revenue <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 2)
profit <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 3)
unsale <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 4)
rentab <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 5) 

#Вызов функции для 2 магазина
graphs(y = sale, fg = "black", main = "График эффективности работы 2 магазина", ylab =  "Объем продаж" )
graphs(y = revenue, fg = "red", main = "График эффективности работы 2 магазина", ylab =  "Выручка" )
graphs(y = profit, fg = "yellow", main = "График эффективности работы 2 магазина", ylab =  "Прибыль" )
graphs(y = unsale, fg = "green", main = "График эффективности работы 2 магазина", ylab =  "Списание" )
graphs(y = rentab, fg = "orange", main = "График эффективности работы 2 магазина", ylab =  "Рентабельность" )


#=========================================================

sum_product <- rep(0, 10)#Сумма продаж каждого магазина одного продукта

#Расчет объема продаж всех товаров для 1 магазина
sale1 <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in1.txt", file2 = "out1.txt", var = 1, product = 2)
sum_product[1] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 1 магазина", ylab =  "Объем продаж(шт)", sub = "Черная линия - 1 товар, Зеленая линия - 2 товар " )
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 1 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 2 магазина
sale1 <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in2.txt", file2 = "out2.txt", var = 1, product = 2)
sum_product[2] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 2 магазина", ylab =  "Объем продаж(шт)", sub = "Черная линия - 1 товар, Зеленая линия - 2 товар ")
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 2 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 3 магазина
sale1 <- keys(file1 = "in3.txt", file2 = "out3.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in3.txt", file2 = "out3.txt", var = 1, product = 2)
sum_product[3] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 3 магазина", ylab =  "Объем продаж(шт)", sub = "Черная линия - 1 товар, Зеленая линия - 2 товар " )
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 3 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 4 магазина
sale1 <- keys(file1 = "in4.txt", file2 = "out4.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in4.txt", file2 = "out4.txt", var = 1, product = 2)
sum_product[4] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 4 магазина", ylab =  "Объем продаж(шт)", sub = "Черная линия - 1 товар, Зеленая линия - 2 товар " )
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 4 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 5 магазина
sale1 <- keys(file1 = "in5.txt", file2 = "out5.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in5.txt", file2 = "out5.txt", var = 1, product = 2)
sum_product[5] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 5 магазина", ylab =  "Объем продаж(шт)" , sub = "Черная линия - 1 товар, Зеленая линия - 2 товар ")
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 5 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 6 магазина
sale1 <- keys(file1 = "in6.txt", file2 = "out6.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in6.txt", file2 = "out6.txt", var = 1, product = 2)
sum_product[6] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 6 магазина", ylab =  "Объем продаж(шт)", sub = "Черная линия - 1 товар, Зеленая линия - 2 товар " )
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 6 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 7 магазина
sale1 <- keys(file1 = "in7.txt", file2 = "out7.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in7.txt", file2 = "out7.txt", var = 1, product = 2)
sum_product[7] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 7 магазина", ylab =  "Объем продаж(шт)" , sub = "Черная линия - 1 товар, Зеленая линия - 2 товар ")
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 7 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 8 магазина
sale1 <- keys(file1 = "in8.txt", file2 = "out8.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in8.txt", file2 = "out8.txt", var = 1, product = 2)
sum_product[8] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 8 магазина", ylab =  "Объем продаж(шт)" , sub = "Черная линия - 1 товар, Зеленая линия - 2 товар ")
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 8 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 9 магазина
sale1 <- keys(file1 = "in9.txt", file2 = "out9.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in9.txt", file2 = "out9.txt", var = 1, product = 2)
sum_product[9] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 9 магазина", ylab =  "Объем продаж(шт)" , sub = "Черная линия - 1 товар, Зеленая линия - 2 товар ")
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 9 магазина", ylab =  "Объем продаж(шт)" )

#Расчет объема продаж всех товаров для 10 магазина
sale1 <- keys(file1 = "in10.txt", file2 = "out10.txt", var = 1, product = 1)
sale2 <- keys(file1 = "in10.txt", file2 = "out10.txt", var = 1, product = 2)
sum_product[10] <- sum(sale1)

graphs(y = sale1, fg = "black", main = "График объема продаж всех товаров для 10 магазина", ylab =  "Объем продаж(шт)", sub = "Черная линия - 1 товар, Зеленая линия - 2 товар " )
lin_gr(y = sale2, fg = "green", main = "График объема продаж всех товаров для 10 магазина", ylab =  "Объем продаж(шт)" )


#=========================================================
#Список цветов, используемых в диаграмме
colour_lst <- c("red", "orange", "yellow", "green", "blue", "purple", "black", "white", "brown", "grey")

#Создание диаграммы
pie(x = sum_product,
    labels = sum_product,
    main = "Диаграмма продаж 1 товара для 10 магазинов",
    sub = "1 - Красный, 2 - Оранжевый
          3 - Желтый, 4 - Зеленый
          5 - Синий, 6 - Фиолетовый
          7 - Черный, 8 - Белый
          9 - Коричневый, 10 - Серый",
    col = colour_lst)

