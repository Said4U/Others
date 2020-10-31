generation_out <- function(file_in, saleLevel = 0, path_out){
  
  in1 <- read.table(file = file_in, head = TRUE)

  for (i in 1:nrow(in1)){
    for (j in  1:ncol(in1)){
      if (is.integer(in1[i, j]) || is.double(in1[i, j])){
        in1[i, j] <- round(in1[i, j] * saleLevel / 100)
      }
    }
  }

  print(in1)
  
  write.table(in1, file = path_out, sep = "  ", dec = ",")
}

generation_out(file_in = "in1.txt", saleLevel = 75, path_out = "out1.txt")
generation_out(file_in = "in2.txt", saleLevel = 60, path_out = "out2.txt")
generation_out(file_in = "in3.txt", saleLevel = 80, path_out = "out3.txt")
generation_out(file_in = "in4.txt", saleLevel = 49, path_out = "out4.txt")
generation_out(file_in = "in5.txt", saleLevel = 70, path_out = "out5.txt")
generation_out(file_in = "in6.txt", saleLevel = 33, path_out = "out6.txt")
generation_out(file_in = "in7.txt", saleLevel = 40, path_out = "out7.txt")
generation_out(file_in = "in8.txt", saleLevel = 55, path_out = "out8.txt")
generation_out(file_in = "in9.txt", saleLevel = 79, path_out = "out9.txt")
generation_out(file_in = "in10.txt", saleLevel = 54, path_out = "out10.txt")

