require('neuralnet')

# data_ers_net= c(199, 231, 249, 180, 142, 173, 183, 258, 284, 331, 290, 439, 352,
#     362, 374, 448, 383, 490, 326, 444, 371, 306, 270, 257)

HCL_neural_net <- function(data_ers_net)
{
  length_of_data <- length(data_ers_net)
  used_samples <- 3
  No_training_samples <- length_of_data -used_samples
  train_x <-  matrix(, nrow =  No_training_samples, ncol = used_samples)
  train_y <- matrix(1,nrow=No_training_samples,1)

  for (n in 1:No_training_samples)
  {
    train_x[n,] = c(data_ers_net[n], data_ers_net[n+1], data_ers_net[n+2])
    train_y[n,] = data_ers_net[n+used_samples]
  }
  trainingdata <- cbind(train_x, train_y)
  colnames(trainingdata) <- c("Input1","Input2","Input3","Output")
  net.ts <- neuralnet(Output~Input1+Input2+Input3,trainingdata, hidden=0,  rep=10, threshold=0.001)

  net.results <- compute(net.ts, train_x)
  print(net.results$net.result)
  data_MAPE <-  (net.results$net.result - train_y)/train_y
  data_MAPE <- mean(abs(data_MAPE))

  return (data_MAPE)

}

