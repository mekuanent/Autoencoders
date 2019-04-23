library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_train <- x_train / 255

model <- keras_model_sequential() 

model %>% 
 layer_dense(units = 32, activation = 'relu', input_shape = c(784)) %>% 
 layer_dense(units=784, activation='sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, x_train, 
  epochs = 15, batch_size = 128, 
  validation_split = 0.2
)

res <- model %>% predict(x_train[1:2,])

m = array_reshape(res[1,], c(28, 28))
m = m * 255
image(m, col= c("white", "black"), useRaster=TRUE)

m_t = array_reshape(x_train[1,], c(28, 28))
m_t = m_t * 255
image(m_t, col= c("white", "black"), useRaster=TRUE)

random_flat_a = c((1:784) %% 255)
random <- matrix(random_flat_a, nrow=28, ncol=28, byrow=TRUE)
mr <- array_reshape(random, c(28, 28))
image(mr, useRaster=TRUE)

random_flat_a <- random_flat_a / 255
rand_res <- model %>% predict(matrix(random_flat_a, nrow=1, n=784, byrow=TRUE))
rand_a <- array_reshape(rand_res[1,]*255, c(28, 28))
image(rand_a, useRaster=TRUE)

#what layers learned

for(layer in intermediate_layer_model$layers){
	print(layer$get_weights()[2])
	#print(ncol(layer$get_weights()[1]))
	#print(nrow(layer$get_weights()[2]))
}

nneeds <- function(weights){
	
	
	
	holder <- list()
	clist <- list()
	
	k <- matrix()
	
	for(w in weights){
		dem <- sqrt(sum(w^2))
		arr <- (w/dem) * 255
		m = array_reshape(arr, c(28, 28))
		holder <- append(holder, list(m))
	}
	
	
	m1 <- do.call(rbind, holder[1:8])
	m2 <- do.call(rbind, holder[9:16])
	m3 <- do.call(rbind, holder[17:24])
	m4 <- do.call(rbind, holder[25:32])
	mc <- do.call(cbind, list(m1, m2, m3, m4))

	image(mc, useRaster=TRUE)

}
weights <- as.data.frame(layer$get_weights()[1])
nneeds(weights)