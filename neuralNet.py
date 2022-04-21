# Neural Network Lab  
# Dataset on Breast Tissue 
# Author: Drew Walizer 
# Date: 12/2/21

import tensorflow as tf
import numpy as np
from tensorflow import keras
from numpy import loadtxt
from keras.models import Sequential
from keras.layers import Dense
from sklearn.preprocessing import MinMaxScaler


# ------------------------------------------------------ #
# When preparing my data I one-hot encoded the target 
# values so that they would be notated as [1, 0, 0, 0] 
# for class 1, [0, 1, 0, 0] for class 2 and so on. I then 
# set up my neural net with an input layer of 9 neurons 
# then hidden layers with 32, 24, and 16 neurons using 
# relu ending with an output layer of 4 neurons using 
# sigmoid. For training and testing my neural network 
# I used the first 50 entries for training and the last 
# 24 entries for testing. My first neural network 
# resulted in an accuracy of around 70%. To improve this 
# I minmaxed my x values before training and testing. 
# I also removed most of the hidden layers in my network 
# since they did not seem to be helping. I kept one hidden 
# layer with 81 neurons. This new model resulted in an 
# accuracy of about 87.5% to 90% on my 50 to 24 training 
# test set. When submiting to autolab I trained my model 
# on all the data in the given dataset resulting in an 
# accuracy of about 90% on the autolab test set. 
# ------------------------------------------------------ #
def get_model():
    # load dataset.
    dataset = loadtxt('breasttissue_train.csv', delimiter=',')

    # set input and output variables.
    x = dataset[:, 1:]
    y = dataset[:, 0]

    # Minmax x values. 
    scaler = MinMaxScaler()
    scaler.fit(x)
    x = scaler.transform(x)

    # one hot encode target value. 
    one_hot_y = np.array([])
    count = 0
    for i in y:
        if i == 1:
            one_hot_y = np.append(one_hot_y, [1, 0, 0, 0])
        elif i == 2:
            one_hot_y= np.append(one_hot_y, [0, 1, 0, 0])
        elif i == 3:
            one_hot_y = np.append(one_hot_y, [0, 0, 1, 0])
        elif i == 4:
            one_hot_y = np.append(one_hot_y, [0, 0, 0, 1])
        count += 1
    one_hot_y = one_hot_y.reshape(count, 4)

    # training and test sets 
    trainX = x[:50]
    testX = x[50:]
    trainY = one_hot_y[:50]
    testY = one_hot_y[50:]
    
    model = Sequential()
    # # first hidden layer 81 neurons, input layer 9 neurons.
    model.add(Dense(81, input_dim=9 , activation="relu"))   
    # # output layer has 4 neurons 
    model.add(Dense(4, activation='sigmoid'))

    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    
    model.fit(x, one_hot_y, epochs=300, batch_size=5, verbose = 0)
    _, accuracy = model.evaluate(testX, testY)
    print(accuracy)
    print('Accuracy: %.2f' % (accuracy*100))
    predictions = (model.predict(testX))
    predictedClass = []
    for i in predictions:
        pmax = max(i)
        predictedClass.append(list(i).index(pmax)+1)

    testY = list(testY)
     
    
    for i in range(5):
	    print('%s => %s (expected %s)' % (testX[i].tolist(), predictedClass[i], testY[i]))
    
    return model



get_model()


