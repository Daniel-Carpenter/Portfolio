import numpy as np
import random

# Function to create set of random weights in the portolio that round to a certain value. 
# E.g., only can invest in 1 percentage point or more
def roundListAndSumTo1(npArray, roundToValue):
    # print('\n---\nInput Array\t', npArray) # for testing
    
    # Convert the input list to a NumPy array
    # npArray = np.array(lst)
      
    # Divide the elements of the array by the sum of the array
    # to get the elements to sum to 1
    npArray = npArray / npArray.sum()
    
    # Round each element of the array to the specified decimal places
    npArray = np.round(npArray, roundToValue)
        
    # While any negative element exists or the sum of weights is not 100%
    while ( np.any(npArray < 0) ) or ( npArray.sum() != 1 ):
        
        # If There are negative values in the array set the element to zero
        if np.sum(npArray < 0) > 0:
            negativeElements = npArray < 0 # Find all negative elements
            npArray[negativeElements]  = 0 # Set to zero
    
        # If the sum of weights is not 100% then plug a reandom element 
        # So that the sum is 1
        if npArray.sum() != 1:
            
            # Choose a random index in the array
            randomIndex = random.randint(0, len(npArray) - 1)
        
            # Add the difference between 1 and the current sum of the array
            # to the randomly chosen element, making the array sum to 1
            plugElement = 1 - npArray.sum()
            npArray[randomIndex] += plugElement
    
    # print('Final Array\t', npArray, '\n---') # for testing
    return npArray.tolist()