# PACKAGES 
# -----------------------------------------------------------------------------
import pandas            as pd
import numpy             as np
import matplotlib.pyplot as plt

# Self made functions and classes
import RoundListAndSumTo1 as rls1    # Rounding function for weights


class OptimizePortfolioPSO():
    """
    Particle Swarm Optimization (PSO) algorithm for optimizing a portfolio.
    
    Parameters:
        
    ranStockFinanceMPT (StockFinanceMPT object): 
            An object of StockFinanceMPT type that contains financial data from yfinance and MPT calculations.
    totalIterations (int, optional):
            The total number of iterations, which is the stopping criteria. Defaults to 1000.
    numPorfolios (int, optional):
            The number of portfolios in the swarm. Defaults to 100.
    minDesiredReturn (float, optional):
            The minimum return that we want to get. This is a soft constraint. Defaults to 0.07.
    evalGoal (str, optional):
            The goal of the evaluation. Possible values are 'risk' or 'return'. Defaults to 'risk'.
    method (str, optional):
            The method used to evaluate the portfolios. Possible values are 'local' or 'global'. Defaults to 'global'.
    ROUND_VALUE_TO (int, optional):
            The number of decimal places to round the weights. Defaults to 2.
    phi1 (float, optional):
            The cognitive weight. Defaults to 2.
    phi2 (float, optional):
            The social weight. Defaults to 2.
    intertiaWeight (float, optional):
            The constant inertia weighting value. Defaults to 0.1.
    absoluteMoveLimit (float, optional):
            The maximum percentage change in weight. Defaults to 0.1.
    investmentThreshold (float, optional):
            The threshold to consider as an investment. Defaults to 0.001.
    printDims (bool, optional):
            A flag to indicate whether to print the stock's value or not. Defaults to False.
    ANNUALIZER_VALUE (int, optional):
            The value used to annualize returns. Defaults to 1.
    """
   
    def __init__(self, 
                 ranStockFinanceMPT,              # StockFinanceMPT object containing yfinance data and MPT calcs
                 totalIterations     = 1000,      # Stopping criteria = the total number of iterations
                 numPorfolios        = 100,       # number of portfolios in swarm
                 minDesiredReturn    = 0.07,      # Minimum return that we want to get. This is a soft contraint!**
                 evalGoal            = 'risk',    # Minimize the risk
                 method              = 'global',  # 'local' or 'global' best function name
                 ROUND_VALUE_TO      = 2,         # WEights rounding baked into model. e.g, 0.021234 = 2%
                 phi1                = 2,         # Cognitive weight
                 phi2                = 2,         # Social weight
                 intertiaWeight      = 0.1,       # Constant Inertia weighting value,
                 absoluteMoveLimit   = 0.1,       # max of 10 pp change in weight
                 investmentThreshold = 0.001,     # the threshold to consider as an investment
                 printDims           = False,     # Print the stock's value or not,
                 ANNUALIZER_VALUE    = 1          # If using non annual returns then set to 12
                 ):
        
        # Set parameters from initalizer
        self.__ranStockFinanceMPT  = ranStockFinanceMPT
        self.__StockTickers        = ranStockFinanceMPT.getStockTickers()
        self.__minDate             = ranStockFinanceMPT.getMinDate()
        self.__maxDate             = ranStockFinanceMPT.getMaxDate()
        
        # From the input parameters
        self.__totalIterations     = totalIterations 
        self.__numPorfolios        = numPorfolios       
        self.__minDesiredReturn    = minDesiredReturn
        self.__evalGoal            = evalGoal
        self.__phi1                = phi1
        self.__phi2                = phi2
        self.__intertiaWeight      = intertiaWeight
        self.__absoluteMoveLimit   = absoluteMoveLimit
        self.__method              = method
        self.__printDims           = printDims
        self.__investmentThreshold = investmentThreshold
        self.__ANNUALIZER_VALUE    = ANNUALIZER_VALUE # initialize
        self.__numStocksInPort     = len(self.__StockTickers)
        
       
        # Initialize to global best function if chosen
        if self.__method == 'global':
            self.__functionToGetBest = self.__getGlobalBest
        else:
            # If not using the global best, then switch to the local best self.getMethod()
            self.__functionToGetBest = self.__getLocalBest    
        
        # Round all weights to n decimal places
        # Note that this is done within the model, not at the end.
        self.__ROUND_VALUE_TO = ROUND_VALUE_TO      
        
        # bounds for evaluation portfolio weights search space
        self.__lowerBound = 0   # no weight in a stock
        self.__upperBound = 1   # Max weight in stock
        
        # Local best neighborhood structure neighbors
        self.__NUM_NEIGHBORS = 2
                        
        # Indices of position and fitness value
        self.VALUE_IDX    = 0
        self.POSITION_IDX = 1
        
        # Run the model to find the global best solution
        # self.optimizePortfolioPSO()
        
            
        

    # =============================================================================
    # Functions to Evaluate Risk, Expected Return, and Sharpe Ratio
    # -- Will be used when optimizing portfolio 
    # =============================================================================
    
    def __evalExpectedReturn(self, Weights):
        """
        Function to Calculate the ExpectedReturn
        Calculation: expectedReturn = MONTH_IN_YEAR * ( Weights * t(ExpectedReturns) )
        
        Parameters:
            
        Weights (list object):
            List of weights for each simulated portfolio
        """
    
        Weights = np.array(Weights)    
    
        expectedReturn = self.getAnnualizerValue() * ( Weights.dot( self.getRanStockFinanceMPT().getExpectedReturns().transpose() ) )
        return expectedReturn[0]
    
    
    def __evalRisk(self, Weights):
        """
        Function to Calculate the Risk
        Calculation: risk = sqrt(12) * sqrt( ( Weights * VarCov ) * t(Weights) )
        
        Parameters:
            
        Weights (list object):
            List of weights for each simulated portfolio
        """
        # convert to numpy array
        Weights = np.array(Weights)
        
        # IF not the return desired then put a penalty on the portfolio
        # if self.__evalExpectedReturn(self.getAnnualizerValue(), Weights, self.getRanStockFinanceMPT().getExpectedReturns()) < self.getMinDesiredReturn():
        #     risk = 9999 # large penalty
            
        # # Else if desired return, then return actual risk
        # else:
        risk = np.sqrt(self.getAnnualizerValue()) * np.sqrt( ( Weights.dot(self.getRanStockFinanceMPT().getVarianceCovariance()) ).dot( Weights.transpose() ) )
        
        return risk
    
    
    def __evalSharpeRatio(self, expectedReturn, risk):
        """
        Function to Calculate the SharpeRatio
        Calculation: (expectedReturn - riskFreeRate) / risk 
        
        Parameters:
            
        expectedReturn (float object):
            Expected return of the portfolio
        risk (float object):
            The standard deviation of the portfolio
        """
        
        sharpeRatio = ( expectedReturn - self.getRanStockFinanceMPT().getRiskFreeRate() ) / risk 
        return sharpeRatio




    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Use metaheuristics to solve for the optimal portfolio
    # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # =============================================1================================
    # evaluation FUNCTION
    # evaluation function to evaluate a real-valued solution x
    # note: the feasible space is an n-dimensional hypercube centered at the origin with side length = 2 * 500
    # =============================================================================
    def __evalFunction(self, Weights):
        
        # For every portfolio portfolio, calculate the fitness value
        risk = self.__evalRisk(Weights) # Calculate the risk
        
        ## Drive home the point that this is the fitines value
        fitnessValue = risk
        return fitnessValue
    
    
    # =============================================================================
    # GLOBAL MIN VALUE AND POSITION SEARCH FUNCTION
    # Returns the 2 element list (each containing a single value) with the global best portfolio's:
    # ---- [0] min value and 
    # ---- [1] associate Weights of 
    # =============================================================================
    def __getGlobalBest(self, FitnessValues, Weights):
        minValue = np.min(FitnessValues)         # Find the Minimum fitness value of all portfolios
        minIndex = FitnessValues.index(minValue) # Find the index of the Weights for the min. fit. value
        
        minPosition = Weights[minIndex][:] # Now get a copy of the portfolio's Weights with min index
        
        # Returns: the global best portfolio's minimum fitness value and its Weights
        return [minValue, minPosition] 
    
    
    # =============================================================================
    # LOCAL MIN VALUE AND POSITION SEARCH FUNCTION
    # Topology: Ring structure with n neighbors  (default 2)
    # Returns the 2 element list of lists with the each portfolio's local best within neighborhood
    # ---- [0] min value and 
    # ---- [1] associate Weights of 
    # Can change numStocksInNbrhd to consider more or less in portfolio's neighborhood
    # =============================================================================
    def __getLocalBest(self, FitnessValues, Weights):  # Number of portfolios to compare to for local best
    
        lBestFitValue = [] # will hold the best VALUE    of the n surrounding portfolios, for each portfolio
        lBestPosition = [] # will hold the best POSITION of the n surrounding portfolios, for each portfolio
        
        
        # For every portfolio in the portfolio swarm, (starting at n less than index 0)
        for portfolio in range(-self.__getNumNeighbors(), self.getNumPorfolios() - self.__getNumNeighbors()):
            
            # Identify the two neighbors fitness value of this portfolio, 
            # which are the two precedng portfolios
            personalBestNeighbor1 = FitnessValues[portfolio]
            personalBestNeighbor2 = FitnessValues[portfolio + 1]
            
            # Identify the lowest fitness value of this portfolio's the two preciding neighbors
            minNeighValue = min(personalBestNeighbor1, personalBestNeighbor2)
            
            # Store the index of the portfolio
            minNeighIndex = FitnessValues.index(minNeighValue)
            
            # Store the portfolio's best neighbors fitness value and Weights
            lBestFitValue.append(FitnessValues[minNeighIndex])
            lBestPosition.append(Weights[minNeighIndex])
            
        # Returns a list of portfolios and the min of their n best fit. valued neighbors
        return[lBestFitValue, lBestPosition]
     
    
    
    # =============================================================================
    # STEP 1 - SWARM INITIALIZATION / EVALUATION
    # Randomly initialize a portfolio swarm instance
    # Set the partical's best to it's starting Weights
    # =============================================================================
    def __initializeSwarm(self):
        
        # In the current time period, Weights[portfolio] and Velocity[portfolio] of each portfolio i, 
        Weights  = [] # to hold the portfolio weights
        Velocity = [] # to hold the portfolio velocities
    
        # Lists containing info related to each portfolio in portfolio swarm
        pCurrFitValue = []  # X[portfolio] The current Weights of portfolio i
        # print(self.get())
        # For each portfolio and stock, randomly initialize the...
        for portfolio in range(self.getNumPorfolios()):
                           
            # Create a random feasible solution
            randomValues  = np.random.rand(self.__getNumStocksInPortfolio())
            randomWeights = rls1.roundListAndSumTo1(randomValues, self.getRoundToValue())
            
            # Ensure that it is an initial feasible solution meeting return contraint
            while self.__evalExpectedReturn(randomWeights) < self.getMinDesiredReturn():
                randomValues  = np.random.rand(self.__getNumStocksInPortfolio())
                randomWeights = rls1.roundListAndSumTo1(randomValues, self.getRoundToValue())
                
             
            # Position: give random solution of weights summing to 1
            Weights.append(randomWeights)
            
            # Velocity: give random value between lower and upper bound
            Velocity.append(rls1.roundListAndSumTo1(np.random.rand(self.__getNumStocksInPortfolio()) * self.getAbsoluteMoveLimit(), self.getRoundToValue()))
        
            # 1.1 - Evaluate fitness value
            pCurrFitValue.append(np.sum( self.__evalFunction(Weights[:]) ))  # evaluate the current Weights's fitness value
            
        # 1.2 - Log the individual and global bests
        pBestPosition = Weights[:]        # initialize pBestPosition to the starting Weights
        pBestFitValue = pCurrFitValue[:]  # initialize pBestPosition to the starting Weights's value
    
    
        # 1.3 - Log the Global or local best (depends on chosen self.getMethod()) fitness value and Weights
        glBestFitValue, glBestPosition = self.__getFunctionToGetBest()(pBestFitValue[:], pBestPosition[:]) 
        
        return [Weights, Velocity, pCurrFitValue, 
                pBestPosition,  pBestFitValue, 
                glBestFitValue, glBestPosition]
    
    
    
    # =============================================================================
    # UPDATE VELOCITY AND POSITION 
    # =============================================================================
    def __updateVelocityAndWeights(self, Velocity, Weights, pBestPosition, glBestPosition):
    # Velocity --------------------------------------------------------------------
        
        ## random weights of r for random Velocity adjustment
        r1, r2 = np.random.randint(self.__getLowerBoundOfSearchSpace(), self.__getUpperBoundOfSearchSpace()*100)/100, np.random.randint(self.__getLowerBoundOfSearchSpace(), self.__getUpperBoundOfSearchSpace()*100)/100
        
        ## Calculations of updating Velocity, separated by 
        ## intertia + cognitive + social (for simplicity)
        vInertia   = np.multiply(self.getIntertiaWeight(), Velocity[:])                          # Interia   component of updated Velocity
        vCognitive = np.multiply(self.getPhi1()*r1, np.subtract( pBestPosition[:], Weights[:])) # Cognitive component of ""
        vSocial    = np.multiply(self.getPhi2()*r2, np.subtract(glBestPosition[:], Weights[:])) # Social    component of ""
        
        ## Update the new Velocity to the summation of intertia, cognitive, and social
        newVelocity =  vInertia[:] + vCognitive[:] + vSocial[:]
        
        ## Limit the Velocity between the upper and lower bound limits
        for portfolio in range(self.getNumPorfolios()):
            for stock in range(self.__getNumStocksInPortfolio()):
            
                # If the new Velocity of portfolio i is > the ub move limit, then reduce to the limit
                if newVelocity[portfolio][stock] > self.getAbsoluteMoveLimit():
                    newVelocity[portfolio][stock] = self.getAbsoluteMoveLimit()
                    
                # If the new Velocity of portfolio i is < the limit, then increase to the limit
                if newVelocity[portfolio][stock] < -self.getAbsoluteMoveLimit():
                    newVelocity[portfolio][stock] = -self.getAbsoluteMoveLimit()
            
        # Position ----------------------------------------------------------------
        
        ## Update new Weights based on the updated Velocity
        newWeights = Weights[:] + newVelocity[:] 
        
        
        ## Make sure that the Weights is within the bounds -----------------------
        
        # For each portfolio and stock
        for portfolio in range(self.getNumPorfolios()):
            for stock in range(self.__getNumStocksInPortfolio()):
                
                # Push the new Weights to lower bound if lower
                if newWeights[portfolio][stock] < self.__getLowerBoundOfSearchSpace():
                    newWeights[portfolio][stock] = self.__getLowerBoundOfSearchSpace()
                
                # Push the new Weights to upper bound if higher
                if newWeights[portfolio][stock] > self.__getUpperBoundOfSearchSpace():
                    newWeights[portfolio][stock] = self.__getUpperBoundOfSearchSpace()
    
            
        for portfolio in range(self.getNumPorfolios()):
                
            # Balance the weights to equal 1.
            # Be sure to keep move limit close to 1
            newWeights[portfolio] = rls1.roundListAndSumTo1(newWeights[portfolio], self.getRoundToValue())
            
        
        # Convert Weights and Velocity back to list ------------------------------
        newWeights  = newWeights.tolist()
        newVelocity = newVelocity.tolist()
        
        return [newWeights, newVelocity]
    
    
    
    # =============================================================================
    # Compare current Weights fitness value to the current best (for each portfolio)
    # =============================================================================
    def __calculatePortfolioBests(self, Weights, pCurrFitValue, pBestPosition, pBestFitValue):
        # Calculate the fitness of the new Weights
        for portfolio in range(self.getNumPorfolios()):
            # for stock in range(self.__getNumStocksInPortfolio()):
                
            # Get the current fitness value of the new Weights
            pCurrFitValue[portfolio] = self.__evalFunction(Weights[:][portfolio])
            
            # Compare the current Weights' value to their person best
            if pCurrFitValue[portfolio] < pBestFitValue[portfolio]:
            
                # If better, then set the best VALUE to the current value (as a copy [:])
                pBestFitValue[portfolio] = pCurrFitValue[:][portfolio]
                
                # If better, then set the best POSITION to the current Weights  (as a copy [:])
                pBestPosition[portfolio] = Weights[:][portfolio]
        
        return [pCurrFitValue, pBestPosition, pBestFitValue]
    
    
    
    # =============================================================================
    # DISPLAY GLOBAL BEST AND DIMENSIONS FUNCTION
    # Function for displaying the global best and its dimensions
    # =============================================================================
    def displayGlobalBest(self):
        
        # Total % return over the entire period
        #monthsInSample = self.getNumPeriodsInSample()/self.getAnnualizerValue() # since on monthly basis
        #returnOverPeriod = (1 + self.getFinalExpectedReturn()) ** monthsInSample
        
        # Flip signs if maximizing sharpe
        #if self.getEvalGoal() != 'risk':
        #    self.__getGlobalBestFitValue() = -self.__getGlobalBestFitValue()
        #    self.getFinalSharpeRatio()    = -self.getFinalSharpeRatio()
            
        # Print the global optima
        print('\n```')
        print('Key Summary Statistics -----------------')
        print(  'Global Best Annualized Risk:\t ' + str(round(100 * self.__getGlobalBestFitValue(), 1))  + '%')
        print(  'Annualized Expected Return:\t ' + str(round(100 * self.getFinalExpectedReturn(), 1))  + '%')
        print(  'Sharpe Ratio:\t\t\t% 0.2f' %   self.getFinalSharpeRatio())
        print(  'Num. Tickers in Portfolio:\t', self.getNumTickersInvestedIn())
        #print(  'Expected Return over', str(round(monthsInSample, 1) ), 'Years:  ' 
        #      + str(round(100 * returnOverPeriod, 1))  + '%')
        
        
        # Print each stock (if toggled)
        if self.getPrintDims():
            print('\nGlobal best weights in each stock (Only includes stocks to invest in):')
            print('```')
            
            # Print the Weights of each stock in markdown table format
            print('| Ticker | Opt. Weight |', sep ='')
            print('|--------|-------------|')
            for stock in range(self.__getNumStocksInPortfolio()):
                if self.__getGlobalBestPosition()[stock] > self.getInvestmentThreshold():
                    print('' + str(self.getStockTickers()[stock]).rjust(7, ' '), 
                          '' + '{:.1f}'.format(100 * round(self.__getGlobalBestPosition()[stock], 3) ).rjust(11, ' ') + '% '
                          )
        
        
                    
    # =============================================================================
    # Plot the optimal weights and prices performance
    # =============================================================================
    def plotOptimalPerformance(self):
        
        # Calculate the weighted prices using adj close prices and optimal weights
        self.__OptimizedPrices = self.getRanStockFinanceMPT().getRawStockDataPivot().dropna().dot(self.__getGlobalBestPosition())
              
        # Create a plot
        fig = plt.figure()
        ax = fig.add_subplot(1,1,1)
        
        # Plot the weighted prices
        plt.plot(self.getOptimizedPrices())
        
        # Labels
        ax.set_title("Wt.'d Monthly Adj. Close Price of Optimal Portfolio")
        ax.set_xlabel('Month of Year (Year Only Shown for Simplicity)')
        ax.set_ylabel('Weighted Adj. Close Price (USD)')
        
                
    
    # =============================================================================
    # SWARM OPTIMIZATION FUNCTION
    # =============================================================================
    
    def optimizePortfolioPSO(self):
            
        # -----------------------------------------------------------------------------
        # INITIALIZE POSITION AND VELOCITY, and INITIAL BESTS
        # the portfolio swarm will be represented as a list of Weights, velocities, values, 
        # pBestPosition, and pBestPosition values
        # Note: Weights[0] and Velocity[0] provides the Weights and Velocity of portfolio 0; 
        # Weights[1] and Velocity[1] provides the Weights and Velocity of portfolio 1; and so on.
        # -----------------------------------------------------------------------------
        
        
        # Step 1: Initialize portfolio swarm and get the portfolios' and global best (and current Weights)
        Weights, Velocity, pCurrFitValue, pBestPosition, pBestFitValue, glBestFitValue, glBestPosition = self.__initializeSwarm()
        
        # Create empty lists for holding the portfolio swarm iterations
        positionIterations      = [] # Each portfolio's Velocity
        velocityIterations      = [] # Each portfolio's Weights
        gBestPositionIterations = [] # The current Global Best Position
        
        
        # -----------------------------------------------------------------------------
        # Main Loop 
        # -----------------------------------------------------------------------------
        
        iteration = 0
        
        # While meeting total iteration and min desired return requirements
        while (iteration < self.getTotalIterations()) and ( ( self.__evalExpectedReturn(glBestPosition[:]) < self.getMinDesiredReturn() ) or ( self.__evalExpectedReturn(glBestPosition[:]) > self.getMinDesiredReturn() * 1.01) ): 
            
            # Step 0: Keep track of each iterations/stock for Velocity, Weights, and current global best
            positionIterations.append(Weights)           
            velocityIterations.append(Velocity)           
            gBestPositionIterations.append(glBestPosition) 
            
            # Step 2: Update the Velocity and Weights
            Weights, Velocity = self.__updateVelocityAndWeights(Velocity, Weights, pBestPosition, glBestPosition)
            
            # Step 3: Recalculate the portfolio and global bests
            pCurrFitValue, pBestPosition, pBestFitValue = self.__calculatePortfolioBests(Weights, pCurrFitValue, 
                                                                                         pBestPosition, pBestFitValue)
                    
            # Step 4: Get the Global or local best (depends on chosen self.getMethod()) fitness value and Weights
            glBestFitValue, glBestPosition = self.__getFunctionToGetBest()(pBestFitValue[:], pBestPosition[:]) 
            
            iteration += 1 # increment iterator
    
        
        # -----------------------------------------------------------------------------
        # Global Best
        # -----------------------------------------------------------------------------
        
        # Finally, if using the local best self.getMethod(), get the absolute best from the local bests
        if self.getMethod() == 'local':
            self.__gBestFitValue, self.__gBestPosition = self.__getGlobalBest(glBestFitValue, glBestPosition)
        
        else: # if not local best, then change the gl best is the global best
            self.__gBestFitValue, self.__gBestPosition = glBestFitValue, glBestPosition
        
        # Then calculate the expected returns with using local or glbal best, depending on chosen
        self.__finalExpectedReturn = self.__evalExpectedReturn(self.__getGlobalBestPosition())
        
        
        # -----------------------------------------------------------------------------
        # Print and Export
        # -----------------------------------------------------------------------------
        
        # Create single data frame with the weights and tickers
        self.__FinalWeights = pd.DataFrame({
            'Ticker': self.getStockTickers(),
            'Weight': self.__getGlobalBestPosition(),
        })
        self.__FinalWeights.set_index('Ticker', inplace = True)
        
        # Get the final expected returns over the period
        self.__FinalExpectedReturns = pd.DataFrame(self.getRanStockFinanceMPT().getReturns().dropna().dot(self.__getGlobalBestPosition())[:])
      
        # The sharpe ratio
        self.__finalSharpeRatio = self.__evalSharpeRatio(self.getFinalExpectedReturn(), self.__getGlobalBestFitValue() )
    
            
        # Number of stocks invested in (over the min investment threshold)
        self.__numTickersInvestedIn = (
            self.getFinalWeights()[self.getFinalWeights()['Weight'] > self.getInvestmentThreshold()]
            .count()
            .iloc[0]   # explicitly positional
)

          
        # Show the weighted price perfgormance of the oprtimal portfolio weights
        if self.getPrintDims():
            # Print the global (or local best) and each dimensions' Weights
            self.displayGlobalBest()
            
            self.plotOptimalPerformance()
             
        # Rename as risk: 
        self.__finalRisk = self.__getGlobalBestFitValue()

        # Return key stats of performance
        return [self.getFinalWeights(), self.getFinalRisk(), self.getFinalExpectedReturn(), 
                self.getFinalSharpeRatio(), self.getNumTickersInvestedIn(),
                self.getFinalExpectedReturns()] 
    
        
            
    
    # GETTERS  ============================================================
    
    
    # Inputs - allow user to retrieve them --------------------------------      
    
    def getStockTickers(self):
        return self.__StockTickers
    
    def getMinDate(self):
        return self.__minDate
    
    def getMaxDate(self):
        return self.__maxDate
    
    def getRanStockFinanceMPT(self):
        return self.__ranStockFinanceMPT
            
    def getAnnualizerValue(self):
        return self.__ANNUALIZER_VALUE
    
    def getTotalIterations(self):        
        return self.__totalIterations
            
    def getNumPorfolios(self):        
        return self.__numPorfolios
         
    def getMinDesiredReturn(self):
        return self.__minDesiredReturn
    
    def getEvalGoal(self):
        return self.__evalGoal
    
    def getPhi1(self):
        return self.__phi1
    
    def getPhi2(self):
        return self.__phi2
    
    def getIntertiaWeight(self):
        return self.__intertiaWeight
    
    def getAbsoluteMoveLimit(self):
        return self.__absoluteMoveLimit
    
    def getMethod(self):
        return self.__method
    
    def getPrintDims(self):
        return self.__printDims
    
    def getInvestmentThreshold(self):
        return self.__investmentThreshold
    
    def getRoundToValue(self):
        return self.__ROUND_VALUE_TO
    
    
    # Functions that the user cannot access -------------------------------
    
    def __getLowerBoundOfSearchSpace(self):
        return self.__lowerBound
    
    def __getUpperBoundOfSearchSpace(self):
        return self.__upperBound
    
    def __getNumNeighbors(self):
        return self.__NUM_NEIGHBORS
    
    def __getNumStocksInPortfolio(self):
        return self.__numStocksInPort
    
    def __getFunctionToGetBest(self):
        return self.__functionToGetBest
    
    def __getGlobalBestFitValue(self):
        return self.__gBestFitValue
    
    def __getGlobalBestPosition(self):
        return self.__gBestPosition.copy()
    
    def __getValueIdx(self):
        return self.VALUE_IDX
    
    def __getPositionIdx(self):
        return self.POSITION_IDX
    
    
    # Final Key Stats - User can access these -----------------------------
    
    def getFinalWeights(self):
        return self.__FinalWeights.copy()

    def getFinalRisk(self):
        return self.__finalRisk
    
    def getFinalExpectedReturn(self):
        return self.__finalExpectedReturn
    
    def getFinalSharpeRatio(self):
        return self.__finalSharpeRatio
    
    def getNumTickersInvestedIn(self):
        return self.__numTickersInvestedIn
    
    def getFinalExpectedReturns(self):
        return self.__FinalExpectedReturns.copy()
    
    def getOptimizedPrices(self):
        return self.__OptimizedPrices.copy()
    


# Examples --------------------------------    

# import datetime        as dt
# import StockFinanceMPT as mpt # Stock pull from yfinance in current wrkdir
# # import OptimizePortfolioPSO as pso

# StockTickers = ['AAPL', 'MSFT', 'TSLA']

# maxDate = dt.datetime.today()                    # Max date to pull from
# minDate = maxDate - dt.timedelta(days = 15*365) # Min date to pull from

# PulledStockData = mpt.StockFinanceMPT(StockTickers, minDate, maxDate)
# # print(PulledStockData.getReturns())


# # Run the PSO Optimization
# zz_out = OptimizePortfolioPSO(
#     PulledStockData,           # From above
#     totalIterations     = 250, # Stopping criteria = the total number of iterations
#     numPorfolios        = 10   # number of portfolios in swarm
#     )


# # Show the output
# zz_out.displayGlobalBest()
# zz_out.plotOptimalPerformance()

