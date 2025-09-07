# PACKAGES 
# -----------------------------------------------------------------------------
import pandas   as pd
import datetime as dt
import time

# Self made functions and classes
import UpdateProgressBar    as progBar # self made progress bar in working directory
import OptimizePortfolioPSO as pso     # Functoin that will run the Simulations of PSO objectss

# Multithreading concurrent processes
from concurrent.futures import ThreadPoolExecutor
from concurrent.futures import ProcessPoolExecutor



class SimulateOptimalPortfolios():
    
    def __init__(self, 
                 ranStockFinanceMPT,              # StockFinanceMPT object containing yfinance data and MPT calcs
                 totalSimulations    = 1000,      # Number of simulated PSO swarms to create and compare
                 totalIterations     = 50,        # Stopping criteria = the total number of iterations
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
        
        # Simulation of PSO main param
        self.__totalSimulations    = totalSimulations 
        
        # Set parameters from initalizer
        self.__ranStockFinanceMPT  = ranStockFinanceMPT
        self.__StockTickers        = ranStockFinanceMPT.getStockTickers()
        self.__minDate             = ranStockFinanceMPT.getMinDate()
        self.__maxDate             = ranStockFinanceMPT.getMaxDate()
        
        # PSO Params
        self.__totalIterations     = totalIterations 
        self.__numPorfolios        = numPorfolios 
        self.__minDesiredReturn    = minDesiredReturn 
        self.__evalGoal            = evalGoal 
        self.__method              = method 
        self.__ROUND_VALUE_TO      = ROUND_VALUE_TO 
        self.__phi1                = phi1 
        self.__phi2                = phi2 
        self.__intertiaWeight      = intertiaWeight 
        self.__absoluteMoveLimit   = absoluteMoveLimit 
        self.__investmentThreshold = investmentThreshold 
        self.__printDims           = printDims 
        self.__ANNUALIZER_VALUE    = ANNUALIZER_VALUE 
                                
        
        # Indices of output for the portfilio model
        self.__WEIGHTS_IDX, self.__RISK_IDX, self.__RETURN_IDX, self.__SHARPE_IDX, self.__NUM_TICKERS_IDX, self.__EXP_RETURNS_IDX = range(6)
        
        
        # EMPTY DATA ---------------------------------------------------------------------------------------
        
        # Hold a dataframe with the tickers as the index
        # Contains no weights yet
        self.__SimulatedPortfolios = pd.DataFrame({
            'Ticker': self.getStockTickers()
        })
        self.__SimulatedPortfolios.set_index('Ticker', inplace = True)
        
        
        # Hold dataframe for expected returns over period
        self.__SimulatedExpectedReturns = pd.DataFrame({
            'Period': self.getRanStockFinanceMPT().getRawStockDataPivot().index
        })
        self.__SimulatedExpectedReturns.set_index('Period', inplace = True)
        
        
        # Hold four lists that have the return and risk of the portfolios
        self.__SimulatedRisks, self.__SimulatedReturns, self.__SimulatedSharpe, self.__SimulatedNumTickers = [[] for i in range(4)]
        
        # Column prefix for each simulation
        self.__simColPrefix = 'Simulation_' # column prefix for simulations
        
        
        # To hold the portfolio best objects created from each simulation
        self.__SwarmHistory = []
    
    
    
    
    def runSimulation(self):
        
        # Simulate the portfolio pulls multiple times and left join to compare results ------------------------
        # print('---------- Simulation Status of', self.getTotalSimulations(), 'Simulations ----------')
        simulationStartTime = time.time()
        
        with ThreadPoolExecutor() as executor:
            for simulation in range(self.getTotalSimulations()):
        
                # Progress bar
                # progBar.updateProgressBar(simulationStartTime, simulation, self.getTotalSimulations())
        
                # Initalize the swarm object without running the model
                SingleSwarm = pso.OptimizePortfolioPSO(ranStockFinanceMPT  = self.getRanStockFinanceMPT(),
                                                       totalIterations     = self.getTotalIterations(),
                                                       numPorfolios        = self.getNumPorfolios(),
                                                       minDesiredReturn    = self.getMinDesiredReturn(),
                                                       evalGoal            = self.getEvalGoal(),
                                                       method              = self.getMethod(),
                                                       ROUND_VALUE_TO      = self.getRoundToValue(),
                                                       phi1                = self.getPhi1(),
                                                       phi2                = self.getPhi2(),
                                                       intertiaWeight      = self.getIntertiaWeight(),
                                                       absoluteMoveLimit   = self.getAbsoluteMoveLimit(),
                                                       investmentThreshold = self.getInvestmentThreshold(),
                                                       printDims           = self.getPrintDims(),
                                                       ANNUALIZER_VALUE    = self.getAnnualizerValue()
                                                       )
                
        
                # Run the swarm optimization
                TempSimulatedPortfolio = executor.submit(SingleSwarm.optimizePortfolioPSO).result() # make sure to get the result
        
                # Keep track of the Swarm object since it will be overritten
                self.__SwarmHistory.append(TempSimulatedPortfolio)
        
        # Run time stats
        self.__simulationEndTime = time.time() # Time that the simulations end
        self.__totalRunTime      = self.__simulationEndTime - simulationStartTime
        self.__simulationEndTime = dt.datetime.now()
        
        
        # JOIN the  DATA ---------------------------------------------------------------------------------------
        
        # Work through each simulation
        for simulation in range(self.getTotalSimulations()):
        
            # join the new simulation weights on to the new data
            NewSimulatedWeights = self.getSwarmHistory()[simulation][self.__getWeightsIdx()]
            NewSimulatedWeights.columns.values[0] = self.__simColPrefix + str(simulation) # rename columns to alleviate joining duplication errors
        
            self.__SimulatedPortfolios = self.__SimulatedPortfolios.merge(NewSimulatedWeights,
                                how = 'left',
        
                                # Join on the index, which is the date
                                left_index  = True,
                                right_index = True
                                )
        
            # Join the expected returns of each simulated portfolio
            NewSimulatedExpReturns = self.getSwarmHistory()[simulation][self.__getExpReturnsIdx()]
            #print(NewSimulatedExpReturns)
            NewSimulatedExpReturns.columns = [self.__simColPrefix + str(simulation)] + list(NewSimulatedExpReturns.columns[1:])  # rename columns to alleviate joining duplication errors
        
            self.__SimulatedExpectedReturns = self.__SimulatedExpectedReturns.merge(NewSimulatedExpReturns,
                                how = 'left',
        
                                # Join on the index, which is the date
                                left_index  = True,
                                right_index = True
                                )
        
            # Get the risk and return of the simulated portfolios
            self.__SimulatedRisks.append(self.getSwarmHistory()[simulation][self.__getRiskIdx()])
            self.__SimulatedReturns.append(self.getSwarmHistory()[simulation][self.__getReturnIdx()])
            self.__SimulatedSharpe.append(self.getSwarmHistory()[simulation][self.__getSharpeIdx()])
            self.__SimulatedNumTickers.append(self.getSwarmHistory()[simulation][self.__getNumTickersIdx()])

            
           
        
    # GETTERS  ============================================================
    
    
    # Inputs - allow user to retrieve them --------------------------------      
    
    # Swarm parameter getters
    
    def getTotalSimulations(self):        
        return self.__totalSimulations
            
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
    
    def getSimulationColumnPrefixName(self):
        return self.__simColPrefix
    
    
    
    # Private getters 
    def __getWeightsIdx(self):
        return self.__WEIGHTS_IDX
    
    def __getRiskIdx(self):
        return self.__RISK_IDX
    
    def __getReturnIdx(self):
        return self.__RETURN_IDX
    
    def __getSharpeIdx(self):
        return self.__SHARPE_IDX
    
    def __getNumTickersIdx(self):
        return self.__NUM_TICKERS_IDX
    
    def __getExpReturnsIdx(self):
        return self.__EXP_RETURNS_IDX
        
    
    
    # Simulation getters
        
    def getSwarmHistory(self):
        return self.__SwarmHistory
    
    def getSimulatedPortfolios(self):
        return self.__SimulatedPortfolios
    
    def getSimulatedExpectedReturns(self):
        return self.__SimulatedExpectedReturns
    
    def getSimulatedRisks(self):
        return self.__SimulatedRisks
    
    def getSimulatedReturns(self):
        return self.__SimulatedReturns
    
    def getSimulatedSharpe(self):
        return self.__SimulatedSharpe
    
    def getSimulatedNumTickers(self):
        return self.__SimulatedNumTickers
    
    def getSimulationEndTime(self):
        return self.__simulationEndTime
    
    def getTotalRunTime(self):
        return self.__totalRunTime
    
    # def getOriginalReturnsData(self):
    #     return self.getRanStockFinanceMPT().getReturns()
    
    
    
# Examples --------------------------------    

#import datetime        as dt
#import StockFinanceMPT as mpt # Stock pull from yfinance in current wrkdir
#import OptimizePortfolioPSO as pso

#StockTickers = ['AAPL', 'TSLA', 'MSFT']
#maxDate = dt.datetime.today()                    # Max date to pull from
#minDate = maxDate - dt.timedelta(days = 15*365) # Min date to pull from

#PulledStockData = mpt.StockFinanceMPT(StockTickers, minDate, maxDate)


#simulatePortfolios = SimulateOptimalPortfolios(    
#    PulledStockData,               # inherited and passed data retreival
#    totalSimulations    = 200,
#    totalIterations     = 10,      # Stopping criteria = the total number of iterations
#    numPorfolios        = 5        # number of portfolios in swarm
#    )

#simulatePortfolios.runSimulation()

#print(simulatePortfolios.getSimulatedPortfolios())
