import PullStockData as psd # in working directory to pull stocks.
import numpy as np


class StockFinanceMPT:
    """
    A class to calculate modern portfolio calculations (returns, etc.) using PullStockData class.
    
    StockTickers: list
        List of stock ticker symbols to download data for
    minDate: str
        Start date in format "YYYY-MM-DD"
    maxDate: str
        End date in format "YYYY-MM-DD"
    interval: str, optional
        The interval to download the data for (default is '1mo').
        Valid intervals: 1m,2m,5m,15m,30m,60m,90m,1h,1d,5d,1wk,1mo,3mo
    N_PERIOD_RETURN: int, optional
        The number of months to calculate the return of this period from.
    """
    def __init__(self, StockTickers, minDate, maxDate, 
                 interval='1mo', N_PERIOD_RETURN=12,
                 exportRawData=False,
                 outputFileName='RawStockDataPivot.csv'):
        self.__StockTickers   = StockTickers
        self.__minDate        = minDate
        self.__maxDate        = maxDate
        self.__interval       = interval
        self.__N_PERIOD_RETURN  = N_PERIOD_RETURN
        self.__tBillTicker    = '^IRX'
        self.__outputFileName = outputFileName
        self.__exportRawData  = exportRawData
        
        # Run the data model
        self.__runDataModel()
        
        
    def __pullStockTickerData(self):
        """
        Function to pull the stock ticker Data
        """
        
        # Create PullStockData object to pull stock tickers
        stockDataPuller = psd.PullStockData(self.getStockTickers(), self.getMinDate(), self.getMaxDate(), self.getInterval())
        RawStockData    = stockDataPuller.getData()
        
        # Create a pivot of the data
        self.__RawStockDataPivot = RawStockData.copy().pivot(index   = 'period', 
                                                             columns = 'stock',  
                                                             values  = 'adjClose')
        # Export to CSV
        if self.__exportRawData:
            self.getRawStockDataPivot().to_csv(self.getOutputFileName())

    
    def __importTBill(self):
        """
        Function to pull t-bill as risk free rate.
        """
        
        # Create PullStockData object to pull t bill
        stockDataPullerTBill = psd.PullStockData([self.getTBillTicker()], self.getMinDate(), self.getMaxDate(), self.getInterval())
        self.__T_BillData    = stockDataPullerTBill.getData()
        
        # get the average risk free rate ove the period
        self.__riskFreeRate = self.getT_BillData()['adjClose'].mean() / 100 # 100%
    
    
    def __calculateReturns(self):
        """
        Function to calculate the reuturns for each stock.
        Calculation: Returns = This period return divided by last year minus N_PERIOD_RETURN, for each stock
        """
        
        # Get the N_PERIOD_RETURN month period return of the stocks
        Returns = self.getRawStockDataPivot()
        Returns[self.getStockTickers()] = Returns[self.getStockTickers()] / Returns[self.getStockTickers()].shift(self.getPeriodNforReturnCalc()) - 1
        self.__Returns = Returns.copy().dropna()
        
        # Keep track of Number of periods in sample
        self.numPeriods = len(self.getReturns().index)
    
    
    def __calculateExpectedReturns(self):
        """
        Function to calculated the expected returns (mean over sample).
        Calculation: ExpectedReturn = mean(Returns), for each stock
        """
        
        # Calculate the expected returns (average)over the period for each stock
        ExpectedReturns = self.getReturns()
        ExpectedReturns['AllPeriods'] = 'AllPeriods'
        self.__ExpectedReturns = ExpectedReturns.groupby('AllPeriods').mean()
    
    def __calculateExcessReturns(self):
        """
        Function to calculate the Excess Returns for each stock and month.
        Calculation: ExcessReturns = Returns - ExpectedReturns, for each stock
        """
        self.__ExcessReturns = self.getReturns() - np.array(self.getExpectedReturns())
        
    def __calculateVarianceCovariance(self):
        """
        Function to Calculate the Variance-Covariance Matrix.
        Calculation: ( t(ExcessReturns) * ExcessReturns ) / ( numPeriods - 1 )
        """
        self.__VarCov = self.getExcessReturns().transpose().dot(self.getExcessReturns()) / ( self.getNumPeriodsInSample() - 1 )
        
    def __runDataModel(self):
        """
        Function that runs self.__pullStockTickerData(), self.__importTBill(), 
        self.__calculateReturns(), self.__calculateExpectedReturns(), 
        self.__calculateExcessReturns(), and self.__calculateVarianceCovariance()
        """
        self.__pullStockTickerData()
        self.__importTBill()
        self.__calculateReturns()
        self.__calculateExpectedReturns()
        self.__calculateExcessReturns()
        self.__calculateVarianceCovariance()
        
        
    # Getters for above calculations or private variables    
    # -------------------------------------------------------------------------
        
    def getStockTickers(self):
        return self.__StockTickers
    
    def getMinDate(self):
        return self.__minDate
    
    def getMaxDate(self):
        return self.__maxDate
    
    def getOutputFileName(self):
        return self.__outputFileName
    
    def getInterval(self):
        return self.__interval
    
    def getPeriodNforReturnCalc(self):
        return self.__N_PERIOD_RETURN
    
    def getNumPeriodsInSample(self):
        return self.numPeriods
    
    def getTBillTicker(self):
        return self.__tBillTicker
    
    def getT_BillData(self):
        return self.__T_BillData.copy()
    
    def getRiskFreeRate(self):
        return self.__riskFreeRate
    
    def getRawStockDataPivot(self):
        return self.__RawStockDataPivot.copy()
    
    def getReturns(self):
        return self.__Returns.copy()
    
    def getExpectedReturns(self):
        return self.__ExpectedReturns.copy()
    
    def getExcessReturns(self):
        return self.__ExcessReturns.copy()
    
    def getVarianceCovariance(self):
        return self.__VarCov.copy()


# Examples --------------------------------    

#import datetime as dt

#StockTickers = ['FXNAX', 'PTRQX', 'PHIYX']
#maxDate = dt.datetime.today()                    # Max date to pull from
#minDate = maxDate - dt.timedelta(days = 2*365) # Min date to pull from


#stockDataPuller = StockFinanceMPT(StockTickers, minDate, maxDate)
#zz_out = stockDataPuller.getVarianceCovariance()

