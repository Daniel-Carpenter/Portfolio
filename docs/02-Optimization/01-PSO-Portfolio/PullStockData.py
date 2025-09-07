import pandas as pd
import yfinance as yf

class PullStockData:
    """
    A class to download and organize stock data from yfinance API
    
    StockTickers: list
        List of stock ticker symbols to download data for
    minDate: str
        Start date in format "YYYY-MM-DD"
    maxDate: str
        End date in format "YYYY-MM-DD"
    interval: str, optional
        The interval to download the data for (default is '1mo').
        Valid intervals: 1m,2m,5m,15m,30m,60m,90m,1h,1d,5d,1wk,1mo,3mo
    """
    def __init__(self, StockTickers, minDate, maxDate, interval='1mo'):
        self.__StockTickers = StockTickers
        self.__minDate      = minDate
        self.__maxDate      = maxDate
        self.__interval     = interval
        
        # List of stock metrics to download
        self.__stockMetrics = ["Close"]
        
        # Run the data model
        self.__pullData() # pull the data
        
    def __pullData(self):
        """
        Downloads stock data for each stock in `StockTickers` and 
        concatenates it into one dataframe `PulledStocks`
        """
        

        # Get the stock data for the specified date range
        PulledStocks = yf.download(
            self.getStockTickers(), 
            start   = self.getMinDate(), 
            end     = self.getMaxDate(), 
            interval= self.getInterval()
        )
        
        
        selected_columns = PulledStocks.filter(regex="Date|Close|Stock").columns
        PulledStocks = PulledStocks[selected_columns]

        # Transform the DataFrame: stack the ticker symbols to create a multi-index (Date, Ticker), then reset the 'Ticker' level to turn it into a column
        PulledStocks = PulledStocks.stack(level=0, future_stack=True).rename_axis(['Date', 'Ticker']).reset_index()

        # Now 'Ticker' is a column, and we can drop it
        PulledStocks = PulledStocks.drop(columns=['Ticker'])

        # Pivot all numeric columns into a single column 'adjClose' with the names in 'stock'
        PulledStocks = PulledStocks.melt(
            id_vars=['Date'],                # Keep 'Date' as an identifier
            var_name='stock',                # New column name for stock identifiers
            value_name='adjClose'            # New column name for all numeric values
        )

        # Rename 'Date' to 'period'
        PulledStocks = PulledStocks.rename(columns={'Date': 'period'})  

        # Assign to member variable and drop na
        self.__PulledStocks = PulledStocks.dropna()

        
    # Getters for private member variables ------------------------------------
    def getData(self):
        """
        Returns the pulled stock data
        """
        return self.getPulledStocks()
    
    
    def getStockTickers(self):
        """
        Returns StockTickers 
        """
        return self.__StockTickers 
    
    
    def getMinDate(self):
        """
        Returns minDate 
        """
        return self.__minDate      
    
    
    def getMaxDate(self):
        """
        Returns maxDate 
        """
        return self.__maxDate      
    
    
    def getInterval(self):
        """
        Returns interval 
        """
        return self.__interval     
    
    
    def getStockMetrics(self):
        """
        Returns stockMetrics 
        """
        return self.__stockMetrics 
    
    
    def getPulledStocks(self):
        """
        Returns PulledStocks 
        """
        return self.__PulledStocks.copy()

      
# Examples --------------------------------    

#import datetime as dt

#StockTickers = ['FXNAX', 'PTRQX', 'PHIYX']
#maxDate = dt.datetime.today()                    # Max date to pull from
#minDate = maxDate - dt.timedelta(days = 3*365) # Min date to pull from


#stockDataPuller = PullStockData(StockTickers, minDate, maxDate)
#RawStockData = stockDataPuller.getData()

