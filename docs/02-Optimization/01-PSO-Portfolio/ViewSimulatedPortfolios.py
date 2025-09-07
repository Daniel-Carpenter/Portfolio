# PACKAGES 
# -----------------------------------------------------------------------------
import pandas   as pd
import numpy    as np
import matplotlib.pyplot as plt
from   matplotlib.ticker import PercentFormatter
import seaborn as sns
import plotly.express as px
import plotly.graph_objects as go
import os


# Function that processes the results of a SimulatedPSO object

class ViewSimulatedPortfolios():

    # Initializer for object
    def __init__(self, 
                 SimulateOptimalPortfolios, 
                 outputLocation = 'Output_Data/',
                 displayResults  = True
                 ):

        # Store input paramters for encapsulation
        self.__SimulateOptimalPortfolios = SimulateOptimalPortfolios
        self.__outputLocation = outputLocation
        self.__displayResults = displayResults
        
        
        # OUTPUT LOCATION -----------------------------------------------------
        
        # Name of the simulation output sub folder containing the date
        self.__outputSubFolder = str(self.__getSimulateOptimalPortfolios().getSimulationEndTime().strftime("%Y-%m-%d %H_%M")) + '/' 
        
        # Create the sub folder to save output to 
        self.__createDirectory(self.__getOutputLocation())                             # ensure the first layer is created
        self.__createDirectory(self.__getOutputLocation() + self.__getOutputSubFolder()) # Create the simulation folder


        # INPUT PARAMTERS -----------------------------------------------------

        # Save the  input paramters
        self.__InputParams = pd.DataFrame({

            # Stock data
            'StockTickers':       [self.__getSimulateOptimalPortfolios().getStockTickers()],
            'minDate':            self.__getSimulateOptimalPortfolios().getMinDate(),
            'maxDate':            self.__getSimulateOptimalPortfolios().getMaxDate(),

            # Important calcs
            'totalRunTime':       self.__getSimulateOptimalPortfolios().getTotalRunTime(),
            'totalSimulations':   self.__getSimulateOptimalPortfolios().getTotalSimulations(),
            'totalIterations':    self.__getSimulateOptimalPortfolios().getTotalIterations(),
            'numPorfolios':       self.__getSimulateOptimalPortfolios().getNumPorfolios(),
            'minDesiredReturn':   self.__getSimulateOptimalPortfolios().getMinDesiredReturn(),
            'evalGoal':           self.__getSimulateOptimalPortfolios().getEvalGoal(),
            'method':             self.__getSimulateOptimalPortfolios().getMethod(),
            'ROUND_VALUE_TO':     self.__getSimulateOptimalPortfolios().getRoundToValue(),
            
            # Less changed ones
            'phi1':                self.__getSimulateOptimalPortfolios().getPhi1(),
            'phi2':                self.__getSimulateOptimalPortfolios().getPhi2(),
            'intertiaWeight':      self.__getSimulateOptimalPortfolios().getIntertiaWeight(),
            'absoluteMoveLimit':   self.__getSimulateOptimalPortfolios().getAbsoluteMoveLimit(),
            'investmentThreshold': self.__getSimulateOptimalPortfolios().getInvestmentThreshold(),
            'printDims':           self.__getSimulateOptimalPortfolios().getPrintDims(),
            'ANNUALIZER_VALUE':    self.__getSimulateOptimalPortfolios().getAnnualizerValue()
        }).transpose()
        self.__InputParams.columns = ['Parameter Value'] # Column labels of above dataframe
        


        # MISC STATIC VARS ----------------------------------------------------

        # Labels for plotting
        self.__MIN_RISK_SIM_NAME, self.__MAX_SHARPE_SIM_NAME, self.__NON_OPTIMAL_SIM_NAME = ['Min Risk Simulation', 
                                                                                       'Max Sharpe Simulation', 
                                                                                       'Non-Optimal Simulations']
        # Store labels as list
        self.__simLabels = [self.__NON_OPTIMAL_SIM_NAME, self.__MIN_RISK_SIM_NAME, self.__MAX_SHARPE_SIM_NAME]

        # Colors used in the palette
        self.__gray, self.__green, self.__magenta = ['#EAEAEA', '#54AC88', '#B07AA1']


        # CALCULATIONS UPON INITALIZATION -------------------------------------

        # Get simulation stats for portfolios and expected returns
        self.__SimulatedPortfoliosStats      = self.__createDataFrameStats(self.__getSimulateOptimalPortfolios().getSimulatedPortfolios())
        self.__SimulatedExpectedReturnsStats = self.__createDataFrameStats(self.__getSimulateOptimalPortfolios().getSimulatedExpectedReturns())

        # Identify the best sharpe and min risk portfolios among all simularions
        self.__getOptimalPortfolios()
        
        # Pivot data for line charts
        self.__prepDataForLineCharts()


        
    # =========================================================================
    # DATA VALIDATION 
    # =========================================================================

    # Function for validation stats - this is to ensure any rounding doesn't cause any issues.
    def checkSumOfWeights(self):
        # Max and mins sum weights 
        print('Max Sum of Weights\t', max(self.__getSimulateOptimalPortfolios().getSimulatedPortfolios().apply(sum)) )
        print('Min Sum of Weights\t', min(self.__getSimulateOptimalPortfolios().getSimulatedPortfolios().apply(sum)) )

        # Min or max investment in each stock across all simulations 
        print('\nMin investment in each stock\n', self.__getSimulateOptimalPortfolios().getSimulatedPortfolios().apply(min, axis = 1))
        print('\nMax investment in each stock\n', self.__getSimulateOptimalPortfolios().getSimulatedPortfolios().apply(max, axis = 1))



    # =========================================================================
    # DATA PREP FOR PLOTS 
    # =========================================================================

    # Function which returns the index of minimum value in the list
    def __calcOptimalPortfolios(self, inputList, 
                                methodName = 'risk'):
    
        # Using the simulated portfolios...
        df_target = self.__getSimulateOptimalPortfolios().getSimulatedPortfolios()

        # Update function and title depending on risk or sharpe being found
        if methodName == 'risk': 
            funUsed     = min # finding the min risk
            titlePrefix = 'Minimum Risk'
        else: 
            funUsed     = max # finding the max sharpe ratio
            titlePrefix = 'Maximum Sharpe'
            
        # Get the minimum value in the list
        valueFromFunction = funUsed(inputList)
    
        # Return the index of minimum value 
        foundIndex = inputList.index(valueFromFunction)
        
        # Find the optimal portfolio
        OptimalPortfolio = df_target.iloc[:, foundIndex]    
        
        # Display results if chosen
        if self.__getDispayResults():
            # Pull values
            risk_val   = 100 * round(self.__getSimulateOptimalPortfolios().getSimulatedRisks()[foundIndex],   3)
            ret_val    = 100 * round(self.__getSimulateOptimalPortfolios().getSimulatedReturns()[foundIndex], 3)
            sharpe_val =        round(self.__getSimulateOptimalPortfolios().getSimulatedSharpe()[foundIndex], 3)
            n_tickers  = self.__getSimulateOptimalPortfolios().getSimulatedNumTickers()[foundIndex]

            # Summary as a Markdown table
            summary_df = pd.DataFrame({
                "Risk":         [f"{risk_val:.1f}%"],
                "Return":       [f"{ret_val:.1f}%"],
                "Sharpe":       [f"{sharpe_val:.3f}"],
                "Num. Tickers": [n_tickers]
            })

            print(f"\n{titlePrefix} Portfolio over all Simulations")
            print(summary_df.to_markdown(index=False))

            # Optimal weights as Markdown (Series -> markdown)
            try:
                weights_md = OptimalPortfolio.to_markdown()
            except AttributeError:
                weights_md = pd.Series(OptimalPortfolio).to_markdown()

            print("\nOptimal Weights:\n", weights_md)


        # Return the index of the sharpe or risk portfolio, as well the optimal portfolio weights
        return [foundIndex, OptimalPortfolio]


    # Function that identifies the max sharpe and the min risk portfolios over all simulations
    def __getOptimalPortfolios(self):
        
        # Get the column number that has the best min risk over all simulations
        self.__minRiskIdx,   self.__MinRiskPortfolio   = self.__calcOptimalPortfolios(inputList = self.__getSimulateOptimalPortfolios().getSimulatedRisks(),  methodName ='risk')
        self.__maxSharpeIdx, self.__MaxSharpePortfolio = self.__calcOptimalPortfolios(inputList = self.__getSimulateOptimalPortfolios().getSimulatedSharpe(), methodName ='sharpe')

        # Get the risk and return for the min risk portfolio over all simulations
        self.__riskOfMinRisk   = self.__getSimulateOptimalPortfolios().getSimulatedRisks()[self.__getMinRiskIdx()]
        self.__returnOfMinRisk = self.__getSimulateOptimalPortfolios().getSimulatedReturns()[self.__getMinRiskIdx()]

        # Get the risk and return for the max sharpe portfolio over all simulations
        self.__riskOfMaxSharpe     = self.__getSimulateOptimalPortfolios().getSimulatedRisks()[self.__getMaxSharpeIdx()]
        self.__returnOfOfMaxSharpe = self.__getSimulateOptimalPortfolios().getSimulatedReturns()[self.__getMaxSharpeIdx()]



    # Function to create a boxplot of the standard deviation across all tickers
    def __createBoxPlotOfStd(self, df, titleVar='Standard Deviation of Weight Allocation over all Simulations (for each Ticker)',
                             valueVariable="std"): 
        
        # Create the boxplot
        fig_boxplot = px.box(df, 
                            x      = valueVariable, 
                            points = "all", # Show the points under the boxplot
                            color_discrete_sequence=px.colors.qualitative.T10)

        # Aesthetics and titles
        fig_boxplot.update_layout(
            title       = titleVar,
            xaxis_title = 'Standard Deviation (Pct. Pts.)',
            margin      = dict(l=0, r=20, t=40, b=20),
            autosize    = False,
            width       = 900,
            height      = 200,
            paper_bgcolor = "White",
            plot_bgcolor = 'White'
        )

        # Grid color
        fig_boxplot.update_xaxes(gridcolor='lightsteelblue')
        
        fig_boxplot.show()
        print()
        


    # Function to create a heatmap of data
    def __createStatsHeatmap(self, df, height=7, width=16):
        fig_heatmap, ax = plt.subplots(figsize=(width, height))
        sns.heatmap(df, annot=True)
        plt.show()
        

    
    # Function to apply stats functions to a dataframe for variance evaluation
    def __createDataFrameStats(self, df):
        
        # Function and column names to apply 
        statNames      = ['median', 'std',  'max',  'min'  ]
        statsFunctions = [np.median, np.std, np.max, np.min]
        
        df = df.copy().dropna()
        df = df.select_dtypes(include=np.number)
        
        # Create data frame of stats of all the simulations
        df_stats = pd.DataFrame()
        for (statName, statFun) in zip(statNames , statsFunctions):
            df_stats[statName] = df.apply(statFun, axis=1) 
        
        df_stats['range'] = df_stats['max'] - df_stats['min']

        # Convert to percentage points 
        df_stats = df_stats * 100
        
        return(df_stats)


    # Function that pivots simulated data to ensure format aligns with plots
    def __prepDataForLineCharts(self):
        # Add the date period as a columns
        self.__getSimulateOptimalPortfolios().getSimulatedExpectedReturns()['Period'] = self.__getSimulateOptimalPortfolios().getSimulatedExpectedReturns().index

        # Pivot the data so that all expected returns are in a single column
        __SimulatedExpectedReturnsPivot = self.__getSimulateOptimalPortfolios().getSimulatedExpectedReturns().melt(id_vars    = 'Period', 
                                                                                                               value_vars = self.__getSimulateOptimalPortfolios().getSimulatedExpectedReturns().columns, 
                                                                                                               var_name   = 'Simulation_Num', 
                                                                                                               value_name = 'Expected_Return')

        # Identify the min and max risk soluation so that we can visually see it
        minRiskSimulationNum   = self.__getSimulateOptimalPortfolios().getSimulationColumnPrefixName() + str(self.__getMinRiskIdx())
        maxSharpeSimulationNum = self.__getSimulateOptimalPortfolios().getSimulationColumnPrefixName() + str(self.__getMaxSharpeIdx())

       

        # Label the data frame with risk or sharpe or non optimal
        __SimulatedExpectedReturnsPivot['simulationDescription'] = np.where(__SimulatedExpectedReturnsPivot['Simulation_Num'] == minRiskSimulationNum,
                                                                        self.__getMinRiskSimName(),
                                                                        np.where(__SimulatedExpectedReturnsPivot['Simulation_Num'] == maxSharpeSimulationNum,
                                                                                self.__getMaxSharpeSimName(),
                                                                                self.__getNonOptimalSimName())
                                                                    )

        # Sort the data        
        self.__SimulatedExpectedReturnsPivot = __SimulatedExpectedReturnsPivot.sort_values(by = ['Simulation_Num', 'Period', 'simulationDescription'])



    # =========================================================================
    # HELPER PLOT FUNCTIONS 
    # =========================================================================

    # Core function to create line chart that highlights max sharpe and min risk portfolios
    # Juxtaposed to other data, whether it be the non-optimal simulations (as an input) or the 
    # historically realized returns of the stock tickers.
    def __createHighlightedLineChart(self, 
                                     df, 
                                     xVarName      = 'Period', 
                                     yVarName      = 'Expected_Return', 
                                     colorName     = 'simulationDescription', 
                                     minDetailName = 'Simulation_Num',
                                     titleName     = 'Optimal Portfolio vs. all Other Simulated Portfolios',
                                     colorValues   = ['#EAEAEA', '#54AC88', '#B07AA1'],
                                     transparencyLevel = 0.5
                                     ):
        
        # labelVarName = self.__getSimulationLabels()
        
        # Ensure no duplcated index
        numDuplicateIndices = len(df[df.index.duplicated()])
        if numDuplicateIndices > 0:
            df = df[~df.index.duplicated()]
            print('Note removed', numDuplicateIndices, 'duplicate indices after concatention.')
        
        # If the min risk portfolio is the same as the sharpe portfolio
        if (len(df[colorName].unique()) == 2):
            colorValues = colorValues[:1]
            print('Note that the min risk portfolio is the same as the sharpe portfolio!')

        # Increase font size for all text elements in the chart
        sns.set(font_scale=1.5)

        # Background style
        sns.set_style("whitegrid")
        
        # size of the chart
        fig_line, ax = plt.subplots(figsize=(8, 5.5))
        
        # Create a line chart
        ax = sns.lineplot(x = xVarName, y = yVarName, 
                          hue = colorName, palette = colorValues, 
                          linewidth = 2.5,
                          units = minDetailName, estimator = None,
                          alpha = transparencyLevel,
                          data  = df)

        # Create a title for the header
        plt.title(titleName)

        # Axis aesthetics
        ax.axhline(y=0, color=self.__getGrayColor(), linestyle="-")           # Add horizontal line at a specific point on the y axis
        ax.yaxis.set_major_formatter(PercentFormatter(xmax=1)) # Percentage on y axis
        ax.yaxis.grid(False)                                   # No y axis grid

        # Label x and y axis
        plt.xlabel(xVarName)
        plt.ylabel(yVarName)
        plt.legend(title='Simulation Description')

        # Save the plot
        fig_line.get_figure().savefig(self.__getOutputLocation() + self.__getOutputSubFolder() + titleName + ".png", 
                                    dpi=300, bbox_inches='tight')

        
        # Show the plot
        plt.show()
        
        
        
    # Core function to plot the Risk vs. Return Scatter dynamic plot
    def __createRiskVsReturnVsSharpe(self, 
                                     xVarName          = "Risk (Standard Deviation of the Portfolio)", 
                                     yVarName          = "Expected Return", 
                                     titleName         = "Risk vs. Return of All Simulated Portfolios", 
                                     legendName        = 'Sharpe Ratio',
                                     continuousPalette = sns.color_palette("ch:start=.2,rot=-.3", as_cmap=True),                            
                                     minRiskColor      = '#54AC88', 
                                     maxSharpeColor    = '#B07AA1',
                                     transparencyLevel = 0.75
                                     ):

        # Consolidate in data frame for plotly
        ScatterData = pd.DataFrame({
            'Simulation_Number': ['Simulation ' + str(simulationNum) for simulationNum in range(len(self.__getSimulateOptimalPortfolios().getSimulatedRisks()))],
            'x':    self.__getSimulateOptimalPortfolios().getSimulatedRisks(), 
            'y':    self.__getSimulateOptimalPortfolios().getSimulatedReturns(), 
            'color':np.round(self.__getSimulateOptimalPortfolios().getSimulatedSharpe(), 3)
        })
        
        # Continous palette
        continuousPalette = px.colors.sequential.Teal

        # Create a scatter plot
        fig = px.scatter(ScatterData, 
                        x='x', y='y', color='color',
                        color_continuous_scale=continuousPalette,
                        hover_data=['Simulation_Number'],
                        opacity=transparencyLevel,
                        labels={"x": "Risk (Standard Deviation of the Portfolio)",
                                "y": "Expected Return",
                                "color": "Sharpe Ratio"})

        # Increase size of market
        fig.update_traces(marker=dict(size=10))

        # Add the min risk and max sharpe ratio portfolio ------------------------------
        fig.add_trace(go.Scatter(x=[self.__getRiskOfMinRisk()], y=[self.__getReturnOfMinRisk()],
                                marker=dict(symbol="x", color=self.__getGreenColor(), size = 20),
                                hoverinfo='text', text=['Min. Risk Portfolio'],
                                name="Min Risk Portfolio"))
        
        fig.add_trace(go.Scatter(x=[self.__getRiskOfMaxSharpe()], y=[self.__getReturnOfOfMaxSharpe()],
                                marker=dict(symbol="star",color=self.__getMagentaColor(), size = 20),
                                hoverinfo='text', text=['Max. Sharpe Portfolio'],
                                name="Max Sharpe Portfolio"))

        # Titles and other aesthetics --------------------------------------
        fig.update_layout(
                font=dict(size=16),
                title       = titleName,
                xaxis_title = xVarName,
                yaxis_title = yVarName,
                autosize    = True,
                paper_bgcolor = "White",
                plot_bgcolor  = 'White',
                legend=dict(
                    yanchor="top",
                    y=0.95,
                    xanchor="left",
                    x=0.01
                )
            )
        
        # Axis'
        fig.update_xaxes(gridcolor  = 'lightgray') # Grid color
        fig.update_yaxes(tickformat = ".2%")            # Percentage on y axis
        fig.update_xaxes(tickformat = ".2%")            # Percentage on y axis

        # Show the plot
        fig.show()
        fig.write_image(self.__getOutputLocation() + self.__getOutputSubFolder() + titleName +".png",
                    height=900, width=1600)



    # =========================================================================
    # AVAILABLE PLOTTING FUNCTIONS
    # =========================================================================

    # Function to plot variance among simulated portfolios and their stats
    def plotPortfolioVarianceStats(self, statsVariables = ['median', 'min', 'max'], figHeight=5, figWidth=3):

        # Create heatmap of each investable fund
        self.__createBoxPlotOfStd(self.__getSimulatedPortfoliosStats())
        self.__createStatsHeatmap(self.__getSimulatedPortfoliosStats().filter(['std']), height=figHeight, width=figWidth)

        # Create heatmap of each investable fund
        self.__createBoxPlotOfStd(self.__getSimulatedPortfoliosStats(), 
                        titleVar='Standard Deviation of E(r) over all Simulations (for each Simulation)')
        self.__createStatsHeatmap(self.__getSimulatedPortfoliosStats().dropna().filter(statsVariables), height=figHeight, width=figWidth)

        
    # Function to plot the Optimal portfolios (min risk and max sharpe over all simulations)
    # vs. the non-optimal simulated portfolios
    def plotOptimalVsRealizedReturns(self):
        
        # Clean up returns data to be in long format ---------------------------------------------------------
        # Add the monthly period as a columns
        ReturnsPivot = self.__getSimulateOptimalPortfolios().getRanStockFinanceMPT().getReturns()
        ReturnsPivot['Period'] = ReturnsPivot.index

        # Pivot the data so that all expected returns are in a single column
        ReturnsPivot = ReturnsPivot.melt(id_vars = 'Period', 
                                        value_vars = self.__getSimulateOptimalPortfolios().getStockTickers(), 
                                        var_name   = 'Ticker', 
                                        value_name = 'Expected_Return')

        ReturnsPivot['isOptimal'] = self.__getNonOptimalSimName()


        # Make df that contains only the optimal prtfolio in above column naming -----------------------------
        # Only optimal portfolio
        TempDataFrame = self.__getSimulatedExpectedReturnsPivot().dropna() # Created temp dataframe for readability of filtering below
        SimulatedExpectedReturnsPivotOpt = TempDataFrame[TempDataFrame['simulationDescription'] != self.__getNonOptimalSimName()]

        # Rename to match each other
        SimulatedExpectedReturnsPivotOpt = SimulatedExpectedReturnsPivotOpt.rename(columns = {'Simulation_Num': 'Ticker',
                                                                                            'simulationDescription': 'isOptimal'})

        # Combine realized ticker returns and the optimal portfolio
        OptimalVsTickers = pd.concat([ReturnsPivot.copy(), SimulatedExpectedReturnsPivotOpt.copy()])

        # Create line chart of Max Sharpe and Min Risk Portfolios vs. Realized Returns of Investable Funds ----
        self.__createHighlightedLineChart(OptimalVsTickers, 
                                colorName         = 'isOptimal', 
                                titleName         = 'Max Sharpe and Min Risk Portfolios vs. Realized Returns of Investable Funds',
                                minDetailName     = 'Ticker',
                                transparencyLevel = 0.66)
        
        
        
    # Function to create Line Plot with Optimal Portfolio vs. all Other Simulated Portfolios
    def plotOptimalVsNonOptSimulations(self):
        self.__createHighlightedLineChart(self.__getSimulatedExpectedReturnsPivot())



    # Function to plot the efficient frontier of best simulated portfolios
    def plotRiskVsReturnVsSharpe(self, TRANSPARENCY_LEVEL=0.5):
        # Create the plotly scatter of risk vs. return vs. sharpe
        self.__createRiskVsReturnVsSharpe(transparencyLevel=TRANSPARENCY_LEVEL)



    # =========================================================================
    # EXPORTING DATA 
    # =========================================================================
    
    # Function to create a director if it does not already exist
    def __createDirectory(self, newDir):
        
        # Create directory of this run
        if not os.path.exists(newDir):
            os.mkdir(newDir)



    # Function to save files as csv
    def __saveToOutputCSV(self, df, fileName):
       
        pathName = self.__getOutputLocation() + self.__getOutputSubFolder() + fileName + '.csv'
        df.to_csv(pathName, index=True)
        print(fileName, 'saved to:\t\t', pathName)



    # Function to exports relevant simulation data
    def exportSimulationData(self):
        
        # output data files
        self.__outputList_df = [self.__getInputParams(),
                              self.__getSimulateOptimalPortfolios().getSimulatedPortfolios(),
                              self.__getSimulatedPortfoliosStats(),
                              self.__getMinRiskPortfolio(),
                              self.__getMaxSharpePortfolio(),
                              self.__getSimulateOptimalPortfolios().getSimulatedExpectedReturns(),
                              self.__getSimulatedExpectedReturnsStats()
                              ]

        self.__outputListNames = ['Input_Parameters',
                                 'All_Simulated_Weights',
                                 'All_Simulated_Weights_Stats',
                                 'Min_Risk_Portfolio',
                                 'Max_Sharpe_Portfolio',
                                 'Expected_Returns',
                                 'Expected_Returns_Stats'
                                 ]
        
        # Save all the files defined above
        for fileNum in range(len(self.__getOutputList_df())):
            self.__saveToOutputCSV(self.__getOutputList_df()[fileNum], self.__getOutputListNames()[fileNum])



    # Function to run all plot outputs at once
    def processAllOutputAtOnce(self):
        
        # Ensure sum of weights for each portfolio is 1
        self.checkSumOfWeights()

        #  Plot the portfolio heatmap and boxplots
        self.plotPortfolioVarianceStats()

        # Create Line Plot with Optimal Portfolio vs. all Other Simulated Portfolios
        self.plotOptimalVsNonOptSimulations()

        # Plot the min risk and max sharpe portfolios over the investable funds
        self.plotOptimalVsRealizedReturns()

        # Plot the scatter of the risk and return data
        self.plotRiskVsReturnVsSharpe()
        
        # Export simulation data as CSV's
        self.exportSimulationData()


    # =========================================================================
    # GETTERS - all are encapsulated privately
    # =========================================================================

    def __getSimulateOptimalPortfolios(self):
        return self.__SimulateOptimalPortfolios 
        
    def __getOutputLocation(self):
        return self.__outputLocation            
        
    def __getOutputSubFolder(self):
        return self.__outputSubFolder    
            
    def __getDispayResults(self):
        return self.__displayResults
        
    def __getInputParams(self):
        return self.__InputParams               
        
    def __getSimulatedPortfoliosStats(self):
        return self.__SimulatedPortfoliosStats  
        
    def __getMinRiskIdx(self):
        return self.__minRiskIdx                
        
    def __getMinRiskPortfolio(self):
        return self.__MinRiskPortfolio
        
    def __getMaxSharpeIdx(self):
        return self.__maxSharpeIdx    
        
    def __getMaxSharpePortfolio(self):
        return self.__MaxSharpePortfolio        
        
    def __getSimulatedExpectedReturnsPivot(self):
        return self.__SimulatedExpectedReturnsPivot 
        
    def __getMinRiskSimName(self):
        return self.__MIN_RISK_SIM_NAME         
        
    def __getMaxSharpeSimName(self):
        return self.__MAX_SHARPE_SIM_NAME       
        
    def __getNonOptimalSimName(self):
        return self.__NON_OPTIMAL_SIM_NAME      
        
    def __getGrayColor(self):
        return self.__gray            
        
    def __getGreenColor(self):
        return self.__green                     
        
    def __getMagentaColor(self):
        return self.__magenta                   
        
    def __getRiskOfMinRisk(self):
        return self.__riskOfMinRisk             
        
    def __getReturnOfMinRisk(self):
        return self.__returnOfMinRisk           
        
    def __getRiskOfMaxSharpe(self):
        return self.__riskOfMaxSharpe           
        
    def __getReturnOfOfMaxSharpe(self):
        return self.__returnOfOfMaxSharpe       
        
    def __getSimulatedExpectedReturnsStats(self):
        return self.__SimulatedExpectedReturnsStats  
        
    def __getOutputListNames(self):
        return self.__outputListNames          
        
    def __getOutputList_df(self) :
        return self.__outputList_df             
    
    def __getSimulationLabels(self):
        return self.__simLabels

    
    
# Examples --------------------------------    

# import datetime        as dt
# import StockFinanceMPT           as mpt # Stock pull from yfinance in current wrkdir
# import SimulateOptimalPortfolios as sop # Run simulations of global best portfolios
# # import ViewSimulatedPortfolios   as vsp # Wrapper class to plot, export, etc., simulations


# StockTickers = ['AAPL', 'TSLA', 'MSFT']
# maxDate = dt.datetime.today()                  # Max date to pull from
# minDate = maxDate - dt.timedelta(days = 3*365) # Min date to pull from

# # Pull stock data
# PulledStockData = mpt.StockFinanceMPT(StockTickers, minDate, maxDate)

# # Create the simulation
# simulatedPortfolios = sop.SimulateOptimalPortfolios(    
#     PulledStockData,               # Using the pulled stock data above
#     totalSimulations    = 100,     # Number of min risk portfolio's to simulate
#     totalIterations     = 10,      # Total number of iterations (or movements within one simulation)
#     numPorfolios        = 5        # Total number of portfolios in a swarm. 
#     )

# simulatedPortfolios.runSimulation() # Run the simulation given parameters above

# # Create the viewer wrapper
# simulationViewer = ViewSimulatedPortfolios(simulatedPortfolios)

# # View all plots and export data
# simulationViewer.processAllOutputAtOnce()

# # Or view each individually
# simulationViewer.checkSumOfWeights()
# simulationViewer.plotPortfolioVarianceStats()
# simulationViewer.plotOptimalVsNonOptSimulations()
# simulationViewer.plotOptimalVsRealizedReturns()
# simulationViewer.plotRiskVsReturnVsSharpe()
# simulationViewer.exportSimulationData()

