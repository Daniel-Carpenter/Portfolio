import time
import sys
from datetime import datetime, timedelta

def updateProgressBar(startTime, iteration, totalIterations):
    
    # Calculate the elapsed time
    timeElapsed = time.time() - startTime

    # Calculate the average time per iteration
    averageTimePerIteration = timeElapsed / (iteration + 1) if iteration > 0 else timeElapsed

    # Estimate the remaining time
    estimatedTimeRemaining = averageTimePerIteration * (totalIterations - (iteration + 1)) if iteration > 0 else 0

    # Split the elapsed time into hours, minutes, and seconds
    hoursElapsed, remainder = divmod(timeElapsed, 3600)
    minutesElapsed, secondsElapsed = divmod(remainder, 60)

    # Split the remaining time into hours, minutes, and seconds
    hoursRemaining, remainder = divmod(estimatedTimeRemaining, 3600)
    minutesRemaining, secondsRemaining = divmod(remainder, 60)

    # Calculate the progress (0.0 to 1.0)
    progress = iteration / totalIterations if totalIterations != 0 else 1

    # Create a string that represents the progress bar
    bar = "=" * int(progress * 50)

    # Calculate the estimated completion time
    endTime = time.time() + estimatedTimeRemaining
    endDatetime = datetime.now() + timedelta(seconds=int(estimatedTimeRemaining))
    endDatetimeStr = endDatetime.strftime('%H:%M:%S')

    # Update the progress bar in the console
    sys.stdout.write(f"\r[{bar:<50}] {progress * 100:.0f}% | Elapsed ({int(hoursElapsed):02d}:{int(minutesElapsed):02d}:{int(secondsElapsed):02d}) "
                     f"| Remaining ({int(hoursRemaining):02d}:{int(minutesRemaining):02d}:{int(secondsRemaining):02d}) "
                     f"| Finish Time ({endDatetimeStr})")
    sys.stdout.flush()
    #time.sleep(0.5) # use for testing




# Examples -----------------------------

#startTime = time.time()

#totalSimulations = 100
#for simulation in range(totalSimulations):
#    updateProgressBar(startTime, simulation, totalSimulations)

#print("\nDone!")
