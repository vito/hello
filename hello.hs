import System (system)
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)
import System.Time
import Data.Maybe (fromMaybe)
import Control.Concurrent (threadDelay)

type Grid = [(Day, String)]
type Schedule = [(Int, Bool)]

grid :: Grid
grid = [ (Saturday,  "                        ")
       , (Friday,    " - - --- -   -   ---  - ")
       , (Thursday,  " - - -   -   -   - -  - ")
       , (Wednesday, " --- --- -   -   - -  - ")
       , (Tuesday,   " - - -   -   -   - -    ")
       , (Monday,    " - - --- --- --- ---  - ")
       , (Sunday,    "                        ")
       ]

pstTime :: IO ClockTime
pstTime = do time <- getClockTime
             return (addToClockTime (TimeDiff 0 0 0 (-3) 0 0 0) time)

formatTime :: String -> IO String
formatTime f = do time <- pstTime
                  calendar <- toCalendarTime time
                  return $ formatCalendarTime defaultTimeLocale f calendar

toSchedule :: String -> Schedule
toSchedule = zip [0..24] . map (== '-')

getSchedule :: Grid -> IO Schedule
getSchedule g = do day <- formatTime "%A"
                   return . toSchedule . fromMaybe "                        " $ lookup (read day :: Day) g

commit :: IO ()
commit = do time <- formatTime "%a %I %p"
            system "echo >> blank"
            system ("git commit -a -m \"" ++ time ++ "\"")
            system "git push origin master"
            return ()

waitOneHour :: IO ()
waitOneHour = threadDelay (1000000 * 60 * 60)

loop :: Grid -> IO ()
loop g = do args <- getArgs

            if "wait" `elem` args
               then waitOneHour
               else return ()
            
            schedule <- getSchedule grid
            h <- formatTime "%H"

            let hour = read h :: Int

            if (lookup hour schedule) == Just True
                then do putStrLn $ "Hour is now \"" ++ show hour ++ "\". Committing:"
                        commit
                else putStrLn $ "Hour is now \"" ++ show hour ++ "\"."
            
            if not ("wait" `elem` args)
               then waitOneHour
               else return ()
            
            loop g

main = loop grid

