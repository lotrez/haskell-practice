import Lib (  someFunc, stepAction, startCycle )
     
import Control.Monad

main :: IO ()
main = do 
    stepTest
    cycleTest

stepTest :: IO ()
stepTest = do
    putStrLn "Testing the step function"
    when (stepAction 2 /= 1) $ error "stepFunctionError"
    when (stepAction 3 /= 10) $ error "stepFunctionError"
    when (stepAction 10 /= 5) $ error "stepFunctionError"
    when (stepAction 6 /= 3) $ error "stepFunctionError"
    when (stepAction 1 /= 4) $ error "stepFunctionError"

cycleTest :: IO ()
cycleTest = do
    putStrLn "Testing the cycle function"
    when (startCycle 2 [] /= [2,1]) $ do 
        print (startCycle 2 [])
        error "cycleFunctionError"
    when (startCycle 3 [] /= [3,10,5,16,8,4,2,1]) $ do
        print (startCycle 3 [])
        error "cycleFunctionError"
    when (startCycle 10 [] /= [10,5,16,8,4,2,1]) $ error "cycleFunctionError"
    when (startCycle 6 [] /= [6,3,10,5,16,8,4,2,1]) $ error "cycleFunctionError"
    when (startCycle 1 [] /= [1]) $ error "cycleFunctionError"