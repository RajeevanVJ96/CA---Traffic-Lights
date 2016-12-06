module TrafficLightRun where
import HDL.Hydra.Core.Lib
import TrafficLight

main :: IO()
main = do
  run_trafficLight testdata
  run_pedestCrossing testdata1

testdata :: [[Int]]
testdata = [[1],[0],[0],[0],[0],[0],[0],[0],[0],[0],[0],
            [0],[0],[0],[0],[1],[0],[0],[0],[0],[0],[0]]

-- Simulation driver for traffLight

run_trafficLight :: [[Int]] -> IO ()
run_trafficLight input = runAllInput input output
   where
   -- Extract input signals from simulation input
    reset = getbit input 0
    -- Connect the circuit to its inputs and outputs
    (green, amber, red) = controller1 reset
    -- Format the simulation output
    output = 
        [string "Input: reset = ", bit reset, 
        string " Output: green = ",bit green, string " Output: amber = ",bit amber, string " Output: red = ", bit red]
   
testdata1 :: [[Int]]
testdata1 = [[1,0], [0,0], [0,0], [0,0], [0,0], [0,0], [0,0], [0,0],
             [0,0], [0,0], [0,0], [0,0], [0,1], [0,0], [0,0], [0,0],
             [0,1], [0,0], [0,0], [0,1], [0,0], [0,0], [0,0], [0,1],
             [0,0]]

run_pedestCrossing :: [[Int]] -> IO()
run_pedestCrossing input = runAllInput input output2
   where
     reset = getbit input 0
     walkRequest = getbit input 1
     (green, amber, red, wait, walk, walkCount) = controller2 reset walkRequest
     output2 = [string "Input reset: ", bit reset, string " walkRequest: ", bit walkRequest,
       string " :: Output: green: ", bit green, string " amber: ", bit amber, string " red: ", bit red,
       string " || wait: ", bit wait, string " walk: ", bit walk,
       string " :: WalkCount: ", hex walkCount]      


