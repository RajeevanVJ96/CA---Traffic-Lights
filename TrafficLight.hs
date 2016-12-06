module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register


{-The state transitions are:
green -> amber
amber -> red
red -> amber
amber -> green-}

controller1 :: CBit a => a -> (a,a,a)                            
controller1 reset = (green, amber, red)                 
    where
        reset' = inv reset
        green1 = dff(or2 reset amber2)                   
        green2 = dff(and2 reset' green1)
        green3 = dff(and2 reset' green2) 
        amber1 = dff(and2 reset' green3)
        red1 = dff(and2 reset' amber1)
        red2 = dff(and2 reset' red1)
        red3 = dff(and2 reset' red2)
        red4 = dff(and2 reset' red3)
        amber2 = dff(and2 reset' red4)

        green = orw[green1, green2, green3]
        amber = orw[amber1, amber2]
        red = orw[red1, red2, red3, red4]




{-The state transitions for traffic lights are:
green -> amber
amber -> red
red -> amber
amber -> green

The state transitions for the pedestrian crossings are:
wait -> walk
walk -> wait
-}

controller2 :: CBit a => a -> a -> (a,a,a,a,a,[a])
controller2 reset walkRequest = (green, amber, red, wait, walk, walkCount)
    where
        walkCount = walkCountCircuit reset walkRequest
        green = dff(or3 reset amber2 (and2 walkRequest' green))
        amber1 = dff(and2 walkRequest green)
        red1 = dff(and2 walkRequest' amber1)
        red2 = dff(and2 walkRequest' red1)
        red3 = dff(and2 walkRequest' red2)
        amber2 = dff(and2 walk red3)


        wait = or3 green amber2 amber
        walk = or3 red1 red2 red3
        red = orw [red1, red2, red3]
        amber = orw [amber1, amber2]
        walkRequest' = inv walkRequest
        reset' = inv reset

walkCountCircuit :: CBit a => a -> a -> [a]
walkCountCircuit reset walkRequest = walkCount
   where
     walkzero = fanout 16 zero
     walkone = boolword 16 one
     (carryOut, walkCountAddOne) = rippleAdd zero(bitslice2 walkCount walkone)
     walkCountLd = mux2w (walkRequest, reset) walkCount walkzero walkCountAddOne walkzero
     walkCount = wlatch 16 walkCountLd

