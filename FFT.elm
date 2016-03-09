-- FFT.elm
-- A simple Fast Fourier Transform implementation in Elm

module FFT where

import Array
import Complex exposing (Complex, cMul, cSub, cAdd, cExp)


--fft : Array.Array Complex -> Array.Array Complex
--fft data = 
--    let n = Array.length data in 
--    if n == 1
--        then data
--    else 
--        let transformed = Array.append (fft <| evens data) (fft <| odds data)
--            twiddle k = Complex (cCos (2 * pi / (toFloat n))) (-1 *  sin (2 * pi * (toFloat k) / (toFloat n))) 
--        in 
--        Array.append 
--            Array.map (Array.map (\x y -> cAdd x (cMul (twiddle x) y)) (Array.slice 0 n//2 transformed)) (Array.slice n//2 n transformed)  
--            Array.map (Array.map (\x y -> cSub x (cMul (twiddle x) y)) (Array.slice 0 n//2 transformed)) (Array.slice n//2 n transformed)  



--odds : Array.Array a -> Array.Array a
--odds data =
--    let indices = List.map (\x -> 2*x - 1) [0..(Array.length data)//2] in
--    Array.fromList 
--        <| List.filterMap identity
--        <| List.map (\x -> Array.get x data) indices

--evens : Array.Array a -> Array.Array a
--evens data =
--    let indices = List.map (\x -> 2*x) [0..(Array.length data)//2] in
--    Array.fromList 
--        <| List.filterMap identity
--        <| List.map (\x -> Array.get x data) indices


listGetAt : Int -> List a -> a
listGetAt idx list = 
    case List.head <| List.drop idx list of 
        Nothing -> Debug.crash "listGetAt called beyond end of list"
        Just a -> a

dft : List Complex -> List Complex
dft data = 
    let length = List.length data
        foo len k xs = 
            let twiddle k n len = cMul Complex.i <| Complex ( -2 * pi * (toFloat k) * (toFloat n) / (toFloat len) ) 0
                dataPt n = listGetAt n xs
            in
            List.foldl (cAdd) (Complex 0 0) 
            <| List.map (\n -> Debug.log "" <| cMul (cExp (twiddle k n len )) (dataPt n)) [0..len-1]
    in
        List.map2 (foo length) [0..length-1] (List.repeat length data)
