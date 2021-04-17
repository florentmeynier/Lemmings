module Mouse where

import SDL

import Model (GameState)
import qualified Model as M

handleEvent :: [Event] -> GameState -> IO()
handleEvent events gs@(M.GameState px py _ _ _) = aux events where
    aux [] = putStr ""
    aux (h:xs) = case eventPayload h of
                    MouseButtonEvent e -> case mouseButtonEventPos e of
                        SDL.P (V2 x y) -> if fromIntegral x >= px && fromIntegral y >= py && fromIntegral x <= px + 100 && fromIntegral y <= py + 100 then putStrLn "Touché !" else aux xs 
                    _ -> aux xs