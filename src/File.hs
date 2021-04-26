module File where

import Map

saveNiveau :: Niveau -> String -> IO ()
saveNiveau n file = do
    writeFile (file ++ ".niv") (show n)

loadNiveau :: String -> IO Niveau
loadNiveau file = do
    txt <- readFile (file ++ ".niv")
    return $ read txt

-- >>> saveNiveau (initNiveau 24 10 20) "data/test"
-- >>> loadNiveau "data/test"


