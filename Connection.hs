module Connection where

import DSL.Execute
import Control.Exception


withConnection :: (ConnectionObject -> IO a) -> IO a
withConnection f = bracket (newConnection "localhost" 9200) closeConnection f
