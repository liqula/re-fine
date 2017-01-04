module Refine.Backend.Server where

import Refine.Backend.App
import Refine.Common.Rest
import Servant



backend :: ServerT RefineAPI (App db)
backend = error "backend"
