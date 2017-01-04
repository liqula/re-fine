module Refine.Backend.Server where

import Refine.Api.Rest
import Refine.Backend.App
import Servant



backend :: ServerT RefineAPI (App db)
backend = error "backend"
