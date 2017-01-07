module Refine.Backend.Server where

import Prelude hiding ((.), id)
import Control.Category

import Control.Monad.Except
import Data.String.Conversions (cs)

import Refine.Backend.App
import Refine.Backend.Database
import Refine.Backend.Natural
import Refine.Common.Rest
import Refine.Common.Types
import Servant

import Network.Wai.Handler.Warp as Warp


startBackend :: IO ()
startBackend = do

  runDb <- createDBRunner $ DBOnDisk "refine.db" -- DBInMemory

  Warp.runSettings Warp.defaultSettings
    $ Servant.serve (Proxy :: Proxy RefineAPI) (serverT runDb refineApi)


serverT :: RunDB m -> ServerT RefineAPI (App m) -> Server RefineAPI
serverT runDb = enter (toServantError . cnToSn (runApp runDb))


toServantError :: (Monad m) => ExceptT AppError m :~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> either (throwError . fromAppError) pure)
  where
    fromAppError :: AppError -> ServantErr
    fromAppError (AppError msg) = err500 { errBody = cs msg }


refineApi :: ServerT RefineAPI (App db)
refineApi =
        listVDocInfos
  :<|>  getVDoc
  :<|>  addVDoc
  :<|>  putTitle
  :<|>  putAbstract
  :<|>  noteVisibility
  :<|>  rmVDoc
  :<|>  getVersion
  :<|>  addPatch
  :<|>  rmPatch
  :<|>  mergePatch
  :<|>  rebasePatch
  :<|>  addComment
  :<|>  rmComment
  :<|>  addNote
  :<|>  rmNote
  :<|>  setVote
  :<|>  rmVote

listVDocInfos :: ServerT ListVDocInfos (App db)
listVDocInfos = undefined

getVDoc :: ServerT GetVDoc (App db)
getVDoc _ = return $ VDoc "title" "desc"

addVDoc :: ServerT AddVDoc (App db)
addVDoc = undefined

putTitle :: ServerT PutTitle (App db)
putTitle = undefined

putAbstract :: ServerT PutAbstract (App db)
putAbstract = undefined

noteVisibility :: ServerT NoteVisibility (App db)
noteVisibility = undefined

rmVDoc :: ServerT RmVDoc (App db)
rmVDoc = undefined

getVersion :: ServerT GetVersion (App db)
getVersion = undefined

addPatch :: ServerT AddPatch (App db)
addPatch = undefined

rmPatch :: ServerT RmPatch (App db)
rmPatch = undefined

mergePatch :: ServerT MergePatch (App db)
mergePatch = undefined

rebasePatch :: ServerT RebasePatch (App db)
rebasePatch = undefined

addComment :: ServerT AddComment (App db)
addComment = undefined

rmComment :: ServerT RmComment (App db)
rmComment = undefined

addNote :: ServerT AddNote (App db)
addNote = undefined

rmNote :: ServerT RmNote (App db)
rmNote = undefined

setVote :: ServerT SetVote (App db)
setVote = undefined

rmVote :: ServerT RmVote (App db)
rmVote = undefined
