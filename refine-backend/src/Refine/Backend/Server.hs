module Refine.Backend.Server where

import Prelude hiding ((.), id)
import Control.Category

import Control.Natural (($$))
import Control.Monad.Except
import Data.String.Conversions (cs)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Database (DB, DBConfig(..), createDBRunner)
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.DocRepo (createRunRepo)
import Refine.Common.Rest
import Servant

import Network.Wai.Handler.Warp as Warp


startBackend :: IO ()
startBackend = do

  runDb      <- createDBRunner $ DBOnDisk "refine.db"
  runDocRepo <- createRunRepo
  let logger = Logger putStrLn

  void $ (natThrowError . runApp runDb runDocRepo logger) $$ do
    migrateDB

  Warp.runSettings Warp.defaultSettings
    . Servant.serve (Proxy :: Proxy RefineAPI)
    $ serverT runDb runDocRepo logger refineApi


serverT :: RunDB m -> RunDocRepo -> Logger -> ServerT RefineAPI (App m) -> Server RefineAPI
serverT b r l = enter (toServantError . cnToSn (runApp b r l))


toServantError :: (Monad m) => ExceptT AppError m :~> ExceptT ServantErr m
toServantError = Nat ((lift . runExceptT) >=> either (throwError . fromAppError) pure)
  where
    -- FIXME: Render JSON from the errors
    fromAppError :: AppError -> ServantErr
    fromAppError (AppUnknownError msg)      = err500 { errBody = cs msg }
    fromAppError (AppDBError      dbError)  = err500 { errBody = cs $ show dbError }
    fromAppError (AppDocRepoError docError) = err500 { errBody = cs $ show docError }


refineApi :: ServerT RefineAPI (App DB)
refineApi =
        getVDoc
  :<|>  createVDoc
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

putTitle :: ServerT PutTitle (App DB)
putTitle = undefined

putAbstract :: ServerT PutAbstract (App DB)
putAbstract = undefined

noteVisibility :: ServerT NoteVisibility (App DB)
noteVisibility = undefined

rmVDoc :: ServerT RmVDoc (App DB)
rmVDoc = undefined

addPatch :: ServerT AddPatch (App DB)
addPatch = undefined

rmPatch :: ServerT RmPatch (App DB)
rmPatch = undefined

mergePatch :: ServerT MergePatch (App DB)
mergePatch = undefined

rebasePatch :: ServerT RebasePatch (App DB)
rebasePatch = undefined

addComment :: ServerT AddComment (App DB)
addComment = undefined

rmComment :: ServerT RmComment (App DB)
rmComment = undefined

addNote :: ServerT AddNote (App DB)
addNote = undefined

rmNote :: ServerT RmNote (App DB)
rmNote = undefined

setVote :: ServerT SetVote (App DB)
setVote = undefined

rmVote :: ServerT RmVote (App DB)
rmVote = undefined
