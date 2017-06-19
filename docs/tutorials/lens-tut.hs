{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

import Prelude hiding ((.), id, const, curry, uncurry)
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Traversable
import qualified Data.Traversable as Traversable

import           Control.Arrow
import qualified Control.Lens as Lens
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Database.SQLite.Simple         as SQLite
import           Database.SQLite.Simple.FromRow as SQLite
import           System.IO.Unsafe (unsafePerformIO)


data Lens a b = Lens {
    lensGet    :: a -> b
  , lensModify :: (b -> b) -> (a -> a)
  }

instance Category Lens where
  id = Lens id id
  (Lens g m) . (Lens g' m') = Lens (g . g') (m' . m)

data PLens f a b = PLens {
    plensGet :: a -> f b
  , plensModify :: (b -> b) -> (a -> a)
  }

plensFirst :: PLens Identity (a, b) a
plensFirst = PLens (Identity . fst) first

plensList :: PLens [] [a] a
plensList = PLens id map

compose :: Functor f => PLens g b c -> PLens f a b -> PLens (Compose f g) a c
compose (PLens g m) (PLens g' m') = PLens (Compose . fmap g . g') (m' . m)

exampleList :: [[(Int, Char)]]
exampleList = [[(1, 'a'), (2, 'b')], [(3, 'c'), (4, 'd')]]

exampleLens :: PLens (Compose [] (Compose [] Identity)) [[(a, b)]] a
exampleLens = plensFirst `compose` plensList `compose` plensList

{-
plensModify exampleLens (+1) exampleList
[[(2,'a'),(3,'b')],[(4,'c'),(5,'d')]]
-}

instance Monad f => Category (PLens f) where
  id = PLens return id
  PLens g m . PLens g' m' = PLens (g <=< g') (m' . m)

plensFst' :: PLens (Free []) (a, b) a
plensFst' = PLens (Pure . fst) first

plensList' :: PLens (Free []) [a] a
plensList' = PLens lift map

type LaarLens a b = forall f . Functor f => (b -> f b) -> (a -> f a)
type Traversal a b = forall f . Applicative f => (b -> f b) -> (a -> f a)

tget :: Traversal a b -> a -> [b]
tget t = getConst . t (Const . (:[]))

tmodify :: Traversal a b -> (b -> b) -> (a -> a)
tmodify t f = runIdentity . t (Identity . f)

travFst :: LaarLens (a, b) a
travFst f (a, b) = (, b) <$> f a

travList :: Traversal [a] a
travList = traverse

exampleTrav :: Traversal [[(Int, String)]] Int
exampleTrav = travList . travList . travFst

{-
So far we have only considered pure getters and modifiers;
what about effectful ones? For instance, we might want
to define lenses into a database, so that our getter and modifier
live in the IO monad.
-}

data GLens cat a b = GLens {
    glensGet    :: cat a b
  , glensModify :: cat (cat b b, a) a
  }

const :: Arrow arr => c -> arr b c
const a = arr (\_ -> a)

curry :: Arrow cat => cat (a, b) c -> (a -> cat b c)
curry m a = m . (const a &&& id)

uncurry :: ArrowApply cat => (a -> cat b c) -> cat (a, b) c
uncurry a = app . arr (first a)

data GPLens cat f a b = GPLens {
    gplensGet    :: cat a (f b)
  , gplensModify :: cat (cat b b, a) a
  }

toMonad :: ArrowApply arr => arr a b -> (a -> ArrowMonad arr b)
toMonad f a = ArrowMonad $ app . (const (f, a))

toArrow :: ArrowApply arr => (a -> ArrowMonad arr b) -> arr a b
toArrow act = app . arr (\a -> (unArrowMonad (act a), ()))
  where
    unArrowMonad (ArrowMonad a) = a

compM :: (Monad m, Monad f, Traversable f)
      => (a -> m (f b)) -> (b -> m (f c)) -> (a -> m (f c))
compM f g a = do
  fb  <- f a
  ffc <- Traversable.mapM g fb
  pure (join ffc)

compA :: (ArrowApply cat, Monad f, Traversable f)
      => cat a (f b) -> cat b (f c) -> cat a (f c)
compA f g = toArrow (compM (toMonad f) (toMonad g))

instance (ArrowApply cat, Monad f, Traversable f) => Category (GPLens cat f) where
  id = GPLens (arr return) app
  GPLens g m . GPLens g' m' =
    GPLens
      (g' `compA` g)
      (uncurry (curry m' . curry m))

{-
excercise for the reader: Define a simple category that operates on a simple database
and manipulates the database via lenses.

With this approach we can have Lenses from a type with keys for the
actual row/coulmn, we can modify entities and we can traverse
their relation to other entities, for the relation we just need to
store the foreign keys for the data.

Cascade modifications can be done as well.

This kind of database can be simulated via Maps and ID types as a first
step, the second we can rewrite that backend to an SQL related one.
-}

{-
Next steps:
 * Define IORef in a Reader monad, for appropiate wrapping
 * Define STMRef in a Reader monad, for change monad
 * Experiment with LensLike', for a well supported library
 * Use ExceptT monad, for appropiate exception handling
 * Try Sqlite, for future DB backend implementation
-}


-- * Natural Transformation

newtype NT f g = NT { runNT :: forall a . f a -> g a }

stm :: NT STM IO
stm = NT atomically

instance Category NT where
  id                = NT id
  (NT bc) . (NT ab) = NT (bc . ab)


-- * Example database.

data ID a      = ID { unID :: Int }
  deriving (Eq, Show, Ord)

data User      = User { _firstNameL :: String, _lastNameL :: String }
  deriving (Eq, Show, Ord)

data Group     = Group String
  deriving (Eq, Show, Ord)

data UserGroup = UserGroup (ID User) (ID UserGroup)
  deriving (Eq, Show, Ord)


-- * STM Persist

newtype STMPersist a = SPM { unPM :: ReaderT (TVar DB, STM Int) STM a }
  deriving (Functor, Applicative, Monad, MonadReader (TVar DB, STM Int))

liftSTM :: STM a -> STMPersist a
liftSTM = SPM . lift

stmRunner :: IO (NT STMPersist STM)
stmRunner = do
  dbRef <- newTVarIO emptyDB
  dbID  <- newTVarIO 0
  let incrementId = do modifyTVar' dbID (+1)
                       readTVar    dbID
  return (NT ((`runReaderT` (dbRef, incrementId)) . unPM))

data DB = DB {
    users      :: Map (ID User)      User
  , groups     :: Map (ID Group)     Group
  , userGroups :: Map (ID UserGroup) UserGroup
  }

emptyDB :: DB
emptyDB = DB Map.empty Map.empty Map.empty

saveUser' :: ID User -> User -> DB -> DB
saveUser' uid u (DB users groups usergroups) =
  DB (Map.insert uid u users) groups usergroups

loadUser' :: ID User -> DB -> User
loadUser' uid db = fromMaybe (error "ID not found") . Map.lookup uid $ users db

updateUser' :: ID User -> User -> DB -> DB
updateUser' uid u (DB users groups usergroups) =
  DB (Map.insert uid u users) groups usergroups

deleteUser' :: ID User -> DB -> DB
deleteUser' uid (DB users groups usergroups) =
  DB (Map.delete uid users) groups usergroups


-- * SQLite persist

newtype SQLPersist a = SQL { unSQL :: ReaderT SQLite.Connection IO a }
  deriving (Functor, Applicative, Monad, MonadReader SQLite.Connection)

sqlRunner :: String -> IO (NT SQLPersist IO)
sqlRunner dbname = do
  conn <- open dbname
  return (NT ((`runReaderT` conn) . unSQL))

sqlIO :: IO a -> SQLPersist a
sqlIO = SQL . liftIO

-- * Simple persist lens

type PersistLens backend a b = GPLens (Kleisli backend) Identity a b

get :: Monad backend
    => PersistLens backend a b -> a -> backend b
get pl x = runIdentity <$> runKleisli (gplensGet pl) x

modify :: Monad backend
       => PersistLens backend a b -> (b -> backend b) -> (a -> backend a)
modify pl = runKleisli . curry (gplensModify pl) . Kleisli

modify' :: Monad backend
        => PersistLens backend a b -> (b -> b) -> (a -> backend a)
modify' pl f = modify pl (pure . f)


-- * Entity based persist lenses.

-- CreateEntity how to create and delete a data, this should be defined
-- only once for every backend and entity type.
-- FIXME: Use Multiparam typeclass instead of an ADT?
data CreateEntity backend e = CreateEntity {
    saveEntity   :: e    -> backend (ID e)
  , deleteEntity :: ID e -> backend ()
  }

-- Entities can be defined on the partial result of the whole entity,
-- like selecting a subset of coulmns.
data Entity backend e r = Entity {
    loadEntity   :: ID e -> backend r
  , updateEntity :: ID e -> r -> backend ()
  }

-- | Converts an entity handler to a PersistLens.
--
-- Update will happen only if the given predicate is satisfied, usually if some
-- value has changed.
entity :: forall backend e r . Monad backend
       => (r -> r -> Bool) -> Entity backend e r -> PersistLens backend (ID e) r
entity updateNeeded handler = GPLens loadEnt modifyEnt
  where
    loadEnt :: Kleisli backend (ID e) (Identity r)
    loadEnt = Kleisli $ \eid -> do
      Identity <$> loadEntity handler eid

    modifyEnt :: Kleisli backend (Kleisli backend r r, ID e) (ID e)
    modifyEnt = Kleisli $ \(kee, eid) -> do
      r  <- loadEntity handler eid
      r' <- runKleisli kee r
      when (updateNeeded r r') $
        updateEntity handler eid r'
      pure eid

entity' :: forall backend e r . (Monad backend, Eq r)
        => Entity backend e r -> PersistLens backend (ID e) r
entity' = entity (/=)

-- | Convert Laarhoven lenses to PersistLens
lh :: forall backend a b . Monad backend
   => Lens.Lens' a b -> PersistLens backend a b
lh l = GPLens get modify
  where
    get :: Kleisli backend a (Identity b)
    get = Kleisli (pure . pure . Lens.view l)

    modify :: Kleisli backend (Kleisli backend b b, a) a
    modify = Kleisli $ \(kbb, a) -> do
      (\b -> (l Lens..~ b $ a)) <$> runKleisli kbb (Lens.view l a)


-- * Examples of Entity STM based lenses.

Lens.makeLenses ''User

userSTMEntity :: Entity STMPersist User User
userSTMEntity = Entity
  { loadEntity   = \uid -> do
      (dbRef, _) <- ask
      (loadUser' uid) <$> liftSTM (readTVar dbRef)
  , updateEntity = \uid user -> do
    (dbRef, _) <- ask
    liftSTM $ modifyTVar dbRef (updateUser' uid user)
  }

userCreateSTMEntity :: CreateEntity STMPersist User
userCreateSTMEntity = CreateEntity
  { saveEntity = \user -> do
      (dbRef, nextId) <- ask
      liftSTM $ do
        i <- ID <$> nextId
        modifyTVar' dbRef (saveUser' i user)
        return i
  , deleteEntity = \uid -> do
      (dbRef, _) <- ask
      liftSTM $ modifyTVar dbRef (deleteUser' uid)
  }

userSTM :: PersistLens STMPersist (ID User) User
userSTM = entity' userSTMEntity

firstName :: Monad backend => PersistLens backend User String
firstName = lh firstNameL

lastName :: Monad backend => PersistLens backend User String
lastName = lh lastNameL


-- * Examples of Entity SQL based lenses.

{-
Template Haskell could generate the followings:
 * Entities for data types
 * CreateEntities for data types
 * Schema creation
 * Schema migration
-}

userSQLEntity :: Entity SQLPersist User User
userSQLEntity = Entity
  { loadEntity = \uid -> do
      conn <- ask
      r <- sqlIO $ SQLite.queryNamed
              conn "SELECT firstname, lastname FROM user WHERE id=:uid"
                   [":uid" := unID uid]
      case r of
        [(fn, ln)] -> pure $ User fn ln
        _          -> error "loadEntity User: invalid number result set."
  , updateEntity = \uid user -> do
      conn <- ask
      sqlIO $ do
        SQLite.executeNamed conn
          "UPDATE user SET firstname = :fn , lastname = :ln WHERE id=:uid"
          [":fn" := _firstNameL user,":ln" := _lastNameL user,":uid" := unID uid]
  }

userCreateSQLEntity :: CreateEntity SQLPersist User
userCreateSQLEntity = CreateEntity
  { saveEntity = \user -> do
      conn <- ask
      sqlIO $ do
        SQLite.execute conn
          "INSERT INTO user (firstname, lastname) VALUES (?,?)"
          (_firstNameL user, _lastNameL user)
        i <- SQLite.lastInsertRowId conn
        pure . ID $ fromIntegral i
  , deleteEntity = \uid -> do
      conn <- ask
      sqlIO $ do
        SQLite.executeNamed conn
          "DELETE FROM user WHERE id=:uid"
          [":uid" := unID uid]
  }

createSchema :: SQLPersist ()
createSchema = do
  conn <- ask
  sqlIO $ execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, firstname TEXT, lastname TEXT)"

userSQL :: PersistLens SQLPersist (ID User) User
userSQL = entity' userSQLEntity

-- * Test

testSTM :: STMPersist User
testSTM = do
  uid <- saveEntity userCreateSTMEntity $ User "first" "last"
  modify' (firstName . userSTM) (const "First") uid
  modify' (lastName . userSTM)  (const "Last")  uid
  get userSTM uid

testSTMIO :: IO User
testSTMIO = do
  persist <- stmRunner
  runNT (stm . persist) testSTM

testSQL :: SQLPersist User
testSQL = do
  uid <- saveEntity userCreateSQLEntity $ User "first" "last"
  modify' (firstName . userSQL) (const "First") uid
  modify' (lastName . userSQL)  (const "Last")  uid
  get userSQL uid

testSQLIO :: IO User
testSQLIO = do
  persist <- sqlRunner ":memory:"
  runNT persist $ do
    createSchema
    testSQL
