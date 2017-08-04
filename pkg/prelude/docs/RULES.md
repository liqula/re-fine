
## re-fine project policy rules

### intro

This document is a code style guide, development process
specification, and container for all other rules that the re-fine team
has decided on.


### establishing new rules

Just open a MR on this file and have everybody comment on it.  If
nobody objects, it's a new rule!


### development process

1. work on branches, not master.  make sure branches are kept small
   and merged soon.  start branch names with the number of the ticket
   they are addressing (solving or partially solving).

   DEVIATING data points: https://mrdevops.io/trunk-based-development-8376fe577c11#.qucopj92o

2. to merge a branch, create an MR.

3. if you want a review, unassign yourself and optionally ask somebody
   on irc for review.

4. roughly every time you start your day, you should look at the list
   of all issues assigned to you and all open merge requests.

   if there are unassigned issues or MRs assigned to you, those have
   the highest priority.

5. before merging an MR, rebase it on master or merge master into it.

6. do not write "Fixes #..." into commit messages.  if you want to
   auto-close issues, add "Closes #..." to the beginning of the MR
   description.  RATIONALE: the trigger is easier to spot for the
   reviewer in the MR.  if you are wrong, it is (slightly) harder to
   change a commit message than an MR description.


### language extensions

`default-extensions:` because tooling (hlint, sensei, ...) does not
reliably honor default-extensions listed in the cabal files, we do not
use this feature, but instead list all language extensions in the
Haskell modules.  The

Copy the following list to a new module from here and edit to your
liking.

```haskell
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
```

The following shell line can be used to re-align all files (poorly
tested -- commit your other changes first!):

```shell
find . -name '*.hs' -exec perl -i -ne 'if (/^({-# LANGUAGE )(\S+)( #-})$/) { printf("$1%26.26s$3\n", $2."                          "); } else { print }' {} \;
```

### qualified imports

Re-exporting qualified imports together with their qualification is
not supported by the GHC module system.  Here is a list of defaults to
cut&paste into new modules:

```haskell
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashMap.Strict as SHM
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Data.Text.Lazy as LT
```


### module imports

1. Qualified imports are never explicit.

   RATIONALE: the qualifier makes it clear for every use of names from
   the module where they originate from, and importing all names
   implicitly avoids having to go back to the import section every
   time you change the names you use.

   Good:

   ```haskell
   import qualified Data.Map as Map
   import qualified Data.Map
   import Data.Map (fromList)
   ```

   Not good:

   ```haskell
   import qualified Data.Map as Map (fromList)
   ```


### string types

1. Use the following imports where needed, in this form:

```haskell
import           Data.String.Conversions (SBS, LBS, ST, LT, cs)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
```

2. When string types are mentioned, use the type aliases from
   string-conversions (see above) where possible.

3. For converting between strings, use `cs` where possible.


### comments

There are some keywords to give special semantics to source code
comments.  They always have the form `-- KEYWORD: ...`, where `...` is
some free text.  The keywords are:

1. TODO: shouldn't make it to master.  it's clearly broken in a known
   way.

2. FIXME: works as it is, but there is a better implementation.  if we
   need to touch this code again, we should probably resolve the FIXME
   first.

3. FUTUREWORK: there are some nice ideas about how to improve things
   here, but they are not urgent.

3. TUNING: performance-specific things to improve.  this should help with
   profiling and benchmarking work.


### code layout

1. two empty lines before section heading (`-- *`).

2. this is a soft rule: try to keep lines shorter than 120 chars.
   RATIONALE: avoids line breaks on small screens.


### finding names

In order to avoid name shadowing warnings, (1) try to think of another
name that is at least as meaningful and informative, and iff failing
to do so, (2) append natural numbers to the shadowed name.

RATIONALE:
- trailing `_` is already used by react-flux, blaze, lucid etc..
- leading `_` is used by lens and, worse, has the special meaning as a
  typed hole.
- tailing `'` has an informal meaning in analysis that is different from
  what we have in mind here (it's somehow the next step in an
  iteration).
- leading `'` is a syntax error (and also would be very confusing).


### tests

Tests are written using hspec, hspec-discover.  Every package has its
on suite that can be run with `stack test`.  Globally in the git repo,
`test-all.hs` runs additional tests (hlint, coverage, ...)

Some example test cases and properties:

```haskell
  -- unit test
  it "number is the same" $ do
    3 `shouldBe` (4 :: Int)

  -- qc property
  it "number is the same" . property $
    \(int :: Int) -> int + 1 `shouldBe` int

  -- monadic qc property (import Test.QuickCheck.Monadic)
  it "number is the same" . property $
    \(int :: Int) -> monadicIO . run $ do
      result <- pure int  -- effectful computation that takes an Int and yields an Int.
      result `shouldBe` 3
```

RATIONALE: quickcheck's `(===)` does work instead of `shouldBe` (and
possibly sometimes even `(==)`?), but the reports on failing test
cases are best for `shouldBe`.
