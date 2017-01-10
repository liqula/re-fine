
## re-fine project policy rules

### intro

This document is a code style guide, development process
specification, and container for all other rules that the re-fine team
has decided on.


### establishing new rules

Just open a PR on this file and have everybody comment on it.  If
nobody objects, it's a new rule!


### development process

1. if you want to work on a branch, create PR early, label it "work in
   progress", and assign yourself.  this way you are allowed to write
   to it, rebase it, and nobody else is allowed to write.

2. (1.) of course does not rule out ad-hoc collaboration coordinated
   through other channels like irc.

3. if you want a review, unassign yourself and optionally ask somebody
   on irc for review.

4. roughly every time you start your day, you should look at the list
   of "everything assigned to you":

```sh
      browser "https://github.com/fisx/aula2/issues?q=is%3Aopen+assignee%3A"`whoami`
```

   if the list contains pull requests for review, those have the
   highest priority.


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


### code layout

1. two empty lines before section heading (`-- *`).


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
      result <- pure int  -- effectful computation that takes and Int and yields an Int.
      result `shouldBe` 3
```

RATIONALE: quickcheck's `(===)` does work instead of `shouldBe` (and
possibly sometimes even `(==)`?), but the reports on failing test
cases are best for `shouldBe`.
