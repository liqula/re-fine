
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
