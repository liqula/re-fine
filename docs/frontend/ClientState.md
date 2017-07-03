# Guidelines for the application state

The application state consists of the server state + clients' state (+ messages on the net which are already sent but not yet received)

- clients communicate directly only with the server (not with each-other)
- cache the communication between server and client which is relatively slow
- keep consistency of application state by avoiding all redundancy in client, in server and between client and server
    - the only allowed redundancy are caches which are refreshed automatically or on demand
- losing the clients' state is an expected event which should be handled gracefully (restore as much data as possible)
    - send all important data to the server immediately or within bounded time
        - consequence with avoiding redundancy: clients' state contains only temporary data (+ cache of global state)
           so losing the clients' state will not leave the application state inconsistent
- security: store sensitive data like votes encrypted on the client side
- the client state should be shaped by the semantics of the client application rather than the UI components
   pros: easier to understand & to define functions on it
              more stable codebase, easier to change the layout

## Methodology for defining the client state

1. define a big sum type following the mock list of typical views of the application
    Note that buttons (usually) have no state as they are related to actions on state.
2. refactor
3. add hidden information (information not present on screen) like page history or remembered form values
4. remove information present on server to avoid redundancy in the application state
5. add temporary state needed for UI components and layout (this step is not needed with a really good UI library)

## Interruptions in the workflow

The users workflow may be interrupted by login requests for example.
On the UI this can be presented as a popup or a separate page which is redirected to the previous page if the login succeeds.

In the client state, interruptions in workflow can be modeled by keeping track of the page history by storing a stack of substates in the client state.

The constraints on the stack history may be encoded into types.

## Guidelines for asynchronous calls (AJAX requests)

The state should be extended with an extra constructor denoting the state when the async call was sent and still no answer was received.
For example, buttons generally don't have state but an `AsyncButton` UI element has the following state:

```{.haskell}
data AsyncButtonState
  | AsyncButtonReady   -- pushable
  = AsyncButtonWaitingForResponse RequestId    -- not pushable
```

Things to consider:

- make possible to cancel waiting for the async answer from the UI e.g. by going to a previous state
- the response action for an async call should be accepted only if the state is `WaitingForRespose i` where i the right request identifier.

General pattern:

```{.haskell}
data Async a
  | AsyncCollect a
  = AsyncWaiting a RequestId    -- `a` is included to be able to show what do we waiting for
```

## Remembering user input

To remember user input it has to be stored it in the client state.

For example, remembering the login name (to fill in the name in case of a previously failed login attempt):

A)

```{.haskell}
data LoginState
  | Login UserName Password   -- user is filling in these data
  | WaitingForAuthentication RequestId UserName     -- store user name here to be able to remember it in case of failed authentication
```

B)

Or we can decouple all input history and we can store them in one place:

```{.haskell}
data ClientState = ClientState InputHistory ...

type InputHistory = Map InputHistoryKey ST  -- user name is stored in input history

data LoginState
  | Login UserName Password   -- user is filling in these data
  | WaitingForAuthentication RequestId    -- no user name is stored here
```

B) has advantages like cleaner and more modular code

## Guidelines of supporting third party state

Sometimes extra state should be added to client state to support third party UI components like the Draft editor:

```{.haskell}
data EditorState = EditorState WhatIsIdeallyNeeded DraftEditorState
```

Now there is a redundancy issue which can be solved in several ways.

A) Keep `WhatIsIdeallyNeeded` in sync with `DraftEditorState` by defining and using the following functions:

```{.haskell}
propagateDraftEditorStateChanges :: DraftEditorState{-old-} -> DraftEditorState{-new-} -> WhatIsIdeallyNeeded{-old-} -> WhatIsIdeallyNeeded{-new-}
propagateDraftEditorStateChanges = ...

propagateWhatIsIdeallyNeededChanges :: WhatIsIdeallyNeeded{-old-} -> WhatIsIdeallyNeeded{-new-} -> DraftEditorState{-old-} -> DraftEditorState{-new-}
propagateWhatIsIdeallyNeededChanges = ...

updateDraftEditorState :: DraftEditorState -> EditorState -> EditorState
updateDraftEditorState d (EditorState i d') = EditorState (propagateDraftEditorStateChanges d' d i) d

updateWhatIsIdeallyNeeded :: WhatIsIdeallyNeeded -> EditorState -> EditorState
updateWhatIsIdeallyNeeded i (EditorState i' d) = EditorState i (propagateWhatIsIdeallyNeededChanges i' i d)
```

B) Replace `WhatIsIdeallyNeeded` by `SquashedWhatIsIdeallyNeeded` such that the redundancy disappears.
You can do anything which you could do before if you implement an isomorphism.

```{.haskell}
data EditorState' = EditorState' SquashedWhatIsIdeallyNeeded DraftEditorState

iso :: Iso' EditorState EditorState'
iso = ...
```

B) is safer than A)

## Guidelines for designing react-hs UI components

- prevent unnecessary re-rendering: components should be based on data only which is actually presented; minimize hidden data in GUI components
    this is not just a tuning issue (see #...)

# Ideal client state for re-fine

```{.haskell}
data ClientState = ClientState ServerCache History LoginInfo MainPage DevState   -- or [MainPage]

-- server cache is derivable from server state and any other part of client state is not derivable from server state
-- any key of any map in the server cache may be deleted at any time with the risk of slowing the application down
data ServerCache = ServerCache
  { translations :: Map Language Translation
  , userInfo :: Map UserId (UserName, [GroupId])  -- alternative solution: store (UserName, [GropuId]) in UserId
  , groupInfo :: Map GroupId GroupDescription     -- alternative solution: store GroupDescription in GroupId
  , docDescription :: Map DocId DocDescription
  , editInfo :: ...
  , ...
  }

-- any key of any map in history may be deleted at any time risking inconvenient (but still correct) user experience
data History = History
  { inputHistory :: Map InputHistoryKey ST
  , filterStateHistory :: Map EditId FilterState
  , languageHistory :: Maybe UserId Language
  , ...
  }

data LoginInfo
  | LoggedOut (Maybe UserId)  -- if this is Just then the page 'user ... is logged out' is shown
  | Login (Async (UserName, Password))  -- the login page is shown
  | Register (Async (UserName, Email, Email, Password, AgreeTermsOfUse))  -- the registration page is shown
  = LoggedIn UserId    -- UserName and [GroupId] can be looked up from cache

MainPage
  = LoadingPage [DocId]         -- will be replaced
  | Groups [GroupId]
  | GroupDetails GroupId
  | Help
  | DocumentReadOnly EditId
  | DocumentView DocumentViewState
  | AddComment (Async (CommentKind, Description)) DocumentViewState                -- presented as a dialog
  | DocumentDiffView DocumentDiffState
  | DocumentEdit DocumentEditState
  | EditSave (Async (EditKind, Description)) DocumentEditState                   -- presented as a dialog

data DocumentViewState = DocumentViewState EditId SelectionStateWithPx BubbleDetails QuickCreateState

data DocumentDiffState = DocumentDiffState EditId{-what we are looking at-} EditId{-parent-} Collapsed SelectionStateWithPx BubbleDetails QuickCreateState

data BubbleDetails = BubbleDetails FocussedBubble AllVerticalSpanBounds BubblePositioning

data DocumentEditState = DocumentEditState EditId EditToolbar SelectionStateWithPx

-- comments and edit may be filtered
-- TODO: de-functionalize this (avoid (->))
type FilterState = Contribution -> Bool

type Collapsed = Bool

data QuickCreateState = ...

data EditToolbar = ...

-- TODO
-- ScreenState is required for toolbar and bubble positioning (so just Document*, i think).
-- ToolbarSticky should be part of screenstate.
```

# `GlobalState`  refactoring plan

1.  Implement and use `ServerCache`
    1. Implement an empty `ServerCache`
    2. Extend `ServerChace` with more fields until #386 is solved.
2. ...