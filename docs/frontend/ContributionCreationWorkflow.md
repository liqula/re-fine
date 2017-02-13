# When creating a contribution


## Creating a comment via direct text selection

- User selects text
- article.onMouseUp: dispatches action TriggerUpdateSelection
- Store: transforms action to UpdateSelection
- Store: action updates state BubblesStore.currentSelection
- Updated state: When x1 is not active and x2 is not active: updates LeftAsideProps
- Updated LeftAsideProps: create QuickCreate-button
- Clicking QuickCreate-button:
   - dispatches action BubblesAction.ClearSelection
   - dispatches action BubblesAction.ShowCommentEditor

## Creating via button "text-specific comment"

- User clicks "text-specific comment" button
- Button dispatches action X1
- BubblesStore: action X1 updates state x1
- State x1 updates toolbar extension (displays info message)
- article.onMouseUp: dispatches action TriggerUpdateSelection
- Store: transforms action to UpdateSelection
- Store: action updates state BubblesStore.currentSelection
- Updated state: When x1 is active: dispatches action BubblesAction.ShowCommentEditor


## Creating via button "general comment"

- User clicks "general comment" button
- Button dispatches action X2 and UpdateSelection(Nothing) and BubblesAction.ShowCommentEditor
- BubblesStore: action X2 updates state x2
- BubblesStore: action updates state currentSelection
