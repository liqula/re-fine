# When creating a contribution


## Creating a comment via direct text selection

- User selects text
- article.onMouseUp: dispatches action TriggerUpdateSelection
- Store: transforms action to UpdateSelection
- Store: action updates state BubblesStore.currentSelection
- Updated state: When hsCommentToolbarExtensionStatus is not CommentToolbarExtensionWithSelection and x2 is not active: updates LeftAsideProps
- Updated LeftAsideProps: create QuickCreate-button
- Clicking QuickCreate-button:
   - dispatches action BubblesAction.ClearSelection
   - dispatches action BubblesAction.ShowCommentEditor
- Submitting the comment: dispatches action BubblesAction.SubmitComment and BubblesAction.HideCommentEditor
- Dismissing the editor: dispatches action BubblesAction.HideCommentEditor


## Creating via button "text-specific comment"

- User clicks "text-specific comment" button
- Button dispatches action StartTextSpecificComment
- BubblesStore: action StartTextSpecificComment updates state hsCommentToolbarExtensionStatus
- State hsCommentToolbarExtensionStatus updates toolbar extension (displays info message)
- Anywhere onClick: cancels the commentToolbarExtension (no matter in which state)
- article.onMouseUp: dispatches action TriggerUpdateSelection
- Store: transforms action to UpdateSelection
- Store: action updates state BubblesStore.currentSelection
- Updated state: When hsCommentToolbarExtensionStatus is CommentToolbarWithSelection: dispatches action BubblesAction.ShowCommentEditor
- Dismissing the editor: dispatches action FinishTextSpecificComment and BubblesAction.HideCommentEditor
- Submitting the comment: dispatches action FinishTextSpecificComment and BubblesAction.SubmitComment and BubblesAction.HideCommentEditor


## Creating via button "general comment"

- User clicks "general comment" button
- Button dispatches action X2 and UpdateSelection(Nothing) and BubblesAction.ShowCommentEditor
- BubblesStore: action X2 updates state x2
- BubblesStore: action updates state currentSelection
- Dismissing the editor: dispatches action RemoveX2 and BubblesAction.HideCommentEditor
- Submitting the comment: dispatches action RemoveX2 and BubblesAction.SubmitComment and BubblesAction.HideCommentEditor
