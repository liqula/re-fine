# When creating a contribution


## Creating a comment via direct text selection

- User selects text
- article.onMouseUp: dispatches action TriggerUpdateSelection
- Store: transforms action to UpdateSelection
- Store: action updates state BubblesStore.currentSelection
- Updated state: updates LeftAsideProps
- Updated LeftAsideProps: create QuickCreate-button
- Clicking QuickCreate-button:
   - dispatches action BubblesAction.ClearSelection
   - dispatches action BubblesAction.ShowCommentEditor
