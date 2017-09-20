## to add a new icon:

- just copy `00_joker.svg` to a new file name under `./icon/`.
- run `./build svg` from the repo root.  (TODO: not implemented yet, but when it is it'll call `/scripts/compile-svg.hs`.)
- edit the svg contents.  this can be done concurrently by the design team.

to find all icons that need design, run `./find-missing-images.sh`.

NOTE: the scss file that contains the classes with the background svg images does not exist any more.


### rules for the svg contents

the svg files are processed by `/scripts/compile-svg.hs`.  the design
team needs to follow a few rules for this to work:

- there should be one style tag that lists any number of color classes.
- each class should have the form `.classname { fill: $colorname }`.
- any color variable from 20-colors.scss can be used for `$colorname`.
- you may use non-svg tags like `title` or xml comments, but everything except the one `style` tag and its contents will be removed.


### icon naming conventions

- no capital letters.
- no '-', just underscore.
- the first word should be the object that this is about (comment etc.).
- the remaining words should be arranged in the order that makes lexicographical sorting most intuitive.  example: ...


## old icon names

icon names have been changed for coherence and compliance with the
naming convention.  this is a list of the old names that may be needed
for refactoring.  (this section can be removed once the refactoring is
over.)

```
        renamed:    Arrow_down.svg -> arrow_down.svg
        renamed:    Arrow_left.svg -> arrow_left.svg
        renamed:    Arrow_right.svg -> arrow_right.svg
        renamed:    Arrow_up.svg -> arrow_up.svg
        renamed:    Close.svg -> close.svg
        renamed:    Comment.svg -> comment.svg
        renamed:    New_Comment.svg -> comment_new.svg
        renamed:    Toggle_collapse_diff.svg -> diff_collapse.svg
        renamed:    Diff_details.svg -> diff_details.svg
        renamed:    Toggle_expand_diff.svg -> diff_expand.svg
        renamed:    Discussion.svg -> discussion.svg
        renamed:    Toggle_discuss_flat.svg -> discussion_flat.svg
        renamed:    Toggle_discuss_tree.svg -> discussion_tree.svg
        renamed:    Edit.svg -> edit.svg
        renamed:    Edit_kind_grammar.svg -> edit_kind_grammar.svg
        renamed:    Edit_kind_meaning.svg -> edit_kind_meaning.svg
        renamed:    Edit_kind_phrasing.svg -> edit_kind_phrasing.svg
        renamed:    New_Edit.svg -> edit_new.svg
        renamed:    Reason.svg -> edit_reason.svg
        renamed:    Edit_toolbar_bold.svg -> edit_toolbar_bold.svg
        renamed:    Edit_toolbar_bullets.svg -> edit_toolbar_bullets.svg
        renamed:    Edit_toolbar_h1.svg -> edit_toolbar_h1.svg
        renamed:    Edit_toolbar_h2.svg -> edit_toolbar_h2.svg
        renamed:    Edit_toolbar_h3.svg -> edit_toolbar_h3.svg
        renamed:    Edit_toolbar_italic.svg -> edit_toolbar_italic.svg
        renamed:    Edit_toolbar_link.svg -> edit_toolbar_link.svg
        renamed:    Edit_toolbar_numbers.svg -> edit_toolbar_numbers.svg
        renamed:    Edit_toolbar_redo.svg -> edit_toolbar_redo.svg
        renamed:    Edit_toolbar_save.svg -> edit_toolbar_save.svg
        renamed:    Edit_toolbar_undo.svg -> edit_toolbar_undo.svg
        renamed:    Edit_view.svg -> edit_view.svg
        renamed:    FAQ.svg -> faq.svg
        renamed:    Filter.svg -> filter.svg
        renamed:    Group.svg -> group.svg
        renamed:    Group_add.svg -> group_new.svg
        renamed:    Group_update.svg -> group_update.svg
        renamed:    Help.svg -> help.svg
        renamed:    Idea.svg -> idea.svg
        renamed:    Index_desktop.svg -> index_desktop.svg
        renamed:    Index_mobile.svg -> index_mobile.svg
        renamed:    Info.svg -> info.svg
        renamed:    Private_lock.svg -> lock.svg
        renamed:    Login.svg -> login.svg
        renamed:    Note.svg -> note.svg
        renamed:    Private_note.svg -> note_private.svg
        renamed:    Next_Phase.svg -> phase_next.svg
        renamed:    Play.svg -> play.svg
        renamed:    Plus.svg -> plus.svg
        renamed:    Process.svg -> process.svg
        renamed:    Process_add.svg -> process_new.svg
        renamed:    Process_update.svg -> process_update.svg
        renamed:    Question.svg -> question.svg
        renamed:    Reader.svg -> reader.svg
        renamed:    Reply.svg -> reply.svg
        renamed:    Save.svg -> save.svg
        renamed:    Search.svg -> search.svg
        renamed:    Share.svg -> share.svg
        renamed:    Sort.svg -> sort.svg
        renamed:    User.svg -> user.svg
        renamed:    User_profile.svg -> user_profile.svg
        renamed:    Vote_negative.svg -> vote_negative.svg
        renamed:    Vote_neutral.svg -> vote_neutral.svg
        renamed:    Vote_positive.svg -> vote_positive.svg
```
