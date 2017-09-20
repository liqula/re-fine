# meeting on 2017-09-15, 16:30, with daniel vde., fabian a., matthias f.


## missing designs

this is a list of screenshots and the pages in
`../liqula_designs_Juli2017_V2.pdf` they correspond to (if any).

- `_`: no design, but we can make a good guess.
- `?`: need clarification

```
 01-help.png                      - _
 02-group-list.png                - 1
 03-group-new.png                 - 5, 4
 04-group-details-members.png     - ?
 05-group-details-processes.png   - 2, 3
 06-process-new.png               - 5, 4
 07-vdoc-edit-initial.png         - _
 08-vdoc-view.png                 - 8, 9, 10
 09-vdoc-comment-add.png          - _
 10-vdoc-discussion-view.png      - 11, 12, 13
 11-vdoc-discussion-view-tree.png - 11, 12, 13
 12-user-profile.png              - 6 (if seen by myself); ? (if seen by others)
 13-vdoc-edit-diff-view.png       - _
```


## agenda

- vpn/gitlab (?)
- missing designs (see above).  is this list accurate?
- do we need the liqula logo?  can we have an svg for this?  or the font css specs?
- roundness of logos
  - do we need non-round logos at all?
  - how do we scale down the images to fit into the radius?  same factor for all of them?  which one?
  - or should the svg be circled to begin with, and not deal with it in scss?
- logos need work.  who does it?  deadline?
- color table:
  - anything missing?
  - should we re-arrange anything?

- do we need background tiling?  that's harder with inlined svgs.
- what color schemes are there?  currently we have RO/dark/bright, RO/bright/dark and a few hidden exceptions.


## meeting log (german)

- <title> in svg drinlassen, das sind die auto-tooltips (siehe xkcd).
- alle icons nur in runder form.  3 farben + hintergrund.
- farbtabelle "rgba" => "rgb" (obwohl egal)

.a: hintergrund
.b: hauptfarbe
.c: optional
.d: optional

- button states:
  - inaktiv (alle farben 50% transparent, kein mouse-over-effekt)
  - gedrückt
  - gedrückt, roll-over
  - nicht gedrückt
  - nicht gedrückt, roll-over: hauptfarbe icon wird gelb

- farbschema:
  - hell
  - dunkel
  - note
  - discussion
  - edit

mystery-icon:
 - untergruppen
 - oder zentrieren, wenn's keine untergruppen gibt.  oder lücke lassen.
