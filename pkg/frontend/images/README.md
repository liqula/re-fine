to add a new icon:

- just copy 00_joker.svg to a new file name.
- run ./build svg  (TODO: not implemented yet, but it'll call ./scripts/compile-svg.hs)
- work on proper contents of new svg file (this can be done concurrently by the design team)
- to find all icons that need design, run `./find-missing-images.sh`.

the scss file that contains the classes with the background svg images does not exist any more.

