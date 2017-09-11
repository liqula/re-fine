source: http://zb1:9001/p/scss-new


## general rules

- inline-styles are ok iff there is a plausible reason for them.  (RATIONALE: whoever wants to touch the scss can also touch the html in the style guide, translation to haskell is a separate step.)

### open questions

- can you make an svg with the liqula logo?


## scss identifiers and name spaces

scss code should have name prefixes that are the react component names
(the element name without the underscore0.  Example: `ibutton` from
`Refine.Frontend.Icon`.

The component name is camelCased.  There usually is a css class with
just the component name that is assigned to the outermost `div` of the
component.  There can be additional css classes that have that name
suffixed with `_` and arbitrary css names that describe the part /
function of the component they are about.

### css-only components

it is ok to create classes that are re-used accross react components,
e.g. a horizontal line that separates an ibutton header from other
contents.  this occurs in tab components in the main menu.

naming convention for these css-only components tbd.  perhaps we
should just make up react component names that then don't exist in
haskell.


## icons and colors

- we can't use css classes inside the svg code because it is un-scoped behind a `url(<file>)` css expression.
- we *could* inline the svg into haskell, and use TH to keep the svg sources exposed to haskell-oblivious web design.  no strong reasons against this.
- instead, we allow for color variables (and nothing else) in the svg code and compile that away with a custom preprocessor.  3 svg files for each icon, but only one source file.
- circles are done with background-radius.

### open questions

- how do we scale down the images to fit into the radius?  same factor for all of them?  which one?  if it's different, can we do that inside the svgs somehow?  in css it would be cheapest, but then it would be nice to have one universally applicable factor.  or should the svg be circled to begin with?


## tile titles

- two lines; wrap; overflow: hidden;  no ellipsis.  (RATIONALE / ADDITIONAL INFO: ellipsis only works with nowrap.)


## white bar at the end of the page

we need to modify the background-color attribute of the body tag of
the page, but don't have access to that via the virtual dom (it's
outside of the app).

instead, we write a foreign function that injects a given color class
name into the body tag, and add it in the lifecycleview hooks for
didmount and didupate.

this motivates two refactorings:

- move the color table types from pkg/frontend/styleguide/Main.hs to
  to pkg/common/src/Refine/Common/Color.hs.

- update lifecycle view, move it out of Outdated, and make it the
  default component.  or alternatively, allow to manipulate callback
  hooks in any view type.
