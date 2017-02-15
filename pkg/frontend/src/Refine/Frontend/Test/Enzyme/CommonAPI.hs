module Refine.Frontend.Test.Enzyme.CommonAPI where

import GHCJS.Types (JSString, JSVal)

import Refine.Frontend.Test.Enzyme.Core

-- | The Enzyme API

find :: EnzymeWrapper w => w -> EnzymeSelector -> IO w
find = execWithSelector "find"

-- TODO: findWhere

-- TODO: filter

-- TODO: filterWhere

-- TODO: contains

-- TODO: containsMatchingElement

-- TODO: containsAllMatchingElements

-- TODO: containsAnyMatchingElements

-- TODO: hasClass

is :: EnzymeWrapper w => w -> EnzymeSelector -> IO Bool
is = execWithSelector "is"

-- TODO: exists

-- TODO: isEmpty

-- TODO: not

children :: EnzymeWrapper w => w -> IO w
children = exec "children"

childAt :: EnzymeWrapper w => w -> Int -> IO w
childAt = execWith1Arg "childAt"

-- TODO: parents

-- TODO: parent

-- TODO: closest

-- TODO: render
-- TODO: CheerioWrapper

text :: EnzymeWrapper w => w -> IO JSString
text = exec "text"

html :: EnzymeWrapper w => w -> IO JSString
html = exec "html"

-- TODO: get

-- TODO: getNode

-- TODO: getNodes

at :: EnzymeWrapper w => w -> Int -> IO w
at = execWith1Arg "at"

-- TODO: first

-- TODO: last

-- TODO: state

-- TODO: context

props :: EnzymeWrapper w => w -> IO JSVal
props = exec "props"

-- TODO: prop

-- TODO: key

simulate :: EnzymeWrapper w => w -> EventType -> IO w
simulate = execWith1Arg "simulate"

-- TODO: setState

-- TODO: setProps

-- TODO: setContext

-- TODO: instance

-- TODO: unmount

-- TODO: update

-- TODO: debug

typeOf :: EnzymeWrapper w => w -> IO JSVal
typeOf = exec "type"

-- TODO: name

-- TODO: forEach

-- TODO: map

-- TODO: reduce

-- TODO: reduceRight

-- TODO: slice

-- TODO: tap

-- TODO: some

-- TODO: someWhere

-- TODO: every

-- TODO: everyWhere

-- | Helper Functions

lengthOf :: EnzymeWrapper w => w -> IO Int
lengthOf = attr "length"

lengthOfIO :: EnzymeWrapper w => IO w -> IO Int
lengthOfIO wrapper = lengthOf =<< wrapper

