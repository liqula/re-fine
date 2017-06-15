// only used by node, so no need for the funny node-vs-browser-switch like in hookup.js)

if (!global.React           ||
    !global.ReactDOM        ||
    !global.Sticky          ||
    !global.Skylight        ||
    !global.Hammer          ||
    !global.Draft           ||
    !global.DraftStateToHTML) {
    throw "hookup.js should have been called by now."
}

global.jsdom = require('jsdom').jsdom;
require('jsdom-global')();

global.document = jsdom('');
global.window = document.defaultView;
global.window.navigator = { userAgent: 'node.js' };

global.enzyme = require('enzyme');

global.refine$getRawContentBetweenElems = function($1, $2) {
    var begin = document.querySelector($1);
    var end = document.querySelector($2);

    console.log('!!!', $1, $2, begin, end);

    // TODO: travers the tom between these two elems and collect all text.  return that.

};
