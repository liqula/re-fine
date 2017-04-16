// only used by node, so no need for the funny node-vs-browser-switch like in hookup.js)

global.jsdom = require('jsdom').jsdom;
require('jsdom-global')();

global.document = jsdom('');
global.window = document.defaultView;
global.window.navigator = { userAgent: 'node.js' };

global.enzyme = require('enzyme');
