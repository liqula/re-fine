var jsdom = require('jsdom').jsdom;
require('jsdom-global')();

var document = jsdom('');
var window = document.defaultView;
window.navigator = { userAgent: 'node.js' };

var enzyme = require('enzyme');
