var jsdom = require('jsdom').jsdom;
require('jsdom-global')();

var document = jsdom('');
var window = document.defaultView;
window.navigator = { userAgent: 'node.js' };

var React = require('react');
var ReactDOM = require('react-dom');

var enzyme = require('enzyme');
