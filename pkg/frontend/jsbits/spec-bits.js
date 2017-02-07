var React = require('react');
var ReactDOM = require('react-dom');

var enzyme = require('enzyme');

var jsdom = require('jsdom').jsdom;
require('jsdom-global')();

var document = jsdom('');
var window = document.defaultView;
window.Hammer = require("react-hammerjs");
window.navigator = { userAgent: 'node.js' };
