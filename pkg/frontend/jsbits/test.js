// only used by node, so no need for the funny node-vs-browser-switch like in hookup.js)

global.jsdom = require('jsdom').jsdom;
require('jsdom-global')();

global.document = jsdom('');
global.window = document.defaultView;
global.window.navigator = { userAgent: 'node.js' };

global.enzyme = require('enzyme');

global.refine_test$testConvertFromToRaw = function(s) {
    if (true) {  // FIXME: debug output, remove (or at least set condition to false when this work.
        console.log(s);
        console.log(JSON.parse(s));
        console.log(Draft.convertFromRaw(JSON.parse(s)));
        console.log(Draft.convertToRaw(Draft.convertFromRaw(JSON.parse(s))));
        console.log(Draft.convertToRaw(Draft.convertFromRaw(JSON.parse(s))) == JSON.parse(s));
        console.log(JSON.stringify(Draft.convertToRaw(Draft.convertFromRaw(JSON.parse(s)))) == s);
    };

    return Draft.convertToRaw(Draft.convertFromRaw(JSON.parse(s))) == JSON.parse(s)
        && JSON.stringify(Draft.convertToRaw(Draft.convertFromRaw(JSON.parse(s)))) == s;
};
