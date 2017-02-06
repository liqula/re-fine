
window.React = require("react");
window.ReactDOM = require("react-dom");

window.Sticky = require("react-sticky").Sticky;
window.StickyContainer = require("react-sticky").StickyContainer;

window.Overlay = require("react-skylight").SkyLightStateless;

window.Hammer = require("react-hammerjs");

require("./../scss/main.scss");


var refine$getSelectionRange = function() {
    return (function (range) {
        var result = {};
        if(range.startContainer.parentElement.attributes['data-uid']) {
            result.start = {
                node: parseInt(range.startContainer.parentElement.attributes['data-uid'].value, 10),
                offset: range.startOffset + parseInt(range.startContainer.parentElement.attributes['data-offset'].value, 10)
            };
        }
        if(range.endContainer.parentElement.attributes['data-uid']) {
            result.end = {
                node: parseInt(range.endContainer.parentElement.attributes['data-uid'].value, 10),
                offset: range.endOffset + parseInt(range.endContainer.parentElement.attributes['data-offset'].value, 10)
            };
        }
        result.top = range.startContainer.parentElement.getBoundingClientRect().top;
        result.bottom = range.endContainer.parentElement.getBoundingClientRect().bottom;
        result.scrollOffset = typeof( window.pageYOffset ) == 'number' && window.pageYOffset
            || document.body && document.body.scrollTop
            || document.documentElement && document.documentElement.scrollTop;
        return JSON.stringify(result);
    }) (window.getSelection().getRangeAt(0));
};
