
window.React = require("react");
window.ReactDOM = require("react-dom");

window.Sticky = require("react-sticky").Sticky;
window.StickyContainer = require("react-sticky").StickyContainer;

window.Overlay = require("react-skylight").SkyLightStateless;

window.Hammer = require("react-hammerjs");

require("./../scss/main.scss");


window.refine$getSelectionRange = function() {
    var leftSiblingLength = function(node) {
        if (node === null) {
            return 0;
        } else {
            return (node.textContent || node.innerText).length + leftSiblingLength(node.previousElementSibling);
        }
    };

    return (function (range) {
        var result = {};
        if(range.startContainer.parentElement.attributes['data-uid']) {
            var leftOffset = leftSiblingLength(range.startContainer);
            var dataOffset = parseInt(range.startContainer.parentElement.attributes['data-offset'].value, 10);
            result.start = {
                node: parseInt(range.startContainer.parentElement.attributes['data-uid'].value, 10),
                offset: range.startOffset + leftOffset + dataOffset
            };
        }
        if(range.endContainer.parentElement.attributes['data-uid']) {
            var leftOffset = leftSiblingLength(range.endContainer);
            var dataOffset = parseInt(range.endContainer.parentElement.attributes['data-offset'].value, 10);
            result.end = {
                node: parseInt(range.endContainer.parentElement.attributes['data-uid'].value, 10),
                offset: range.endOffset + leftOffset + dataOffset
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
