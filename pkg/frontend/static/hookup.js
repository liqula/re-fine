
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
            return textLength(node) + leftSiblingLength(node.previousSibling);
        }
    };

    var textLength = function(node) {
        if (node.nodeType === Node.ELEMENT_NODE || node.nodeType === Node.TEXT_NODE) {
            return (node.textContent || node.innerText).length;
        } else {
            return 0;
        }
    };

    return (function (range) {
        var result = {};
        // The container tags have data-uid attributes iff they are
        // inside the article.  If they are outside, the corresponding
        // chunk points are Nothing.
        if(range.startContainer.parentElement.attributes['data-uid']) {
            var leftOffset = leftSiblingLength(range.startContainer.previousSibling);
            var dataOffset = parseInt(range.startContainer.parentElement.attributes['data-offset'].value, 10);
            result.start = {
                node: parseInt(range.startContainer.parentElement.attributes['data-uid'].value, 10),
                offset: range.startOffset + leftOffset + dataOffset
            };
        }
        if(range.endContainer.parentElement.attributes['data-uid']) {
            var leftOffset = leftSiblingLength(range.endContainer.previousSibling);
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
