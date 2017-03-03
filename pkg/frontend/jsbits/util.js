(function(target) {
    target.refine$getSelectionRange = function() {
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

            var getSelectionPoint = function(container, offset) {
                // The container tags have data-uid attributes iff they are
                // inside the article.  If they are outside, the corresponding
                // chunk points are Nothing.
                if(container.parentElement.attributes['data-uid']) {
                    var leftOffset = leftSiblingLength(container.previousSibling);
                    var dataOffset = parseInt(container.parentElement.attributes['data-offset'].value, 10);
                    return {
                        node: parseInt(container.parentElement.attributes['data-uid'].value, 10),
                        offset: offset + leftOffset + dataOffset
                    };
                }
            };

            result.start = getSelectionPoint(range.startContainer, range.startOffset);
            result.end = getSelectionPoint(range.endContainer, range.endOffset);
            result.top = range.startContainer.parentElement.getBoundingClientRect().top;
            result.bottom = range.endContainer.parentElement.getBoundingClientRect().bottom;
            result.scrollOffset = typeof( target.pageYOffset ) === 'number' && target.pageYOffset
                || document.body && document.body.scrollTop
                || document.documentElement && document.documentElement.scrollTop;
            return JSON.stringify(result);
        }) (target.getSelection().getRangeAt(0));
    };
})((typeof global === 'undefined') ? window : global);
