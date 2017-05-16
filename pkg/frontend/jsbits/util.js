(function(target) {
    target.refine$editorContentFromHtml = function(html) {
        const blocksFromHTML = Draft.convertFromHTML(html);
        return Draft.ContentState.createFromBlockArray(
            blocksFromHTML.contentBlocks,
            blocksFromHTML.entityMap
        );
    };

    target.refine$getDraftSelectionStateViaBrowser = function() {
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

        var getKeyFromBlock = function(node) {
            /* <div data-block="true" data-editor="c3ddk" data-offset-key="5n4ph-0-0">... */
            if (node.attributes['data-block'] && node.attributes['data-block'].value  === 'true') {
                var [key, x, y] = node.attributes['data-offset-key'].value.split('-');
                if (x !== '0' || y !== '0') {
                    console.log(node, node.attributes['data-offset-key']);
                    throw "unexpected block key value";
                }
                return key;
            }
        };

        var iterate = function(node, offset) {
            // out of parents (happens if point lies outside of editor_ component).
            if (!node) {
                return {};
            }

            // no block key - move to parent with current offset plus text length of left siblings.
            var blockkey = getKeyFromBlock(node);
            if (!blockkey) {
                return iterate(node.parentElement, offset + leftSiblingLength(node.previousSibling));
            }

            // block key found - return with current offset.
            return {
                "_selectionBlock": blockkey,
                "_selectionOffset": offset
            };
        };

        var mkPoint = function(container, offset) {
            return iterate(container.parentElement, offset + leftSiblingLength(container.previousSibling));
        };

        var sel        = getSelection();
        var range      = sel.getRangeAt(0);
        var backward   = sel.anchorNode !== range.startContainer;
        var startpoint = mkPoint(range.startContainer, range.startOffset);
        var endpoint   = mkPoint(range.endContainer, range.endOffset);

        return { "_selectionIsBackward": backward,
                 "_selectionStart": startpoint,
                 "_selectionEnd": endpoint
               };
    };
})((typeof global === 'undefined') ? window : global);
