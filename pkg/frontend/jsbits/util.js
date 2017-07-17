(function(target) {

    // https://github.com/facebook/draft-js/blob/master/examples/draft-0-10-0/link/link.html
    target.refine$linkDecorator = new Draft.CompositeDecorator([
        {
          strategy:
              function (contentBlock, callback, contentState) {
                contentBlock.findEntityRanges(
                  function (character) {
                    const entityKey = character.getEntity();
                    return (
                      entityKey !== null &&
                      contentState.getEntity(entityKey).getType() === 'LINK'
                    );
                  },
                  callback
                );
              },
          component:
              function (props) {
                var url = props.contentState.getEntity(props.entityKey).getData();
                return React.createElement(
                    'a',
                    { className: "tooltip", href: url, style: { color: '#3b5998', textDecoration: 'underline' } },
                    React.createElement(
                        "span",
                        { className: "tooltiptext" },
                        url
                    ),
                    props.children
                );
              },
        },
      ]);

    var refine$previousDocumentBodyClientWidth = 0;

    target.refine$documentBodyClientWidth = function() {
        var current = document.body.clientWidth;
        if (current === refine$previousDocumentBodyClientWidth) {
            return -1;
        } else {
            refine$previousDocumentBodyClientWidth = current;
            return current;
        }
    };

    var refine$previousHeaderHeight = 0;

    target.refine$getHeaderHeight = function(domThis) {
        var current = Math.floor(domThis.getBoundingClientRect().height);
        if (current === refine$previousHeaderHeight) {
            return -1;
        } else {
            refine$previousHeaderHeight = current;
            return current;
        }
    };

    target.refine$editorContentFromHtml = function(html) {
        const blocksFromHTML = Draft.convertFromHTML(html);
        return Draft.ContentState.createFromBlockArray(
            blocksFromHTML.contentBlocks,
            blocksFromHTML.entityMap
        );
    };

    target.refine$createSelectionState = function(beginBlock, beginOffset, endBlock, endOffset, backwards, hasFocus) {
        return false;  // TODO
    };

    target.refine$getDraftSelectionStateViaBrowser = (function() {
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
                var attrs = node.attributes['data-offset-key'].value.split('-');
                var key = attrs[0];
                var x = attrs[1];
                var y = attrs[2];
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
                _blockIndex: blockkey,
                _columnIndex: offset
            };
        };

        var mkPoint = function(container, offset) {
            return iterate(container.parentElement, offset + leftSiblingLength(container.previousSibling));
        };

        return function() {
            try {
                var sel        = getSelection(); if (!sel.anchorNode || !sel.focusNode) { return { Left: "no selection" }; }
                var range      = sel.getRangeAt(0);
                var backward   = sel.anchorNode !== range.startContainer;
                var startpoint = mkPoint(range.startContainer, range.startOffset);
                var endpoint   = mkPoint(range.endContainer, range.endOffset);

                // TODO: this doesn't work any more with the new custom from/tojson instances.
                // do `return refine$createSelectionState(beginBlock, beginOffset, endBlock, endOffset, backwards, hasFocus);` instead.
                return { Right: { _selectionIsBackward: backward,
                                  _selectionRange: {
                                      _rangeBegin: startpoint,
                                      _rangeEnd: endpoint
                                  }
                                }
                       };
            } catch(e) {
                return ({ Left: JSON.stringify(e) });
            };
        };
    })();
})((typeof global === 'undefined') ? window : global);
