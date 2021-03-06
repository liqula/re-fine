(function(target) {

    target.onscroll = function() {
        var el = document.getElementById("c-toolbar-menu-label");
        if (el) {
          if (el.getBoundingClientRect().top <= 50) {
              el.className = "c-toolbar-menu-label-visible";
          } else {
              el.className = "c-toolbar-menu-label-hidden";
          }
        }
    };

    // https://github.com/facebook/draft-js/blob/master/examples/draft-0-10-0/link/link.html
    target.refine$linkDecorator = new Draft.CompositeDecorator([
        {
            strategy:
            function (contentBlock, callback, contentState) {
                contentBlock.findEntityRanges(
                    function (character) {
                        const entityKey = character.getEntity();
                        return (
                            entityKey !== null && (contentState.getEntity(entityKey).getType() === 'LINK' ||
                                                   contentState.getEntity(entityKey).getType() === 'IMAGE')
                        );
                    },
                    callback
                );
            },
            component:
            function (props) {
                const entity = props.contentState.getEntity(props.entityKey);
                const data = entity.getData();
                const type = entity.getType();
                var elem;
                if (type === 'IMAGE') {
                    const src = data.src;
                    elem = React.createElement(
                        'img',
                        { src: src,
                          alt: src,
                          style: { marginLeft: 'auto',
                                   marginRight: 'auto',
                                   display: 'block'
                                 }
                        }
                    );
                } else {
                    elem = React.createElement(
                        'a',
                        { className: "tooltip", href: data, style: { color: '#3b5998', textDecoration: 'underline' } },
                        React.createElement(
                            "span",
                            { className: "tooltiptext" },
                            data
                        ),
                        props.children
                    );
                }
                return elem;
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

    target.refine$createSelectionState = function(beginBlockKey, beginOffset,
                                                  endBlockKey, endOffset,
                                                  backward, hasFocus) {
        return {
            "anchorKey"         : (backward ? endBlockKey   : beginBlockKey),
            "anchorOffset"      : (backward ? endOffset     : beginOffset),
            "focusKey"          : (backward ? beginBlockKey : endBlockKey),
            "focusOffset"       : (backward ? beginOffset   : endOffset),
            "isBackward"        : backward,
            "hasFocus"          : hasFocus
        };
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
            // out of parents (happens if point lies outside of draftEditor_ component).
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

                return { Right: refine$createSelectionState(startpoint._blockIndex, startpoint._columnIndex,
                                                            endpoint._blockIndex, endpoint._columnIndex,
                                                            backward, true)
                       };
            } catch(e) {
                return ({ Left: JSON.stringify(e) });
            };
        };
    })();

    target.refine$getRangeTopBottomOffset = function () {
        if (typeof getSelection === 'undefined') {
            return { top: 0, bottom: 0 };
        } else {
            var r = getSelection().getRangeAt(0).startContainer.parentElement.getBoundingClientRect();
            return { top: r.top, bottom: r.bottom };
        }
    };

    target.refine$removeAllRanges = function () {
        if (typeof getSelection === 'undefined') {
            return;
        } else {
            return getSelection().removeAllRanges();
        }
    };

})((typeof global === 'undefined') ? window : global);
