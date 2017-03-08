(function(target) {
    target.React = require("react");
    target.ReactDOM = require("react-dom");

    target.Sticky = require("react-sticky").Sticky;
    target.StickyContainer = require("react-sticky").StickyContainer;

    target.Skylight = require("react-skylight").SkyLightStateless;

    target.Hammer = require("react-hammerjs");

    target.Editor = require("draft-js").Editor;
    target.EditorState = require("draft-js").EditorState;
    target.ContentState = require("draft-js").ContentState;
})((typeof global === 'undefined') ? window : global);
