(function(target) {
    target.React = require("react");
    target.ReactDOM = require("react-dom");

    target.Sticky = require("react-sticky").Sticky;
    target.StickyContainer = require("react-sticky").StickyContainer;

    target.Skylight = require("react-skylight").SkyLightStateless;

    target.Hammer = require("react-hammerjs");

    target.Draft = require("draft-js");
    target.DraftEditor = target.Draft.Editor;  // (we need this to be a name (not an expression) for 'foreign_' to work.)

    target.DraftStateToHTML = require("draft-js-export-html").stateToHTML;

})((typeof global === 'undefined') ? window : global);
