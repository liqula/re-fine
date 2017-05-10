(function(target) {
    target.React = require("react");
    target.ReactDOM = require("react-dom");
    target.Sticky = require("react-sticky");
    target.Skylight = require("react-skylight");
    target.Hammer = require("react-hammerjs");
    target.Draft = require("draft-js");
    target.DraftStateToHTML = require("draft-js-export-html").stateToHTML;  // TUNING: we don't need this in production.
})((typeof global === 'undefined') ? window : global);
