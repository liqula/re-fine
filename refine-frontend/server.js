var webpack = require("webpack");
var WebpackDevServer = require("webpack-dev-server");
var config = require("./webpack.config");

// settings for local dev server
var localPort = 9090;
var proxiedServer = "http://localhost:3000";

new WebpackDevServer(webpack(config), {
    publicPath: config.output.publicPath,
    hot: true,
    historyApiFallback: true,
    proxy: {
        "*": proxiedServer
    }
}).listen(localPort, "localhost", function (err) {
    if (err) {
        console.log(err);
    }

    console.log("Listening at localhost:" + localPort + " proxying to " + proxiedServer);
});
