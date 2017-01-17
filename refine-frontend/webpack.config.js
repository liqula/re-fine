var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var jsonImporter = require("node-sass-json-importer");

var localPort = "9090";

module.exports = {
    entry: [
        "webpack-dev-server/client?http://localhost:" + localPort,
        "webpack/hot/only-dev-server",
        "./static/hookup",
        "./js-build/frontend"
    ],
     devServer: {
         contentBase: "./js-build"
     },
    devtool: "source-map",
    output: {
        path: path.join(__dirname, "js-build"),
        //filename: "bundle.js"
        filename: "[hash].bundle.js" // for cache busting
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        // modifies index.html to include bundle.js:
        new HtmlWebpackPlugin({
            template: "./index.html"
        }),
        new webpack.DefinePlugin({
            "process.env": {
                IS_IN_WEBPACK: true,
                NODE_ENV: '"development"'
            }
        })
    ],
    devtool: "source-map",
    module: {
        loaders: [
            { test: /\.css$/, loader: "style-loader!css-loader" },
            { test: /\.scss$/, loader: "style-loader!css-loader!sass-loader" },
            { test: /\.(ttf|woff|svg)\?32imnj/, loader: "url-loader" }

        ]
    },
    sassLoader: {
        includePaths: [path.resolve(__dirname, "./node_modules/gridle/sass"), path.resolve(__dirname, "./node_modules/bourbon/app/assets/stylesheets")],
        importer: jsonImporter
    }
};
