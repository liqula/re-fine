var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var jsonImporter = require("node-sass-json-importer");

module.exports = {
    entry: [
        "./static/hookup",
        "./js-build/frontend"
    ],
    output: {
        path: path.join(__dirname, "js-build"),
        // publicPath: './rankings/',
        filename: "[hash].bundle.js"  // for cache busting
    },
    plugins: [
        // modifies index.html to include bundle.js:
        new HtmlWebpackPlugin({
            template: "./index.html"
        }),
        new webpack.DefinePlugin({
            "process.env": {
                IS_IN_WEBPACK: true,
                NODE_ENV: '"production"'
            }
        })
    ],
    devtool: "source-map",
    module: {
        loaders: [
            { test: /\.css$/, loader: "style-loader!css-loader" },
            { test: /\.scss$/, loader: "style-loader!css-loader!sass-loader" },
            { test: /\.(ttf|woff|svg)\?32imnj/, loader: "url-loader" },
            { test: /\.svg$/, loader: "url-loader" }
        ]
    },
    sassLoader: {
        includePaths: [path.resolve(__dirname, "./node_modules/gridle/sass"), path.resolve(__dirname, "./node_modules/bourbon/app/assets/stylesheets")],
        importer: jsonImporter
    }
};
