var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");

var IS_DEV = require('isdev');

module.exports = {
    entry: [
        "./jsbits/hookup",
        "./jsbits/hookup_scss",
        "./js-build/frontend"
    ],
    output: {
        path: path.join(__dirname, "js-build"),
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
        rules: [
            { test: /\.css$/, use: [ { loader: "style-loader" }, { loader: "css-loader" } ] },
            {
                test: /\.scss$/,
                include: [
                    path.resolve(__dirname, "sass"),
                    path.resolve(__dirname, "scss")
                ],
                use: [{
                    loader: 'style-loader',
                    options: {
                        sourceMap: IS_DEV
                    }
                },{
                    loader: 'css-loader',
                    options: {
                        sourceMap: IS_DEV
                    }
                },{
                    loader: 'sass-loader',
                    options: {
                        sourceMap: IS_DEV,
                        includePaths: [path.resolve(__dirname, "./node_modules/gridle/sass"), path.resolve(__dirname, "./node_modules/bourbon/app/assets/stylesheets")]
                    }
                }]
            },
            { test: /\.(svg|woff2)$/, use: [ { loader: "url-loader" } ] }
        ]
    }
};
