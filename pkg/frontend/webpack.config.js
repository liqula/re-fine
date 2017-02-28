var path = require("path");
var webpack = require("webpack");
var HtmlWebpackPlugin = require("html-webpack-plugin");

var IS_DEV = require('isdev');

var localPort = "9090";
var proxiedServer = "http://localhost:3000";

module.exports = {
    entry: [
        "./jsbits/util",
        "./jsbits/hookup",
        "./jsbits/hookup_scss"
         // do not serve the frontend JS via webpack "./js-build/frontend"
         /*
        "./js-build/rts",
        "./js-build/lib",
        "./js-build/out"
         */
    ],
     devServer: {
         contentBase: "./js-build"
         , lazy: false // always compile immediately to save time
         , compress: false // do not spend time on this
         , host: "0.0.0.0"  // server is also available externally
         , port: localPort
         , hot: true // hot module replacement
         , historyApiFallback: true
         , proxy: { "/": proxiedServer }

     },
    devtool: "source-map",
    output: {
        path: path.join(__dirname, "js-build"),
        filename: "[hash].bundle.js"  // for cache busting
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin(),
        // modifies index.html to include bundle.js:
        new HtmlWebpackPlugin({
            template: "./index-dev.html"
            , inject: 'head'  // we need it in the head because it must be loaded
                              // before the frontend bundle that is directly included
                              // in the HTML body
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
