const path = require('path');
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");

module.exports = {
    mode: "development",
    entry: './app/index.js',
    output: {
        filename: 'output.js',
        path: path.resolve(__dirname, 'dist'),
    },
    resolve: {
        // We use node_modules provided by Nix shell via an environment variable
        modules: [process.env.NODE_PATH],
        extensions: [".js"],
        fallback: {
            buffer: require.resolve("buffer/"),
            http: false,
            url: false,
            stream: false,
            crypto: false,
            https: false,
            net: false,
            tls: false,
            zlib: false,
            os: false,
            path: false,
            fs: false,
            readline: false,
            child_process: false,
        },
        alias: {
            Scripts: path.resolve(__dirname, "dist"),
        }
    },
    plugins: [
        new webpack.DefinePlugin({
            BROWSER_RUNTIME: !!process.env.BROWSER_RUNTIME,
        }),
        new NodePolyfillPlugin(),
        new webpack.LoaderOptionsPlugin({
            debug: true,
        }),
        new HtmlWebpackPlugin(),
        new webpack.ProvidePlugin({
            Buffer: ["buffer", "Buffer"],
        })
    ]
};
