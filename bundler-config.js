const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const webpack = require('webpack')

const devConf = {
  entry: [
    'webpack-dev-server/client?http://localhost:8000',
    path.join(__dirname, 'index.js')
  ],
  output: {
    path: path.join(__dirname, 'build'),
    filename: '[name].js',
    publicPath: '/'
  },
  resolve: {
    extensions: ['', '.js', '.elm'],
    alias: { 'config': './config.dev.js' }
  },
  module: {
    noParse: [/.elm$/],
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-hot!elm-webpack'
      },
      {
        test: /\.css$/,
        loader: 'style!css'
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      inject: true
    })
  ],
  devServer: {
    port: 8000,
    open: true,
    contentBase: './build'
  }
}

const prodConf = {
  entry: path.join(__dirname, 'index.js'),
  output: {
    path: path.join(__dirname, 'build'),
    filename: '[name].[hash].js',
    publicPath: '/harvest-overtime'
  },
  resolve: {
    extensions: ['', '.js', '.elm'],
    alias: { 'config': './config.prod.js' }
  },
  module: {
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack'
      },
      {
        test: /\.css$/,
        loader: ExtractTextPlugin.extract('style', 'css')
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      inject: true,
      minify: {
        removeComments: true,
        collapseWhitespace: true,
        removeRedundantAttributes: true,
        useShortDoctype: true,
        removeEmptyAttributes: true,
        removeStyleLinkTypeAttributes: true,
        keepClosingSlash: true,
        minifyJS: true,
        minifyCSS: true,
        minifyURLs: true
      }
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
    new webpack.optimize.DedupePlugin(),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        screw_ie8: true,
        warnings: false
      },
      mangle: {
        screw_ie8: true
      },
      output: {
        comments: false,
        screw_ie8: true
      }
    }),
    new ExtractTextPlugin('[name].[contenthash:20].css')
  ]
}

module.exports = process.env.NODE_ENV === 'production' ? prodConf : devConf
