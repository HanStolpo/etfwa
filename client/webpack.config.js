
var webpack = require('webpack');
var path = require('path');
var HtmlwebpackPlugin = require('html-webpack-plugin');
var merge = require('webpack-merge');

var TARGET = process.env.npm_lifecycle_event;
var ROOT_PATH = path.resolve(__dirname);
var APP_PATH = path.resolve(ROOT_PATH, 'src');
var BUILD_PATH = path.resolve(ROOT_PATH, '../data/client');

var srcsP = ['src[]=bower_components/purescript-*/src/**/*.purs', 'src[]=src/**/*.purs']
var ffisP = ['ffi[]=bower_components/purescript-*/src/**/*.js'];
var outputP = 'output';

console.log(TARGET);

var modulesDirectories = [
  'node_modules',
  // The bower component for purescript-prelude is specified here to
  // allow JavaScript files to require the 'Prelude' module globally.
  'bower_components/purescript-prelude/src'
];

var common = {
  entry: "./src/entry",
  output: {
    path: BUILD_PATH,
    filename: 'bundle.js'
  },
  module: {
    loaders: [
      {
        test: /\.css$/,
        loaders: ['style', 'css'],
        include: APP_PATH
      },
      { test: /\.purs$/,
        loader: 'purs-loader?output=' + outputP + '&' + srcsP.concat(ffisP).join('&')
      }
    ]
  },
  resolve: { modulesDirectories: modulesDirectories , extensions: ['', '.js', '.purs'] },
  resolveLoader: { root: path.join(ROOT_PATH, 'node_modules') },
};

if(TARGET === 'build' || !TARGET) {
  module.exports = merge(common, {
    externals: {
      "./settings": "SettingsETFWA"
    },
    devtool: 'eval-source-map',
    plugins: [
      new HtmlwebpackPlugin({
        title: 'ETFWA',
        filename: '../index.html',
        template: 'html/index_template.html',
        inject: true
      })
    ]
  });
}

var common_start = merge(common, {
  devtool: 'eval-source-map',
  devServer: {
    historyApiFallback: true,
    hot: true,
    inline: true,
    progress: true,
    // parse host and port from env so this is easy
    // to customize
    host: process.env.HOST,
    port: process.env.PORT
  },
  plugins: [
    new HtmlwebpackPlugin({title: 'ETFWA'}),
    new webpack.HotModuleReplacementPlugin()
  ]
});


if(TARGET === 'start-css') {
  module.exports = merge(common_start, {entry: "./src/entry-css"});
}

if(TARGET === 'start-counter') {
  module.exports = merge(common_start, {entry: "./src/entry-routes"});
}

if(TARGET === 'start-routes') {
  module.exports = merge(common_start, {entry: "./src/entry-routes"});
}

if(TARGET === 'start-echo') {
  module.exports = merge(common_start, {entry: "./src/entry-echo"});
}


if(TARGET === 'start') {
  module.exports = merge(common_start, {entry: "./src/entry"});
}
