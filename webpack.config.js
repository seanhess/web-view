 
const path = require('path');

module.exports = {
  entry: "./client/index.ts",
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    modules: ["node_modules",path.resolve(__dirname, "client")],
    extensions: ['.tsx', '.ts', '.js'],
  }
}

