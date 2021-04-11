"use strict";

const path = require('path'),
	  webpack = require('webpack'),
	  HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
	output: {
		assetModuleFilename:"images/[name][ext]"
	},
	resolve: {
		modules: [
			"node_modules",
		],
		extensions: ['.js', '.purs'],
	},
	devtool: "eval-source-map",
	devServer: {
		hot: true,
	},
	plugins: [
		new webpack.HotModuleReplacementPlugin(),
		new HtmlWebpackPlugin({
			title: 'Proof Editor',
		}),
	],
	module: {
		rules: [
			{
				test: /\.purs$/,
				use: [
					{
						loader: "purs-loader",
						options: {
							spago: true,
							psc: "psa",
							watch: true,
							// pscIde: true,
						}
					},
				]
			},
			{
				test: /\.s[ac]ss$/i,
				use: [
					// Creates `style` nodes from JS strings
					"style-loader",
					// Translates CSS into CommonJS
					"css-loader",
					// Compiles Sass to CSS
					"sass-loader",
				],
			},
			{
              test: /\.(png|jpeg|jpg)$/i,
			  type: "asset/resource"
			},
			{
				test: /\.html$/i,
				use: [ "html-loader" ]
			}
		]
	}
};
