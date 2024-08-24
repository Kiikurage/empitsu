const path = require("node:path");
const HTMLWebpackPlugin = require("html-webpack-plugin");

module.exports = {
	mode: process.env.NODE_ENV ?? "development",
	entry: {
		index: path.resolve(__dirname, "./src/index.tsx"),
	},
	output: {
		path: path.resolve(__dirname, "build"),
		filename: "[name].js",
	},
	resolve: {
		extensions: [".js", ".jsx", ".ts", ".tsx"],
	},
	module: {
		rules: [
			{
				test: /\.(?:js|jsx|ts|tsx)$/,
				exclude: /node_modules/,
				use: "babel-loader",
			},
			{
				test: /\.css$/,
				use: ["style-loader", "css-loader"],
			},
			{
				test: /\.svg$/,
				type: "asset/inline",
			},
		],
	},
	experiments: {
		asyncWebAssembly: true,
	},
	plugins: [
		new HTMLWebpackPlugin({
			template: path.resolve(__dirname, "./src/index.html"),
		}),
	],
};
