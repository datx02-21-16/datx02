"use strict";

/**
 * Returns a string URL to the PNG image with the given name.
 */
exports.pictureURL = name => require("./Pictures/" + name + ".png");
