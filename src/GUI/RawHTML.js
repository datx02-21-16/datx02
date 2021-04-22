"use strict";

exports.setInnerHTML
	= htmlString => element => () => element.innerHTML = htmlString;
