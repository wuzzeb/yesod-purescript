// module ModA

exports.def = function(x) { return x + "def"; };
exports.append = function(x) { return function(y) { return x + y; }; };
