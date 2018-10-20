// module Saturnal.Event

var STATE = null;

exports._setState = function (unit) {
    return function (state) {
        return function () {
            STATE = state;
            return unit;
        };
    };
};

exports.withState = function (x) {
    return function (handler) {
        return function () {
            if (STATE) {
                return handler(STATE);
            } else {
                return x;
            };
        };
    };
};

exports._listen = function (unit) {
    return function (id) {
        return function (event) {
            return function (handler) {
                return function () {
                    document.getElementByID(id).addEventListener(event, function (_) {
                        handler();
                    });
                    return unit;
                };
            };
        };
    };
};

exports._key = function (unit) {
    return function (key) {
        return function (handler) {
            return function () {
                var timer = null;
                window.addEventListener("keydown", function(event) {
                    if (key === event.key && !timer) {
                        timer = setInterval(handler, 1000/60);
                    };
                });
                window.addEventListener("keyup", function(event) {
                    if (key === event.key && timer) {
                        clearInterval(timer);
                        timer = null;
                    };
                });
                return unit;
            };
        };
    };
};

exports._frames = function (unit) {
    return function (handler) {
        return function () {
            window.requestAnimationFrame(function tick(_) {
                handler();
                window.requestAnimationFrame(tick);
            });
            return unit;
        };
    };
};
