// module Saturnal.Event

exports._listen = function (unit) {
    return function (id) {
        return function (event) {
            return function (handler) {
                return function () {
                    var elem = document.getElementById(id);
                    if (elem) {
                        elem.addEventListener(event, function (_) {
                            handler();
                        });
                    };
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
                window.addEventListener("keydown", function (event) {
                    if (key === event.key && !timer) {
                        timer = setInterval(handler, 1000/60);
                    };
                });
                window.addEventListener("keyup", function (event) {
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

exports._mousedown = function (unit) {
    return function (id) {
        return function (handler) {
            return function () {
                var elem = document.getElementById(id);
                if (elem) {
                    elem.addEventListener("mousedown", function (event) {
                        handler(event.clientX)(event.clientY)();
                    });
                };
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

exports._after = function (unit) {
    return function (delay) {
        return function (handler) {
            return function () {
                setTimeout(handler, delay);
                return unit;
            };
        };
    };
};

exports._resize = function (unit) {
    return function (handler) {
        return function () {
            window.addEventListener("resize", function(_) {
                handler();
            });
            return unit;
        };
    };
};
