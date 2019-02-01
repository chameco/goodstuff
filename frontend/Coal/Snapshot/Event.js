// module Coal.Snapshot.Event

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
