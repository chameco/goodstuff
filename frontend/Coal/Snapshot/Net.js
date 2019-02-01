var SNAPSHOT = null;
var INFO = null;

exports._setSnapshot = function (unit) {
    return function (snapshot) {
        return function () {
            SNAPSHOT = snapshot;
            return unit;
        };
    };
};

exports._unsetSnapshot = function (unit) {
    return function () {
        SNAPSHOT = null;
        return unit;
    };
};

exports._getSnapshot = function (Just) {
    return function (Nothing) {
        return function () {
            if (SNAPSHOT) {
                return Just(SNAPSHOT);
            } else {
                return Nothing;
            };
        };
    };
};


exports._retrieveSnapshot = function (unit) {
    return function (playerid) {
        return function (handler) {
            return function () {
                var req = new XMLHttpRequest();
                req.open("GET", "/coal/snapshot/" + playerid, true);
                req.onreadystatechange = function () {
                    if (req.readyState == 4) {
                        if (req.status == 200) {
                            handler(req.responseText)();
                        } else {
                            alert("Could not access snapshot");
                        };
                    };
                };
                req.send();
                return unit;
            };
        };
    };
};

exports._submitSnapshot = function (unit) {
    return function (playerid) {
        return function (snapshot) {
            return function (handler) {
                return function () {
                    var req = new XMLHttpRequest();
                    req.open("PUT", "/coal/snapshot/" + playerid, true);
                    req.setRequestHeader("Content-type", "application/json");
                    req.onreadystatechange = function () {
                        if (req.readyState == 4) {
                            if (req.status == 200) {
                                handler();
                            } else {
                                alert("Failed to submit snapshot");
                            };
                        };
                    };
                    req.send(snapshot);
                    return unit;
                };
            };
        };
    };
};

exports._setInfo = function (unit) {
    return function (info) {
        return function () {
            INFO = info;
            return unit;
        };
    };
};

exports._unsetInfo = function (unit) {
    return function () {
        INFO = null;
        return unit;
    };
};

exports._getInfo = function (Just) {
    return function (Nothing) {
        return function () {
            if (INFO) {
                return Just(INFO);
            } else {
                return Nothing;
            };
        };
    };
};


exports._retrieveInfo = function (unit) {
    return function (playerid) {
        return function (handler) {
            return function () {
                var req = new XMLHttpRequest();
                req.open("GET", "/coal/snapshot/info/" + playerid, true);
                req.onreadystatechange = function () {
                    if (req.readyState == 4) {
                        if (req.status == 200) {
                            handler(req.responseText)();
                        } else {
                            alert("Could not access snapshot");
                        };
                    };
                };
                req.send();
                return unit;
            };
        };
    };
};
