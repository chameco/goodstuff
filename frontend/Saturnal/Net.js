var PLAYER = null;
var GAME = null;

exports._getPlayer = function (Just) {
    return function (Nothing) {
        return function () {
            if (PLAYER) {
                return Just(PLAYER);
            } else {
                return Nothing;
            };
        };
    };
};

exports._setGame = function (unit) {
    return function (game) {
        return function () {
            GAME = game;
            return unit;
        };
    };
};

exports._getGame = function (Just) {
    return function (Nothing) {
        return function () {
            if (GAME) {
                return Just(GAME);
            } else {
                return Nothing;
            };
        };
    };
};

exports._login = function (unit) {
    return function (username) {
        return function (password) {
            return function (handler) {
                return function () {
                    var req = new XMLHttpRequest();
                    req.open("POST", "/saturnal/login", true);
                    req.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
                    req.onreadystatechange = function () {
                        if (req.readyState == 4) {
                            if (req.status == 200) {
                                PLAYER = username;
                                handler();
                            } else {
                                alert("Login failed");
                            };
                        };
                    };
                    req.send("player=" + username + "&pass=" + password);
                };
            };
        };
    };
};

exports._poll = function (unit) {
    return function (game) {
        return function (handler) {
            return function () {
                var req = new XMLHttpRequest();
                req.open("GET", "/saturnal/board/" + game, true);
                req.onreadystatechange = function () {
                    if (req.readyState == 4) {
                        if (req.status == 200) {
                            handler(req.responseText)();
                        } else {
                            alert("Failed to access game");
                        };
                    };
                };
                req.send();
            };
        };
    };
};

exports._sendMoves = function (unit) {
    return function (moves) {
        return function () {
        };
        return unit;
    };
};
