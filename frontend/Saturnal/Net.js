var PLAYER = null;
var GAME = null;

exports._getPlayerCookie = function (Just) {
    return function (Nothing) {
        return function () {
            var cookieValue = document.cookie.replace(/(?:(?:^|.*;\s*)player\s*\=\s*([^;]*).*$)|^.*$/, "$1");
            if (!cookieValue || cookieValue === "") {
                return Nothing;
            } else {
                return Just(cookieValue);
            }
        };
    };
};

exports._getGameURL = function () {
    return window.location.hash;
}

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

exports._setPlayer = function (unit) {
    return function(player) {
        return function() {
            PLAYER = player;
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

exports._setGame = function (unit) {
    return function (game) {
        return function () {
            GAME = game;
            return unit;
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

exports._newGame = function (unit) {
    return function (width) {
        return function (height) {
            return function (handler) {
                return function () {
                    var req = new XMLHttpRequest();
                    req.open("POST", "/saturnal/board", true);
                    req.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
                    req.onreadystatechange = function () {
                        if (req.readyState == 4) {
                            if (req.status == 200) {
                                handler(req.responseText)();
                            } else {
                                alert("Failed to create game");
                            };
                        };
                    };
                    req.send("width=" + width.toString() + "&height=" + height.toString());
                };
            };
        };
    };
};

exports._invite = function (unit) {
    return function (player) {
        return function (handler) {
            return function () {
                var req = new XMLHttpRequest();
                req.open("PUT", "/saturnal/board/" + GAME + "/invite", true);
                req.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
                req.onreadystatechange = function () {
                    if (req.readyState == 4) {
                        if (req.status == 200) {
                            handler();
                        } else {
                            alert("Failed to invite player");
                        };
                    };
                };
                req.send("player=" + player);
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
                            alert("Could not access game");
                        };
                    };
                };
                req.send();
                return unit;
            };
        };
    };
};

exports._submitTurn = function (unit) {
    return function (turn) {
        return function (handler) {
            return function () {
                var req = new XMLHttpRequest();
                req.open("POST", "/saturnal/board/" + GAME + "/turn", true);
                req.setRequestHeader("Content-type", "application/json");
                req.onreadystatechange = function () {
                    if (req.readyState == 4) {
                        if (req.status == 200) {
                            handler();
                        } else {
                            alert("Failed to submit turn");
                        };
                    };
                };
                req.send(turn);
                return unit;
            };
        };
    };
};
