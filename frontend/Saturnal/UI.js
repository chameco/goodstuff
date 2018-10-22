var POPUP = null;

exports._toggle = function (unit) {
    return function(id) {
        return function () {
            var elem = document.getElementById(id);
            if (elem) {
                elem.classList.toggle("hidden");
            }
            return unit;
        };
    };
};

exports._hide = function (unit) {
    return function(id) {
        return function () {
            var elem = document.getElementById(id);
            if (elem) {
                elem.classList.add("hidden");
            }
            return unit;
        };
    };
};

exports._unhide = function (unit) {
    return function(id) {
        return function () {
            var elem = document.getElementById(id);
            if (elem) {
                elem.classList.remove("hidden");
            }
            return unit;
        };
    };
};

exports._display = function (unit) {
    return function(id) {
        return function () {
            var elem = document.getElementById(id);
            if (elem) {
                elem.style.display = "block";
            }
            return unit;
        };
    };
};

exports._undisplay = function (unit) {
    return function(id) {
        return function () {
            var elem = document.getElementById(id);
            if (elem) {
                elem.style.display = "none";
            }
            return unit;
        };
    };
};

exports._popup = function (unit) {
    return function (popup) {
        return function (title) {
            return function (body) {
                return function () {
                    var elem = document.getElementById("popup");
                    if (elem && POPUP === popup) {
                        elem.classList.add("hidden");
                        POPUP = null;
                    } else {
                        var popuptitle = document.getElementById("popuptitle");
                        var popupbody = document.getElementById("popupbody");
                        if (popuptitle && popupbody) {
                            POPUP = popup;
                            popuptitle.innerHTML = title;
                            popupbody.innerHTML = body;
                        };
                        elem.classList.remove("hidden");
                    };
                    return unit;
                };
            };
        };
    };
};

exports._setHTML = function(unit) {
    return function (id) {
        return function (val) {
            return function () {
                var elem = document.getElementById(id);
                if (elem) {
                    elem.innerHTML = val;
                };
                return unit;
            };
        };
    };
};

exports._getValue = function (Just) {
    return function (Nothing) {
        return function (id) {
            return function () {
                var elem = document.getElementById(id);
                if (elem) {
                    return Just(elem.value.toString());
                } else {
                    return Nothing;
                };
            };
        };
    };
};
