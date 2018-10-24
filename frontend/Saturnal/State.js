var STATE = null;

exports._setState = function (unit) {
    return function (state) {
        return function () {
            STATE = state;
            return unit;
        };
    };
};

exports._getState = function (Just) {
    return function (Nothing) {
        return function () {
            if (STATE) {
                return Just(STATE);
            } else {
                return Nothing;
            };
        };
    };
};
