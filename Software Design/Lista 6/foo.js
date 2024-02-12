"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.square = exports.pi = void 0;
// only one per file, doesn't have to be named
function default_1() {
    console.log("Hello world!");
}
exports.default = default_1;
// multiple named exports
exports.pi = 3.14;
function square(n) {
    return n * n;
}
exports.square = square;
