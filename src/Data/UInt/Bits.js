"use strict";

// module Data.UInt.Bits

exports.and = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return (n1 & n2) >>> 0;
  };
};

exports.or = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return (n1 | n2) >>> 0;
  };
};

exports.xor = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return (n1 ^ n2) >>> 0;
  };
};

exports.shl = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return (n1 << n2) >>> 0;
  };
};

exports.shr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return (n1 >> n2) >>> 0;
  };
};

exports.zshr = function (n1) {
  return function (n2) {
    /* jshint bitwise: false */
    return (n1 >>> n2) >>> 0;
  };
};

exports.complement = function (n) {
  /* jshint bitwise: false */
    return (~n >>> 0);
};
