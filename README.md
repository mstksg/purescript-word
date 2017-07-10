# purescript-word

[![Latest release](http://img.shields.io/github/release/Proclivis/purescript-word.svg)](https://github.com/Proclivis/purescript-word/releases)
[![Build status](https://travis-ci.org/Proclivis/purescript-word.svg?branch=master)](https://travis-ci.org/Proclivis/purescript-word)

Fixed width words and operations.

## Module documentation

- [Published on Pursuit](http://pursuit.purescript.org/packages/purescript-word/)

## Example

```purescript
import Data.Integral
import Data.Word
fromIntegral 4 .|. fromIntegral 2 :: Word8
Word8 0x06 (6u)
```

## Installation and usage
You can install this package via Bower. You will also need [BigInteger.js](https://github.com/peterolson/BigInteger.js), which can be installed via `npm`:
```
bower install purescript-word
```
For the browser, remember to bundle `BigInteger.min.js` with your code.

## Development
```
bower install
npm install
```
Then, use `pulp` to build, run tests and generate the documentation.