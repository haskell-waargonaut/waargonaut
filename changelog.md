# Revision history for waargonaut

## 0.2.0.3  -- 2018-11-13

* Add `MonadError` and `Alt` instance for `Decoder`
* Add property tests for the typeclass laws for `Encoder` and `Decoder`
* Removed need for `MonadError` constraint on `prismDOrFail`

## 0.2.0.2  -- 2018-11-12

* Fix `Applicative` instance for `Decoder`.

## 0.2.0.1  -- 2018-11-07

* Update `moveToKey` to record a successful movement to a key, before continuing

## 0.2.0.0  -- 2018-11-06

* Provide more precise errors from Decoder for missing or invalid keys
* Removed a parameter from `KeyDecodeFailed` error constructor
* Fix issue where printing the zipper movements had left and right movement arrows swapped.

## 0.1.0.0  -- 2018-11-01

* First version. Released on an unsuspecting world.
