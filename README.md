<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

[![Build Status](https://travis-ci.org/qfpl/waargonaut.svg?branch=master)](https://travis-ci.org/qfpl/waargonaut)

# Waargonaut

_NB:_ **BETA Release**

Flexible, precise, and efficient JSON decoding/encoding library. This package
provides a plethora of tools for decoding, encoding, and manipulating JSON data.

## Features

* Fully RFC compliant, with property based testing used to ensure the desired
  invariants are preserved.

* Encoders and Decoders are values, they are not tied to a typeclass and as such
  you are not tied to a single interpretation of how a particular type
  "_should_" be handled.
  
* No information is discarded on parsing. Trailing whitespace, and any
  formatting whitespace (carriage returns etc) are all preserved. 

* A history keeping zipper is used for Decoding, providing precise control of
  how _you_ decode _your_ JSON data. With informative error messages if things
  don't go according to plan.

* Flexible and expressive Decoder & Encoder functions let you parse and build
  the JSON structures _you_ require, with no surprises.

* BYO parsing library, the parser built into Waargonaut does not tie you to a
  particular parsing library. With the caveat that your parsing library must
  have an instance of `CharParsing` from the [parsers](https://hackage.haskell.org/package/parsers) package.

* Generic functions are provided to make the creation of Encoders and Decoders
  are bit easier. However these _are_ tied to typeclasses, so they do come with
  some assumptions.

* Lenses, Prisms, and Traversals are provided to allow you to investigate and
  manipulate the JSON data structures to your hearts content, without breaking
  the invariants.

* The awesome work on succinct data structures by John Ky and [Haskell Works](https://github.com/haskell-works/) 
  is used to power the decoder. Providing the same zipper capabilities and
  property based guarantees, but with all the speed and efficiency capabilities
  that succinct data structures have to offer.

## Example

- Data Structure:
```haskell
data Image = Image
  { _imageWidth    :: Int
  , _imageHeight   :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
```

- Encoder:
```haskell
encodeImage :: Applicative f => Encoder f Image
encodeImage = E.mapLikeObj $ \img ->
    E.intAt "Width" (_imageWidth img)
  . E.intAt "Height" (_imageHeight img)
  . E.textAt "Title" (_imageTitle img)
  . E.boolAt "Animated" (_imageAnimated img)
  . E.listAt E.int "IDs" (_imageIDs img)
```

- Decoder:
```haskell
imageDecoder :: Monad f => D.Decoder f Image
imageDecoder = D.withCursor $ \curs -> do
  -- Move down into the JSON object.
  io <- D.down curs
  -- We need individual values off of our object,
  Image
    <$> D.fromKey "Width" D.int io
    <*> D.fromKey "Height" D.int io
    <*> D.fromKey "Title" D.text io
    <*> D.fromKey "Animated" D.bool io
    <*> D.fromKey "IDs" (D.list D.int) io
```

### Zippers

Waargonaut uses zippers for its decoding which allows for precise control in
how you interrogate your JSON input. Take JSON structures and decode them
precisely as you require:

##### Input:

```JSON
["a","fred",1,2,3,4]
```

##### Data Structure:

```haskell
data Foo = Foo (Char,String,[Int])
```

##### Decoder:

The zipper starts the very root of the JSON input, we tell it to move 'down'
into the first element.
```haskell
fooDecoder :: Monad f => Decoder f Foo
fooDecoder = D.withCursor $ \cursor -> do
  fstElem <- D.down cursor
```
From the first element we can then decode the focus of the zipper using a
specific decoder:
```haskell
  aChar <- D.focus D.unboundedChar fstElem
```
The next thing we want to decode is the second element of the array, so we
move right one step or tooth, and then attempt to decode a string at the
focus.
```haskell
  aString <- D.moveRight1 fstElem >>= D.focus D.string
```
Finally we want to take everything else in the list and combine them into a
single list of Int values. Starting from the first element, we move right
two positions (over the char and the string elements), then we use one of
the provided decoder functions that will repeatedly move in a direction and
combine all of the elements it can until it can no longer move.
```haskell
  aIntList <- D.moveRightN 2 fstElem >>= D.rightwardSnoc [] D.int
```
Lastly, we build the Foo using the decoded values.
```haskell
  pure $ Foo (aChar, aString, aIntList)
```

The zipper stores the history of your movements, so any errors provide
information about the path they took prior to encountering an error. Making
debugging precise and straight-forward.

### Property Driven Development

This library is built to parse and produce JSON in accordance with the [RFC
8259](https://tools.ietf.org/html/rfc8259) standard. The data structures,
parser, and printer are built to satify the [Round Trip Property](https://teh.id.au/posts/2017/06/07/round-trip-property/):

Which may be expressed using the following pseudocode:

```
parse . print = id
```
This indicates that any JSON produced by this library will be parsed back in as
the exact data structure that produced it. This includes whitespace such as
carriage returns and trailing whitespace. There is no loss of information.

There is also this property, again in pseudocode:

```
print . parse . print = print
```
This states that the printed form of the JSON will not change will be identical
after parsing and then re-printing. There is no loss of information.

This provides a solid foundation to build upon.

**NB:** The actual code will of course return values that account for the
possibility of failure. Computers being what they are.

### TODO(s)

In no particular order...

- [ ] improve/bikeshed encoding object api 
- [ ] gather feedback on tests/benchmarks that matter to people
- [ ] provide testing functions so users can be more confident in their Encoder/Decoder construction
- [ ] documentation in the various modules to explain any weirdness or things that users may consider to be 'missing' or 'wrong'.
- [ ] provide greater rationale behind lack of reliance in typeclasses for encoding/decoding
- [ ] provide functions to add preset whitespace layouts to encoded json.
