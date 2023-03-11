module Test.DeriveCheck (test) where

import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup
    "TODO(bladyjoker): Reimplement tests in Test.LambdaBuffers.Compiler.TypeClassCheck"
    []

{- | Tests to implement with MiniLog.

> Test 1: Basic test. Should pass
module A where

opaque Int
opaque Bool
opaque String

data Maybe a = Nothing | Just a

class C a

instance C Int
instance C Bool
instance C String
instance C a => C (Maybe a)

> Test 2: With imports (have to do them by hand). Should pass

module B where

import A

data Foo = Foo (Maybe Int)

instance C Foo

> Test 3: With imports / tyvar

module A where (...same as above...)

---------------------

module B where

import A

data Foo a = Foo (Maybe a)

instance C a => C (Foo a)

> Test 4. Circular instant constraints, should fail condition 1

module B where

import A

data Foo a = Foo (Maybe a)

instance C (Foo a) => C (Foo a)

> Test 5. Overlapping instances. Should fail w/ Overlap error

-- imports are as if this were A
module A where

(... same as above ...)
instance C (Maybe Int)

-----------------------------

module B where

data Foo = Foo (Maybe Int)

instance C Foo

> Test 6. Should fail w/ missing Int instance

same as moduleB'2 w/ the imported (C Int) instance removed

> Test 7. Should fail w/ constructor in constraint context error.

same as moduleB'2 except the instance dec is replaced by:

instance C (Maybe Int) => C Foo

> Test 8. Multiple TyVars. Should pass

module C where

import A

data Bar a b = MkBar a b Int

instance C (Bar a b)

> Test 9. Phantom tyvar. Should pass

module D where

import A

data Bar a b = MkBar a Int

instance C (Bar a b)

> Test 10. Phantom concrete type. Should pass (b/c instance isn't needed for phantom type)

module D where

import A

data Bar a b = MkBar a Int

instance C (Bar a Bool)

> Test 11. Modification of test 9 where non-phantom instance is missing. Should fail w/ missing Int instance

module D where

import A

data Bar a b = MkBar a Int

instance C (Bar a Bool)
-}
