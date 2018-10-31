/* All "Feature Macros" (defined by this file) start with
 * either « HAS_ » or « IS_ ».
 * 
 * 
 */

/************************************************/

#if !defined(HS_SBOO_BASE_FEATURE_MACROS_H)

#define HS_SBOO_BASE_FEATURE_MACROS_H

/************************************************************************************************/

#ifndef __GLASGOW_HASKELL__
#define __GLASGOW_HASKELL__ 000
/* i.e. « v0.0.0 » a.k.a. « False » */
#endif

/************************************************/

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(x,y,z1,z2) 0
/* i.e. « const False » */
#endif

/************************************************/

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
/* i.e. « const False » */
#endif

/* NOTE `ghc-7.10` introduced `MIN_VERSION_GLASGOW_HASKELL`. */

/************************************************/

#define IS_COMPILER_ghc   __GLASGOW_HASKELL__
#define IS_COMPILER_ghcjs ghcjs_HOST_OS

/************************************************/

#define IS_OS_LINUX   defined(linux_HOST_OS)
#define IS_OS_WINDOWS defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS) 
#define IS_OS_APPLE   defined(darwin_HOST_OS)

/* #define IS_OS_ANDRIOD defined(linux_HOST_OS) || TODO */
/* #define IS_OS_IOS     defined(darwin_HOST_OS) || TODO */

/************************************************/

#define IS_ARCH_64_BIT_INTEL i386_HOST_ARCH
#define IS_ARCH_32_BIT_INTEL x86_64_HOST_ARCH

/************************************************************************************************/

/* « ghc-7.10 » ships with « base-4.8 ».

#define HAS_APPLICATIVE_MONAD                     MIN_VERSION_base(4,8,0)
/* the Applicative-Monad proposal */

#define HAS_FOLDABLE_TRAVERSABLE_IN_PRELUDE       MIN_VERSION_base(4,8,0)

#define HAS_PRELUDE_Monoid                        MIN_VERSION_base(4,8,0)
/* module Data.Monoid */
/* HAS_MONOID_IN_PRELUDE? */

#define HAS_BASE_Identity                         MIN_VERSION_base(4,8,0)
/* module Data.Functor.Identity */

#define HAS_BASE_Natural                          MIN_VERSION_base(4,8,0)
/* module Numeric.Natural */

#define HAS_BASE_Bifunctor                        MIN_VERSION_base(4,8,0)
/* module Data.Bifunctor */

#define HAS_METHOD_Exception_displayException     MIN_VERSION_base(4,8,0)
/* import "base" Control.Exception (Exception(displayException)) */

/************************************************/

#define HAS_MONAD_FAIL                            MIN_VERSION_base(4,9,0)
/* the Monad-Fail proposal */

#define HAS_PRELUDE_OPERATOR_Append               MIN_VERSION_base(4,9,0)
/* import "base" Data.Monoid ((<>)) */

#define HAS_BASE_Semigroup                        MIN_VERSION_base(4,9,0)
/* module Data.Semigroup */

#define HAS_BASE_NonEmpty                         MIN_VERSION_base(4,9,0)
/* module Data.List.NonEmpty */

#define HAS_BASE_MonadIO                          MIN_VERSION_base(4,9,0)
/* Control.Monad.IO.Class */
/* HAS_MonadIO_IN_base? */

#define HAS_BASE_Functors                         MIN_VERSION_base(4,9,0)
/* module Data.Functor.Sum     */
/* module Data.Functor.Product */

#define HAS_BASE_Type                             MIN_VERSION_base(4,9,0)
/* import Data.Kind (Type) */

#define HAS_BASE_UNARY_LIFTED_CLASSES             MIN_VERSION_base(4,9,0)
/* module Data.Functor.Classes */
/* Eq1, Ord1, Show1, Read1 */

#define HAS_BASE_BINARY_LIFTED_CLASSES            MIN_VERSION_base(4,9,0)
/* module Data.Functor.Classes */
/* Eq2, Ord2, Show2, Read2 ... */

/************************************************/

#define HAS_GHC_HasCallStack                      MIN_VERSION_base(4,9,0) && defined(__GLASGOW_HASKELL__)
/* module GHC.Stack */

/************************************************/

#define HAS_BASE_Bifoldable_Bitraversable         MIN_VERSION_base(4,10,0)
/* module Data.Bifoldable    */
/* module Data.Bitraversable */

/************************************************/

/* #define HAS_                                   MIN_VERSION_base(4,11,0) */

/************************************************/

#define HAS_BASE_Contravariant                    MIN_VERSION_base(4,12,0)
/* module Data.Functor.Contravariant */

/************************************************************************************************/

#define HAS_EXTENSION_DerivingLift            MIN_VERSION_GLASGOW_HASKELL(7,2,1,0)

/************************************************/
/* #define HAS_EXTENSION_                     MIN_VERSION_GLASGOW_HASKELL(7,4,1,0)
*/
/************************************************/

#define HAS_EXTENSION_LambdaCase              MIN_VERSION_GLASGOW_HASKELL(7,6,1,0)

/************************************************/

#define HAS_EXTENSION_OverloadedLists         MIN_VERSION_GLASGOW_HASKELL(7,8,1,0) 
#define HAS_EXTENSION_NegativeLiterals        MIN_VERSION_GLASGOW_HASKELL(7,8,1,0) 
#define HAS_EXTENSION_NumDecimals             MIN_VERSION_GLASGOW_HASKELL(7,8,1,0) 
#define HAS_EXTENSION_PatternSynonyms         MIN_VERSION_GLASGOW_HASKELL(7,8,1,0) 
#define HAS_EXTENSION_RoleAnnotations         MIN_VERSION_GLASGOW_HASKELL(7,8,1,0) 

/************************************************/

#define HAS_EXTENSION_DeriveAnyClass          MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)

#define HAS_EXTENSION_DeriveFunctor           MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
#define HAS_EXTENSION_DeriveFoldable          MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
#define HAS_EXTENSION_DeriveTraversable       MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
/* DeriveFunctor /*
/* and DeriveFoldable */
/* and DeriveTraversable */

#define HAS_EXTENSION_BinaryLiterals          MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
#define HAS_EXTENSION_PostfixOperators        MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)

#define HAS_EXTENSION_NamedWildCards          MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)
#define HAS_EXTENSION_StaticPointers          MIN_VERSION_GLASGOW_HASKELL(7,10,1,0)

/************************************************/

#define HAS_EXTENSION_Strict                  MIN_VERSION_GLASGOW_HASKELL(8,0,1,0) 
#define HAS_EXTENSION_StrictData              MIN_VERSION_GLASGOW_HASKELL(8,0,1,0) 

#define HAS_EXTENSION_TypeApplications        MIN_VERSION_GLASGOW_HASKELL(8,0,1,0) 
#define HAS_EXTENSION_UndecidableSuperClasses MIN_VERSION_GLASGOW_HASKELL(8,0,1,0) 

#define HAS_EXTENSION_ApplicativeDo           MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
#define HAS_EXTENSION_DuplicateRecordFields   MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)
#define HAS_EXTENSION_OverloadedLabels        MIN_VERSION_GLASGOW_HASKELL(8,0,1,0) 
#define HAS_EXTENSION_TemplateHaskellQuotes   MIN_VERSION_GLASGOW_HASKELL(8,0,1,0)

/************************************************/

#define HAS_EXTENSION_UnboxedSums             MIN_VERSION_GLASGOW_HASKELL(8,2,0,0) 

/************************************************/

#define HAS_EXTENSION_DerivingStrategies      MIN_VERSION_GLASGOW_HASKELL(8,4,1,0)

#define HAS_EXTENSION_HexFloatLiterals        MIN_VERSION_GLASGOW_HASKELL(8,4,1,0) 

/************************************************/

#define HAS_EXTENSION_DerivingVia             MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)

#define HAS_EXTENSION_BlockArguments          MIN_VERSION_GLASGOW_HASKELL(8,6,1,0) 
#define HAS_EXTENSION_NumericUnderscores      MIN_VERSION_GLASGOW_HASKELL(8,6,1,0) 
#define HAS_EXTENSION_StarIsType              MIN_VERSION_GLASGOW_HASKELL(8,6,1,0) 

/************************************************************************************************/

#define HAS_PRAGMA_COMPLETE                   MIN_VERSION_GLASGOW_HASKELL(8,2,1,0) 
/* e.g. « {-# COMPLETE LeftChoice, RightChoice #-} » */

/************************************************************************************************/
#endif