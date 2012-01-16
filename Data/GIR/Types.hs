{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.GIR.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.GIR.Types
	( Doc(..)
	, Type(..)
	, Deprecated
	, TransferOwnership(..)
	, Direction(..)
	, Parameter(..)
	, Class(..)
	, Record(..)
	, Constant(..)
	, Alias(..)
	, ReturnValue(..)
	, Field(..)
	, Enumeration(..)
	, Bitfield(..)
	, Constructor(..)
	, Callback(..)
	, Interface(..)
	, Union(..)
	, Function(..)
	, VirtualMethod(..)
	, Method(..)
	, Member(..)
	, GlibSignal(..)
	, Property(..)
	, UnionContent(..)
	, ClassContent(..)
	, InterfaceContent(..)
	, Include(..)
	, Namespace(..)
	, Package(..)
	, Repository(..)
	, newNamespace
	, newFunction
	, appendNamespaceAlias
	, appendNamespaceFunction
	, appendNamespaceConstant
	, appendNamespaceClass
	, appendNamespaceInterface
	, appendNamespaceEnumeration
	, appendNamespaceBitfield
	, appendNamespaceUnion
	, appendNamespaceRecord
	, appendNamespaceCallback
	, appendClass
	, appendInterface
	, appendUnion
	) where

import Data.Data
import Data.Typeable ()

data Doc = Doc (Maybe String)
	deriving (Show,Eq,Data,Typeable)

type Deprecated = Maybe (String, String) -- (text, version)

data Type =
	  BasicType (String) (String) -- (type.name, type.c:type)
	| ArrayType Type
	deriving (Show,Eq,Data,Typeable)

data TransferOwnership =
	  TransferUnspecified
	| TransferNone
	| TransferContainer
	| TransferFull
	deriving (Show,Eq,Data,Typeable)

data Direction =
	  DirectionUnspecified
	| DirectionIn
	| DirectionOut
	| DirectionInOut
	deriving (Show,Eq,Data,Typeable)

data Parameter =
	  Parameter
		{ parameterName              :: String
		, parameterTransferOwnership :: TransferOwnership
		, parameterAllowNone         :: Bool
		, parameterCallerAllocates   :: Bool
		, parameterCalleeAllocates   :: Bool
		, parameterDirection         :: Direction
		, parameterDoc               :: Doc
		, parameterType              :: Type
		}
	| VarArgs
		{ varargsTransferOwnership :: TransferOwnership }
	deriving (Show,Eq,Data,Typeable)

data ReturnValue = ReturnValue
	{ retvalTransferOwnership :: TransferOwnership
	, retvalDoc               :: Doc
	, retvalType              :: Type
	}
	deriving (Show,Eq,Data,Typeable)

data Function = Function
	{ functionName        :: String
	, functionCIdentifier :: String
	, functionReturnValue :: ReturnValue
	, functionParameters  :: [Parameter]
	}
	deriving (Show,Eq,Data,Typeable)

data Callback = Callback
	{ callbackName        :: String
	, callbackCType       :: String
	, callbackReturnValue :: ReturnValue
	, callbackParameters  :: [Parameter]
	} deriving (Show,Eq,Data,Typeable)

data Field = Field
	{ fieldName              :: String
	, fieldReadable          :: Bool
	, fieldPrivate           :: Bool
	, fieldWritable          :: Bool
	, fieldIntrospectable    :: Bool
	, fieldTransferOwnership :: TransferOwnership
	, fieldType              :: Type
	, fieldCallback          :: Maybe Callback
	} deriving (Show,Eq,Data,Typeable)

data VirtualMethod = VirtualMethod
	{ virtualmethodName        :: String
	, virtualmethodInvoker     :: Maybe String
	, virtualmethodVersion     :: Maybe String
	, virtualmethodDoc         :: Doc
	, virtualmethodReturnValue :: ReturnValue
	, virtualmethodParameters  :: [Parameter]
	} deriving (Show,Eq,Data,Typeable)

data Method = Method
	{ methodName :: String
	, methodCIdentifier :: Maybe String
	, methodVersion :: Maybe String
	, methodDoc :: Doc
	, methodDeprecated :: Deprecated
	, methodReturnValue :: ReturnValue
	, methodParameters :: [Parameter]
	} deriving (Show,Eq,Data,Typeable)

data Member = Member
	{ memberName :: String
	, memberValue :: String
	, memberCIdentifier :: String
	, memberGlibNick :: Maybe String
	} deriving (Show,Eq,Data,Typeable)

data Property = Property
	{ propertyName              :: String
	, propertyVersion           :: Maybe String
	, propertyWritable          :: Bool
	, propertyConstructOnly     :: Maybe String
	, propertyTransferOwnership :: TransferOwnership
	, propertyDoc               :: Doc
	, propertyType              :: Type
	} deriving (Show,Eq,Data,Typeable)

data GlibSignal = GlibSignal
	{ glibsignalReturnValue :: ReturnValue
	, glibsignalParameters  :: [Parameter]
	, glibsignalDoc         :: Doc
	} deriving (Show,Eq,Data,Typeable)

data Constructor = Constructor
	{ constructorName        :: String
	, constructorCIdentifier :: String
	, constructorDoc         :: Doc
	, constructorReturnValue :: ReturnValue
	, constructorParameters  :: [Parameter]
	} deriving (Show,Eq,Data,Typeable)

data Class = Class
	{ className           :: String
	, classCSymbolPrefix  :: String
	, classCType          :: String
	, classParent         :: String
	, classGlibTypeName   :: String
	, classGlibGetType    :: String
	, classGlibTypeStruct :: Maybe String
	, classDoc            :: Doc
	, classContent        :: [ClassContent]
	} deriving (Show,Eq,Data,Typeable)

data Record = Record
	{ recordName                 :: String
	, recordCType                :: String
	, recordGlibIsGtypeStructFor :: Maybe String
	, recordFields               :: [Field]
	, recordMethods              :: [Method]
	, recordFunctions            :: [Function]
	, recordDisguised            :: Bool
	} deriving (Show,Eq,Data,Typeable)

data ClassContent =
	  Implements String
	| ClassConstructor Constructor
	| ClassFunction Function
	| ClassVirtualMethod VirtualMethod
	| ClassMethod Method
	| ClassProperty Property
	| ClassField Field
	| ClassGlibSignal GlibSignal
	deriving (Show,Eq,Data,Typeable)

data InterfaceContent =
	  InterfaceMethod Method
	| InterfaceVirtualMethod VirtualMethod
	| InterfaceProperty Property
	| InterfaceGlibSignal GlibSignal
	| Prerequisite String
	deriving (Show,Eq,Data,Typeable)

data UnionContent =
	  UnionMethod Method
	| UnionConstructor Constructor
	| UnionField Field
	| UnionRecord Record
	deriving (Show,Eq,Data,Typeable)

data Constant = Constant
	{ constantName  :: String
	, constantCType :: Maybe String
	, constantValue :: String
	, constantType  :: Type
	}
	deriving (Show,Eq,Data,Typeable)

data Alias = Alias
	{ aliasName  :: String
	, aliasCType :: String
	, aliasDoc   :: Doc
	, aliasType  :: Type
	}
	deriving (Show,Eq,Data,Typeable)

data Interface = Interface
	{ interfaceName              :: String
	, interfaceCSymbolPrefix     :: String
	, interfaceCType             :: String
	, interfaceGlibTypeName      :: Maybe String
	, interfaceGlibGetType       :: Maybe String
	, interfaceGlibGetTypeStruct :: Maybe String
	, interfaceDoc               :: Doc
	, interfaceContent           :: [InterfaceContent]
	}
	deriving (Show,Eq,Data,Typeable)

data Union = Union
	{ unionName :: String
	, unionCType :: String
	, unionCSymbolPrefix :: String
	, unionGlibTypeName :: Maybe String
	, unionGlibGetType  :: Maybe String
	, unionDoc :: Doc
	, unionContent :: [UnionContent]
	}
	deriving (Show,Eq,Data,Typeable)

data Enumeration = Enumeration
	{ enumerationName         :: String
	, enumerationGlibTypeName :: Maybe String
	, enumerationGlibGetType  :: Maybe String
	, enumerationCType        :: String
	, enumerationDoc          :: Doc
	, enumerationMember       :: [Member]
	}
	deriving (Show,Eq,Data,Typeable)

data Bitfield = Bitfield
	{ bitfieldName         :: String
	, bitfieldCType        :: String
	, bitfieldGlibTypeName :: Maybe String
	, bitfieldGlibGetType  :: Maybe String
	, bitfieldDoc          :: Doc
	, bitfieldMember       :: [Member]
	}
	deriving (Show,Eq,Data,Typeable)

data Include = Include
	{ includeName    :: String
	, includeVersion :: String
	}
	deriving (Show,Eq,Data,Typeable)

data Package = Package { packageName :: String }
	deriving (Show,Eq,Data,Typeable)

data Namespace = Namespace
	{ namespaceName                :: String
	, namespaceVersion             :: String
	, namespaceSharedLibrary       :: String
	, namespaceCIdentifierPrefixes :: String
	, namespaceCSymbolPrefixes     :: String
	, namespaceAliases             :: [Alias]
	, namespaceFunctions           :: [Function]
	, namespaceConstants           :: [Constant]
	, namespaceClasses             :: [Class]
	, namespaceInterfaces          :: [Interface]
	, namespaceEnumerations        :: [Enumeration]
	, namespaceBitfields           :: [Bitfield]
	, namespaceUnions              :: [Union]
	, namespaceRecords             :: [Record]
	, namespaceCallbacks           :: [Callback]
	}
	deriving (Show,Eq,Data,Typeable)

data Repository = Repository
	{ repositoryVersion  :: String
	, repositoryIncludes :: [Include]
	, repositoryCInclude :: String
	, repositoryPackage  :: Package
	, repositoryNamespace :: Namespace
	} deriving (Show,Eq,Data,Typeable)

newNamespace :: Namespace
newNamespace = Namespace
	{ namespaceName                = ""
	, namespaceVersion             = ""
	, namespaceSharedLibrary       = ""
	, namespaceCIdentifierPrefixes = ""
	, namespaceCSymbolPrefixes     = ""
	, namespaceAliases             = []
	, namespaceFunctions           = []
	, namespaceConstants           = []
	, namespaceClasses             = []
	, namespaceInterfaces          = []
	, namespaceEnumerations        = []
	, namespaceBitfields           = []
	, namespaceUnions              = []
	, namespaceRecords             = []
	, namespaceCallbacks           = []
	}

newFunction :: Function
newFunction = Function
	{ functionName = "", functionCIdentifier = "", functionReturnValue = undefined, functionParameters = [] }

--appendNamespace :: Monad m => Namespace -> ([a] -> N
appendNamespaceAlias ::  Monad m => Namespace -> Alias -> m Namespace
appendNamespaceAlias r i = return $ r { namespaceAliases = i : namespaceAliases r }

appendNamespaceFunction ::  Monad m => Namespace -> Function -> m Namespace
appendNamespaceFunction r i = return $ r { namespaceFunctions = i : namespaceFunctions r }

appendNamespaceConstant ::  Monad m => Namespace -> Constant -> m Namespace
appendNamespaceConstant r i = return $ r { namespaceConstants = i : namespaceConstants r }

appendNamespaceClass ::  Monad m => Namespace -> Class -> m Namespace
appendNamespaceClass r i = return $ r { namespaceClasses = i : namespaceClasses r }

appendNamespaceInterface ::  Monad m => Namespace -> Interface -> m Namespace
appendNamespaceInterface r i = return $ r { namespaceInterfaces = i : namespaceInterfaces r }

appendNamespaceEnumeration ::  Monad m => Namespace -> Enumeration -> m Namespace
appendNamespaceEnumeration r i = return $ r { namespaceEnumerations = i : namespaceEnumerations r }

appendNamespaceBitfield ::  Monad m => Namespace -> Bitfield -> m Namespace
appendNamespaceBitfield r i = return $ r { namespaceBitfields = i : namespaceBitfields r }

appendNamespaceUnion ::  Monad m => Namespace -> Union -> m Namespace
appendNamespaceUnion r i = return $ r { namespaceUnions = i : namespaceUnions r }

appendNamespaceRecord ::  Monad m => Namespace -> Record -> m Namespace
appendNamespaceRecord r i = return $ r { namespaceRecords = i : namespaceRecords r }

appendNamespaceCallback ::  Monad m => Namespace -> Callback -> m Namespace
appendNamespaceCallback r i = return $ r { namespaceCallbacks = i : namespaceCallbacks r }

appendClass :: Monad m => Class -> ClassContent -> m Class
appendClass r i = return $ r { classContent = i : classContent r }

appendInterface :: Monad m => Interface -> InterfaceContent -> m Interface
appendInterface r i = return $ r { interfaceContent = i : interfaceContent r }

appendUnion :: Monad m => Union -> UnionContent -> m Union
appendUnion r i = return $ r { unionContent = i : unionContent r }
