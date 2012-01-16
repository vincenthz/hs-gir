-- |
-- Module      : Data.GIR.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.GIR.Parse 
	( fromXml
	, fromData
	, fromFile
	) where

import Text.XML.Light
import Data.GIR.Types
import Control.Monad

isText :: Content -> Bool
isText (Text _) = True
isText _        = False

fqname :: QName -> String
fqname q = case qPrefix q of
	Nothing -> qName q
	Just p  -> p ++ ":" ++ qName q

elFQname :: Element -> String
elFQname = fqname . elName

findElem :: String -> [Content] -> [Element]
findElem name l = map toElement $ filter isElemNamed l
	where
		toElement (Elem e) = e
		toElement _        = error "not an element"

		isElemNamed (Elem e) = elFQname e == name
		isElemNamed _        = False

findChildElems :: String -> Element -> [Element]
findChildElems name e = findElem name $ elContent e

findChildElem :: String -> Element -> Element
findChildElem name e = case findChildElems name e of
	[x] -> x
	[]  -> error ("\n###########\nno element found for " ++ name)
	_   -> error ("\n###########\nmultiples attributes found for " ++ name)

findChildMaybeElem :: String -> Element -> Maybe Element
findChildMaybeElem name e = case findChildElems name e of
	[x] -> Just x
	[]  -> Nothing
	_   -> error ("multiples attributes found for " ++ name)

getAttribs :: String -> Element -> [String]
getAttribs name e = map attrVal $ filter (\a -> fqname (attrKey a) == name) $ elAttribs e

getAttrib :: String -> Element -> String
getAttrib name e = case getAttribs name e of
	[x] -> x
	[]  -> error ("\n############\nno attribute found for " ++ name ++ " element: " ++ show e)
	_   -> error ("\n############\nmultiples attributes found for " ++ name)

getMaybeAttrib :: String -> Element -> Maybe String
getMaybeAttrib name e = case getAttribs name e of
	[x] -> Just x
	[]  -> Nothing
	_   -> error ("multiples attributes found for " ++ name)

getText :: Element -> String
getText e = concatMap toText $ filter isText $ elContent e
	where
		toText (Text s) = cdData s
		toText _ = error "not a text"

mapRepository :: Element -> IO Repository
mapRepository repository = do
	ns       <- parseNamespace (findChildElem "namespace" repository)
	package  <- parsePackage (findChildElem "package" repository)
	includes <- mapM parseInclude $ findChildElems "include" repository
	return $ Repository
		{ repositoryVersion  = getAttrib "version" repository
		, repositoryIncludes = includes
		, repositoryCInclude = getAttrib "name" $ findChildElem "c:include" repository
		, repositoryPackage = package
		, repositoryNamespace = ns
		}

	where
		--------------------------------------------------------------------
		parseInclude e = return $ Include
			{ includeName    = getAttrib "name" e
			, includeVersion = getAttrib "version" e
			}
		parsePackage e = return $ Package $ getAttrib "name" e

		--------------------------------------------------------------------
		parseNamespace e = do
			let namespace = newNamespace
				{ namespaceName                = getAttrib "name" e
				, namespaceVersion             = getAttrib "version" e
				, namespaceSharedLibrary       = getAttrib "shared-library" e
				, namespaceCIdentifierPrefixes = getAttrib "c:identifier-prefixes" e
				, namespaceCSymbolPrefixes     = getSymbolPrefixes e
				}
			foldM parseNamespaceContent namespace $ elContent e
		
		parseNamespaceContent a (Elem e)
			| elFQname e == "alias" = parseAlias e >>= appendNamespaceAlias a
			| elFQname e == "function" = parseFunction e >>= appendNamespaceFunction a
			| elFQname e == "constant" = parseConstant e >>= appendNamespaceConstant a
			| elFQname e == "class"    = parseClass e >>= appendNamespaceClass a
			| elFQname e == "interface"= parseInterface e >>= appendNamespaceInterface a
			| elFQname e == "enumeration"= parseEnumeration e >>= appendNamespaceEnumeration a
			| elFQname e == "bitfield"= parseBitfield e >>= appendNamespaceBitfield a
			| elFQname e == "union"    = parseUnion e >>= appendNamespaceUnion a
			| elFQname e == "record"   = parseRecord e >>= appendNamespaceRecord a
			| elFQname e == "callback" = return (parseCallback e) >>= appendNamespaceCallback a
			| otherwise = putStrLn ("unhandled namespace element: " ++ (show $ fqname $ elName e)) >> return a
		parseNamespaceContent a (CRef c) = putStrLn ("cref: " ++ show c) >> return a
		parseNamespaceContent a _        = return a

		parseAlias e = return $ Alias
				{ aliasName  = getAttrib "name" e
				, aliasCType = getAttrib "c:type" e
				, aliasDoc   = getDoc e
				, aliasType  = getType e
				}
		
		--------------------------------------------------------------------
		parseUnion e = do
			let union = Union
				{ unionName = getAttrib "name" e
				, unionCSymbolPrefix = getSymbolPrefix e
				, unionCType = getAttrib "c:type" e
				, unionGlibTypeName = getMaybeAttrib "glib:type-name" e
				, unionGlibGetType = getMaybeAttrib "glib:get-type" e
				, unionDoc = getDoc e
				, unionContent = []
				}
			foldM parseUnionContent union $ elContent e
		parseUnionContent a (Elem e)
			| elFQname e == "doc"         = return a
			| elFQname e == "method"      = parseMethod e >>= appendUnion a . UnionMethod
			| elFQname e == "field"       = return (parseField e) >>= appendUnion a . UnionField
			| elFQname e == "constructor" = parseConstructor e >>= appendUnion a . UnionConstructor
			| elFQname e == "record"      = parseRecord e >>= appendUnion a . UnionRecord
			| otherwise = putStrLn ("unhandled union element: " ++ (show $ elFQname e)) >> return a
		parseUnionContent a (CRef c) = putStrLn ("cref: " ++ show c) >> return a
		parseUnionContent a _        = return a
		--------------------------------------------------------------------
		parseInterface e = do
			let interface = Interface
				{ interfaceName = getAttrib "name" e
				, interfaceCSymbolPrefix = getSymbolPrefix e
				, interfaceCType = getAttrib "c:type" e
				, interfaceGlibTypeName = getMaybeAttrib "glib:type-name" e
				, interfaceGlibGetType = getMaybeAttrib "glib:get-type" e
				, interfaceGlibGetTypeStruct = getMaybeAttrib "glib:type-struct" e
				, interfaceDoc = getDoc e
				, interfaceContent = []
				}
			foldM parseInterfaceContent interface $ elContent e
		parseInterfaceContent a (Elem e)
			| elFQname e == "doc" = return a
			| elFQname e == "virtual-method" = parseVirtualMethod e >>= appendInterface a . InterfaceVirtualMethod
			| elFQname e == "method" = parseMethod e >>= appendInterface a . InterfaceMethod
			| elFQname e == "property" = parseProperty e >>= appendInterface a . InterfaceProperty
			| elFQname e == "prerequisite"= parsePrerequisite e >>= appendInterface a
			| elFQname e == "glib:signal" = parseGlibSignal e >>= appendInterface a . InterfaceGlibSignal
			| otherwise = putStrLn ("unhandled interface element: " ++ (show $ elFQname e)) >> return a
		parseInterfaceContent a (CRef c) = putStrLn ("cref: " ++ show c) >> return a
		parseInterfaceContent a _        = return a
		--------------------------------------------------------------------
		parseRecord e = do
			meths <- mapM parseMethod $ findChildElems "method" e
			funcs <- mapM parseFunction $ findChildElems "function" e
			return $ Record
				{ recordName = getAttrib "name" e
				, recordCType = getAttrib "c:type" e
				, recordGlibIsGtypeStructFor = getMaybeAttrib "glib:is-gtype-struct-for" e
				, recordFields = map parseField $ findChildElems "field" e
				, recordMethods = meths
				, recordFunctions = funcs
				, recordDisguised = getBool False "disguised" e
				}
		--------------------------------------------------------------------
		parseEnumeration e = return $ Enumeration
				{ enumerationName = getAttrib "name" e
				, enumerationGlibTypeName = getMaybeAttrib "glib:type-name" e
				, enumerationGlibGetType = getMaybeAttrib "glib:get-type" e
				, enumerationCType = getAttrib "c:type" e
				, enumerationDoc = getDoc e
				, enumerationMember = map parseMember $ findChildElems "member" e
				}
		parseBitfield e = return $ Bitfield
				{ bitfieldName = getAttrib "name" e
				, bitfieldGlibTypeName = getMaybeAttrib "glib:type-name" e
				, bitfieldGlibGetType = getMaybeAttrib "glib:get-type" e
				, bitfieldCType = getAttrib "c:type" e
				, bitfieldDoc = getDoc e
				, bitfieldMember = map parseMember $ findChildElems "member" e
				}
		parseMember e = Member
			{ memberName        = getAttrib "name" e
			, memberValue       = getAttrib "value" e
			, memberCIdentifier = getAttrib "c:identifier" e
			, memberGlibNick    = getMaybeAttrib "glib:nick" e
			}

		--------------------------------------------------------------------
		parseClass e = do
			let class_ = Class
				{ className = getAttrib "name" e
				, classCSymbolPrefix = getSymbolPrefix e
				, classCType = getAttrib "c:type" e
				, classParent = getAttrib "parent" e
				, classGlibTypeName = getAttrib "glib:type-name" e
				, classGlibGetType = getAttrib "glib:get-type" e
				, classGlibTypeStruct = getMaybeAttrib "glib:type-struct" e
				, classDoc = getDoc e
				, classContent = []
				}
			foldM parseClassContent class_ $ elContent e

		parseClassContent a (Elem e)
			| elFQname e == "doc" = return a
			| elFQname e == "implements" = appendClass a (Implements $ getAttrib "name" e)
			| elFQname e == "constructor" = parseConstructor e >>= appendClass a . ClassConstructor
			| elFQname e == "function" = parseFunction e >>= appendClass a . ClassFunction
			| elFQname e == "virtual-method" = parseVirtualMethod e >>= appendClass a . ClassVirtualMethod
			| elFQname e == "method" = parseMethod e >>= appendClass a . ClassMethod
			| elFQname e == "property" = parseProperty e >>= appendClass a . ClassProperty
			| elFQname e == "field" = appendClass a (ClassField $ parseField e)
			| elFQname e == "glib:signal" = parseGlibSignal e >>= appendClass a . ClassGlibSignal
			| otherwise = putStrLn ("unhandled class element: " ++ (show $ elFQname e)) >> return a
		parseClassContent a (CRef c) = putStrLn ("cref: " ++ show c) >> return a
		parseClassContent a _        = return a

		--------------------------------------------------------------------

		parseConstructor e = return $ Constructor
			{ constructorName = getAttrib "name" e
			, constructorDoc = getDoc e
			, constructorCIdentifier = getAttrib "c:identifier" e
			, constructorReturnValue = getReturnValue e
			, constructorParameters = getParameters e
			}

		parseVirtualMethod e = return $ VirtualMethod
			{ virtualmethodName = getAttrib "name" e
			, virtualmethodInvoker = getMaybeAttrib "invoker" e
			, virtualmethodVersion = getMaybeAttrib "version" e
			, virtualmethodDoc = getDoc e
			, virtualmethodReturnValue = getReturnValue e
			, virtualmethodParameters = getParameters e
			}
		parseMethod e = return $ Method
			{ methodName = getAttrib "name" e
			, methodCIdentifier = getMaybeAttrib "c:identifier" e
			, methodVersion = getMaybeAttrib "version" e
			, methodDeprecated = getDeprecated e
			, methodDoc = getDoc e
			, methodReturnValue = getReturnValue e
			, methodParameters = getParameters e
			}

		parseProperty e = return $ Property
			{ propertyName = getAttrib "name" e
			, propertyVersion = getMaybeAttrib "version" e
			, propertyWritable = getBool False "writable" e
			, propertyConstructOnly = getMaybeAttrib "construct-only" e
			, propertyTransferOwnership = getTransferOwnership e
			, propertyDoc = getDoc e
			, propertyType = getType e
			}

		parseField e = Field
			{ fieldName = getAttrib "name" e
			, fieldIntrospectable = getBool True "introspectable" e
			, fieldReadable = getBool True "readable" e
			, fieldWritable = getBool False "writable" e
			, fieldPrivate = getBool False "private" e
			, fieldTransferOwnership = getTransferOwnership e
			, fieldType = getType e
			, fieldCallback = getCallback e
			}

		parseGlibSignal e = return $ GlibSignal
			{ glibsignalReturnValue = getReturnValue e
			, glibsignalParameters = getParameters e
			, glibsignalDoc = getDoc e
			}

		parsePrerequisite e = return $ Prerequisite $ getAttrib "name" e

		parseFunction :: Monad m => Element -> m Function
		parseFunction e = do
			return $ Function
				{ functionName        = getAttrib "name" e
				, functionCIdentifier = getAttrib "c:identifier" e
				, functionReturnValue = getReturnValue e
				, functionParameters  = getParameters e
				}

		parseConstant e = return $ Constant
			{ constantName        = getAttrib "name" e
			, constantValue       = getAttrib "value" e
			, constantType        = getType e
			, constantCType       = getMaybeAttrib "c:type" e
			}

		getReturnValue :: Element -> ReturnValue
		getReturnValue e = ReturnValue
			{ retvalTransferOwnership = getTransferOwnership c
			, retvalDoc               = getDoc c
			--, retvalArray             = maybe Nothing (Just . getAttrib "c:type") c
			, retvalType              = getType c
			}
			where c = findChildElem "return-value" e


		getParameters :: Element -> [Parameter]
		getParameters e = map getParameter paramElems
			where
				parameters = findChildMaybeElem "parameters" e
				paramElems = maybe [] (findChildElems "parameter") parameters
				getParameter p = case findChildMaybeElem "varargs" p of
					Nothing -> Parameter
						{ parameterName = getAttrib "name" p
						, parameterTransferOwnership = getTransferOwnership p
						, parameterAllowNone = getBool False "allow-none" p
						, parameterCallerAllocates = getBool False "caller-allocates" p
						, parameterCalleeAllocates = getBool False "callee-allocates" p
						, parameterDirection = getDirection p
						, parameterDoc = getDoc p
						, parameterType = getType p
						}
					Just _ -> VarArgs { varargsTransferOwnership = getTransferOwnership p }

		getDeprecated :: Element -> Deprecated
		getDeprecated e = do
			t <- getMaybeAttrib "deprecated" e
			v <- getMaybeAttrib "deprecated-version" e
			return (t,v)

		getTransferOwnership e = case getMaybeAttrib "transfer-ownership" e of
			Nothing          -> TransferUnspecified
			Just "none"      -> TransferNone
			Just "container" -> TransferContainer
			Just "full"      -> TransferFull
			Just "floating"  -> TransferNone
			Just s           -> error ("unrecognized transfer ownership " ++ show s)

		getDirection e = case getMaybeAttrib "direction" e of
			Nothing      -> DirectionUnspecified
			Just "in"    -> DirectionIn
			Just "out"   -> DirectionOut
			Just "inout" -> DirectionInOut
			Just s       -> error ("unrecognized direction " ++ show s)

		getType :: Element -> Type
		getType e = case a of
			Nothing  -> parseType $ findChildElem "type" e
			Just arr -> ArrayType $ parseType $ findChildElem "type" arr
			where
				parseType c = BasicType n (maybe n id t)
					where
						n = getAttrib "name" c
						t = getMaybeAttrib "c:type" c
				a = findChildMaybeElem "array" e

		parseCallback e = Callback
			{ callbackName = getAttrib "name" e
			, callbackCType = getAttrib "c:type" e
			, callbackReturnValue = getReturnValue e
			, callbackParameters = getParameters e
			}

		getCallback e = case findChildMaybeElem "callback" e of
			Nothing -> Nothing
			Just c  -> Just $ parseCallback c

		getSymbolPrefixes e = maybe "" id $ getMaybeAttrib "c:symbol-prefixes" e
		getSymbolPrefix e = maybe "" id $ getMaybeAttrib "c:symbol-prefix" e

		getDoc :: Element -> Doc
		getDoc e = Doc $ maybe Nothing (Just . getText) $ findChildMaybeElem "doc" e


		getBool :: Bool -> String -> Element -> Bool
		getBool def name e = maybe def toBool $ getMaybeAttrib name e
			where
				toBool "1" = True
				toBool "0" = False
				toBool s   = error ("toBool unrecognized value: " ++ s)

fromXml :: [Content] -> IO Repository
fromXml xml = do
	case findElem "repository" xml of
		[]  -> error "no repository node found."
		[x] -> mapRepository x
		_   -> error "multiples repositories found, expecting only one."

fromData :: String -> IO Repository
fromData content = fromXml (parseXML content)

fromFile :: FilePath -> IO Repository
fromFile filename = readFile filename >>= fromData
