{-# Language
    ConstraintKinds
    , FlexibleContexts
    , OverloadedStrings
    #-}
module Analysis.Java where

import Data.Maybe
import Filesystem.Path.CurrentOS
import Language.Java.Syntax
import System.FilePath.Find

import Prelude hiding (FilePath)


data AnnotationType = Controller | RestController deriving (Eq, Show)


findAll :: FilePath -> IO [String]
findAll d = (find always filterPred $ encodeString d) >>=
    \fs -> sequence $ map readFile fs
  where
    filterPred = fileName ~~? "*.java"




-- | Filter a list of CompilationUnits by annotation.
-- Note that the check for annotations in CompilationUnits spans to subclasses
-- also. So if a CompilationUnit has a subclass with the annotation you are
-- filtering this CompilationUnit will be included in the final result.
filterByAnnotation :: [CompilationUnit] -> AnnotationType -> [CompilationUnit]
filterByAnnotation cus a = checkCompilationUnits cus
  where
    checkCompilationUnits :: [CompilationUnit] -> [CompilationUnit]
    checkCompilationUnits cs =
        let filterTypes x = checkTypes $ getTypeDecls x
            cuAcc acc x = if filterTypes x then x:acc else acc
        in  foldl cuAcc [] cs

    checkTypes :: [TypeDecl] -> Bool
    checkTypes ts = foldl (\acc t -> acc || hasAnnotation t) False ts

    hasAnnotation :: TypeDecl -> Bool

    -- Classes
    hasAnnotation (ClassTypeDecl (ClassDecl ms _ _ _ _ _)) =
        checkModifiers ms a
    -- Interfaces
    hasAnnotation (InterfaceTypeDecl (InterfaceDecl ms _ _ _ _)) =
        checkModifiers ms a
    -- Something else ....
    hasAnnotation _ = False


-- | Verifies whether one or more of the given Modifiers are annotated with
-- the given annotation.
checkModifiers :: [Modifier] -> AnnotationType -> Bool
checkModifiers ms a = foldl (\acc x -> acc || isAnnotatedWith a x) False ms


isAnnotatedWith :: AnnotationType -> Modifier -> Bool
isAnnotatedWith a m
    | isAnnotation m = isAnnotationNamed (show a) $ fromJust $ getAnnotation m
    | otherwise      = False


isAnnotationNamed :: String -> Annotation -> Bool
isAnnotationNamed s a = s == getAnnotationName a


isClassTypeDecl :: TypeDecl -> Bool
isClassTypeDecl (ClassTypeDecl _) = True
isClassTypeDecl _                 = False


isAnnotation :: Modifier -> Bool
isAnnotation (Annotation _) = True
isAnnotation _              = False


getAnnotation :: Modifier -> Maybe Annotation
getAnnotation (Annotation a) = Just a
getAnnotation _              = Nothing


getAnnotationName :: Annotation -> String
getAnnotationName a = extract $ annName a
  where
    extract (Name ((Ident s):[])) = s


getTypeDecls :: CompilationUnit -> [TypeDecl]
getTypeDecls (CompilationUnit _ _ ts) = ts


-- | Extract information.
data JavaClassMetaInfo = JavaClassMetaInfo
    { className :: String
    , apiState  :: ApiState
    } deriving (Eq, Show)

-- | Currently I just want to focus on RESTful API's and only their method
-- signatures.
data ApiState = ApiState
    { methodSigs :: [[MethodInfo]]
    } deriving (Eq, Show)


data MethodInfo = MethodInfo
    { modifiers :: [Modifier]
    , typeParams:: [TypeParam]
    , generics  :: Maybe Type
    , ident     :: Ident
    , params    :: [FormalParam]
    , exceptoins:: [ExceptionType]
    } deriving (Eq, Show)

gather :: [CompilationUnit] -> [JavaClassMetaInfo]
gather cus = map gather' cus

-- |
-- TODO : Optimize this, way too many list passes ...
-- TODO : Java Classes can have nested classes, JavaClassMetaInfo
-- needs to relect this and be updated with nested Maybe [JavaClassMetaInfo]
-- or something similar and this method must then be updated to
-- parse data into that structure.
gather' :: CompilationUnit -> JavaClassMetaInfo
gather' cu = do
    JavaClassMetaInfo "test" $ ApiState  $ extractMethodInfo $
        filterMethodDecl $ extractMemberDecl $ filterMemberDecl $
            extractDecls  $ extractClassBodies $ filterClassTypeDecl $ getTypeDecls cu

-------------------------------------------------------------------------------
-- | Filtering

filterClassTypeDecl :: [TypeDecl] -> [TypeDecl]
filterClassTypeDecl = filter isClassTypeDecl

extractClassBodies :: [TypeDecl] -> [ClassBody]
extractClassBodies = map ( xClassBody . xClassDecl )

extractDecls :: [ClassBody] -> [[Decl]]
extractDecls = map xDecls

filterMemberDecl :: [[Decl]] -> [[Decl]]
filterMemberDecl = map (filter memberDeclPred)

extractMemberDecl :: [[Decl]] -> [[MemberDecl]]
extractMemberDecl = map xMemberDecls

filterMethodDecl :: [[MemberDecl]] -> [[MemberDecl]]
filterMethodDecl = map (filter methodDeclPred)

extractMethodInfo :: [[MemberDecl]] -> [[MethodInfo]]
extractMethodInfo = map xMethodInfo

-- Extraction
-- | TODO : Functionality for Enums must be added, currently
-- most below functions are partial and only deal with classes

xClassDecl :: TypeDecl -> ClassDecl
xClassDecl (ClassTypeDecl c) = c
xClassDecl _ = error "undefined"

xClassBody :: ClassDecl -> ClassBody
xClassBody (ClassDecl _ _ _ _ _ cb) = cb
xClassBody (EnumDecl _ _ _ _) =
    error "Functionality for Enums not in place yet."

xDecls :: ClassBody -> [Decl]
xDecls (ClassBody dcls) = dcls

xMemberDecls :: [Decl] -> [MemberDecl]
xMemberDecls = map xMemberDecl

xMemberDecl :: Decl -> MemberDecl
xMemberDecl (MemberDecl m) = m
xMemberDecl _ = error "undefined"

xMethodInfo :: [MemberDecl] -> [MethodInfo]
xMethodInfo = map xMethodInfo'

xMethodInfo' :: MemberDecl -> MethodInfo
xMethodInfo' (MethodDecl a b c d e f _) = MethodInfo a b c d e f
xMethodInfo' _ = error "undefined"


memberDeclPred :: Decl -> Bool
memberDeclPred (MemberDecl _) = True
memberDeclPred _                 = False

methodDeclPred :: MemberDecl -> Bool
methodDeclPred (MethodDecl _ _ _ _ _ _ _) = True
methodDeclPred _                          = False


-- Utils
maybeRight :: Either a b -> Maybe b
maybeRight e = case e of
    Left _ -> Nothing
    Right cu -> Just cu
