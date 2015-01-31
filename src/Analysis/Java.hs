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

findAll :: FilePath -> IO [String]
findAll d = (find always filterPred $ encodeString d) >>=
    \fs -> sequence $ map readFile fs
  where
    filterPred = fileName ~~? "*.java"

--getTypeDecls bbr >>= \c-> return $ if isClassTypeDecl c then getModifiers $ getClassDecl c else error "test"


data AnnotationType = RestController | Controller deriving (Eq, Show)


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
    hasAnnotation (ClassTypeDecl (ClassDecl ms _ _ _ _ _)) =
        foldl (\acc x -> acc || isAnnotatedWith a x) False ms

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

-- Annotations


