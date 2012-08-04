import Control.Monad (liftM, filterM, mapM, forM, forM_)
import qualified Data.ByteString.Lazy as LS
import Data.Digest.Pure.MD5 (md5, MD5Digest)
import Data.List (group, sort, intercalate)
import Foreign.C.Types (CTime)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (splitPath, takeFileName, takeDirectory, (</>))
import System.Environment (getArgs)
import System.IO
import System.Mem (performGC)
import System.Posix.Files (getFileStatus, modificationTime)

data FileInfo = FileInfo {
     fiFile :: FilePath
    ,fiHash :: MD5Digest
    } deriving Show

instance Eq FileInfo where
    x == y = fiHash x == fiHash y

instance Ord FileInfo where
    x <= y = fiHash x <= fiHash y

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths path = putStrLn "Getting directory content" >> getFilePaths' [path]
    where getFilePaths' []           = return []
          getFilePaths' (path:paths) = do
              plainFiles <- getDirectoryContents path
              let filesWithPath = map (path </>) $ drop 2 plainFiles
              files <- filterM doesFileExist filesWithPath
              dirs <- filterM doesDirectoryExist filesWithPath
              filesInSubDirs <- getFilePaths' (dirs ++ paths)
              return (files ++ filesInSubDirs)

getHash :: FilePath -> IO FileInfo
getHash file = do
    hdl <- openFile' file
    contents <- LS.hGetContents hdl
    let hash = md5 contents
    putStrLn $ "MD5 hash " ++ show hash ++ " for file " ++ file
    hClose hdl
    return $ FileInfo file hash

openFile' :: FilePath -> IO Handle
openFile' file = openFile file ReadMode `catch` \_ -> do
    performGC
    openFile' file

getHashes :: FilePath -> IO [FileInfo]
getHashes path = do
    paths <- getFilePaths path
    mapM getHash paths

run :: FilePath -> FilePath -> IO ()
run path outputFile = do
    hashes <- getHashes path
    let matches = map showMatches $ filter ((> 1) . length) $ group . sort $ hashes
    writeFile outputFile $ intercalate "\n" matches
    putStrLn "Done!"
    where showMatches :: [FileInfo] -> String
          showMatches aGroup = "MD5 hash: " ++ (show . fiHash . head) aGroup ++
                               "\nFiles:\n" ++
                               intercalate "\n" (map fiFile aGroup) ++ "\n"

usage :: String
usage = "Usage: findDoubles PATH FILE\nWhere FILE is an output file for the results"

main :: IO ()
main = do
    args <- getArgs
    case args of
         (path:outputFile:[]) -> do run path outputFile
         otherwise            -> putStrLn usage

