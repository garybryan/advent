module Advent2022.Day07.Base
  ( Command (..),
    FS (..),
    FSNode (..),
    FSKey,
    nodeSize,
    dirsWithSize,
    addNode,
    chDir,
    parseLine,
    parseLines,
    fsFromCommands,
  )
where

import qualified Data.Dequeue as DQ
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Text.Read (readMaybe)

{-
  Represent the filesystem tree with directories as as adjacency lists (or sets
  actually) with node IDs, rather than an actual tree as was my first approach.
  This makes it much easier to add nodes: instead of rebuilding the whole tree
  with the new node, it's just a case of updating the adjacency list entry.

  Node names are not unique, except for children of one parent, so we need
  unique IDs using a monotonic sequence or UUIDs etc. I'm going with ascending
  monotonic IDs here since we don't need to worry about concurrency.
-}

type FSKey = Int

type AdjSet = Set.Set FSKey

data FSNode = FileNode String Int | DirNode String AdjSet deriving (Eq, Show)

type FSMap = Map.Map FSKey FSNode

type DirStack = DQ.BankersDequeue FSKey

type Keys = [FSKey]

data FS = FS
  { fsMap :: FSMap,
    nextKeys :: Keys
  }
  deriving (Eq, Show)

-- Get the names and sizes of all directory nodes under and including the given
-- one with a size matching the given condition.

nodeSize :: FSMap -> FSKey -> Int
nodeSize fm key = case lookupNode fm key of
  (FileNode _ size) -> size
  (DirNode _ childKeys) -> Set.foldr ((+) . nodeSize fm) 0 childKeys

dirsWithSize :: (Int -> Bool) -> FSMap -> FSKey -> [(String, Int)]
dirsWithSize cond fm key = fst $ dirsWithSizeHelper cond fm key

dirsWithSizeHelper :: (Int -> Bool) -> FSMap -> FSKey -> ([(String, Int)], Int)
dirsWithSizeHelper cond fm key = case lookupNode fm key of
  (FileNode _ size) -> ([], size)
  (DirNode name childKeys) -> (namesAndSizes', size)
    where
      childResults = map (dirsWithSizeHelper cond fm) (Set.toList childKeys)
      namesAndSizes = concatMap fst childResults
      size = sum $ map snd childResults
      namesAndSizes'
        | cond size = (name, size) : namesAndSizes
        | otherwise = namesAndSizes

{-
  Directory navigation: maintain a stack of the current working directory and
  its parents.

  (An alternative would be to also have reverse adjacency lists to look up
  parents, but then we'd have a second source of truth to maintain.)

  When changing to a directory, look for it in the current working directory
  and create it if it doesn't exist.
-}

lookupNode :: FSMap -> FSKey -> FSNode
lookupNode fm key = fromMaybe (error $ "Node not found: " ++ show key) (Map.lookup key fm)

nodeName :: FSNode -> String
nodeName (FileNode name _) = name
nodeName (DirNode name _) = name

children :: FSMap -> FSKey -> AdjSet
children fm key = case lookupNode fm key of
  DirNode _ adj -> adj
  _ -> error $ "A file node cannot have children: " ++ show key

addNodeToMap :: FSNode -> FSMap -> FSKey -> FSKey -> FSMap
addNodeToMap node fm parentKey key = Map.insert key node (Map.insert parentKey newParent fm)
  where
    parentName = nodeName $ lookupNode fm parentKey
    newParent = DirNode parentName (Set.insert key (children fm parentKey))

getCwd :: DirStack -> FSKey
getCwd st = fromMaybe (error "Cannot get working directory from an empty stack") (DQ.last st)

addNode :: FSNode -> FSKey -> FS -> (FS, FSKey)
addNode node parentKey (FS {fsMap = fm, nextKeys = nks}) = (FS {fsMap = fm', nextKeys = nks'}, key)
  where
    key = fromMaybe (error "No more keys available") (listToMaybe nks)
    nks' = tail nks
    fm' = addNodeToMap node fm parentKey key

addNodeToCwd :: FSNode -> (FS, DirStack) -> (FS, DirStack)
addNodeToCwd node (fs, st) = (fst $ addNode node (getCwd st) fs, st)

enterDir :: String -> (FS, DirStack) -> (FS, DirStack)
enterDir name (fs, st) = (fs', st `DQ.pushBack` key)
  where
    cwd = getCwd st
    fm = fsMap fs
    childNamesToKeys = Map.fromList [(nodeName $ lookupNode fm cKey, cKey) | cKey <- Set.toList $ children fm cwd]
    (fs', key) = case name `Map.lookup` childNamesToKeys of
      Nothing -> addNode (DirNode name Set.empty) cwd fs
      Just k -> (fs, k)

enterRootDir :: FS -> FS
enterRootDir (FS {fsMap = fm, nextKeys = nks}) = FS {fsMap = fm', nextKeys = nks}
  where
    fm'
      | 0 `Map.member` fm = fm
      | otherwise = Map.insert 0 (DirNode "/" Set.empty) fm

exitDir :: DirStack -> DirStack
exitDir st = snd $ fromMaybe (error "Trying to go up from empty directory stack") (DQ.popBack st)

chDir :: String -> (FS, DirStack) -> (FS, DirStack)
chDir "/" (fm, _) = (enterRootDir fm, DQ.fromList [0])
chDir ".." (fm, st) = (fm, exitDir st)
chDir name (fm, st) = enterDir name (fm, st)

data Command = Cd String | Ls | AddDir String | AddFile String Int deriving (Eq, Show)

parseLineWords :: [String] -> Command
parseLineWords ["$", "cd", key] = Cd key
parseLineWords ["$", "ls"] = Ls
parseLineWords ["dir", key] = AddDir key
parseLineWords ss@[sizeStr, key] = AddFile key size
  where
    size = fromMaybe (error $ "Invalid line: " ++ unwords ss) (readMaybe sizeStr)
parseLineWords ss = error ("Invalid line: " ++ unwords ss)

parseLine :: String -> Command
parseLine = parseLineWords . words

parseLines :: [String] -> [Command]
parseLines = map parseLine

{-
  Map commands to functions that (potentially) modify a filesystem and
  directory stack.

  We could just parse lines directly to functions without the commands part,
  but keeping them decoupled is a good thing.

  In the given data, `ls` is only ever run for the current directory, meaning
  that `ls` lines can just be ignored as they have no effect.

  If it were possible to list different directories, we'd have to mantain state
  for the directory being listed while parsing the output lines.
-}

commandFunction :: Command -> ((FS, DirStack) -> (FS, DirStack))
commandFunction (Cd key) = chDir key
commandFunction Ls = id
commandFunction (AddDir name) = addNodeToCwd (DirNode name Set.empty)
commandFunction (AddFile name size) = addNodeToCwd (FileNode name size)

functionForCommands :: [Command] -> ((FS, DirStack) -> (FS, DirStack))
functionForCommands cmds = foldl1 (flip (.)) (map commandFunction cmds)

applyCommandsToEmpty :: [FSKey] -> [Command] -> (FS, DirStack)
applyCommandsToEmpty nks cmds = functionForCommands cmds (emptyFs, DQ.empty)
  where
    emptyFs = FS {fsMap = Map.empty, nextKeys = nks}

fsFromCommands :: [FSKey] -> [Command] -> FS
fsFromCommands nks cmds = fst $ applyCommandsToEmpty nks cmds
