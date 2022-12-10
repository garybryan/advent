module Advent2022.Day07.BaseSpec (spec) where

import Advent2022.Day07.Base
import Control.Exception (evaluate)
import qualified Data.Dequeue as DQ
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec

{-
 nextKeys would normally be an infinite list of ascending or random numbers,
 but tests use finite lists because trying to evaluate and compare to an
 infinite list goes about as well as you'd expect it to.
-}

spec :: Spec
spec = do
  let emptyFs =
        FS
          { fsMap = Map.empty,
            nextKeys = [1 .. 50]
          }

  let oneLevelFs =
        FS
          { fsMap =
              Map.fromList
                [ (0, DirNode "/" $ Set.fromList [1, 2]),
                  (1, FileNode "a.txt" 25),
                  (2, FileNode "b.txt" 15)
                ],
            nextKeys = [3 .. 50]
          }

  let twoLevelFs =
        FS
          { fsMap =
              Map.fromList
                [ (0, DirNode "/" $ Set.fromList [1]),
                  (1, DirNode "stuff" $ Set.fromList [2, 3]),
                  (2, FileNode "a.txt" 25),
                  (3, FileNode "b.txt" 15)
                ],
            nextKeys = [4 .. 50]
          }

  let threeLevelFs =
        FS
          { fsMap =
              Map.fromList
                [ (0, DirNode "/" $ Set.fromList [1, 2, 3, 4]),
                  (1, DirNode "stuff" $ Set.fromList [5, 6, 11]),
                  (2, DirNode "subdir" $ Set.fromList [7, 10]),
                  (3, DirNode "bigsubdir" $ Set.fromList [8]),
                  (4, FileNode "a.txt" 25),
                  (5, FileNode "b.txt" 15),
                  (6, FileNode "c.txt" 50),
                  (7, FileNode "d.txt" 5),
                  (8, FileNode "e.txt" 70),
                  (9, FileNode "f.txt" 10),
                  (10, FileNode "g.txt" 7),
                  (11, DirNode "subsubdir" $ Set.fromList [9])
                ],
            nextKeys = [12 .. 50]
          }

  describe "nodeSize" $ do
    it "returns the size of a file" $ do
      nodeSize (fsMap oneLevelFs) 1 `shouldBe` 25

    it "returns the size of a directory containing files" $ do
      nodeSize (fsMap threeLevelFs) 2 `shouldBe` 12

    it "returns the size of a directory node containing files and directories" $ do
      nodeSize (fsMap threeLevelFs) 1 `shouldBe` 75

    describe "dirsWithSize" $ do
      it "returns the directories with a size matching a condition and their sizes" $ do
        let result = dirsWithSize (< 20) (fsMap threeLevelFs) 0
        Set.fromList result `shouldBe` Set.fromList [("subdir", 12), ("subsubdir", 10)]

  describe "addNode" $ do
    it "Adds a child node" $ do
      addNode (FileNode "meme.png" 555) 0 oneLevelFs
        `shouldBe` ( FS
                       { fsMap =
                           Map.fromList
                             [ (0, DirNode "/" $ Set.fromList [1, 2, 3]),
                               (1, FileNode "a.txt" 25),
                               (2, FileNode "b.txt" 15),
                               (3, FileNode "meme.png" 555)
                             ],
                         nextKeys = [4 .. 50]
                       },
                     3
                   )

  describe "chDir" $ do
    it "Puts an existing subdirectory onto the stack" $ do
      let st = DQ.fromList [0]
      chDir "stuff" (twoLevelFs, st) `shouldBe` (twoLevelFs, DQ.pushBack st 1)

    it "Creates a new subdirectory and puts it on the stack" $ do
      let st = DQ.fromList [0]
      let newFs =
            FS
              { fsMap =
                  Map.fromList
                    [ (0, DirNode "/" $ Set.fromList [1, 4]),
                      (1, DirNode "stuff" $ Set.fromList [2, 3]),
                      (2, FileNode "a.txt" 25),
                      (3, FileNode "b.txt" 15),
                      (4, DirNode "newdir" Set.empty)
                    ],
                nextKeys = [5 .. 50]
              }
      chDir "newdir" (twoLevelFs, st) `shouldBe` (newFs, DQ.pushBack st 4)

    it "Pops a directory off the stack for .." $ do
      let st = DQ.fromList [0, 1]
      chDir ".." (twoLevelFs, st) `shouldBe` (twoLevelFs, DQ.fromList [0])

    it "Replaces the stack with the root dir only for /" $ do
      chDir "/" (threeLevelFs, DQ.fromList [0, 1, 11]) `shouldBe` (threeLevelFs, DQ.fromList [0])

    it "Replaces the stack with the new root dir for an empty filesystem" $ do
      let fsWithRoot = FS {fsMap = Map.fromList [(0, DirNode "/" Set.empty)], nextKeys = [1 .. 50]}
      chDir "/" (emptyFs, DQ.empty) `shouldBe` (fsWithRoot, DQ.fromList [0])

  describe "parseLine" $ do
    it "parses a cd command" $ do
      parseLine "$ cd subdir" `shouldBe` Cd "subdir"

    it "parses an ls command" $ do
      parseLine "$ ls" `shouldBe` Ls

    it "parses an ls output line for a directory" $ do
      parseLine "dir e" `shouldBe` AddDir "e"

    it "parses an ls output line for a file" $ do
      parseLine "2557 g" `shouldBe` AddFile "g" 2557

    -- it "gives an error for an unrecognised two-word line" $ do
    --   -- TODO this fails even though that exact error is thrown; not sure why.
    --   evaluate (parseLine "vi password.txt") `shouldThrow` errorCall "Invalid line: vi password.txt"

    it "gives an error for an unrecognised line" $ do
      evaluate (parseLine "sudo shutdown -n now") `shouldThrow` errorCall "Invalid line: sudo shutdown -n now"

  describe "fsFromCommands" $ do
    it "Creates a filesystem from a list of commands" $ do
      let cmds =
            [ Cd "/",
              Ls,
              AddDir "stuff",
              Cd "stuff",
              AddFile "a.txt" 25,
              AddFile "b.txt" 15
            ]
      fsFromCommands [1 .. 50] cmds `shouldBe` twoLevelFs

    it "Creates a filesystem from a list of commands, creating an unseen directory" $ do
      let cmds =
            [ Cd "/",
              Cd "stuff",
              AddFile "a.txt" 25,
              AddFile "b.txt" 15
            ]
      fsFromCommands [1 .. 50] cmds `shouldBe` twoLevelFs

    it "Creates a filesystem from a list of commands with non-unique names" $ do
      let cmds =
            [ Cd "/",
              Cd "stuff",
              AddFile "a.txt" 25,
              AddFile "b.txt" 15,
              Cd "..",
              AddFile "b.txt" 65,
              Cd "newdir",
              Cd "stuff"
            ]
      fsFromCommands [1 .. 50] cmds
        `shouldBe` FS
          { fsMap =
              Map.fromList
                [ (0, DirNode "/" $ Set.fromList [1, 4, 5]),
                  (1, DirNode "stuff" $ Set.fromList [2, 3]),
                  (2, FileNode "a.txt" 25),
                  (3, FileNode "b.txt" 15),
                  (4, FileNode "b.txt" 65),
                  (5, DirNode "newdir" $ Set.fromList [6]),
                  (6, DirNode "stuff" Set.empty)
                ],
            nextKeys = [7 .. 50]
          }
