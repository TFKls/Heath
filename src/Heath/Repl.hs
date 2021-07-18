{- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

{-# LANGUAGE LambdaCase #-}

module Heath.Repl ( runRepl ) where

import           Data.Char   (isSpace)
import           Data.List   (dropWhileEnd)
import           Data.Monoid
import           Heath.Eval  (evalInteractive)
import           Prelude     hiding (until)
import           System.IO

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ test get act = do
  res <- get
  if test res
    then return ()
    else act res >> until_ test get act

untilM :: (Monad m, Monoid b) => (a -> Bool) -> m a -> (a -> m b) -> m b
untilM test get act = untilM' test get act mempty

untilM' :: (Monad m, Monoid b) => (a -> Bool) -> m a -> (a -> m b) -> b -> m b
untilM' test get act acc = do
  res <- get
  if test res
    then return acc
    else do effect <- act res
            untilM' test get act (acc <> effect)

prompt :: String -> IO String
prompt str = (putStr str) >> (hFlush stdout) >> getLine >>= (\case
  "`{" -> getMultiLine
  x    -> return x) . trim

getMultiLine :: IO String
getMultiLine = return . init =<< untilM ((== "`}") . trim) getLine (return . (++" "))

runRepl :: IO ()
runRepl = displayInit >> until_ ((`elem` ["`quit", "`q"]) . trim) (prompt "H\\> ") ((\case
                                                                              "`h" -> displayHelp
                                                                              "`help" -> displayHelp
                                                                              "`l" -> displayLicense
                                                                              "`license" -> displayLicense
                                                                              "`w" -> displayWarranty
                                                                              "`warranty" -> displayWarranty
                                                                              x -> putStrLn $ evalInteractive x) . trim)
displayHelp :: IO ()
displayHelp = putStrLn "Heath - A simple scheme-like language\n\
                       \Licensed under GPLv3\n\
                       \Possible special commands:\n\
                       \- `q, `quit: quit the interpreter (You can also use the primitive 'exit function)\n\
                       \- `h, `help: display this help message\n\
                       \- `l, `license: display license information\n\
                       \- `w, `warranty: display warranty information\n\
                       \n\
                       \To write multiline expressions, open a block with `{,\n\
                       \complete the expression then close the block in a new line with `}."

displayLicense :: IO ()
displayLicense = putStrLn "Heath - A simple scheme-like language\n\
                          \Copyright (C) 2021 Tomasz \"TFKls\" Kulis\n\n\
                          \This program is free software: you can redistribute it and/or modify\n\
                          \it under the terms of the GNU General Public License as published by\n\
                          \the Free Software Foundation, version 3.\n\n\
                          \This program is distributed in the hope that it will be useful,\n\
                          \but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
                          \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
                          \GNU General Public License for more details.\n\n\
                          \You should have received a copy of the GNU General Public License\n\
                          \along with this program.  If not, see <https://www.gnu.org/licenses/gpl.html>."

displayInit :: IO ()
displayInit = putStrLn "Heath - A simple scheme like language\n\
                       \Copyright (C) 2021 Tomasz \"TFKls\" Kulis\n\n\
                       \To show help about using the REPL interactive mode, type `h.\n\
                       \This program comes with ABSOLUTELY NO WARRANTY; for details type `w.\n\
                       \This is free software, and you are welcome to redistribute it under certain conditions; type `l for details"

displayWarranty :: IO ()
displayWarranty = putStrLn "Heath is distributed WITHOUT ANY WARRANTY. The following\n\
                           \sections from the GNU General Public License, version 3, should\n\
                           \make that clear.\n\
                           \\n\
                           \  15. Disclaimer of Warranty.\n\
                           \\n\
                           \  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\n\
                           \APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\n\
                           \HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\n\
                           \OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\n\
                           \THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n\
                           \PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\n\
                           \IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\n\
                           \ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\
                           \\n\
                           \  16. Limitation of Liability.\n\
                           \\n\
                           \  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\n\
                           \WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS\n\
                           \THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY\n\
                           \GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE\n\
                           \USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF\n\
                           \DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD\n\
                           \PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),\n\
                           \EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF\n\
                           \SUCH DAMAGES.\n\
                           \\n\
                           \  17. Interpretation of Sections 15 and 16.\n\
                           \\n\
                           \  If the disclaimer of warranty and limitation of liability provided\n\
                           \above cannot be given local legal effect according to their terms,\n\
                           \reviewing courts shall apply local law that most closely approximates\n\
                           \an absolute waiver of all civil liability in connection with the\n\
                           \Program, unless a warranty or assumption of liability accompanies a\n\
                           \copy of the Program in return for a fee.\n\
                           \\n\
                           \See <http://www.gnu.org/licenses/gpl.html>, for more details."

