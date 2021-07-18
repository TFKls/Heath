 {- |
Copyright: (c) 2021 Tomasz "TFKls" Kulis
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tomasz "TFKls" Kulis <tfk@tfkls.dev>
-}

module Heath.Func.System
  (  primitives  ) where

import qualified Data.Map         as M
import qualified System.Exit      as Exit
import           System.IO.Unsafe (unsafePerformIO)

import           Heath.Types

primitives :: M.Map String HPrimitive
primitives = M.fromList [ ("exit", exit)
                        ]

exit :: HPrimitive
exit [] = exit [Integer 0]
exit [Integer arg] = unsafePerformIO . Exit.exitWith $ (if arg == 0 then Exit.ExitSuccess else Exit.ExitFailure . fromIntegral $ arg)
exit (Integer _ : _ : _) = Left $ ArgNumErr GT "exit takes zero or one argument"
exit _ = Left $ FuncErr "requires an integer value" "exit"
