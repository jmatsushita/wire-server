{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Federator.Utils.PolysemyServerError where

import Control.Monad.Except (MonadError (..))
import Federator.App (Federator)
import Imports
import Mu.Server (ServerError)
import Polysemy
import qualified Polysemy.Error as Polysemy

instance Member (Polysemy.Error ServerError) r => MonadError ServerError (Sem r) where
  throwError = Polysemy.throw
  catchError = Polysemy.catch

absorbServerError :: forall r a. (Member (Embed Federator) r) => Sem (Polysemy.Error ServerError ': r) a -> Sem r a
absorbServerError action = do
  eitherResult <- Polysemy.runError action
  case eitherResult of
    Left err -> embed @Federator $ throwError err
    Right res -> pure res
