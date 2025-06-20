{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.UI.Notification.DS.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as B
import Data.Conduit
import Data.Default
import qualified Data.Text as T
import Data.Aeson
import Control.Monad.Except

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.UI.Notification.DM.Type


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| work .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.McpNotification AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.McpNotification
    go = do
      queue <- view DM.notificationQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT DM.McpNotification DM.JsonRpcNotification AppContext ()
work = await >>= \case
  Just reqBS -> flip catchError errHdl $ do
    lift (go reqBS) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work

  where
    errHdl :: String -> ConduitT DM.McpNotification DM.JsonRpcNotification AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: parse error. skip. " ++ msg
      work

    go :: DM.McpNotification -> AppContext DM.JsonRpcNotification
    go res = do
      jsonRes <- mcp2json res
      $logDebugS DM._LOGTAG $ T.pack $ "work: notification: " ++ show jsonRes
      return jsonRes

-- |
--
mcp2json :: DM.McpNotification -> AppContext DM.JsonRpcNotification
mcp2json (DM.McpToolListChangedNotification dat) = do
  $logDebugS DM._LOGTAG $ T.pack $ "mcp2json: " ++ show dat

  let params = encode (dat^.DM.paramsMcpToolListChangedNotificationData)
  $logDebugS DM._LOGTAG $ T.pack $ "mcp2json: " ++ show params

  let json = def {
             DM._methodJsonRpcNotification = dat^.DM.methodMcpToolListChangedNotificationData
           , DM._paramsJsonRpcNotification = Just (DM.RawJsonByteString params)
           } 
  return json

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT DM.JsonRpcNotification Void AppContext ()
sink = await >>= \case
  Just req -> lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    go :: DM.JsonRpcNotification -> AppContext ()
    go res = do
      hdl <- view outputHandleAppData <$> ask
      let bs = encode res
      $logDebugS DM._LOGTAG $ T.pack $ "sink: notification bs: " ++ DM.lbs2str bs

      liftIO $ B.hPutStr hdl bs
      liftIO $ B.hPutStr hdl "\n"
      liftIO $ hFlush hdl
