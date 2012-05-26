module Client.Message where

printMessage ∷ S2C → IO ()
printMessage message = do
  timestamp ← formatTime defaultTimeLocale timeFormat messageTimestamp
  printf (format message) timestamp (text message)
  where messageTimestamp ∷ S2C → UTCTime
        messageTimestamp (S
