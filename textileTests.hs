------

callCheckOutputRecursive :: Game -> POSIXTime -> Contract w s Text ()
callCheckOutputRecursive game x = do
    m'   <- findGameOutput game
    now' <- currentTime
    if now' > gProveDeadline game 
        then logInfo @String "[guesser] time passed the prove deadline, game over"
    else case m' of
        Nothing                 -> logInfo @String "[guesser] GAME OVER - CHALLENGER WON!!!"
        Just (oref', o', FinishedAsDraw)  -> do
            logInfo @String "[guesser] challenger provided proof that first guess was incorrect and declared the game as DRAW"
            let lookups' = ...
        Just (oref', o', _) -> do
            logInfo @String "[guesser] game is finished."
            let lookups' = ...
    else do
        logInfo @String "[guesser] game not finished, checking again in " ++ show x ++ " seconds"
        waitNSlots (timeToSlot x)
        callCheckOutputRecursive game x

--_________________________________________________________________________________
---Each client is created using the newClientWithKeyInfo function, which takes an API key and a KeyInfo object as parameters. The API key and KeyInfo object for each owner should be different, to ensure that each owner has their own unique access to the bucket.
-- A common format for a BucketId is a Universally Unique Identifier (UUID), which is a string of 32 hexadecimal characters separated by hyphens, such as: "6ba7b810-9dad-11d1-80b4-00c04fd430c8"

--- -****************************************

import qualified Textile.API as Textile
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Except


createTaggedBucket :: T.Text -> IO T.Text
createTaggedBucket tag = do
    bucketId <- Textile.createBucket
    let taggedId = T.append tag ("_" <> bucketId)
    return taggedId

-- Author sets the variables and access permissions
let paperTag = "journalnum002"
let bucketIdFormat <- createTaggedBucket paperTag


let fileMeta = Textile.FileMeta {
    path = "path/to/manuscript",
    meta = Map.fromList [("owner", "Author")]
    }

putStrLn "Enter your secret key:"
AutSecretKey <- getLine
keyValidity <- liftIO $ Textile.isKeyValid AutSecretKey
case keyValidity of
    Left err -> throwError $ T.pack $ show err
    Right False -> throwError "The provided key is invalid"
    Right True -> do
        let keyInfoA = Textile.KeyInfo AutSecretKey "Author_key_name"
        let client = Textile.newClientWithKeyInfo keyInfoA -- First, create a new client object that is associated with the provided secret key

let permissions = Textile.Permissions {
    read = Textile.AllowEveryone,
    write = Textile.AllowOnly [AutSecretKey],
    delete = Textile.DenyEveryone
    }

--Author pushes a first file and fetches the newly generated IPNS address
submitFile :: Client -> BucketId -> Textile.Permissions -> Textile.FileMeta -> ExceptT Text IO Text
submitFile client bucketIdFormat perm fileMeta = do
    setPermissionsEither <- runExceptT $ Textile.setBucketPermissions client bucketIdFormat perm
    case setPermissionsEither of
        Left err -> throwError $ T.pack $ show err
        Right _ -> do
            fileEither <- Textile.pushFileWithMeta client bucketIdFormat [fileMeta]
            case fileEither of
                Left err -> throwError $ T.pack $ show err
                Right cid -> do
                    let ipnsEither <- Textile.ipnsForBucketId client bucketIdFormat
                    case ipnsEither of
                        Left err -> throwError $ T.pack $ show err
                        Right ipns -> return $ T.pack $ "File CID " ++ (show cid) ++ 
                        " of " ++ (show $ fileMeta.meta) ++ " was successfully pushed to the Bucket address: " ++ ipns

-- So, only someone who has access to the "clientA_secret_key" can use this "client" object to push files to
-- to the bucket "journalnum002" whose permission was set to only allow writes by the holder of secret key.
let result <- runExceptT $ submitFile client bucketIdFormat permissions fileMeta
case result of
    Left err -> TIO.putStrLn $ T.pack $ show err
    Right message -> TIO.putStrLn message

-----------------------------------------------------------------------------------
-- *** Author shares the BucketId with the Reviewer *** (via a tx+datum in cardano)
-----------------------------------------------------------------------------------

-- Reviewer sets the variables and access permissions

let fileMeta = Textile.FileMeta {
    path = "path/to/manuscript",
    meta = Map.fromList [("owner", "Reviewer")]
    }

putStrLn "Enter your secret key:"
RevSecretKey <- getLine
keyValidity <- liftIO $ Textile.isKeyValid RevSecretKey
case keyValidity of
    Left err -> throwError $ T.pack $ show err
    Right False -> throwError "The provided key is invalid"
    Right True -> do
        let keyInfoA = Textile.KeyInfo RevSecretKey "Reviewer_key_name"
        let client = Textile.newClientWithKeyInfo keyInfoA -- First, create a new client object that is associated with the provided secret key

let permissions = Textile.Permissions {
    read = Textile.AllowEveryone,
    write = Textile.AllowOnly [RevSecretKey],
    delete = Textile.DenyEveryone
    }

--Reviewer retrieves and read the CID of last pushed file from the Author:

let downloadPath = "path/to/save/file"
let lastClientBCid <- runExceptT $ getLastCidWithMeta bucketIdFormat (\cid-> Map.lookup "owner" cid == Just "Author")
case lastClientBCid of
    Left _ -> TIO.putStrLn "No CID file uploaded by the Author"
    Right (Just cid) -> do
        fileDownload <- runExceptT $ Textile.pullFile bucketIdFormat cid downloadPath 
        case fileDownload of
            Left err -> TIO.putStrLn $ T.pack $ show err
            Right _ -> TIO.putStrLn $ T.pack $ "File downloaded to " ++ downloadPath
    Right Nothing -> TIO.putStrLn "No CID file uploaded by the Author"



--Reviewer pushes a reply file and fetches the newly generated IPNS address

submitFile :: Client -> BucketId -> Textile.Permissions -> Textile.FileMeta -> ExceptT Text IO Text
submitFile client bucketIdFormat perm fileMeta = do
    setPermissionsEither <- runExceptT $ Textile.setBucketPermissions client bucketIdFormat perm
    case setPermissionsEither of
        Left err -> throwError $ T.pack $ show err
        Right _ -> do
            fileEither <- Textile.pushFileWithMeta client bucketIdFormat [fileMeta]
            case fileEither of
                Left err -> throwError $ T.pack $ show err
                Right cid -> do
                    let ipnsEither <- Textile.ipnsForBucketId client bucketIdFormat
                    case ipnsEither of
                        Left err -> throwError $ T.pack $ show err
                        Right ipns -> return $ T.pack $ "File CID " ++ (show cid) ++ 
                        " of " ++ (show $ fileMeta.meta) ++ " was successfully pushed to the Bucket address: " ++ ipns

-- So, only someone who has access to the secret key can use this "client" object to push files to
-- to the bucket "journalnum002" whose permission was set to only allow writes by the holder of secret key.
let result <- runExceptT $ submitFile client bucketIdFormat permissions fileMeta
case result of
    Left err -> TIO.putStrLn $ T.pack $ show err
    Right message -> TIO.putStrLn message


--Author retrieves and read the CID of last pushed file from the Reviewer

let downloadPath = "path/to/save/file"
let lastClientBCid <- runExceptT $ getLastCidWithMeta bucketIdFormat (\cid-> Map.lookup "owner" cid == Just "clientB")
case lastClientBCid of
    Left _ -> TIO.putStrLn "No CID file uploaded by the Reviewer"
    Right (Just cid) -> do
        fileDownload <- runExceptT $ Textile.pullFile bucketIdFormat cid downloadPath 
        case fileDownload of
            Left err -> TIO.putStrLn $ T.pack $ show err
            Right _ -> TIO.putStrLn $ T.pack $ "File downloaded to " ++ downloadPath
    Right Nothing -> TIO.putStrLn "No CID file uploaded by the Reviewer"

    

--- incolla qui le utils functions
getLastCidWithMeta :: BucketId -> (Cid -> Bool) -> ExceptT Text IO (Maybe Cid)
getLastCidWithMeta bucketId pred = do
    cid <- Textile.lastCidInBucket bucketId pred
    return $ Just cid








