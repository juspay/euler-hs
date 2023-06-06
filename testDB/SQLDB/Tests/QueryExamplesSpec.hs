{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module SQLDB.Tests.QueryExamplesSpec where

import           Data.Time
import           Database.Beam ((&&.), (<.), (==.), (>=.))
import qualified Database.Beam as B
import           Database.Beam.Sqlite (SqliteM)
import           EulerHS.Interpreters
import           EulerHS.Language
import qualified EulerHS.Language as L
import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Runtime (withFlowRuntime)
import qualified EulerHS.Runtime as R
import           EulerHS.Types (_isAsync, _logFilePath, _logToFile,
                                defaultLoggerConfig)
import qualified EulerHS.Types as T
import           Test.Hspec hiding (runIO)

date1 :: LocalTime
date1 =  LocalTime
  { localDay = toEnum 56195
  , localTimeOfDay = defaultTimeOfDay1
  }

defaultTimeOfDay1 :: TimeOfDay
defaultTimeOfDay1 = TimeOfDay
  { todHour = 1
  , todMin = 1
  , todSec  = 1
  }

-- 2012-09-26 18:08:45
date2 :: LocalTime
date2 = LocalTime
  { localDay = toEnum 56196
  , localTimeOfDay = td2
  }

td2 :: TimeOfDay
td2 = TimeOfDay
  { todHour = 18
  , todMin = 8
  , todSec  = 45
  }

date3 :: LocalTime
date3 = LocalTime
  { localDay = toEnum 56190
  , localTimeOfDay = td3
  }

date4 :: LocalTime
date4 = LocalTime
  { localDay = toEnum 56191
  , localTimeOfDay = td3
  }

td3 :: TimeOfDay
td3 = TimeOfDay
  { todHour = 0
  , todMin  = 0
  , todSec  = 0
  }

data MemberT f = Member
    { memberId      :: B.C f Int
    , surName       :: B.C f Text
    , firstName     :: B.C f Text
    , address       :: B.C f Text
    , zipCode       :: B.C f Int
    , telephone     :: B.C f Text
    , recommendedBy :: B.C f (Maybe Int)
    , joinDate      :: B.C f LocalTime
    } deriving (Generic, B.Beamable)

instance B.Table MemberT where
  data PrimaryKey MemberT f =
    MemberId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = MemberId . memberId

type Member = MemberT Identity

type MemberId = B.PrimaryKey MemberT Identity

deriving instance Show MemberId
deriving instance Eq MemberId
deriving instance ToJSON MemberId
deriving instance FromJSON MemberId

deriving instance Show Member
deriving instance Eq Member
deriving instance ToJSON Member
deriving instance FromJSON Member


membersEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity MemberT)
membersEMod = B.modifyTableFields
  B.tableModification
    { memberId = B.fieldNamed "memid"
    , surName = B.fieldNamed "surname"
    , firstName = B.fieldNamed "firstname"
    , address = B.fieldNamed "address"
    , zipCode = B.fieldNamed "zipcode"
    , telephone = B.fieldNamed "telephone"
    , recommendedBy = B.fieldNamed "recommendedby"
    , joinDate = B.fieldNamed "joindate"
    }

data FacilityT f = Facility
    { facilityId         :: B.C f Int
    , name               :: B.C f Text
    , memberCost         :: B.C f Double
    , guestCost          :: B.C f Double
    , initialOutlay      :: B.C f Double
    , monthlyMaintenance :: B.C f Double
    } deriving (Generic, B.Beamable)

instance B.Table FacilityT where
  data PrimaryKey FacilityT f =
    FacrId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = FacrId . facilityId

type Facility = FacilityT Identity

type FacrId = B.PrimaryKey FacilityT Identity

deriving instance Show FacrId
deriving instance Eq FacrId
deriving instance ToJSON FacrId
deriving instance FromJSON FacrId

deriving instance Show Facility
deriving instance Eq Facility
deriving instance ToJSON Facility
deriving instance FromJSON Facility

facilitiesEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity FacilityT)
facilitiesEMod = B.modifyTableFields
  B.tableModification
    { facilityId = B.fieldNamed "facid"
    , name = B.fieldNamed "name"
    , memberCost = B.fieldNamed "membercost"
    , guestCost = B.fieldNamed "guestcost"
    , initialOutlay = B.fieldNamed "initialoutlay"
    , monthlyMaintenance = B.fieldNamed "monthlymaintenance"
    }


data BookingT f = Booking
    { bookId    :: B.C f Int
    , facId     :: B.PrimaryKey FacilityT f
    , bmemid    :: B.PrimaryKey MemberT f
    , starttime :: B.C f LocalTime
    , slots     :: B.C f Int
    } deriving (Generic, B.Beamable)

instance B.Table BookingT where
  data PrimaryKey BookingT f =
    BookId (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = BookId . bookId

type Booking = BookingT Identity

type BookId = B.PrimaryKey BookingT Identity

deriving instance Show Booking
deriving instance Eq Booking
deriving instance ToJSON Booking
deriving instance FromJSON Booking

bookingsEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity BookingT)
bookingsEMod = B.modifyTableFields
  B.tableModification
    { bookId = B.fieldNamed "bookid"
    , facId = FacrId (B.fieldNamed "facid")
    , bmemid = MemberId (B.fieldNamed "memid")
    , starttime = B.fieldNamed "starttime"
    , slots = B.fieldNamed "slots"
    }


data ClubDB f = ClubDB
    { members    :: f (B.TableEntity MemberT)
    , facilities :: f (B.TableEntity FacilityT)
    , bookings   :: f (B.TableEntity BookingT)
    } deriving (Generic, B.Database be)

clubDB :: B.DatabaseSettings be ClubDB
clubDB = B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { facilities = facilitiesEMod
    , members = membersEMod
    , bookings = bookingsEMod
    }

testDBName :: String
testDBName = "./testDB/SQLDB/TestData/test.db"

testDBTemplateName :: String
testDBTemplateName = "./testDB/SQLDB/TestData/query_examples.db.template"

poolConfig :: T.PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

sqliteCfg :: T.DBConfig SqliteM
sqliteCfg = T.mkSQLitePoolConfig "clubSQliteDB" testDBName poolConfig

connectOrFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connectOrFail cfg = L.initSqlDBConnection cfg >>= \case
    Left e     -> error $ show e
    Right conn -> pure conn

rmTestDB :: L.Flow ()
rmTestDB = void $ L.runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: L.Flow ()
prepareTestDB = do
  rmTestDB
  void $ L.runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName


--Basic string searches
textSearchLike :: L.Flow (T.DBResult [Facility])
textSearchLike = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn
    $ L.findRows
    $ B.select
    $ B.filter_ (\f -> name f `B.like_` "%Tennis%")
    $ B.all_ (facilities clubDB)

-- Matching against multiple possible values
matchMultValues :: L.Flow (T.DBResult [Facility])
matchMultValues = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn
    $ L.findRows
    $ B.select
    $ B.filter_ (\f -> facilityId f `B.in_` [1,5])
    $ B.all_ (facilities clubDB)

searchByDate :: L.Flow (T.DBResult [Member])
searchByDate = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn
    $ L.findRows
    $ B.select
    $ B.filter_ (\m -> joinDate m >=. B.val_ date1)
    $ B.all_ (members clubDB)


-- Removing duplicates, and ordering results
groupDistinctLimit :: L.Flow (T.DBResult [Text])
groupDistinctLimit = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn
    $ L.findRows
    $ B.select
    $ B.limit_ 10
    $ B.nub_
    $ fmap surName
    $ B.orderBy_ (B.asc_ . surName)
    $ B.all_ (members clubDB)

-- Combining results from multiple queries with union
selectWithUnion:: L.Flow (T.DBResult [Text])
selectWithUnion = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn $
    let
      sn = surName
        <$> B.all_ (members clubDB)
      n = name
        <$> B.all_ (facilities clubDB)
    in L.findRows $ B.select $ B.limit_ 3 $ B.union_ sn n


aggregate1 :: L.Flow (T.DBResult (Maybe LocalTime))
aggregate1 = do
  conn <- connectOrFail sqliteCfg
  res <- L.runDB conn $
    L.findRow $ B.select
    $ B.aggregate_ (B.max_ . joinDate)
    $ B.all_ (members clubDB)
  pure $ join <$> res

aggregate2 :: L.Flow (T.DBResult (Maybe (Text, Text, LocalTime)))
aggregate2 = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn $
    L.findRow $ B.select $ do
      mdate <- B.aggregate_ (B.max_ . joinDate)
          $ B.all_ (members clubDB)
      lm <- B.filter_ (\m -> joinDate m ==. B.fromMaybe_ (B.val_ date2) mdate) $ B.all_ (members clubDB)
      pure (firstName lm, surName lm, joinDate lm)


join1 :: L.Flow (T.DBResult [LocalTime])
join1 = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn $
    L.findRows $ B.select $ fmap starttime $  do
        ms <- B.all_ (members clubDB)
        bs <- B.join_ (bookings clubDB) (\book -> bmemid book ==. B.primaryKey ms)
        B.guard_ (firstName ms ==. "David" &&. surName ms ==. "Farrell")
        pure bs

join2 :: L.Flow (T.DBResult [(LocalTime, Text)])
join2 = do
  conn <- connectOrFail sqliteCfg
  L.runDB conn
    $ L.findRows
    $ B.select
    $ B.orderBy_ (B.asc_ . fst)
    $ fmap (\(fs,bs) -> (starttime bs,name fs))
    $ do
      fs <- B.all_ (facilities clubDB)
      bs <- B.join_ (bookings clubDB) (\book -> facId book ==. B.primaryKey fs)
      B.guard_ ( starttime bs >=. B.val_ date3
             &&. starttime bs <. B.val_ date4
             &&. name fs `B.like_` "%Tennis Court%")
      pure (fs, bs)

loggerCfg :: T.LoggerConfig
loggerCfg = defaultLoggerConfig
        { _logToFile = True
        , _logFilePath = "/tmp/euler-backend.log"
        , _isAsync = True
        }

withEmptyDB :: (R.FlowRuntime -> IO ()) -> IO ()
withEmptyDB act = withFlowRuntime Nothing (\rt -> do
  try (runFlow rt prepareTestDB) >>= \case
    Left (e :: SomeException) ->
      runFlow rt rmTestDB
      `finally` error ("Preparing test values failed: " <> show e)
    Right _ -> act rt `finally` runFlow rt rmTestDB
    )


spec :: Spec
spec =
  around withEmptyDB $

    describe "Query examples" $ do
      it "Text search with 'like' operator " $ \rt -> do
        eRes <- runFlow rt textSearchLike
        eRes `shouldBe` Right [Facility {facilityId = 0, name = "Tennis Court 1", memberCost = 5.0, guestCost = 25.0, initialOutlay = 10000.0, monthlyMaintenance = 200.0},Facility {facilityId = 1, name = "Tennis Court 2", memberCost = 5.0, guestCost = 25.0, initialOutlay = 8000.0, monthlyMaintenance = 200.0},Facility {facilityId = 3, name = "Table Tennis", memberCost = 0.0, guestCost = 5.0, initialOutlay = 320.0, monthlyMaintenance = 10.0}]

      it "Match against mult values" $ \rt -> do
        eRes <- runFlow rt matchMultValues
        eRes `shouldBe` Right [Facility {facilityId = 1, name = "Tennis Court 2", memberCost = 5.0, guestCost = 25.0, initialOutlay = 8000.0, monthlyMaintenance = 200.0},Facility {facilityId = 5, name = "Massage Room 2", memberCost = 35.0, guestCost = 80.0, initialOutlay = 4000.0, monthlyMaintenance = 3000.0}]


      it "search by date" $ \rt -> do
        eRes <- runFlow rt searchByDate
        (length <$> eRes) `shouldBe` Right 1

      it "orderDistinctLimit" $ \rt -> do
        eRes <- runFlow rt groupDistinctLimit
        eRes `shouldBe` Right ["Bader","Baker","Boothe","Butters","Coplin","Crumpet","Dare","Farrell","GUEST","Genting"]

      it "selectWithUnion" $ \rt -> do
        eRes <- runFlow rt selectWithUnion
        eRes `shouldBe` Right ["Bader","Badminton Court","Baker"]

      it "aggregate1" $ \rt -> do
        eRes <- runFlow rt aggregate1
        eRes `shouldBe` Right (Just date2)

      it "aggregate2" $ \rt -> do
        eRes <- runFlow rt aggregate2
        eRes `shouldBe` Right (Just ("Darren","Smith",  date2))

      it "join1" $ \rt -> do
        eRes <- runFlow rt join1
        length <$> eRes `shouldBe` Right 34

      it "join2" $ \rt -> do
        eRes <- runFlow rt join2
        eRes `shouldBe` fmap (sortOn fst) eRes
        length <$> eRes `shouldBe` Right 12
