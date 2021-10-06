{-# LANGUAGE OverloadedStrings #-}
module MigrationValidations where

import Prelude
import Data.Text
data KnownMigration = KnownMigration { hash :: Text, filepath :: Text } deriving (Eq, Show)

knownMigrations :: [KnownMigration]
knownMigrations = [ 
    KnownMigration "ea7dc5ed4b4a5e20ff5a491f9713ccb59aa456fcf8ca88fd738bff3f283109c7" "migration-1-0000-20190730.sql",
    KnownMigration "e6eb7aded3271d99be21394110bd916be239d6d3bd54357f8f484a9c6f38c421" "migration-1-0001-20190730.sql",
    KnownMigration "7cb965e9409c170bd34014237a54cc52f1cfe9a35b7d2a0d94348f172a76e443" "migration-1-0002-20190912.sql",
    KnownMigration "7e5564738fd4d2988cef911246aeebacded621c76ae03297f7a3f8ff6b1095fc" "migration-1-0003-20200211.sql",
    KnownMigration "6c1f79fab3f3fe1ba3512bd9208c9588cd40339db265616de60a7b1cc63a6c06" "migration-1-0004-20201026.sql",
    KnownMigration "df7a119a834a9d7ea87067b32c9da169041e23ef724469608003703856350728" "migration-1-0005-20210311.sql",
    KnownMigration "e5f8cb45355e8173b7b0c9d32cb5eb3643ef9d7aa61c4b1f75769d945b11602d" "migration-1-0006-20210531.sql",
    KnownMigration "042ebf0e02af2bbc56f12ce61a9ddc5cf8c837d89e95286035fd7b655b37ccb8" "migration-1-0007-20210611.sql",
    KnownMigration "4adad1502e58a745b59f04fbbe90b71c248df973b6939024b5e101d3327d7427" "migration-1-0008-20210727.sql",
    KnownMigration "d883380495a255f5fb8a030343ce0a8e1a56c0d87ea7e927041f703d0a68849e" "migration-1-0009-20210727.sql",
    KnownMigration "18362de1d9d5daf7a43f6afb66548476b86d7b85cda102d5c02db4cf37bc2659" "migration-2-0001-20210526.sql",
    KnownMigration "3d0cd8ba1f2a28e54007bb10632773bdd8434c2fbcf52b0dfc463b170042959d" "migration-2-0002-20210527.sql",
    KnownMigration "13b56380ce5a5e852722b831edf8e14037821bc1ac81460bb554fbe89e3fafcd" "migration-2-0003-20210531.sql",
    KnownMigration "4d2e4dc36f0d4687b3323dfdb7cf8d539a7febf1520cfabdde7ccd43ad9a06d0" "migration-2-0004-20210609.sql",
    KnownMigration "f25505eeddfde19c62f44e9258010fcb037613b7f085d2b6f544c2d004dddd64" "migration-2-0005-20210610.sql",
    KnownMigration "dbe8be61dda6dac6f5a562355f8cb39d6ce26d2733288a320ecd4a3a37100779" "migration-2-0006-20210611.sql",
    KnownMigration "638fca9d73bc391a7e3d19debc0df87ec6ab9f626725834aca845ba07810090e" "migration-2-0007-20210617.sql",
    KnownMigration "5df03747d311b25acce84796552bb17ea269ddb7254653099612aceaef96f9ea" "migration-2-0008-20210623.sql",
    KnownMigration "1d33e7c39c0e297387f94c24af9a883d6d2567838d0074edc003a9a7ee73571d" "migration-2-0009-20210624.sql",
    KnownMigration "be1841f482d2aec9a5ce1eebfbb59f5f5c97e8841563697e9793ab7e6333e932" "migration-2-0010-20210711.sql",
    KnownMigration "ac4a6953ab8cbf91e7c776e08a95f9f926f40f01ea0c74b92263dd7365ae9fea" "migration-2-0011-20210719.sql",
    KnownMigration "4dfac06c5c767ce3c3fd3976c9bbd6e2f420407f69c805151def84693b592fde" "migration-2-0012-20210723.sql",
    KnownMigration "ab977266665133ecd0645f69010fb10fd1b23c60dd0ed1c47506a86b2403e8b5" "migration-2-0013-20210802.sql",
    KnownMigration "d635b08402601434329e4decd104019a5c526687716387692ee5b0c429145d11" "migration-2-0014-20210802.sql",
    KnownMigration "1ef46c530e6ee9d2ed92dd4a7347e172ffa4cf5e7153b572923b3b54bd84430d" "migration-2-0015-20210802.sql",
    KnownMigration "74a58de114112d069abacec9e6ddde60cc5de3cf8b95d4c4fdf1b923a259b003" "migration-2-0016-20210804.sql",
    KnownMigration "a4ca5db9f6d92a8a833e4b1bb9dbd95bb98386dae026ec57b9affb536352010f" "migration-2-0017-20210806.sql",
    KnownMigration "64726844c501355fd371fb97c120aa51b4eddd0b41d5cdc60fdfaec8640b624f" "migration-2-0018-20210807.sql",
    KnownMigration "28e292a6251ce37295d77c87d29a468c702fec3d2b950d13b918d43bb5af0f0f" "migration-2-0019-20210810.sql",
    KnownMigration "20f69b712b9f372869cf9851e9b5bc662b5044efa1222d8e25de405e2f2034a0" "migration-2-0020-20210810.sql",
    KnownMigration "480a0b0da50979052f2096557c17036b17e9f02a1dc4a579a997cfd4f69ad52a" "migration-2-0021-20210811.sql",
    KnownMigration "ffd65f381150d7e68efcf9fa58a4fa6d936a9292d94364e14e9cc3b331841791" "migration-2-0022-20210819.sql",
    KnownMigration "fc4173483f591aeaaed9999badc6592b54d6d5a4bf1a0d556dc44f508064e2d2" "migration-2-0023-20210907.sql",
    KnownMigration "b9f4c3ceeb13ef77203dfeb941404573274434faf89d782c9cc8a507c31853e6" "migration-2-0024-20210913.sql",
    KnownMigration "0fc4f7b2d0d64e4e843329dc6c63cc5e05c83ad4f517b6eb19bd653bf541bdf3" "migration-2-0025-20211001.sql",
    KnownMigration "defe6519a4122239c0d679c01677be44a40aef674640d3fcfc192f3d786a3a6c" "migration-3-0001-20190816.sql",
    KnownMigration "362cb55ea651bc58fe842815f55518bdde2f42128fcc6985e764ad8a2a83c623" "migration-3-0002-20200521.sql",
    KnownMigration "ff4f3100ca6013a8fc214df24b0d061be6f6f08c33f2d80babd761826c021623" "migration-3-0003-20200702.sql",
    KnownMigration "d115dd8dd6db6b4c38fd54c955128ed7c25fe98783e67621afbeae1131e71afd" "migration-3-0004-20200810.sql",
    KnownMigration "59f29b9844fbd412713cba5811aa8a661ab1329bccae2ab587c3dc4255c5bf21" "migration-3-0005-20210116.sql",
    KnownMigration "462d08ddbcd3240a7fe4aff6467b414a999cc56997b03fb70352ec747e53d0c6" "migration-3-0006-20210116.sql",
    KnownMigration "62253000c8cae8552864d067134ff560c191789fc8aed0d23cd75d541221bc7d" "migration-3-9999-20200728.sql"]
