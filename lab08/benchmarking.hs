import qualified AbstractSet as AS
import qualified AbstractMap as AM
import qualified ListSet as LS
import qualified TreeSet as TS
import qualified DefaultSet as DS
import qualified ListMap as LM
import qualified DefaultMap as DM
import Criterion.Main


testInsert s n = AS.member 1 $ foldl (flip AS.insert) s [1..n]

testFromAscListLS n = AS.member 1 $ ((AS.fromAscList [1..n]) :: LS.Set Int)
testFromAscListTS n = AS.member 1 $ ((AS.fromAscList [1..n]) :: TS.Set Int)
testFromAscListDS n = AS.member 1 $ ((AS.fromAscList [1..n]) :: DS.Set Int)

testRemove s n = AS.member 1 $ foldl (flip AS.remove) s [1..n]

testInsertMap m n = AM.hasKey 1 $ foldl (flip AM.insert) m (zip [1..n] [1..n])

testGetValue m n = map (\k -> AM.getValue k m) [1..n]

main = do
        let q1 = AS.empty :: LS.Set Int
        let q2 = AS.empty :: TS.Set Int
        let q3 = AS.empty :: DS.Set Int

        let s1 = (AS.fromAscList [1..100]) :: LS.Set Int
        let s3 = (AS.fromAscList [1..100]) :: TS.Set Int
        let s2 = (AS.fromAscList [1..100]) :: DS.Set Int

        let m1 = AM.empty :: LM.Map Int Int
        let m100 = foldl (flip AM.insert) m1 (zip [1..100] [1..100])

        let m2 = AM.empty :: DM.Map Int Int

        defaultMain [
          bgroup "ListSet" [
            bench "insert 100" $ nf (testInsert q1) 100
            , bench "fromAscList [1..100]" $ nf testFromAscListLS 100
            , bench "remove from [1..100] [1..50]" $ nf (testRemove s1) 50
            ]
          , bgroup "TreeSet" [
            bench "insert 100" $ nf (testInsert q2) 100
            , bench "fromAscList [1..100]" $ nf testFromAscListTS 100
            , bench "remove from [1..100] [1..50]" $ nf (testRemove s2) 50
            ]
          , bgroup "DefaultSet" [
            bench "insert 100" $ nf (testInsert q3) 100
            , bench "fromAscList [1..100]" $ nf testFromAscListDS 100
            , bench "remove from [1..100] [1..50]" $ nf (testRemove s3) 50
            ]
          , bgroup "ListMap" [
            bench "insert 100" $ nf (testInsertMap m1) 100
            , bench "getValue 100" $ nf (testGetValue m100) 100
            ]
          , bgroup "defaultMap" [
            bench "insert 100" $ nf (testInsertMap m1) 100
            , bench "getValue 100" $ nf (testGetValue m100) 100
            ]
                    ]