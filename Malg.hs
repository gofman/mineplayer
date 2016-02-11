module Malg
( CIdx,
  Cell(Mine,Digit,Closed),
  Show, Eq,
  Functor,
  Monad,
  Prob,Field, FieldArr, field, CountType, getArr, (>!),
  getix, getiy, getSurrIdx,
  getClosedNearDigits,
  makeChains,sortChain,sortChains,
  initUpdateFld,
  Action(Open,SetM, getIdx),Actions(Actions,Impossible,getActions),
  ActionsProb(ActionsProb,ImpossibleP,getActionsProb,getEnsNum),
  checkAllDigitsOpen,
  findActions, findActions2,
  sortActionsProb,
  getBestAction,
  mineCnt, closedCnt,
  getAllClosed,
  choose
 -- ActIntersectable
) 
where  

import Data.Array
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Vector as V
--import Data.Array.Diff
import Data.List
import qualified Data.List.Ordered as OL
import Data.Ord
import Control.Monad
import Control.Monad.State
import System.IO.Unsafe
--import Debug.Trace
import Data.Ratio

{-|
class ActInters a where
	intersectActions :: (Eq b, Ord b) => a b -> a b -> a b
	joinActions :: (Show b, Eq b, Ord b) => a b -> a b -> a b
	imposs :: a b
	retacts :: (b -> Bool) -> b -> a b
	retempty :: Field -> (CIdx -> b) -> a b
-}
class ActInters a where
	intersectActions :: a -> a -> a
	joinActions :: a -> a -> a
	imposs :: a
	retacts :: (Action -> Bool) -> Action -> a
	retempty :: Field -> (CIdx -> Action) -> a


data Cell = Mine | ToOpen | Digit {getDigit :: Int} | Closed deriving(Eq)
type CIdx = (Int,Int)
type FieldArr = Array CIdx Cell
data Field = Field {getArr :: FieldArr, getClosedCnt :: Int, getMineCnt :: Int}

--infixl 1 `!! 
(>!) :: Field -> CIdx -> Cell
(Field arr _ _) >! cidx = arr!cidx

closedCnt fld = fromIntegral $ getClosedCnt fld
mineCnt fld = fromIntegral $ getMineCnt fld

field :: Array CIdx Cell -> Field
field fldarr = Field fldarr (closedCntCount fldarr) (mineCntCount fldarr)

type CountType = Integer

data Action = Open {getIdx :: ! CIdx} | SetM {getIdx :: ! CIdx} deriving(Eq,Ord,Show)
data Actions = Actions {getActions :: [Action]} | Impossible deriving(Eq, Show)

--type Prob = Ratio CountType
type Prob = Ratio Integer
--type Prob = Double
type ActProb = (Prob,Action)


type ActProbColl = V.Vector (ActProb)

{-|
fNX = 30
fNY = 16
fN = fNX*fNY
fNN = fN + 1

ind2xy :: Int -> (Int,Int)
ind2xy ind
	| ind == fN = (-1,-1)
	| otherwise = ((ind `mod` fNX)+1, (ind `div` fNX) + 1)

xy2ind :: (Int,Int) -> Int
xy2ind (-1,-1) = fN
xy2ind (ix,iy) = (iy - 1)*fNX + (ix - 1)

actProbCollS v = V.singleton v
actProbCollE = V.generate fNN (\ind -> (0, Open $ ind2xy ind))
actProbColl lst = V.fromList lst
actProbCollToList coll = V.toList coll

actProbMergeBy :: (ActProb -> ActProb -> Ordering) -> ActProbColl -> ActProbColl -> ActProbColl
actProbMergeBy comp a1l a2l 
	| l1 == 1 && l2 == 1 = actProbMergeBy comp (actProbCollE V.// [(i1,h1)]) a2l
	| l1 == 1 &&  l2 == fNN = a2l V.// [(i1, (p1 + (fst $ a2l V.! i1),a1))]
	| l1 == fNN && l2 == 1 = a1l V.// [(i2, (p2 + (fst $ a1l V.! i2),a2))]
	| l1 == fNN && l2 == fNN = V.zipWith (\(p1,a1) (p2, a2) -> (p1+p2,a1)) a1l a2l
	| otherwise = error ("actProbMergeBy: l1=" ++ (show l1) ++ " l2=" ++ (show l2))
--	| a1 == a2 = error "actProbMergeBy: equal element in vectors to merge"
	where 
		l1 = V.length a1l
		l2 = V.length a2l
		h1@(p1,a1) = V.head a1l
		h2@(p2,a2) = V.head a2l
		i1 = xy2ind $ getIdx a1
		i2 = xy2ind $ getIdx a2
	--OL.mergeBy actCompInProbAct a1l a2l
	--V.fromList $ OL.mergeBy comp (V.toList a1l) (V.toList a2l)

actMergeWithProb = actProbMergeBy
-}

actProbCollS v = V.singleton v
actProbCollE = V.empty
actProbColl lst = V.fromList lst
actProbCollToList coll = V.toList coll

actProbMergeBy :: (ActProb -> ActProb -> Ordering) -> ActProbColl -> ActProbColl -> ActProbColl 
actProbMergeBy comp a1l a2l 
	| l1 == 0 || l2 == 0 = a1l V.++ a2l
	| a1 == a2 = error "actProbMergeBy: equal element in vectors to merge"
	| a1 < a2 = V.cons h1 $ actProbMergeBy comp t1 a2l
	| a2 < a1 = V.cons h2 $ actProbMergeBy comp a1l t2
	where 
		l1 = V.length a1l
		l2 = V.length a2l
		h1@(p1,a1) = V.head a1l
		h2@(p2,a2) = V.head a2l
		t1 = V.tail a1l
		t2 = V.tail a2l
	--OL.mergeBy actCompInProbAct a1l a2l
	--V.fromList $ OL.mergeBy comp (V.toList a1l) (V.toList a2l)

actMergeWithProb :: (ActProb -> ActProb -> Ordering) -> Prob -> ActProbColl -> Prob -> ActProbColl -> ActProbColl 
actMergeWithProb actCompInProbAct mulleft acts1m mulright acts2m 
	| l1 == 0 || l2 == 0 = 
			--trace ("pmf") $
			(fmap (\(p,a) -> (p*mulleft,a)) acts1m) V.++ (fmap (\(p,a) -> (p*mulright,a)) acts2m)
	| a1 == a2 = V.cons (p1*mulleft+p2*mulright, a1) $ actMergeWithProb actCompInProbAct mulleft t1 mulright t2
	| a1 < a2 = V.cons (p1*mulleft,a1) $ actMergeWithProb actCompInProbAct mulleft t1 mulright acts2m
	| a2 < a1 = V.cons (p2*mulright,a2) $ actMergeWithProb actCompInProbAct mulleft acts1m mulright t2
	where
		l1 = V.length acts1m
		l2 = V.length acts2m
		(p1,a1) = V.head acts1m
		(p2,a2) = V.head acts2m
		t1 = V.tail acts1m
		t2 = V.tail acts2m
		
--	acts
--	where
--		actsg = groupBy actEqInProbAct $ OL.mergeBy actCompInProbAct (V.toList acts1m) (V.toList acts2m)
--		acts = V.fromList $ map (foldl1 (\(psum, actsum) (p, act) -> (psum+p, act))) actsg



{-|
type ActProbColl a = [] (ActProb a)
actProbCollS v = [v]
actProbCollE = []
actProbColl lst = lst
actProbCollToList coll = coll

actProbMergeBy :: (Ord a) => (ActProb a -> ActProb a -> Ordering) -> ActProbColl a -> ActProbColl a -> ActProbColl a
actProbMergeBy comp a1l a2l =
	--OL.mergeBy actCompInProbAct a1l a2l
	OL.mergeBy comp a1l a2l

actMergeWithProb :: (Ord a) => (ActProb a -> ActProb a -> Ordering) -> ActProbColl a -> ActProbColl a -> ActProbColl a
actMergeWithProb actCompInProbAct acts1m acts2m =
	acts
	where
		actsg = groupBy actEqInProbAct $ OL.mergeBy actCompInProbAct acts1m acts2m
		acts = map (foldl1 (\(psum, actsum) (p, act) -> (psum+p, act))) actsg
-}

data ActionsProb = ActionsProb {getActionsProb :: ActProbColl, getEnsNum :: CountType} | ImpossibleP deriving(Show)	

aintersect a1l a2l = OL.isect a1l a2l

choose :: (Integral a, Show a) => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n k 
	| k > n = error $ "choose k > n (k=" ++ (show k) ++ " n=" ++ (show n) ++ ")"
	| otherwise = choose (n-1) (k-1) * n `div` k

	
--intersectActions :: (Eq a) => Actions a -> Actions a -> Actions a
instance ActInters Actions where
--joinActions :: Actions a -> Actions a -> Actions a
	imposs = Impossible
	retacts _ act = Actions [act]
	retempty fld _ = Actions []
	
	joinActions Impossible a2 = Impossible
	joinActions a1 Impossible = Impossible
	joinActions (Actions a1l) (Actions a2l) = Actions (OL.merge a1l a2l)

	intersectActions Impossible a2 = a2
	intersectActions a1 Impossible = a1
	intersectActions (Actions []) (Actions _) = error "intersecting with empty list"
	intersectActions (Actions _) (Actions []) = error "intersecting with empty list"
	intersectActions (Actions a1l) (Actions a2l) = Actions $ aintersect a1l a2l

actCompInProbAct :: ActProb -> ActProb -> Ordering
actCompInProbAct (p1,a1) (p2,a2) = compare a1 a2

actEqInProbAct :: ActProb -> ActProb -> Bool
actEqInProbAct (p1,a1) (p2,a2) = (a1 == a2)


instance ActInters ActionsProb where
	imposs = ImpossibleP
	retacts filtact act 
		| filtered = ActionsProb (actProbCollS (1,act)) 1
		| otherwise = ActionsProb (actProbCollE)  1
		where
			filtered = filtact act
	
	retempty fld freeact
		| freecnt >= 1 = ActionsProb (actProbCollS (fromRational(freeprob), freeact (-1,-1) )) freecnt
--		| freecnt >= 1 = ActionsProb (map (\ind -> (freeprob, freeact ind)) (getAllClosed fld)) (freecnt)
--		| freecnt >= 1 = ActionsProb [] (freecnt)
		| freecnt <= 0 = error ("retempty (prob): freecnt <= 0: closedCnt=" ++ (show  c) ++ " mCnt=" ++ (show m))
		where 
			c = (closedCnt fld)
			m = 99 - (mineCnt fld)
			freecnt = choose c m
			freeprob = if c == 0 then 0 else (c - m) % c
			
	joinActions ImpossibleP a2 = ImpossibleP
	joinActions a1 ImpossibleP = ImpossibleP
	joinActions (ActionsProb a1l nens1) (ActionsProb a2l nens2) 
		| nens == 0 = error ("nens==0 on join: " ++ (show (a1l,nens1)) ++ (show (a2l,nens2)))
		| otherwise = ActionsProb {getActionsProb=(actProbMergeBy actCompInProbAct a1l a2l), getEnsNum = nens}
		where
			nens = nens1*nens2

	intersectActions ImpossibleP a2 = a2
	intersectActions a1 ImpossibleP = a1
	intersectActions (ActionsProb a1l ens1) (ActionsProb a2l ens2) 
		| a1l == actProbCollE || a2l == actProbCollE = error "intersecting with empty list"
		| totens == 0 = error "totens==0 on intersect"
--		| ll >= 30*16 = error "too long list in ActionsProb"
		| otherwise = ActionsProb acts totens
		where
			totens = ens1+ens2
			mulleft = fromRational(ens1 % totens)
			mulright = fromRational(ens2 % totens)
			acts = actMergeWithProb actCompInProbAct mulleft a1l mulright a2l


getix :: CIdx -> Int
getix (ix,_) = ix
getiy :: CIdx -> Int
getiy (_,iy) = iy

getSurrIdx :: Field -> CIdx -> [CIdx]
getSurrIdx fld (ix,iy) = do
	(ixs,iys) <- [(ix-1,iy-1), (ix,iy-1), (ix+1,iy-1),(ix+1,iy),(ix+1,iy+1),(ix,iy+1),(ix-1,iy+1),(ix-1,iy)]
	let (_,(nx,ny)) = bounds $ getArr fld
	guard (ixs >= 1 && ixs <= nx && iys >= 1 && iys <= ny)
	return (ixs,iys)

getSurrCells :: Field -> CIdx -> [Cell]
getSurrCells fld ind = fmap ((getArr fld)!) (getSurrIdx fld ind)

isDigitC :: Cell -> Bool
isDigitC (Digit _) = True
isDigitC _ = False

isClosedC :: Cell -> Bool
isClosedC Closed = True
isClosedC _ = False

isMineC :: Cell -> Bool
isMineC Mine = True
isMineC _ = False

isToOpenC :: Cell -> Bool
isToOpenC ToOpen = True
isToOpenC _ = False


instance Show Cell where
	show Mine = "M"
	show Closed = "C"
	show (Digit v) = "d" ++ show v

getClosedNearDigits :: Field -> [CIdx]
getClosedNearDigits fld = do
	ccell <- (range $ bounds $ getArr fld)
	guard (isClosedC $ (getArr fld)!ccell)
	guard (any (isDigitC) $ getSurrCells fld ccell)
	return ccell

getNextChainedCells :: Field -> [CIdx] -> CIdx -> [CIdx]
getNextChainedCells fld cells ind = 
	filter (`elem` cells) surcells
	where
		surdigits = filter (isDigitC.((getArr fld)!)) $ getSurrIdx fld ind
		surcells = nub $ concat $ forM (surdigits) (\dig -> filter (isClosedC.(fld>!)) $ getSurrIdx fld dig)

joinChains :: ([CIdx],[CIdx]) -> ([CIdx],[CIdx]) -> ([CIdx],[CIdx])
joinChains (ch1,_) (ch2, restcells) = (nub (ch1++ch2), restcells)

makeChain :: Field -> [CIdx] -> CIdx -> ([CIdx], [CIdx])
makeChain fld cells ind = 
	case chain of 
		[] ->  ([], restcells)
		_ -> foldl (\(ch,rs) iind -> joinChains (ch,rs) (makeChain fld rs iind)) (chain,restcells) chain 
	where
		chain = getNextChainedCells fld cells ind 
		restcells = filter ((`notElem` chain)) cells
	
makeChains :: Field -> [CIdx] -> [[CIdx]]
makeChains fld [] = []
makeChains fld (ind:cells) = 
	chain:(makeChains fld restcells)
	where
		(chain,restcells) = makeChain fld (ind:cells) ind

closedCellScore :: Field -> CIdx -> Int
closedCellScore fld ind =
	minimum $ map (\(mcnt, ccnt) -> choose ccnt mcnt) digval
	where
		digits = (filter (isDigitC.(fld>!)) (range $ bounds $ getArr fld))
		digval = map (\digind -> (getDigit (fld>!digind) ,length $ filter (==Closed) (getSurrCells fld digind))) digits

sortChain :: Field -> [CIdx] -> [CIdx]
sortChain fld chain =
	sortBy (comparing (closedCellScore fld)) chain

sortChains :: Field -> [[CIdx]] -> [[CIdx]]
sortChains fld [] = []
sortChains fld chains@(chain:rest) = 
	(sortChain fld chain):sortChains fld rest

		
updDig :: Field -> CIdx -> Cell
updDig fld ind 
	| v >= mcnt = Digit (v - mcnt)
	| otherwise = error "Incorrect field: too much mines for digit" 
	where
		(Digit v) = fld>!ind
		mcnt = length (filter isMineC (getSurrCells fld ind))

initUpdateFld :: Field -> Field
initUpdateFld fld@(Field fldarr closedC mineC) =
	Field (fldarr // (zip digits (fmap (updDig fld) digits))) closedC mineC
	where
		digits = filter (isDigitC.(fld>!)) (range $ bounds $ fldarr)


checkDigitOpen :: Field -> CIdx -> Bool
checkDigitOpen fld ind =
	closecnt >= dv
	where
		closecnt = (length $ filter (==Closed) (getSurrCells fld ind))
		Digit dv = fld>!ind

checkDigitCanOpen :: Field -> CIdx -> Bool
checkDigitCanOpen fld ind =
	closecnt > dv
	where
		closecnt = (length $ filter (==Closed) (getSurrCells fld ind))
		Digit dv = fld>!ind

checkAllDigitsOpen :: Field -> Bool
checkAllDigitsOpen fld =
	all id (fmap (checkDigitOpen fld) (filter (isDigitC.(fld>!)) (range $ bounds $ getArr fld)))

mineCntCount :: (Num a) => FieldArr -> a
mineCntCount fld = fromIntegral(length $ filter isMineC (fmap (fld!) (range $ bounds fld)))

closedCntCount :: (Num a) => FieldArr -> a
closedCntCount fld = fromIntegral(length $ filter isClosedC (fmap (fld!) (range $ bounds fld)))

checkAction :: Field -> Action -> Bool
checkAction fld (Open cc) =
	all id (fmap (checkDigitCanOpen fld) surri) && closedcnt > (99 - minescnt)
	where
		surri = filter (isDigitC.(fld>!)) (getSurrIdx fld cc)
		minescnt = mineCnt fld
		closedcnt = closedCnt fld
checkAction fld (SetM cc) =
	(all (\cell -> (getDigit cell) > 0) surrd) && (totmns < 99)
	where
		surrd = filter isDigitC (getSurrCells fld cc)
		totmns = mineCnt fld


updSurrDig :: Field -> Action -> Field
updSurrDig fld@(Field fldarr closedC mineC) (SetM ind) 
	| closedC <= 0 || mineC >= 99 = error "updSurrDug with mine: closedC <= 0 || mineC >= 99"
	| otherwise = Field (fldarr//((ind,Mine):(zip surri (fmap ((\(Digit v) -> Digit (v - 1)).(fldarr!)) surri )))) (closedC - 1) (mineC + 1)
	where
		surri = filter (isDigitC.(fld>!)) (getSurrIdx fld ind)
updSurrDig fld@(Field fldarr closedC mineC) (Open ind) 
	| closedC <= 0 = error "updSurrDug with open: closedC <= 0"
	| mineC < 0 = error "updSurrDug with open: mineC < 0"
	| otherwise = Field (fldarr//[(ind,ToOpen)]) (closedC - 1) mineC

actProbFilt :: Action -> Bool
actProbFilt (SetM _) = False
actProbFilt (Open _) = True

tryAction :: (ActInters actions) => Field -> [CIdx] -> (CIdx -> Action) -> actions 
tryAction fld (cc:chain) acttype 
	| isPossible = joinActions (findActions (updSurrDig fld act) chain) acts
	| otherwise = imposs
	where
		act = acttype cc
		isPossible = checkAction fld act
		acts = retacts actProbFilt act 

findActions :: (ActInters actions) => Field -> [CIdx] -> actions 
findActions fld [] = retempty fld Open
findActions fld chain = intersectActions ts tc
	where
		ts = tryAction fld chain SetM
		tc = tryAction fld chain Open


compActionProb :: (Prob,Action) -> (Prob,Action) -> Ordering
compActionProb (r1, act1) (r2, act2) =
	case (act1,act2) of
		(Open _,Open _) -> compare r1 r2
		(SetM _,SetM _) -> compare r1 r2
		(Open _,SetM _) -> GT
		(SetM _,Open _) -> LT

sortActionsProb :: ActionsProb -> ActionsProb 
sortActionsProb (ActionsProb lst enscnt) = ActionsProb {getActionsProb = actProbColl (sortBy compActionProb (actProbCollToList lst)), getEnsNum=enscnt}
sortActionsProb ImpossibleP = ImpossibleP

evalAct :: Field -> [CIdx] -> Action -> Int
evalAct fld chain act 
--	| actions == Impossible = error ("evalAct: got Impossible actions: action tested: " ++ (show act))
	| otherwise = if inchain 
			then
				10 + (length $ filter (\act -> case act of 
					Open _ -> True
					SetM _ -> False) actlst)
			else
--				trace ("eval short: " ++ (show $ getIdx act)) $ 
					length $ intersect chain (getSurrIdx fld (getIdx act))
	where
		inchain = (getIdx act) `elem` chain
		actions = ((findActions (updSurrDig fld act) (filter (/= (getIdx act)) chain)) :: Actions)
		actlst = getActions $ actions


candAct :: Prob -> (Prob, Action) -> Bool
candAct _ (_,SetM _) = False
candAct p (p1, Open _) = p == p1

procFreeAct :: Field -> [CIdx] -> (Prob, Action) -> [(Prob, Action)]
procFreeAct fld cellsset pact@(p, Open (-1,-1)) = map (\ind -> (p, Open ind)) $ filter (`notElem` cellsset) (getAllClosed fld)
procFreeAct _ _ (_, SetM (-1,-1)) = error "SetM (-1,-1)"
procFreeAct _ _ pact = [pact]

getBestAction :: Field -> [CIdx] -> ActionsProb -> (Prob, Action)
getBestAction fld cellsset actsp = 
	--trace ("length of list to test for best: " ++ (show $ length actlist))
--	trace ("cellsset: " ++ (show cellsset)) $ trace ("length actspsort=" ++ (show $ length actspsort)) $ trace ("length evallist=" ++ (show $ length evallist)) 
	(pbest, abest)
	where 
		actspsort = reverse $ actProbCollToList $ getActionsProb $ sortActionsProb actsp
		(pfst,_) =  head actspsort
		actlist = (filter (candAct pfst) actspsort) >>= (procFreeAct fld cellsset)
		evallist = fmap ( \(p,act) -> (evalAct fld cellsset act, (p,act))) actlist
		(pbest, abest) = snd $ maximumBy (comparing fst) evallist

getAllClosed :: Field -> [CIdx]
getAllClosed fld = filter (isClosedC.(fld>!)) (range $ bounds $ getArr fld)



--tryActionPM :: Field -> [CIdx] -> (CIdx -> Action) -> ST s (STArray s (Int,Int) Integer) -> ST  s (STArray s (Int,Int) Integer)
tryActionPM :: Field -> [CIdx] -> (CIdx -> Action) -> Integer -> STArray s (Int,Int) Integer -> [CIdx] -> ST s (Integer)
tryActionPM fld (cc:chain) acttype enscnt arr toopencells
	| isPossible = findActionsPM (updSurrDig fld act) chain enscnt arr nextcells
	| otherwise = return enscnt
	where 
		act = acttype cc
		isPossible = checkAction fld act
		nextcells = case acttype cc of
				Open _ -> cc:toopencells
				_ -> toopencells

--findActionsPM :: Field -> [CIdx] -> Integer -ST  s (STArray s (Int,Int) Integer) -> ST  s (STArray s (Int,Int) Integer)
findActionsPM :: Field -> [CIdx] -> Integer -> STArray s (Int,Int) Integer -> [CIdx] -> ST s (Integer)
findActionsPM fld [] enscnt arri toopencells = 
	do
		--arr <- unsafeThaw arri
		let
			arr = arri
		let 
			--toopencells = (filter (isToOpenC.(fld>!)) (range $ bounds $ getArr fld))
			c = (closedCnt fld)
			m = 99 - (mineCnt fld)
			freecnt = choose c m
			ffreeind = head $ (filter (isClosedC.(fld>!)) (range $ bounds $ getArr fld))
			freecntpercell = if c == m then 0 else choose (c - 1) m
		
		{-|
		foldM_ (\_ ind -> do
					cnt <- readArray arr ind
					cnt `seq` writeArray arr ind (cnt + freecnt)
			) () toopencells
		-}
		forM_ toopencells (\ind -> do
					cnt <- readArray arr ind
					cnt `seq` writeArray arr ind (cnt + freecnt)
			) 
		
		if c > 0 then do
			cnt <- readArray arr ffreeind
			cnt `seq` writeArray arr ffreeind (cnt + freecntpercell)
		else
			return ()
		
		if freecnt <= 0 
		then
			error "findActionsPM: freecnt <= 0"
		else	
			return (enscnt + freecnt)
		
		
findActionsPM fld chain enscnt arr toopencells =
	do
		cnt1 <- tryActionPM fld chain Open enscnt arr toopencells
		cnt2 <- tryActionPM fld chain SetM cnt1 arr toopencells
		--trace ((show cnt1)++" "++(show cnt2)) $	
		cnt2 `seq` return cnt2
		

findActions2 :: Field -> [CIdx] -> ActionsProb
findActions2 fld chain 
	| enscnt == 0 = ActionsProb actProbCollE 0
	| otherwise = --trace ("enscnt=" ++ (show enscnt)) 
		ActionsProb (actProbColl actproblst) enscnt
	where
		(arr,enscnt) = 
			runST $ do
					arra <- newArray ((1,1),(30,16)) 0  :: ST s (STArray s (Int,Int) Integer)
					e_cnt <- findActionsPM fld chain 0 arra []
					elems <- freeze arra
					return (elems,e_cnt)

		--enscnt = sum [arr!ind | ind <- range $ bounds arr]
		actproblst = [ (fromRational((arr!ind) % enscnt), if ind `elem` chain then Open ind else Open (-1,-1)) | ind <- range $ bounds arr, arr!ind > 0]

