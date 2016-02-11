import Graphics.GD
import Data.Bits
import Data.List
import Data.Ord
import Control.Monad
import Control.Monad.Zip
import Control.Monad.State
import System.IO.Unsafe
import Data.Array.Unboxed
import Data.Array.Unsafe
import qualified Data.Array.MArray as MA

import qualified Data.Array as A
import Data.Int
import Data.Word
import Data.Bits
import Data.Ratio

import System.IO
import System.Process
import System.Exit
import qualified Data.Foldable as F
import Malg 
import qualified Graphics.UI.Gtk as G
import Text.Printf
import System.Environment
import GHC.Conc
import Control.Exception

data PictGeom = PictGeom { x1 :: Int, y1 :: Int, sx :: Int, sy :: Int, nx :: Int, ny :: Int} deriving(Show)


data PictCellStat = PictCellStat { avghs::(Float,Float,Float), sq :: Float} deriving(Show)
	
statRefG :: [(PictCellStat, Cell)]
statRefG = [
		(PictCellStat {avghs=(0.0,0.0,0.0), sq=0.0}, Digit 0),
		(PictCellStat {avghs=(240.0,0.95,0.51), sq=0.13}, Digit 1),
		(PictCellStat {avghs=(120.0,0.9,0.35), sq=0.18}, Digit 2),
		(PictCellStat {avghs=(0.0,0.9,0.53), sq=0.18}, Digit 3),
		(PictCellStat {avghs=(240.0,0.94,0.27), sq=0.2}, Digit 4),
		(PictCellStat {avghs=(0.0,0.895,0.35), sq=0.21}, Digit 5),
		(PictCellStat {avghs=(180.0,0.91,0.53), sq=0.24}, Digit 6),
		(PictCellStat {avghs=(-60,0.90,0.35), sq=0.164}, Digit 7),
		(PictCellStat {avghs=(1.0,0.75,0.46), sq=0.435}, Mine)
	  ]

statRefK :: [(PictCellStat, Cell)]
statRefK = [
		(PictCellStat {avghs=(0.0,0.0,0.0), sq=0.0}, Digit 0),
		
		(PictCellStat {avghs=(239.1,0.828,0.585), sq=0.196}, Digit 1),
		(PictCellStat {avghs=(118.5,0.68,0.40), sq=0.233}, Digit 2),
		(PictCellStat {avghs=(61.04,0.70,0.38), sq=0.241}, Digit 3),
		(PictCellStat {avghs=(-59.4,0.83,0.32), sq=0.207}, Digit 4),
		(PictCellStat {avghs=(1.5,0.85,0.58), sq=0.236}, Digit 5),
		(PictCellStat {avghs=(1.7,0.79,0.34), sq=0.240}, Digit 6),
		(PictCellStat {avghs=(178.5,0.79,0.34), sq=0.159}, Digit 7),
		
		(PictCellStat {avghs=(0.0,0.86,0.43), sq=0.139}, Mine)
	  ]


{-|
instance (Ix i, IArray ar a, IArray ar a) => Functor (UArray i) where
	--fmap :: (IArray UArray a, IArray UArray b) => (a -> b) -> UArray i a -> UArray i b
	fmap f arr = 
		array bnds [(i, f (arr!i)) | i <- range bnds]
		where
			bnds = bounds arr
-}

fmapA :: (Ix i, IArray ar a, IArray ar b) => (a -> b) -> ar i a -> ar i b
fmapA f arr = 
	array bnds [(i, f (arr!i)) | i <- range bnds]
		where
			bnds = bounds arr

sumA :: (Ix i, Num a, IArray ar a) => ar i a -> a
sumA arr = sum [arr!ind | ind <- range $ bounds arr]
			
getPixR img pix = getPixel pix img
setPixR img pnt (r,g,b,a) = setPixel pnt (rgba r g b a) img
setPixRc img pnt clr = 
	do 
		setPixel pnt clr img
		--print pnt
setPixRcz img _ (pnt,clr) = 
	do 
		setPixel pnt clr img
		--print pnt

setPixRiz img _ (pnt,int) = 
	do 
		setPixel pnt (rgba int int int 0) img
		
setIntArrPix :: Image -> UArray (Int, Int) Int32 -> IO ()
setIntArrPix img arr =
	do
	tmp <- sequence_ [ let 
			       (r,g,b) =  int32toRGB (arr!ind)
			   in 
			      setPixel ind (rgba r g b 0) img | ind <- range bnds ]
	return ()
	where
		bnds = bounds arr


intensity :: (Int,Int,Int) -> Float
intensity (r,g,b) = (0.30*(fromIntegral r) + 0.59*(fromIntegral g) + 0.11*(fromIntegral b))



allPoints :: Int -> Int -> [(Int, Int)]
allPoints wx wy = [(a, b) | a <- [1..wx], b <- [1..wy]]
{-|	do
		a <- [1..wx]
		b <- [1..wy]
		return (a,b)
-}

allClrs wx wy = [(a, b) | a <- [1..wx], b <- [1..wy]]

int_diff :: (Ord a, Num a) => a -> a -> a
int_diff c1 c2 = min (2*abs(c2-c1)) 255


cint32 c = fromIntegral(c) :: Int32

color2int :: Color -> Int32
color2int c = (shift (cint32 r) 16) .|. (shift (cint32 g) 8) .|. (cint32 b)
	where
		(r,g,b,_) = toRGBA c

rgbtoint32 :: (Int, Int, Int) -> Int32
rgbtoint32 (r, g, b) = (shift (cint32 r) 16) .|. (shift (cint32 g) 8) .|. (cint32 b)

int32toRGB :: Int32 -> (Int, Int, Int)
int32toRGB c32 = (r, g, b)
	where
		c = fromIntegral(c32)::Int
		b = c .&. 0xFF
		g = (shift c (-8)) .&. 255
		r = (shift c (-16)) .&. 0xFF
		
getPixels :: Image -> UArray (Int, Int) Int32
getPixels img =
	array ((1,1),(wx,wy)) [ ((ix,iy),color2int $ unsafePerformIO $ getPixel (ix,iy) img) | ix <- [1..wx], iy <- [1..wy]]
	where
		(wx, wy) =  unsafePerformIO $ imageSize img

list2arr :: Int -> Int -> [Float] -> UArray (Int,Int) Float
list2arr nx ny lst = array ((1,1),(nx,ny)) (zip [(ix, iy) | ix <- [1..nx], iy <- [1..ny] ] lst)

getCellPix :: UArray (Int,Int) Int32 -> PictGeom -> Int -> Int -> UArray (Int,Int) Int32
getCellPix img g icx icy =
	--array bnds [(ind, img!ind) | ind <- range bnds ]
	ixmap bnds (\x -> x) img
	where
		bnds = (((x1 g) + (icx-1)*(sx g), (y1 g) + (icy-1)*(sy g)), ((x1 g) + icx*(sx g) - 1, (y1 g) + icy*(sy g) - 1))

horsum :: UArray (Int, Int) Float -> UArray (Int) Float
horsum p = 
	array (y1,y2) [(iy,(sum [p!(ix,iy) | ix <- [x1..x2]])/( fromIntegral $ x2-x1+1))  | iy <- [y1..y2]]
	where
		((x1,y1),(x2,y2)) = bounds p
vertsum :: UArray (Int, Int) Float -> UArray (Int) Float
vertsum p = 
	array (x1,x2) [(ix,(sum [p!(ix,iy) | iy <- [y1..y2]])/( fromIntegral $ y2-y1+1))  | ix <- [x1..x2]]
	where
		((x1,y1),(x2,y2)) = bounds p



sum2imgarr :: UArray (Int) Float -> UArray (Int) Float -> UArray (Int,Int) Float
sum2imgarr h v = 
	array bnds [ ((ix,iy), min ((h!iy) + (v!ix)) 255)
		| ix <- [x1..x2], iy <- [y1..y2] ]
	where
		(x1,x2) = bounds v
		(y1,y2) = bounds h
		bnds = ((x1,y1),(x2,y2))

acorr :: UArray (Int) Float -> Int -> Float
acorr s gap =
	sum [ (s!(i1+i))*(s!(i1+gap+i)) | i <- [0..(i2 - i1 - gap)] ]
	where
		(i1,i2) = bounds s

acorr2max :: UArray (Int) Float -> UArray (Int) Float -> Int
acorr2max h v = g
	where
		(_,nx) = bounds v
		(_,ny) = bounds h
		ah0 = acorr h 0
		av0 = acorr v 0
		(m, g) = maximumBy (comparing fst) [ ( (acorr h gap)/ah0 + (acorr v gap)/av0, gap) | gap <- [5..((min nx ny) `div` 5)] ]

{-|
calcsxsy :: UArray (Int) Float -> UArray (Int) Float -> Int -> (Int, Int)
calcsxsy h v hsz =
	(sx+1, sy+1)
	where
		(_,nx) = bounds v
		(_,ny) = bounds h
		(_,sy) = maximumBy (comparing fst) [ (h!i,i) | i <- [(hsz `div` 2)..(ny `div` 4)] ]
		(_,sx) = maximumBy (comparing fst) [ (v!i,i) | i <- [(1)..(nx `div` 4)] ]
-}

testShift :: UArray (Int) Float -> Int -> Int -> Int -> Float
testShift hv sz shift n =
	sum (fmap (hv!) pnts)
	where
		(i1,i2) = bounds hv
		pnts = enumFromThenTo (i1 + shift) (i1 + shift + sz) (min (shift + n*sz+1) i2)

calcShift :: UArray (Int) Float -> Int -> Int-> Int
calcShift hv sz n =
	snd $ maximumBy (comparing fst) [(testShift hv sz shift n, shift) | shift <- [1..10*sz]]

calcsxsy :: UArray (Int) Float -> UArray (Int) Float -> Int -> Int -> Int -> (Int, Int)
calcsxsy h v hsz nx ny = (calcShift v hsz nx, calcShift h hsz ny)

calcnxny wx wy hsz sx sy = ((wx - sx) `div` hsz, (wy - sy) `div` hsz)

diff_x :: UArray (Int, Int) Float -> UArray (Int, Int) Float
diff_x ints =
	array bnds ([ ((ix,iy),int_diff (ints!(ix,iy)) (ints!(ix+1,iy))) | ix <- [x1..x2-1], iy <- [y1..y2] ] ++ [ ((x2,iy),0) | iy <- [y1..y2] ])  
	where
		bnds = bounds ints
		((x1,y1),(x2,y2)) = bnds

diff_y :: UArray (Int, Int) Float -> UArray (Int, Int) Float
diff_y ints =
	array bnds ([ ((ix,iy),int_diff (ints!(ix,iy)) (ints!(ix,iy+1))) | ix <- [x1..x2], iy <- [y1..y2-1] ] ++ [ ((ix,y2),0) | ix <- [x1..x2] ])
	where
		bnds = bounds ints
		((x1,y1),(x2,y2)) = bnds

calcGeom :: UArray (Int,Int) Int32 -> PictGeom
calcGeom clrs = 
	PictGeom {x1=ox,y1=oy,sx=hsz,sy=hsz,nx=nxc,ny=nyc}
	where
		((x1,y1),(x2,y2)) = bounds clrs
		wx = x2 - x1 + 1
		wy = y2 - y1 + 1
		ints = fmapA (intensity.int32toRGB) clrs
		hs = horsum $ diff_y ints
		vs = vertsum $ diff_x ints
		hsz = acorr2max hs vs
		(nxc, nyc) = (30,16)
		--(ox,oy) = calcsxsy hs vs hsz
		(_,oy) = calcsxsy hs vs hsz nxc nyc
		--(nxc, nyc) = calcnxny wx wy hsz ox oy
		ox = (wx - nxc*hsz) `div` 2

removegray :: (Int, Int, Int) -> (Int, Int,Int)
removegray (r, g, b)
	| abs(r - g) < 10 && abs (g - b) < 10 = (0,0,0)
	| otherwise = (r,g,b)

-- data PictCellStat = PictCellStat { avgH :: Float, avgV :: Float, sq :: Float} deriving(Show)
nonzclr :: (Int, Int, Int) -> Float
nonzclr (r, g, b) 
	| r /= 0 || g /= 0 || b /= 0 = 1.0
	| otherwise = 0.0

rgb2hs :: (Int, Int, Int) -> (Float, Float, Float) -- (Hue, Saturation, Brightness)
rgb2hs (r,g,b)
	| d < 0.1 = (0.0,0.0,0.0)
	| cmax == rf = (60.0*((gf-bf)/d), (d/dd),l)
	| cmax == gf = (60.0*((bf-rf)/d+2), (d/dd),l)
	| cmax == bf = (60.0*((rf-gf)/d+4), (d/dd),l)
	where
		rf = fromIntegral(r)/255.0
		gf = fromIntegral(g)/255.0
		bf = fromIntegral(b)/255.0
		cmax = maximum [rf,gf,bf]
		cmin = minimum [rf,gf,bf]
		d  = cmax - cmin
		l = (cmax + cmin)/2
		dd = (1 - abs(2*l - 1))

avgclr :: UArray (Int,Int) Int32 -> (Float, Float, Float)
avgclr cimg =
	(hs/fcnt, ss/fcnt,brs/fcnt)
	where
	hss = foldr (\(hue, sat, br) ((hs,ss,brs),cnt) -> if br > 0.2 then ((hs+hue,sat+ss,br+brs),cnt+1) else ((hs,ss,brs),cnt))
 	    ((0.0,0.0, 0.0),0)
	    [let 
		(r,g,b) = int32toRGB (cimg!ind) 
	    in rgb2hs (r,g,b) 
	    	| ind <- range $ bounds cimg]
	(hs,ss,brs) = fst hss
	fcnt = (fromIntegral $ snd hss) + 0.001
		

cellStat :: UArray (Int,Int) Int32 -> PictCellStat
cellStat cimg = 
	PictCellStat {avghs = frgb, sq=square}
	where
		frgb = avgclr cimg
		d = fmapA (nonzclr.removegray.int32toRGB) cimg
		((x1,y1),(x2,y2)) = bounds d
		square = sumA(d)/(fromIntegral(x2 - x1 + 1)*fromIntegral(y2 - y1 + 1))
		
statDist :: PictCellStat -> PictCellStat -> Float
statDist st1 st2 = 
	abs(h1 - h2)/100.0 + abs(s1 - s2) + 2*abs(sq1 - sq2) + abs(br1 - br2)
	where
		(h1,s1,br1) = avghs st1
		(h2,s2,br2) = avghs st2
		sq1 = sq st1
		sq2 = sq st2


avg :: (Integral a) => [a] -> Float
avg lst = 
	fromIntegral(sum)/fromIntegral(cnt)
	where
		(sum,cnt) = foldl (\(s,cnt) x -> (s+x,cnt+1)) (0,0) lst

parseGrayCell :: UArray (Int,Int) Int32 -> Cell
parseGrayCell cimg 
	| ravg < 0.85*255 = Digit 0
	| otherwise = Closed
	where 
		ravg = avg $ fmap ((\(r,_,_)->r).int32toRGB.(cimg !)) (range $ bounds cimg)

parseCell :: [(PictCellStat, Cell)] -> UArray (Int,Int) Int32 -> Cell
parseCell rstat cimg 
	| (ct == Digit 0) = parseGrayCell cimg
	| otherwise = ct
	where
		stat = cellStat cimg
		ct = snd (minimumBy (comparing fst) [ (statDist stat pcs, cl) | (pcs, cl) <- rstat])

parseField :: UArray (Int,Int) Int32 ->  PictGeom -> [(PictCellStat, Cell)] -> Field
parseField img g rstat =
	field $ array cbnds [(cind, parseCell rstat $ getCellPix img g (fst cind) (snd cind)) | cind <- range cbnds]
	where
		cbnds = ((1,1),(nx g, ny g))

drawCell :: Image -> PictGeom -> (Int,Int) -> Cell -> IO ()
drawCell img g ind cell =
	do
		let s = show cell
		let icx = (fst ind) - 1
		let icy = (snd ind) - 1
		drawString "variable" 12.0 0.0 ((x1 g) + icx*(sx g),(y1 g)+12 + icy*(sy g)) s (rgba 255 0 0 0) img
		return ()
		
drawAction :: Image -> PictGeom -> Action -> IO ()
drawAction img g (SetM ind) =
	do
		let icx = (fst ind) - 1
		let icy = (snd ind) - 1
		let cwx = sx g
		let cwy = sy g
		drawFilledEllipse ((x1 g) + icx*cwx + (cwx `div` 2),(y1 g) + icy*cwy + (cwy `div` 2)) (cwx, cwy) (rgba 255 0 0 0) img
		return ()
drawAction img g (Open ind) =
	do
		let icx = (fst ind) - 1
		let icy = (snd ind) - 1
		let cwx = sx g
		let cwy = sy g
		drawFilledEllipse ((x1 g) + icx*cwx + (cwx `div` 2),(y1 g) + icy*cwy + (cwy `div` 2)) (cwx, cwy) (rgba 0 255 0 0) img
		return ()
		
drawActions imgnew g (Actions acts1) = forM_ (acts1) (\act -> drawAction imgnew g act )
drawActions imgnew g Impossible = return ()


setMouseTo :: PictGeom -> Action -> IO ()
setMouseTo g act = 
	do
		let (ind,mb) = case act of
			Open ind_ -> (ind_,1)
			SetM ind_ -> (ind_,3)
		let 
			icx = (fst ind) - 1
			icy = (snd ind) - 1
			cwx = sx g
			cwy = sy g
			mx = (x1 g) + icx*cwx + (cwx `div` 2)
			my = (y1 g) + icy*cwy + (cwy `div` 2)
		excode <- system $ "xdotool search --onlyvisible \"Mines\" windowactivate mousemove --sync --window \"%1\" " ++ (show mx) ++ " " ++ (show my)
		return ()

playAction :: PictGeom -> Action -> IO ()
playAction g act =
	do
		let (ind,mb) = case act of
			Open ind_ -> (ind_,1)
			SetM ind_ -> (ind_,3)
			
		let 
			icx = (fst ind) - 1
			icy = (snd ind) - 1
			cwx = sx g
			cwy = sy g
			mx = (x1 g) + icx*cwx + (cwx `div` 2)
			my = (y1 g) + icy*cwy + (cwy `div` 2)
			
		excode <- system $ "xdotool search --onlyvisible \"Mines\" windowactivate mousemove --sync --window \"%1\" " ++ (show mx) ++ " " ++ (show my) ++ " click " ++ show mb
		if (excode == ExitSuccess) then return () else error "could not move mouse pointer" 
--		excode <- system $ "xdotool click " ++ show mb
--		if (excode == ExitSuccess) then return () else error "could not move mouse pointer" 
		return ()

playActions :: PictGeom -> Actions -> IO ()
playActions _ Impossible = return ()
playActions g acts =
	do
		--windids <- readProcess "/usr/bin/xdotool" ["search","Mines"] ""
		--let wndid = (words windid)!!1
		forM_ (getActions acts) (\act -> playAction g act)
		return ()

playActionsJ :: PictGeom -> Actions -> IO ()
playActionsJ _ Impossible = return ()
playActionsJ g acts =
	do
		let str = concat $ map (\act ->  let
					(ind,mb) = case act of
						Open ind_ -> (ind_,1)
						SetM ind_ -> (ind_,3)	
					icx = (fst ind) - 1
					icy = (snd ind) - 1
					cwx = sx g
					cwy = sy g
					mx = (x1 g) + icx*cwx + (cwx `div` 2)
					my = (y1 g) + icy*cwy + (cwy `div` 2)
				in
					" mousemove --window=%1 " ++ (show mx) ++ " " ++ (show my) ++ " click " ++ (show mb)
		     ) (getActions acts)
		excode <- system $ "xdotool search --onlyvisible \"Mines\" windowactivate" ++ str
		if (excode == ExitSuccess) then return () else error "could not move mouse pointer" 

get3fromWord32 :: UArray Int Word32 -> Int -> (Int,Int,Int)
get3fromWord32 arr pos =
	(r,g,b)
	where
		posw = [pos `div` 4, (pos + 1) `div` 4, (pos + 2) `div` 4]
		posb = [(pos `mod` 4), ((pos + 1) `mod` 4), ((pos + 2) `mod` 4)]
		w = fmap (arr!) posw
		bb = zipWith (\w32 ib -> (shift (fromIntegral w32 ) (-ib*8) .&. 0xFF)) w posb
		r = head bb
		g = head $ tail bb
		b = head $ tail $ tail bb

getPixelsFromPB :: UArray Int Word32 -> Int -> Int -> Int -> Int -> UArray (Int, Int) Int32
getPixelsFromPB clrsw wx wy rs nch = 
	array bnds (map (\(ix,iy) -> let
					pos = (iy-1)*rs + (ix-1)*nch
					(r,g,b) = get3fromWord32 clrsw pos
				     in
					((ix,iy), rgbtoint32 (fromIntegral(r),fromIntegral(g),fromIntegral(b)))
		    ) (range bnds))
	where
		bnds = ((1,1),(wx,wy))

getPixelsFromPixbuf :: G.Pixbuf -> IO (UArray (Int, Int) Int32)
getPixelsFromPixbuf pbuf =
	do
		pbData <- G.pixbufGetPixels pbuf :: IO (G.PixbufData Int Word32)
		wx <- G.pixbufGetWidth pbuf
		wy <- G.pixbufGetHeight pbuf
		rs <- G.pixbufGetRowstride pbuf
		nch <- G.pixbufGetNChannels pbuf
		--putStrLn $ printf "wx=%d wy=%d rs=%d nch=%d" wx wy rs nch
		clrsw <- pbData `seq` (MA.freeze pbData) :: IO (UArray Int Word32)
				
		return (clrsw `seq` (getPixelsFromPB clrsw wx wy rs nch))
		
	--array ((1,1),(wx,wy)) [ ((ix,iy),color2int $ unsafePerformIO $ getPixel (ix,iy) img) | ix <- [1..wx], iy <- [1..wy]]
	--where
	--	(wx, wy) =  unsafePerformIO $ imageSize img

grabWindow :: IO (UArray (Int, Int) Int32)
grabWindow = 
	do 
		winidstr <- readProcess "xdotool" ["search","--onlyvisible", "Mines", "windowactivate","search","--onlyvisible", "Mines"] ""
		threadDelay 40000
		let 
			winid = read winidstr
		print winid
		--system "xdotool search --onlyvisible \"Mines\" windowactivate"
		--if (excode == ExitSuccess) then return () else error "could not activate window" 
		--G.initGUI
		mwnd <- G.drawWindowForeignNew $ G.toNativeWindowId winid
		--mwnd <- G.drawWindowGetDefaultRootWindow
		let
			(Just wnd) = mwnd
		--G.drawWindowRaise wnd
		--excode <- system "usleep 200000"
		--excode <- system "gnome-screenshot -w -B -f /tmp/qq.png"
		--if (excode == ExitSuccess) then return () else error "could not grab window" 
		(wx,wy) <- G.drawableGetSize wnd
		mpbuf <- wnd `seq` G.pixbufGetFromDrawable wnd (G.Rectangle 0 0 wx wy)
		let
			(Just pbuf) = mpbuf
		
		--G.pixbufSave pbuf "/tmp/qq.png" "png" []
		--img <- loadPngFile "/tmp/qq.png"
		--return (getPixels img)
		clrs <- pbuf `seq` (getPixelsFromPixbuf pbuf)
		clrs `seq` return clrs
		

dumpCellImgStat :: UArray (Int, Int) Int32 -> PictGeom -> (Int,Int) -> IO ()
dumpCellImgStat clrs g (icx,icy) =
	do
		let
			arr_r = getCellPix clrs g icx icy
			((ix0,iy0),(_,_)) = bounds arr_r
			(cwx, cwy) = (sx g, sy g)
			cimg = ixmap ((1,1),(cwx, cwy)) (\(ix,iy) -> (ix + ix0 - 1, iy + iy0 - 1)) (fmapA (rgbtoint32.removegray.int32toRGB) arr_r)
			--cimg = arr_r
			fn = (printf "/home/gofman/Pictures/%02d_%02d.png" icx icy)::String
		imgnew <- newImage (cwx,cwy)
		setIntArrPix imgnew (cimg)
		--print $ sumA cimg
		print ((icx,icy),(cellStat cimg))
		savePngFile fn imgnew

getRefStat :: IO [(PictCellStat, Cell)]
getRefStat = 
	do
		winname <- readProcess "/usr/bin/xdotool" ["search","--onlyvisible","Mines","windowactivate","getwindowname"] ""
		case winname of
			"Mines\n" -> return $ statRefG
			"KMines\n" -> return $ statRefK
			_ -> error ("Could not recognize window: " ++ winname)


evalPictures :: IO ()
evalPictures = 
	do 
		clrs <- grabWindow
		let 
			g = calcGeom clrs
		print g
		forM_ [(icx,icy) | icx <- [1..nx g], icy <- [1..ny g]] (\ind -> dumpCellImgStat clrs g ind)
		
		--(wx, wy) <- imageSize img
		let
			((_,_),(wx,wy)) = bounds clrs
		
		imgnew <- newImage (wx, wy)
		setIntArrPix imgnew clrs
		forM_ [1..(ny g)] (\i -> setIntArrPix imgnew (fmapA (rgbtoint32.removegray.int32toRGB) (getCellPix clrs g i i)) )
		
		rs <- getRefStat
		let 
			ff = parseField clrs g rs
		useFontConfig True
		forM_ (range $ bounds (getArr ff)) (\ind -> drawCell imgnew g ind (ff>!ind) )
		savePngFile "/home/gofman/Pictures/qqq.png" imgnew
		return ()
		
getProbStr :: Prob -> String
getProbStr prob = printf "%.0f%% (%s)" ((fromRational(toRational(prob)) :: Float)*100.0 ) (show prob)

adviseGuess :: PictGeom -> (Prob,Action) -> IO ()
adviseGuess g (prob,act) =
		do
			let 
				probstr = getProbStr prob
			--G.initGUI
			dlg <- G.messageDialogNew Nothing [G.DialogModal] G.MessageInfo G.ButtonsClose "Estimated probability of guess"
			--G.messageDialogSetSecondaryText dlg probstr
			G.messageDialogSetSecondaryMarkup dlg ("<span foreground=\"blue\" size=\"x-large\">" ++ probstr ++ "</span>")
			G.dialogRun dlg
			setMouseTo g act
			return ()
			

decodeImg :: Bool -> Bool -> Prob -> IO ()
decodeImg repeatact autoguess probprev = 
	do 
		putStr "overall prob: "
		putStrLn $ getProbStr probprev
		
		clrs <- grabWindow
		
		let 
			((_,_),(wx,wy)) = bounds clrs
		
		let 
			--pnts = allPoints wx wy
			g = calcGeom clrs
		print g
		
		--forM_ [1..(ny g)] (\i -> setIntArrPix imgnew (fmapA (rgbtoint32.removegray.int32toRGB) (getCellPix clrs g i i)) )
		--forM_ [1..(ny g)] (\i -> do print (i,(cellStat (getCellPix clrs g i i))) )
		
		--setIntArrPix imgnew (fmapA (rgbtoint32.removegray.int32toRGB) (getCellPix clrs g 4 8))
		--print ((4,8),(cellStat (getCellPix clrs g 4 8)))
		
		--setIntArrPix imgnew (fmapA (rgbtoint32.removegray.int32toRGB) (getCellPix clrs g 22 10))
		--print ((22,10),(cellStat (getCellPix clrs g 22 10)))
		
		rs <- getRefStat
		let 
			ff = parseField clrs g rs
		let ffu = initUpdateFld ff
		
		print $ mineCnt ffu
		print $ closedCnt ffu
		
		
		if (checkAllDigitsOpen ffu) then putStrLn("Digit closed cnt checked") else error "incorrect field"
		
		
		--let intsd = sum2imgarr hs vs
		--setIntArrPix imgnew (Main.fmap round intsd)
		--foldM_ (setPixRiz imgnew) () (zip pnts (Main.fmap round intsd))
		
		--foldM_ (\_ i -> drawFilledRectangle (sx + i*hsz,sy + i*hsz) (sx + (i+1)*hsz,sy + (i+1)*hsz) (rgba 255 0 0 0) imgnew) () [0..(ny-1)]
		useFontConfig True
		print $ bounds $ getArr ff
		
		--forM_ (range $ bounds ffu) (\ind -> drawCell imgnew g ind (ffu!ind) )
		--print $ chains
		
		--let (Actions acts1) = findActions ffu (chains!!0)
		--forM_ (chains) (\chain -> drawActions imgnew g (findActions ffu chain))
		
		--forM_ (getClosedNearDigits ff) (\ind -> drawCell imgnew g ind (ff!ind) )
		--forM_ (chains!!1) (\ind -> drawCell imgnew g ind (ff!ind) )

		let chainsj = concat $ sortChains ffu $ makeChains ffu (getClosedNearDigits ffu)
		let chains = 
			if (mineCnt ffu) >= 89 
			then 
				if (closedCnt ffu) - (length chainsj) <= 15 
					then 
						[getAllClosed ffu] 
					else
						[chainsj]
			else 
				sortChains ffu $ makeChains ffu (getClosedNearDigits ffu)
		putStr "N chains: "
		print $ length chains
		
		print "Calculating actions"
		let acts = if (closedCnt ffu) == ((nx g)*(ny g))
			then
				[Actions [Open ((nx g) `div` 2, (ny g) `div` 2)]]
			else
				filter (\acts -> acts /= Impossible) (map ((findActions ffu)::([CIdx]->Actions) ) chains)
				
		let nonempty = any (\(Actions lst) -> (length lst) > 0) acts 
		
		if any (== Impossible) acts then 
			error "Impossible condition on input field"
		else
			return ()
		
		
		if nonempty 
		then 
			-- forM_ (acts) (\act -> playActionsJ g act) 
			playActionsJ g (Actions $ concat $ map getActions acts )
		else do
			print "No actions, calculating probabilities"
			let
				cellsset = chainsj
				allact = findActions2 ffu cellsset
			--print allact
			let
				actssort = (sortActionsProb allact)
				actbest = getBestAction ffu cellsset actssort 	
			print $ actssort 
			let 
				(prob,act) = actbest
			if autoguess 
			then 
				do
					playActionsJ g (Actions [act])
					decodeImg repeatact autoguess (probprev*prob)
					
			else 
				adviseGuess g (prob,act) 
	
		if nonempty && repeatact
		then
			decodeImg repeatact autoguess probprev
		else
			return ()
		--savePngFile "/home/gofman/Pictures/qqq.png" imgnew


parseArg :: (Bool, Bool) -> String -> (Bool, Bool)
parseArg vals "" = vals
parseArg (_,b2) ('r':s) = parseArg (True,b2) s
parseArg (b1,_) ('g':s) = parseArg (b1,True) s
parseArg _ (c:s) = error ("parse argument: unknown argument \'" ++ [c,'\''])

parseArgs :: (Bool, Bool) -> [String] -> (Bool, Bool)
parseArgs vals [] = vals
parseArgs vals (('-':str):strs) = parseArgs (parseArg vals str) strs
parseArgs vals (str:strs) = error ("parse arguments: could not parse \'" ++ str ++ "\'")
			
main :: IO ()
main = 
	do
		G.initGUI
		args <- getArgs
		case args of
			[] -> putStrLn("Usage:\n\tmine [-e] | -[rg]\n\t-\tguess a cell to open once\n\
\\t-r:\tauto repeat\n\
\\t-g:\topen on probability guess without confirmation (may bomb)\n\
\\t-e:\tcompute digit recognition parameters and dump to console (for updating recognition tables in mine)\n")
			["-e"] -> evalPictures
			_ -> 	do
					(rep, guess) <- evaluate $ parseArgs (False,False) args
					(decodeImg rep guess 1 >> return ())

