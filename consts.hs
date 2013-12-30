module Main where

    import Math.NumberTheory.GCD
    import Data.Tuple.All
    import Data.Bits
    import Numeric
    import Data.Char
    import Data.List

    m = 0xAD107E1E9123A9D0D660FAA79559C51FA20D64E5683B9FD1B54B1597B61D0A75E6FA141DF95A56DBAF9A3C407BA1DF15EB3D688A309C180E1DE6B85A1274A0A66D3F8152AD6AC2129037C9EDEFDA4DF8D91E8FEF55B7394B7AD5B7D0B6C12207C9F98D11ED34DBF6C6BA0B2C8BBC27BE6A00E0A0B9C49708B3BF8A317091883681286130BC8985DB1602E714415D9330278273C7DE31EFDC7310F7121FD5A07415987D9ADC0A486DCDF93ACC44328387315D75E198C641A480CD86A1B9E587E8BE60E69CC928B2B9C52172E413042E9B23F10B0E16E79763C9B53DCF4BA80A29E3FB73C16B8E75B97EF363E2FFA31F71CF9DE5384E71B81C0AC4DFFE0C10E64F :: Integer

    toWords 0   _ = []
    toWords cnt x = x .&. 0xffffffff : toWords (cnt - 1) (x `shiftR` 32)

    toC [] = []
    toC list = x : toC y
        where
        (x, y) = splitAt 6 list

    thing m = intercalate "\n" $ map (intercalate ", ") $ toC $  map (\x -> "0x" ++ showIntAtBase 16 intToDigit x "") (toWords 64 m)

    main = do
        putStrLn $ thing $ (2^4096) `mod` m
        let (gcd, m', r_inv) = extendedGCD m (2^2048)
        putStrLn $ thing (-m')
        putStrLn $ thing $ (2^2048) `mod` m
