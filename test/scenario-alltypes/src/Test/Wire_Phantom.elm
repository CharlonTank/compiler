module Test.Wire_Phantom exposing (..)

import Lamdera.Wire2
import Test.Wire_Phantom2 exposing (..)



-- elm-explorations/test's mutually recursive types
--
-- type Quantity number units
--     = Quantity number
--
--
-- type Rate dependentUnits independentUnits
--     = Rate dependentUnits independentUnits
--


{-| -}
type alias Density =
    CubicMeters


expected_w2_encode_Density =
    Test.Wire_Phantom2.w2_encode_CubicMeters


expected_w2_decode_Density =
    Test.Wire_Phantom2.w2_decode_CubicMeters