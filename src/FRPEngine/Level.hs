module FRPEngine.Level where

import Control.Lens
import Linear
import FRPEngine.Types

-- Reason why we need to do this is because SDL renders from top left
negateYAxis :: (Num a) => V2 a -> V2 a
negateYAxis = _y `over` negate

scaleColl :: (Integral a, Fractional b) => a -> V2 a -> V2 b
scaleColl maxDim curr = curr' / maxDim'
  where
    maxDim' = fmap fromIntegral (V2 maxDim 3000)
    curr' = fmap fromIntegral curr

convexGen :: (RealFloat a) => [[V2 a]] -> [V2 a] -> [[V2 a]]
convexGen sum (x : rest@(x' : _)) = convexGen ([[x, x', V2 (x ^. _x) (-1)]] <> sum) rest
convexGen sum (x : _) = sum

scaleSize x = x * 8

convex = filter (not . null) . mconcat . fmap (convexGen [[]])

initialGame =
  GameState
    (CameraState 3)
    ( PhysicalState
        ( MovingState
            (Player (Living True (Object (V2 3000 0) (V2 500 500) 0 SobjectSprite)) 0 10)
            [(Living True (Object (V2 0 99999) (V2 500 500) 0 SobjectSprite))]
        )
        -- Here the points are relative to the object position. In game loop those gets turned into world position
        ( Scene
            ( [ -- ( Terrain
                --     ( (fmap . fmap)
                --         negateYAxis
                --         [ [ (V2 0 0.36666666666666664),
                --             (V2 0.3807291666666667 0.38055555555555554),
                --             (V2 0.3807291666666667 1),
                --             (V2 0 1)
                --           ],
                --           [ (V2 0.3807291666666667 0.7666666666666667),
                --             (V2 0.7197916666666667 0.7666666666666667),
                --             (V2 0.7229166666666667 1),
                --             (V2 0.3807291666666667 1)
                --           ],
                --           [ (V2 0.7322916666666667 0.3435185185185185),
                --             (V2 1 0.3675925925925926),
                --             (V2 1 1),
                --             (V2 0.7322916666666667 1)
                --           ]
                --         ]
                --     )
                --     (Object (V2 3000 10000) (V2 5000 5000) 0 SsceneDangerousSprite)
                -- ),
                -- (Terrain
                --   [[(V2 0 0), (V2 900 0), (V2 900 900), (V2 0 900)]]
                --   (Object (V2 0 0) (V2 1 1) 0))

                -- NEW TERRAIN!!!
                ( Terrain
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 989))
                            [ [ (V2 0 1896),
                                (V2 162 1925),
                                (V2 358 1998),
                                (V2 507 2005),
                                (V2 703 2122),
                                (V2 743 2341),
                                (V2 989 2563)
                              ]
                            ]
                        )
                    )
                    (Object (V2 (scaleSize 0) 0) (scaleSize (V2 989 3000)) 0 Sterr1)
                ),

                ( Terrain
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 2290))
                            [ [ (V2 0 2562),
                                (V2 182 2549),
                                (V2 433 2428),
                                (V2 538 2441),
                                (V2 710 2589),
                                (V2 806 2799),
                                (V2 968 2880),
                                (V2 1137 2868),
                                (V2 1354 2702),
                                (V2 1430 2524),
                                (V2 1583 2409),
                                (V2 1653 2208),
                                (V2 1842 2086),
                                (V2 1914 1839),
                                (V2 2151 1741),
                                (V2 2290 1588)
                              ]
                            ]
                        )
                    )
                    (Object (V2 (scaleSize (989 + 324)) 0) (scaleSize (V2 2290 3000)) 0 Sterr2)
                ),

                ( Terrain
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 3373))
                            [ [ (V2 0 1587),
                                (V2 83 1382),
                                (V2 115 1165),
                                (V2 182 1037),
                                (V2 309 974),
                                (V2 396 1050),
                                (V2 369 1200),
                                (V2 580 1362),
                                (V2 616 1619),
                                (V2 782 1702),
                                (V2 1102 1791),
                                (V2 1206 1974),
                                (V2 1412 2014),
                                (V2 1642 1775),
                                (V2 1970 1639),
                                (V2 2097 1322),
                                (V2 2313 1194),
                                (V2 2392 950),
                                (V2 2555 733),
                                (V2 2595 392),
                                (V2 2799 212),
                                (V2 3074 239),
                                (V2 3324 520),
                                (V2 3373 760)
                              ]
                            ]
                        )
                    )
                    (Object (V2 (scaleSize (989 + 324 + 2290 + 256)) 0) (scaleSize (V2 3373 3000)) 0 Sterr3)
                ),

                ( Terrain
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 1023))
                            [ [ (V2 0 767),
                                (V2 226 1005),
                                (V2 328 1274),
                                (V2 569 1464),
                                (V2 1023 1612)
                              ]
                            ]
                        )
                    )
                    (Object (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373 + 157)) 0) (scaleSize (V2 1023 3000)) 0 Sterr4)
                ),

                ( Terrain
                    ( convex
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 1109))
                            [ [ (V2 0 1614),
                                (V2 168 1770),
                                (V2 346 1816),
                                (V2 421 1998),
                                (V2 646 2089),
                                (V2 774 2347),
                                (V2 994 2270),
                                (V2 1109 2296)
                              ]
                            ]
                        )
                    )
                    (Object (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373 + 157 + 1023 + 479)) 0) (scaleSize (V2 1109 3000)) 0 Sterr5)
                )
              ]
            )

            -- Landing spots
            ( [ -- ( LandingSpot
                --     3
                --     ( Terrain
                --         ( (fmap . fmap)
                --             negateYAxis
                --             [ [ (V2 0 0.36666666666666664),
                --                 (V2 0.3807291666666667 0.38055555555555554),
                --                 (V2 0.3807291666666667 1),
                --                 (V2 0 1)
                --               ],
                --               [ (V2 0.3807291666666667 0.7666666666666667),
                --                 (V2 0.7197916666666667 0.7666666666666667),
                --                 (V2 0.7229166666666667 1),
                --                 (V2 0.3807291666666667 1)
                --               ],
                --               [ (V2 0.7322916666666667 0.3435185185185185),
                --                 (V2 1 0.3675925925925926),
                --                 (V2 1 1),
                --                 (V2 0.7322916666666667 1)
                --               ]
                --             ]
                --         )
                --         (Object (V2 (-2000) 10000) (V2 5000 5000) 0 SsceneSprite)
                --     )
                -- ),
                -- -- NEW LANDING!!!

                ( LandingSpot
                    1
                    ( Terrain
                    (convex
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 324))
                            [ [ (V2 0 2571),
                                (V2 324 2571)
                              ]
                            ]
                        )
                    )
                        (Object (V2 (scaleSize (989)) 0) (scaleSize (V2 324 3000)) 0 Sland1)
                    )
                ),

                ( LandingSpot
                    2
                    ( Terrain
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 256))
                            [ [ (V2 0 1588),
                                (V2 256 1588)
                              ]
                            ]
                        )
                        (Object (V2 (scaleSize (989 + 324 + 2290)) 0) (scaleSize (V2 256 3000)) 0 Sland2)
                    )
                ),

                ( LandingSpot
                    3
                    ( Terrain
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 157))
                            [ [ (V2 0 766),
                                (V2 157 766)
                              ]
                            ]
                        )
                        (Object (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373)) 0) (scaleSize (V2 157 3000)) 0 Sland3)
                    )
                ),

                ( LandingSpot
                    1
                    ( Terrain
                        ( (fmap . fmap)
                            (negateYAxis . (scaleColl 479))
                            [ [ (V2 0 1613),
                                (V2 479 1613)
                              ]
                            ]
                        )
                        (Object (V2 (scaleSize (989 + 324 + 2290 + 256 + 3373 + 157 + 1023)) 0) (scaleSize (V2 479 3000)) 0 Sland4)
                    )
                )
              ]
            )
        )
    )
