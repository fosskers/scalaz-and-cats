freer-effects-0.3.0.0: benchmarks
Running 1 benchmarks...
Benchmark core: RUNNING...
benchmarking State/freer.get
time                 17.51 ns   (16.78 ns .. 18.14 ns)
                     0.993 R²   (0.991 R² .. 0.999 R²)
                     mean                 16.80 ns   (16.61 ns .. 17.17 ns)
                     std dev              899.2 ps   (550.2 ps .. 1.334 ns)
                     variance introduced by outliers: 76% (severely inflated)

benchmarking State/mtl.get
time                 650.3 ps   (645.7 ps .. 657.6 ps)
                     0.996 R²   (0.993 R² .. 0.999 R²)
                     mean                 694.8 ps   (674.7 ps .. 731.1 ps)
                     std dev              92.57 ps   (64.95 ps .. 141.9 ps)
                     variance introduced by outliers: 96% (severely inflated)

benchmarking State/ee.get
time                 65.99 ns   (65.59 ns .. 66.42 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
                     mean                 66.11 ns   (65.83 ns .. 66.43 ns)
                     std dev              1.007 ns   (786.0 ps .. 1.261 ns)
                     variance introduced by outliers: 18% (moderately inflated)

benchmarking Countdown Bench/freer.State
time                 370.9 μs   (368.9 μs .. 373.6 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
                     mean                 370.7 μs   (368.8 μs .. 372.7 μs)
                     std dev              6.905 μs   (5.943 μs .. 8.198 μs)
                     variance introduced by outliers: 10% (moderately inflated)

benchmarking Countdown Bench/freer.StateRW
time                 382.3 μs   (380.7 μs .. 384.0 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
                     mean                 382.5 μs   (381.3 μs .. 384.6 μs)
                     std dev              5.143 μs   (3.614 μs .. 8.425 μs)

benchmarking Countdown Bench/mtl.State
time                 9.998 μs   (9.934 μs .. 10.06 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
                     mean                 9.965 μs   (9.928 μs .. 10.03 μs)
                     std dev              158.7 ns   (114.4 ns .. 215.8 ns)
                     variance introduced by outliers: 13% (moderately inflated)

benchmarking Countdown Bench/ee.State
time                 2.053 ms   (2.039 ms .. 2.072 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
                     mean                 2.052 ms   (2.042 ms .. 2.067 ms)
                     std dev              40.56 μs   (29.43 μs .. 54.87 μs)

benchmarking Countdown+Except Bench/freer.ExcState
time                 385.3 μs   (377.4 μs .. 392.9 μs)
                     0.997 R²   (0.996 R² .. 0.999 R²)
                     mean                 370.6 μs   (367.2 μs .. 375.6 μs)
                     std dev              13.54 μs   (9.984 μs .. 17.96 μs)
                     variance introduced by outliers: 31% (moderately inflated)

benchmarking Countdown+Except Bench/mtl.ExceptState
time                 3.222 μs   (3.206 μs .. 3.243 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
                     mean                 3.264 μs   (3.247 μs .. 3.290 μs)
                     std dev              77.11 ns   (56.89 ns .. 114.2 ns)
                     variance introduced by outliers: 28% (moderately inflated)

benchmarking Countdown+Except Bench/ee.ExcState
time                 2.048 ms   (2.039 ms .. 2.059 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
                     mean                 2.035 ms   (2.027 ms .. 2.045 ms)
                     std dev              31.17 μs   (25.23 μs .. 44.28 μs)

benchmarking HTTP Simple DSL/freer
time                 80.38 ns   (80.06 ns .. 80.75 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
                     mean                 80.53 ns   (80.23 ns .. 80.92 ns)
                     std dev              1.182 ns   (942.9 ps .. 1.586 ns)
                     variance introduced by outliers: 17% (moderately inflated)

benchmarking HTTP Simple DSL/free
time                 59.78 ns   (59.55 ns .. 60.05 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
                     mean                 59.65 ns   (59.52 ns .. 59.84 ns)
                     std dev              528.4 ps   (446.9 ps .. 649.0 ps)

benchmarking HTTP Simple DSL/freerN
time                 91.40 μs   (91.02 μs .. 91.75 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
                     mean                 91.87 μs   (91.53 μs .. 92.40 μs)
                     std dev              1.455 μs   (1.094 μs .. 1.975 μs)
                     variance introduced by outliers: 10% (moderately inflated)

benchmarking HTTP Simple DSL/freeN
time                 28.62 ms   (28.13 ms .. 29.11 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
                     mean                 28.48 ms   (28.34 ms .. 28.65 ms)
                     std dev              325.8 μs   (239.5 μs .. 452.4 μs)

Benchmark core: FINISH