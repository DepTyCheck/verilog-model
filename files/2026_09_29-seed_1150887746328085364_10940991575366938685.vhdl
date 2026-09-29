-- Seed: 1150887746328085364,10940991575366938685

entity sqtckplnhd is
  port (otexdkwkz : inout integer; cdfgir : buffer time; ijilnwspmd : linkage time);
end sqtckplnhd;

architecture m of sqtckplnhd is
  
begin
  -- Single-driven assignments
  cdfgir <= 8#3# fs;
  otexdkwkz <= otexdkwkz;
end m;

library ieee;
use ieee.std_logic_1164.all;

entity tnjasd is
  port (kyvoupl : out integer; p : linkage boolean_vector(3 to 3); mvtxnliyu : out std_logic; jyviqhc : in std_logic);
end tnjasd;

architecture hjhbvfk of tnjasd is
  signal wqpzyod : time;
  signal yvg : time;
  signal eesxspufn : time;
  signal dsqeqozs : time;
  signal oxst : integer;
  signal tfdn : time;
  signal owepy : time;
  signal rwdsa : integer;
  signal kahcr : time;
  signal gqevp : time;
  signal tq : integer;
begin
  nynmf : entity work.sqtckplnhd
    port map (otexdkwkz => tq, cdfgir => gqevp, ijilnwspmd => kahcr);
  oqmxhqae : entity work.sqtckplnhd
    port map (otexdkwkz => rwdsa, cdfgir => owepy, ijilnwspmd => tfdn);
  pudx : entity work.sqtckplnhd
    port map (otexdkwkz => oxst, cdfgir => dsqeqozs, ijilnwspmd => eesxspufn);
  uahkbim : entity work.sqtckplnhd
    port map (otexdkwkz => kyvoupl, cdfgir => yvg, ijilnwspmd => wqpzyod);
  
  -- Multi-driven assignments
  mvtxnliyu <= mvtxnliyu;
end hjhbvfk;



-- Seed after: 14084475562253622965,10940991575366938685
