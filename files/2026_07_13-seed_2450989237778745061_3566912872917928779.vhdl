-- Seed: 2450989237778745061,3566912872917928779

use std.reflection.all;

entity uepgs is
  port (bqt : inout file_value_mirror; cjcs : out integer; xgkjbs : buffer integer);
end uepgs;

architecture thxzy of uepgs is
  
begin
  -- Single-driven assignments
  xgkjbs <= 1_2_4;
  cjcs <= xgkjbs;
end thxzy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pyorlv is
  port (zkwjzuzrjv : inout std_logic; wzdue : inout array_value_mirror; mnzyxx : out std_logic_vector(2 downto 3); sjj : out real);
end pyorlv;

use std.reflection.all;

architecture tsh of pyorlv is
  signal epozosho : integer;
  signal pvljswi : integer;
  shared variable zc : file_value_mirror;
  signal tkapz : integer;
  signal tr : integer;
  shared variable jce : file_value_mirror;
  signal qvv : integer;
  signal jzdeq : integer;
  shared variable bhztgpffz : file_value_mirror;
begin
  hnhkpqyu : entity work.uepgs
    port map (bqt => bhztgpffz, cjcs => jzdeq, xgkjbs => qvv);
  tpc : entity work.uepgs
    port map (bqt => jce, cjcs => tr, xgkjbs => tkapz);
  erpoyn : entity work.uepgs
    port map (bqt => zc, cjcs => pvljswi, xgkjbs => epozosho);
  
  -- Single-driven assignments
  sjj <= 2#0_1_1_1_0.1#;
  
  -- Multi-driven assignments
  zkwjzuzrjv <= '-';
  zkwjzuzrjv <= zkwjzuzrjv;
end tsh;



-- Seed after: 10351565459849119008,3566912872917928779
