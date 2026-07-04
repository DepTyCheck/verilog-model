-- Seed: 2165612807002687711,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity drmnjct is
  port (xvo : out integer; hhe : out integer; hwpwqvt : out std_logic; xmrgfu : inout integer_value_mirror);
end drmnjct;

architecture ucagx of drmnjct is
  
begin
  -- Single-driven assignments
  hhe <= 8#6_5_7_2_7#;
  xvo <= hhe;
  
  -- Multi-driven assignments
  hwpwqvt <= hwpwqvt;
  hwpwqvt <= '-';
end ucagx;

library ieee;
use ieee.std_logic_1164.all;

entity lu is
  port (lwyfzk : out std_logic_vector(2 to 4); kz : inout integer);
end lu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture beqr of lu is
  shared variable vprfh : integer_value_mirror;
  signal pbrr : std_logic;
  signal ifxhbkv : integer;
begin
  cgintruol : entity work.drmnjct
    port map (xvo => kz, hhe => ifxhbkv, hwpwqvt => pbrr, xmrgfu => vprfh);
end beqr;



-- Seed after: 3813546031331360593,6290177331721581829
