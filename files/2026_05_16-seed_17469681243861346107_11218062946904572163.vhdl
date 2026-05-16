-- Seed: 17469681243861346107,11218062946904572163

library ieee;
use ieee.std_logic_1164.all;

entity ui is
  port (yprdhcbrf : out std_logic; hfpeo : inout std_logic; ioilojn : in real; pvues : in time);
end ui;



architecture pqlbwrbzdu of ui is
  
begin
  
end pqlbwrbzdu;



entity bv is
  port (bqqtia : in real; ahavisig : inout integer);
end bv;

library ieee;
use ieee.std_logic_1164.all;

architecture yuguspwlzt of bv is
  signal b : time;
  signal trunmmoy : real;
  signal tlipy : std_logic;
  signal pjxuz : real;
  signal vv : time;
  signal gv : std_logic;
begin
  gmuagwd : entity work.ui
    port map (yprdhcbrf => gv, hfpeo => gv, ioilojn => bqqtia, pvues => vv);
  dzpxiaeybp : entity work.ui
    port map (yprdhcbrf => gv, hfpeo => gv, ioilojn => pjxuz, pvues => vv);
  ntob : entity work.ui
    port map (yprdhcbrf => gv, hfpeo => gv, ioilojn => bqqtia, pvues => vv);
  wcpekbhg : entity work.ui
    port map (yprdhcbrf => gv, hfpeo => tlipy, ioilojn => trunmmoy, pvues => b);
end yuguspwlzt;

library ieee;
use ieee.std_logic_1164.all;

entity dgc is
  port (iwnqc : buffer std_logic; b : in time);
end dgc;



architecture g of dgc is
  signal mip : integer;
  signal umju : real;
begin
  r : entity work.bv
    port map (bqqtia => umju, ahavisig => mip);
end g;



-- Seed after: 5247409216287551951,11218062946904572163
