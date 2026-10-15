-- Seed: 9877952560317806913,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity kjidvxhux is
  port (wlkihljxj : buffer integer; ymkluq : inout std_logic; tbxe : inout std_logic; meywcc : in time);
end kjidvxhux;

architecture ngcfvo of kjidvxhux is
  
begin
  -- Single-driven assignments
  wlkihljxj <= 4_2_4_1_4;
  
  -- Multi-driven assignments
  ymkluq <= 'H';
  ymkluq <= 'U';
  ymkluq <= 'L';
  ymkluq <= tbxe;
end ngcfvo;

entity azq is
  port (h : out integer);
end azq;

library ieee;
use ieee.std_logic_1164.all;

architecture qdep of azq is
  signal nvl : integer;
  signal ztbold : time;
  signal kaqbryirb : std_logic;
  signal hyhj : time;
  signal j : std_logic;
  signal yyv : integer;
  signal a : time;
  signal pwk : std_logic;
  signal ex : integer;
begin
  sxlylo : entity work.kjidvxhux
    port map (wlkihljxj => ex, ymkluq => pwk, tbxe => pwk, meywcc => a);
  tij : entity work.kjidvxhux
    port map (wlkihljxj => yyv, ymkluq => j, tbxe => pwk, meywcc => hyhj);
  tllcmkv : entity work.kjidvxhux
    port map (wlkihljxj => h, ymkluq => kaqbryirb, tbxe => pwk, meywcc => ztbold);
  tsqg : entity work.kjidvxhux
    port map (wlkihljxj => nvl, ymkluq => pwk, tbxe => pwk, meywcc => a);
  
  -- Single-driven assignments
  hyhj <= 0 sec;
  
  -- Multi-driven assignments
  pwk <= 'Z';
  pwk <= 'L';
  kaqbryirb <= pwk;
  pwk <= 'X';
end qdep;



-- Seed after: 13301265365045157334,6377581586043967497
