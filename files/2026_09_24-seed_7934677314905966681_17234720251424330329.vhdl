-- Seed: 7934677314905966681,17234720251424330329

entity daozb is
  port (bwtefjqjs : in bit);
end daozb;

architecture auddvv of daozb is
  
begin
  
end auddvv;

entity epylilkzaf is
  port (xct : buffer real; egicfgav : out time; jciblesmnk : linkage real; zqwpbeuczk : linkage real_vector(4 downto 2));
end epylilkzaf;

architecture slcfuest of epylilkzaf is
  signal qrwsm : bit;
begin
  vq : entity work.daozb
    port map (bwtefjqjs => qrwsm);
end slcfuest;

library ieee;
use ieee.std_logic_1164.all;

entity yqaqb is
  port (ezjczhrxuf : inout boolean; em : out std_logic; xxsjsd : linkage integer; uudyk : buffer bit_vector(0 to 2));
end yqaqb;

architecture ufagw of yqaqb is
  signal u : bit;
  signal hiupym : bit;
  signal qxo : bit;
  signal lebtp : real_vector(4 downto 2);
  signal gx : real;
  signal m : time;
  signal b : real;
begin
  snnizq : entity work.epylilkzaf
    port map (xct => b, egicfgav => m, jciblesmnk => gx, zqwpbeuczk => lebtp);
  bdie : entity work.daozb
    port map (bwtefjqjs => qxo);
  fhnf : entity work.daozb
    port map (bwtefjqjs => hiupym);
  tocxturqje : entity work.daozb
    port map (bwtefjqjs => u);
  
  -- Single-driven assignments
  uudyk <= uudyk;
  u <= qxo;
  hiupym <= '0';
  qxo <= '0';
  ezjczhrxuf <= ezjczhrxuf;
  
  -- Multi-driven assignments
  em <= 'L';
  em <= em;
  em <= 'L';
end ufagw;



-- Seed after: 12439552265698422437,17234720251424330329
