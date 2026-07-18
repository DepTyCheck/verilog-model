-- Seed: 9380596074822069207,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity ri is
  port (cxwmao : linkage std_logic_vector(3 downto 4); vfmsem : inout string(1 to 3); aptx : in time);
end ri;

architecture y of ri is
  
begin
  -- Single-driven assignments
  vfmsem <= vfmsem;
end y;

entity b is
  port (bqjx : inout time; ht : in time);
end b;

architecture nnjjt of b is
  
begin
  -- Single-driven assignments
  bqjx <= 3_0_3_0_3 us;
end nnjjt;

library ieee;
use ieee.std_logic_1164.all;

entity vzlwr is
  port (af : linkage std_logic_vector(4 to 2));
end vzlwr;

architecture trksgfay of vzlwr is
  signal ijxdy : string(1 to 3);
  signal vge : time;
  signal vjjxvvyomz : string(1 to 3);
  signal eqhspnd : time;
  signal yp : time;
  signal ya : time;
  signal ivvuglz : string(1 to 3);
begin
  zuuayaqzab : entity work.ri
    port map (cxwmao => af, vfmsem => ivvuglz, aptx => ya);
  wnem : entity work.b
    port map (bqjx => yp, ht => eqhspnd);
  mgbfzumhc : entity work.ri
    port map (cxwmao => af, vfmsem => vjjxvvyomz, aptx => vge);
  bgyoio : entity work.ri
    port map (cxwmao => af, vfmsem => ijxdy, aptx => ya);
end trksgfay;



-- Seed after: 10084664751632475167,1112937151005418631
