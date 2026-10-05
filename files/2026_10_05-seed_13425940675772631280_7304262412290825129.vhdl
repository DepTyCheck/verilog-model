-- Seed: 13425940675772631280,7304262412290825129

entity fcrchuomps is
  port (fnw : out boolean_vector(0 downto 3); ymdhmoh : out time);
end fcrchuomps;

architecture hb of fcrchuomps is
  
begin
  -- Single-driven assignments
  ymdhmoh <= 2#0.01101# ps;
  fnw <= (others => TRUE);
end hb;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (s : buffer std_logic_vector(4 to 3); q : buffer real; cpokqjuvea : linkage time);
end a;

architecture zmbxbokf of a is
  signal flvc : time;
  signal vzu : boolean_vector(0 downto 3);
begin
  kolbvwouu : entity work.fcrchuomps
    port map (fnw => vzu, ymdhmoh => flvc);
  
  -- Single-driven assignments
  q <= q;
  
  -- Multi-driven assignments
  s <= s;
  s <= s;
  s <= "";
  s <= s;
end zmbxbokf;



-- Seed after: 9146425886983426700,7304262412290825129
