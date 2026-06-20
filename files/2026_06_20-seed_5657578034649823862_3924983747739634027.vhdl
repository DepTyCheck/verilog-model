-- Seed: 5657578034649823862,3924983747739634027

entity hjtchwt is
  port (tjzww : inout string(5 downto 1));
end hjtchwt;

architecture w of hjtchwt is
  
begin
  -- Single-driven assignments
  tjzww <= ('w', 'a', 'j', 'h', 'f');
end w;

library ieee;
use ieee.std_logic_1164.all;

entity fdan is
  port (cbnccvfi : linkage real; zbcv : linkage character; snk : in time; icjsm : buffer std_logic);
end fdan;

architecture aaganq of fdan is
  
begin
  -- Multi-driven assignments
  icjsm <= 'X';
end aaganq;

library ieee;
use ieee.std_logic_1164.all;

entity zirylvjevd is
  port (uhfpdjvczc : buffer boolean; pa : linkage std_logic);
end zirylvjevd;

library ieee;
use ieee.std_logic_1164.all;

architecture x of zirylvjevd is
  signal jguz : std_logic;
  signal kxlsb : time;
  signal vslrgbab : character;
  signal rvccdwjnjd : real;
begin
  zfgcvr : entity work.fdan
    port map (cbnccvfi => rvccdwjnjd, zbcv => vslrgbab, snk => kxlsb, icjsm => jguz);
  
  -- Single-driven assignments
  uhfpdjvczc <= FALSE;
  kxlsb <= 02201.1_2_3 ms;
  
  -- Multi-driven assignments
  jguz <= 'U';
  jguz <= '-';
  jguz <= '0';
  jguz <= 'H';
end x;



-- Seed after: 8180690958570497368,3924983747739634027
