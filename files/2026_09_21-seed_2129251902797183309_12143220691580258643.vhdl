-- Seed: 2129251902797183309,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity xavzb is
  port (koyargfq : buffer std_logic_vector(4 to 3); hq : linkage std_logic_vector(4 to 1));
end xavzb;

architecture hiyatyye of xavzb is
  
begin
  
end hiyatyye;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (vcwvsxdqsk : buffer std_logic; bcm : linkage integer; scsm : linkage integer);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture bwxwniwoa of w is
  signal jdtpeicb : std_logic_vector(4 to 3);
  signal fvhisbp : std_logic_vector(4 to 3);
  signal qkosyn : std_logic_vector(4 to 1);
begin
  rz : entity work.xavzb
    port map (koyargfq => qkosyn, hq => qkosyn);
  furrgx : entity work.xavzb
    port map (koyargfq => qkosyn, hq => fvhisbp);
  zo : entity work.xavzb
    port map (koyargfq => fvhisbp, hq => qkosyn);
  apit : entity work.xavzb
    port map (koyargfq => jdtpeicb, hq => qkosyn);
  
  -- Multi-driven assignments
  jdtpeicb <= "";
end bwxwniwoa;



-- Seed after: 16778275328714486711,12143220691580258643
