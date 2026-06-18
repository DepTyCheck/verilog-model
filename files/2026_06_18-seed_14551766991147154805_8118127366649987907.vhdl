-- Seed: 14551766991147154805,8118127366649987907

entity rjqy is
  port (vyqvnevfg : in time; oxbvzwicv : buffer time);
end rjqy;

architecture vwo of rjqy is
  
begin
  -- Single-driven assignments
  oxbvzwicv <= 2#0_0_0.001# us;
end vwo;

library ieee;
use ieee.std_logic_1164.all;

entity inkpdcy is
  port (ctreu : inout std_logic_vector(0 to 2));
end inkpdcy;

architecture ymfzlcek of inkpdcy is
  signal edhz : time;
  signal pv : time;
begin
  mm : entity work.rjqy
    port map (vyqvnevfg => pv, oxbvzwicv => edhz);
  
  -- Single-driven assignments
  pv <= 8#50666# us;
end ymfzlcek;

entity eilzfea is
  port (br : out severity_level; lokm : in integer; pufj : linkage real; viq : in severity_level);
end eilzfea;

library ieee;
use ieee.std_logic_1164.all;

architecture vudk of eilzfea is
  signal flniaf : std_logic_vector(0 to 2);
begin
  hti : entity work.inkpdcy
    port map (ctreu => flniaf);
  
  -- Single-driven assignments
  br <= WARNING;
  
  -- Multi-driven assignments
  flniaf <= ('1', 'H', '-');
  flniaf <= ('Z', '-', 'X');
  flniaf <= "-L0";
end vudk;

library ieee;
use ieee.std_logic_1164.all;

entity uixk is
  port (umcqkdgh : buffer integer; eswamkt : buffer character; ml : inout std_logic_vector(0 to 1));
end uixk;

architecture bmvnozinvs of uixk is
  
begin
  -- Single-driven assignments
  eswamkt <= 'n';
  
  -- Multi-driven assignments
  ml <= "U0";
end bmvnozinvs;



-- Seed after: 8130875909959585098,8118127366649987907
