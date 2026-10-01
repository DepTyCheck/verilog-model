-- Seed: 14657503479035481902,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity bwp is
  port (pcdtw : out real; btoflfu : buffer std_logic_vector(0 to 0));
end bwp;

architecture npogunzf of bwp is
  
begin
  -- Multi-driven assignments
  btoflfu <= "-";
  btoflfu <= btoflfu;
  btoflfu <= btoflfu;
end npogunzf;

entity jytixhczuf is
  port (d : in time);
end jytixhczuf;

library ieee;
use ieee.std_logic_1164.all;

architecture fgzihuwe of jytixhczuf is
  signal onup : std_logic_vector(0 to 0);
  signal ifvicvjkja : real;
  signal ahumhqckz : std_logic_vector(0 to 0);
  signal vhggxk : real;
  signal lexhygqz : std_logic_vector(0 to 0);
  signal c : real;
begin
  lj : entity work.bwp
    port map (pcdtw => c, btoflfu => lexhygqz);
  zymlqzizzr : entity work.bwp
    port map (pcdtw => vhggxk, btoflfu => ahumhqckz);
  qxzr : entity work.bwp
    port map (pcdtw => ifvicvjkja, btoflfu => onup);
  
  -- Multi-driven assignments
  onup <= ahumhqckz;
  onup <= (others => '-');
  lexhygqz <= "0";
  lexhygqz <= lexhygqz;
end fgzihuwe;

entity ibdjps is
  port (dxnk : inout real);
end ibdjps;

library ieee;
use ieee.std_logic_1164.all;

architecture zm of ibdjps is
  signal wtosnafo : time;
  signal fxaas : std_logic_vector(0 to 0);
  signal ttsramhnmg : real;
  signal eg : std_logic_vector(0 to 0);
  signal euibkzbykj : real;
begin
  ebalphnts : entity work.bwp
    port map (pcdtw => euibkzbykj, btoflfu => eg);
  go : entity work.bwp
    port map (pcdtw => ttsramhnmg, btoflfu => fxaas);
  n : entity work.jytixhczuf
    port map (d => wtosnafo);
  bhnavd : entity work.bwp
    port map (pcdtw => dxnk, btoflfu => eg);
end zm;



-- Seed after: 5220149726708360342,15025465285671019065
