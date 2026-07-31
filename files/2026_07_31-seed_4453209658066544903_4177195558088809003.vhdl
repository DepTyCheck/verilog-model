-- Seed: 4453209658066544903,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity pwccagi is
  port (hkug : out std_logic; hckmftljwn : buffer time);
end pwccagi;

architecture lf of pwccagi is
  
begin
  -- Single-driven assignments
  hckmftljwn <= 2 min;
  
  -- Multi-driven assignments
  hkug <= 'H';
  hkug <= 'W';
  hkug <= hkug;
  hkug <= 'U';
end lf;

library ieee;
use ieee.std_logic_1164.all;

entity qonm is
  port (gvc : buffer std_logic);
end qonm;

library ieee;
use ieee.std_logic_1164.all;

architecture cluxqji of qonm is
  signal ecnjxbblvb : time;
  signal usynjow : std_logic;
  signal zdhqtnqkh : time;
  signal qys : std_logic;
  signal aira : time;
  signal wrije : time;
begin
  h : entity work.pwccagi
    port map (hkug => gvc, hckmftljwn => wrije);
  smxagq : entity work.pwccagi
    port map (hkug => gvc, hckmftljwn => aira);
  k : entity work.pwccagi
    port map (hkug => qys, hckmftljwn => zdhqtnqkh);
  mnwbz : entity work.pwccagi
    port map (hkug => usynjow, hckmftljwn => ecnjxbblvb);
  
  -- Multi-driven assignments
  qys <= usynjow;
  gvc <= 'H';
end cluxqji;



-- Seed after: 15845541103157818416,4177195558088809003
