-- Seed: 2784966786999083351,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity wx is
  port (vupqj : in std_logic);
end wx;

architecture nbzdhc of wx is
  
begin
  
end nbzdhc;

entity pzhnfz is
  port (lpnfkma : out time; lk : out bit);
end pzhnfz;

library ieee;
use ieee.std_logic_1164.all;

architecture nnlf of pzhnfz is
  signal vr : std_logic;
begin
  vqfzds : entity work.wx
    port map (vupqj => vr);
end nnlf;

entity n is
  port (igokbuj : buffer integer);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture v of n is
  signal k : std_logic;
  signal g : bit;
  signal dyl : time;
begin
  hkmcgg : entity work.pzhnfz
    port map (lpnfkma => dyl, lk => g);
  wth : entity work.wx
    port map (vupqj => k);
  
  -- Single-driven assignments
  igokbuj <= igokbuj;
  
  -- Multi-driven assignments
  k <= k;
  k <= k;
  k <= k;
end v;



-- Seed after: 892613296757817869,2230106469645304029
