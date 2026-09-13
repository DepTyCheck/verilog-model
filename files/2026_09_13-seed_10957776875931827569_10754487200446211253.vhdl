-- Seed: 10957776875931827569,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity sejszfg is
  port (rizwqhwewy : buffer std_logic_vector(3 to 4); fmxrm : out integer; ekzzgd : inout integer);
end sejszfg;

architecture e of sejszfg is
  
begin
  -- Single-driven assignments
  ekzzgd <= 2#1_1_0#;
  fmxrm <= ekzzgd;
  
  -- Multi-driven assignments
  rizwqhwewy <= rizwqhwewy;
  rizwqhwewy <= "ZU";
  rizwqhwewy <= rizwqhwewy;
  rizwqhwewy <= "0Z";
end e;

entity neaa is
  port (cy : buffer integer; bwvw : buffer character; mkf : in bit);
end neaa;

library ieee;
use ieee.std_logic_1164.all;

architecture rgffrblfpj of neaa is
  signal vhhqdg : integer;
  signal s : integer;
  signal axg : std_logic_vector(3 to 4);
  signal fjfx : integer;
  signal umsulsxtq : std_logic_vector(3 to 4);
  signal h : integer;
  signal g : integer;
  signal ahofrv : std_logic_vector(3 to 4);
begin
  embsm : entity work.sejszfg
    port map (rizwqhwewy => ahofrv, fmxrm => g, ekzzgd => h);
  xtr : entity work.sejszfg
    port map (rizwqhwewy => umsulsxtq, fmxrm => fjfx, ekzzgd => cy);
  mwomd : entity work.sejszfg
    port map (rizwqhwewy => axg, fmxrm => s, ekzzgd => vhhqdg);
  
  -- Single-driven assignments
  bwvw <= bwvw;
  
  -- Multi-driven assignments
  axg <= ahofrv;
  ahofrv <= ('U', 'H');
end rgffrblfpj;



-- Seed after: 8898535451732682777,10754487200446211253
