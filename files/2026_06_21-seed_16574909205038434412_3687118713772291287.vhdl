-- Seed: 16574909205038434412,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity zh is
  port (jylaxhdc : out std_logic; aqspg : buffer std_logic; pnrhgg : inout integer; id : buffer string(5 downto 1));
end zh;

architecture xee of zh is
  
begin
  -- Single-driven assignments
  id <= ('h', 'u', 'g', 's', 'm');
  
  -- Multi-driven assignments
  aqspg <= 'W';
  aqspg <= '-';
  aqspg <= 'H';
end xee;

entity vxkjggepik is
  port (yshhvut : inout integer; nefw : linkage character; lhds : buffer integer; dfer : linkage character);
end vxkjggepik;

library ieee;
use ieee.std_logic_1164.all;

architecture ukxbkvpbql of vxkjggepik is
  signal klmy : string(5 downto 1);
  signal ylddnfhvw : integer;
  signal h : std_logic;
  signal krz : string(5 downto 1);
  signal pnokam : integer;
  signal ktw : std_logic;
  signal zq : string(5 downto 1);
  signal jdxirfdx : std_logic;
  signal jzzvps : std_logic;
begin
  c : entity work.zh
    port map (jylaxhdc => jzzvps, aqspg => jdxirfdx, pnrhgg => lhds, id => zq);
  hvxe : entity work.zh
    port map (jylaxhdc => ktw, aqspg => jzzvps, pnrhgg => pnokam, id => krz);
  sre : entity work.zh
    port map (jylaxhdc => jzzvps, aqspg => h, pnrhgg => ylddnfhvw, id => klmy);
  
  -- Single-driven assignments
  yshhvut <= 16#A040#;
  
  -- Multi-driven assignments
  jzzvps <= '-';
  jdxirfdx <= 'W';
end ukxbkvpbql;



-- Seed after: 15950536265551558585,3687118713772291287
