-- Seed: 3361517376405761919,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity tadp is
  port (ybpbgi : in std_logic; znxgin : in string(4 downto 5); fhxn : inout time);
end tadp;

architecture iv of tadp is
  
begin
  
end iv;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (mhw : in std_logic_vector(1 to 4); rvbjanj : buffer integer; vrzmsylp : linkage real; calvih : out severity_level);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture mocmlcld of h is
  signal chyuareb : time;
  signal kvidszhtgj : string(4 downto 5);
  signal vxzjjvrkcn : std_logic;
  signal nzvlxp : time;
  signal loa : time;
  signal smnhbj : string(4 downto 5);
  signal wdqw : std_logic;
begin
  rq : entity work.tadp
    port map (ybpbgi => wdqw, znxgin => smnhbj, fhxn => loa);
  cdpyfx : entity work.tadp
    port map (ybpbgi => wdqw, znxgin => smnhbj, fhxn => nzvlxp);
  pumm : entity work.tadp
    port map (ybpbgi => vxzjjvrkcn, znxgin => kvidszhtgj, fhxn => chyuareb);
  
  -- Single-driven assignments
  smnhbj <= "";
  
  -- Multi-driven assignments
  wdqw <= 'W';
  wdqw <= 'W';
  wdqw <= 'W';
  vxzjjvrkcn <= '-';
end mocmlcld;

library ieee;
use ieee.std_logic_1164.all;

entity znhao is
  port (coknqidlj : inout real; k : linkage integer; yfpqjb : out std_logic_vector(3 downto 0); gltqzpxwwj : in character);
end znhao;

library ieee;
use ieee.std_logic_1164.all;

architecture iqmqaknc of znhao is
  signal bkbsjzdo : severity_level;
  signal nbvkedws : real;
  signal nw : integer;
  signal s : severity_level;
  signal wtu : real;
  signal tvi : integer;
  signal inw : time;
  signal cixbkd : string(4 downto 5);
  signal uncykvz : std_logic;
  signal ovxpqvccw : severity_level;
  signal rwrysnb : real;
  signal xlxtaaecoo : integer;
begin
  kofuicfw : entity work.h
    port map (mhw => yfpqjb, rvbjanj => xlxtaaecoo, vrzmsylp => rwrysnb, calvih => ovxpqvccw);
  mrofxqpc : entity work.tadp
    port map (ybpbgi => uncykvz, znxgin => cixbkd, fhxn => inw);
  ktkmiy : entity work.h
    port map (mhw => yfpqjb, rvbjanj => tvi, vrzmsylp => wtu, calvih => s);
  imolh : entity work.h
    port map (mhw => yfpqjb, rvbjanj => nw, vrzmsylp => nbvkedws, calvih => bkbsjzdo);
  
  -- Single-driven assignments
  coknqidlj <= 3_2_3_0_0.2;
  cixbkd <= "";
end iqmqaknc;



-- Seed after: 13252968331896391615,14629254427735353553
