-- Seed: 14084475562253622965,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity kasdh is
  port (d : buffer std_logic; xcojkdxj : linkage std_logic_vector(0 downto 3); fs : out std_logic; lmcdilfu : buffer integer_vector(4 downto 0));
end kasdh;

architecture jhrhwsfjoy of kasdh is
  
begin
  -- Single-driven assignments
  lmcdilfu <= lmcdilfu;
end jhrhwsfjoy;

entity sdizawvs is
  port (m : out real);
end sdizawvs;

library ieee;
use ieee.std_logic_1164.all;

architecture nnzebry of sdizawvs is
  signal f : integer_vector(4 downto 0);
  signal dqwrnuta : std_logic;
  signal gzvfav : std_logic_vector(0 downto 3);
  signal z : integer_vector(4 downto 0);
  signal qtm : integer_vector(4 downto 0);
  signal djjhdmgad : integer_vector(4 downto 0);
  signal k : std_logic;
  signal ukixktu : std_logic_vector(0 downto 3);
  signal se : std_logic;
begin
  xnhoctu : entity work.kasdh
    port map (d => se, xcojkdxj => ukixktu, fs => k, lmcdilfu => djjhdmgad);
  xws : entity work.kasdh
    port map (d => se, xcojkdxj => ukixktu, fs => k, lmcdilfu => qtm);
  luor : entity work.kasdh
    port map (d => k, xcojkdxj => ukixktu, fs => se, lmcdilfu => z);
  xbx : entity work.kasdh
    port map (d => se, xcojkdxj => gzvfav, fs => dqwrnuta, lmcdilfu => f);
  
  -- Multi-driven assignments
  se <= se;
  dqwrnuta <= k;
  se <= 'X';
  k <= '1';
end nnzebry;



-- Seed after: 8828785847285699989,10940991575366938685
