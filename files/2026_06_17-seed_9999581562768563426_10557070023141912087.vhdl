-- Seed: 9999581562768563426,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity zvktthkmn is
  port (qaqy : linkage time; pdjdkticjd : buffer integer; ea : buffer std_logic);
end zvktthkmn;

architecture jpkbljovz of zvktthkmn is
  
begin
  -- Multi-driven assignments
  ea <= 'Z';
  ea <= 'L';
  ea <= 'W';
  ea <= '0';
end jpkbljovz;

library ieee;
use ieee.std_logic_1164.all;

entity qlmgvrkets is
  port (mssncczvg : out std_logic; ofhumvaoh : buffer integer_vector(1 to 3));
end qlmgvrkets;

architecture wflih of qlmgvrkets is
  
begin
  -- Single-driven assignments
  ofhumvaoh <= (2_4, 8#6_6_6_3_4#, 4_1_3_1);
  
  -- Multi-driven assignments
  mssncczvg <= '0';
  mssncczvg <= 'W';
end wflih;

library ieee;
use ieee.std_logic_1164.all;

entity zm is
  port (tqdoewoayr : out std_logic; y : linkage integer; abcqzmgnn : buffer severity_level; dwqlqifzk : buffer boolean_vector(0 to 1));
end zm;

architecture jyfdnkapj of zm is
  
begin
  -- Single-driven assignments
  dwqlqifzk <= (TRUE, TRUE);
  abcqzmgnn <= WARNING;
  
  -- Multi-driven assignments
  tqdoewoayr <= '1';
  tqdoewoayr <= '1';
  tqdoewoayr <= '0';
end jyfdnkapj;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (nxljioemwb : inout real; mkz : out std_logic_vector(4 downto 4); focmfsqls : buffer integer);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture kgjpw of x is
  signal qm : std_logic;
  signal yx : integer;
  signal fnonn : time;
  signal ttnnnvpq : integer;
  signal bef : time;
  signal fhmlbvm : integer;
  signal kgwezfoi : time;
  signal qez : integer_vector(1 to 3);
  signal yjgduxhe : std_logic;
begin
  mso : entity work.qlmgvrkets
    port map (mssncczvg => yjgduxhe, ofhumvaoh => qez);
  hmymwflwi : entity work.zvktthkmn
    port map (qaqy => kgwezfoi, pdjdkticjd => fhmlbvm, ea => yjgduxhe);
  lxfkhvmk : entity work.zvktthkmn
    port map (qaqy => bef, pdjdkticjd => ttnnnvpq, ea => yjgduxhe);
  jed : entity work.zvktthkmn
    port map (qaqy => fnonn, pdjdkticjd => yx, ea => qm);
  
  -- Single-driven assignments
  focmfsqls <= 01;
  nxljioemwb <= 2.4_2_3_3;
  
  -- Multi-driven assignments
  mkz <= "Z";
  yjgduxhe <= 'U';
  yjgduxhe <= 'Z';
end kgjpw;



-- Seed after: 15806793534784459840,10557070023141912087
