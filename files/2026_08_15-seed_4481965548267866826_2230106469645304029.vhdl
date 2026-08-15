-- Seed: 4481965548267866826,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity ea is
  port (db : out std_logic_vector(4 downto 0));
end ea;

architecture pf of ea is
  
begin
  -- Multi-driven assignments
  db <= db;
  db <= db;
  db <= ('0', 'Z', '1', 'X', '0');
end pf;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (arg : inout time_vector(1 to 3); jpsuifbosz : linkage std_logic);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture fu of v is
  signal mq : std_logic_vector(4 downto 0);
begin
  idaza : entity work.ea
    port map (db => mq);
  vqlg : entity work.ea
    port map (db => mq);
  
  -- Single-driven assignments
  arg <= (2#1# us, 1 min, 3.3_0_3_1_3 us);
  
  -- Multi-driven assignments
  mq <= "-UHZX";
  mq <= mq;
end fu;

library ieee;
use ieee.std_logic_1164.all;

entity qndszil is
  port (knadv : out std_logic_vector(2 downto 3); qlw : inout std_logic_vector(4 downto 0));
end qndszil;

architecture row of qndszil is
  
begin
  -- Multi-driven assignments
  qlw <= ('W', '1', '-', 'Z', 'W');
  qlw <= qlw;
  qlw <= ('L', 'Z', 'W', 'X', 'Z');
  qlw <= qlw;
end row;

library ieee;
use ieee.std_logic_1164.all;

entity fpfbyvu is
  port (gczjot : inout std_logic_vector(0 to 2); m : buffer time);
end fpfbyvu;

library ieee;
use ieee.std_logic_1164.all;

architecture rovauvpt of fpfbyvu is
  signal ejhhy : std_logic_vector(4 downto 0);
  signal dtvjcl : std_logic;
  signal kjhgdrh : time_vector(1 to 3);
  signal aih : std_logic_vector(4 downto 0);
begin
  pvbnvkneoc : entity work.ea
    port map (db => aih);
  s : entity work.v
    port map (arg => kjhgdrh, jpsuifbosz => dtvjcl);
  nwouapesm : entity work.ea
    port map (db => ejhhy);
  
  -- Single-driven assignments
  m <= m;
end rovauvpt;



-- Seed after: 17968290554115864970,2230106469645304029
