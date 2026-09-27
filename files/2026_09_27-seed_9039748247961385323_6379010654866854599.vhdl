-- Seed: 9039748247961385323,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity vvtrigob is
  port (i : buffer integer; oikjhn : in integer; dbkmylauxi : inout std_logic; f : in real);
end vvtrigob;

architecture jabg of vvtrigob is
  
begin
  -- Single-driven assignments
  i <= oikjhn;
  
  -- Multi-driven assignments
  dbkmylauxi <= 'H';
  dbkmylauxi <= dbkmylauxi;
end jabg;

library ieee;
use ieee.std_logic_1164.all;

entity giqfvruv is
  port (gazxsfvpg : inout character; edti : inout std_logic; hstavjk : linkage time; emqsoht : inout integer);
end giqfvruv;

library ieee;
use ieee.std_logic_1164.all;

architecture zwqjxs of giqfvruv is
  signal kdryeb : std_logic;
  signal hygwe : real;
  signal imqaldm : std_logic;
  signal gkumxh : integer;
  signal kdlrbkz : real;
  signal pmifmqlljo : integer;
begin
  bsmwa : entity work.vvtrigob
    port map (i => emqsoht, oikjhn => pmifmqlljo, dbkmylauxi => edti, f => kdlrbkz);
  uvwbpw : entity work.vvtrigob
    port map (i => pmifmqlljo, oikjhn => gkumxh, dbkmylauxi => imqaldm, f => hygwe);
  r : entity work.vvtrigob
    port map (i => gkumxh, oikjhn => emqsoht, dbkmylauxi => kdryeb, f => hygwe);
  
  -- Single-driven assignments
  gazxsfvpg <= 'n';
  hygwe <= hygwe;
  kdlrbkz <= kdlrbkz;
  
  -- Multi-driven assignments
  edti <= edti;
  edti <= edti;
  edti <= edti;
  kdryeb <= edti;
end zwqjxs;



-- Seed after: 6239843259812126518,6379010654866854599
