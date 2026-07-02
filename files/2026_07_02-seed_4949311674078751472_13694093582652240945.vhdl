-- Seed: 4949311674078751472,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity bba is
  port (bmpumgpfs : in real; detob : linkage std_logic; dybsv : out time; rlaapn : buffer std_logic_vector(1 to 2));
end bba;

architecture u of bba is
  
begin
  -- Single-driven assignments
  dybsv <= 2 ms;
  
  -- Multi-driven assignments
  rlaapn <= "0Z";
  rlaapn <= ('0', 'X');
  rlaapn <= "ZW";
  rlaapn <= ('H', 'H');
end u;

library ieee;
use ieee.std_logic_1164.all;

entity ikiitkhhbq is
  port (koj : out std_logic_vector(4 to 4));
end ikiitkhhbq;

library ieee;
use ieee.std_logic_1164.all;

architecture ml of ikiitkhhbq is
  signal qedn : std_logic_vector(1 to 2);
  signal naafszxqci : time;
  signal oxsuzvcpzj : real;
  signal ibadbh : std_logic_vector(1 to 2);
  signal ofqlmr : time;
  signal zcwy : std_logic;
  signal fzjqvera : real;
begin
  o : entity work.bba
    port map (bmpumgpfs => fzjqvera, detob => zcwy, dybsv => ofqlmr, rlaapn => ibadbh);
  rmwslk : entity work.bba
    port map (bmpumgpfs => oxsuzvcpzj, detob => zcwy, dybsv => naafszxqci, rlaapn => qedn);
  
  -- Single-driven assignments
  fzjqvera <= 0_1_3.3_2_0;
end ml;



-- Seed after: 12962996961129424008,13694093582652240945
