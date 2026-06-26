-- Seed: 11251865461183747159,12011142928354116943

entity pdtrmbgpth is
  port (gvnnggnyvr : buffer time; mnrsmh : out character);
end pdtrmbgpth;

architecture tvan of pdtrmbgpth is
  
begin
  -- Single-driven assignments
  mnrsmh <= 'j';
  gvnnggnyvr <= 1 hr;
end tvan;

library ieee;
use ieee.std_logic_1164.all;

entity laqqdmwifx is
  port (dsjz : buffer std_logic; ko : linkage std_logic_vector(2 to 4); lsounkxqpo : out std_logic);
end laqqdmwifx;

architecture rtbr of laqqdmwifx is
  signal pdqqo : character;
  signal bnjmxwieg : time;
  signal eb : character;
  signal pqvuphi : time;
  signal umq : character;
  signal ususdmo : time;
begin
  b : entity work.pdtrmbgpth
    port map (gvnnggnyvr => ususdmo, mnrsmh => umq);
  bmbsyxqba : entity work.pdtrmbgpth
    port map (gvnnggnyvr => pqvuphi, mnrsmh => eb);
  dgxt : entity work.pdtrmbgpth
    port map (gvnnggnyvr => bnjmxwieg, mnrsmh => pdqqo);
  
  -- Multi-driven assignments
  lsounkxqpo <= 'H';
  dsjz <= 'W';
  lsounkxqpo <= 'H';
end rtbr;



-- Seed after: 9084182486325662198,12011142928354116943
