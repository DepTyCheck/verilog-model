-- Seed: 9328400653813052147,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity syiwfi is
  port (sr : in bit_vector(1 downto 1); pfpzvdqtew : linkage std_logic_vector(1 downto 4));
end syiwfi;

architecture vkcqahny of syiwfi is
  
begin
  
end vkcqahny;

library ieee;
use ieee.std_logic_1164.all;

entity rlsi is
  port (cggg : linkage std_logic; ymhqlt : linkage std_logic_vector(3 downto 0));
end rlsi;

library ieee;
use ieee.std_logic_1164.all;

architecture nqzlqf of rlsi is
  signal psgkzs : bit_vector(1 downto 1);
  signal eyuww : std_logic_vector(1 downto 4);
  signal swsefalpb : bit_vector(1 downto 1);
begin
  xdvkfnne : entity work.syiwfi
    port map (sr => swsefalpb, pfpzvdqtew => eyuww);
  bhajskp : entity work.syiwfi
    port map (sr => psgkzs, pfpzvdqtew => eyuww);
  pbw : entity work.syiwfi
    port map (sr => psgkzs, pfpzvdqtew => eyuww);
  
  -- Single-driven assignments
  swsefalpb <= swsefalpb;
  psgkzs <= swsefalpb;
  
  -- Multi-driven assignments
  eyuww <= "";
  eyuww <= eyuww;
  eyuww <= eyuww;
  eyuww <= (others => '0');
end nqzlqf;



-- Seed after: 1896835873036926813,11481034001933599325
