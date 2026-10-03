-- Seed: 11295084923322048340,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity kkw is
  port (nqpjg : in std_logic_vector(3 downto 2); uaqq : linkage std_logic_vector(0 downto 4); zyw : in bit);
end kkw;

architecture nwvbeaooq of kkw is
  
begin
  
end nwvbeaooq;

library ieee;
use ieee.std_logic_1164.all;

entity bfmh is
  port (vhpx : in std_logic; v : buffer real);
end bfmh;

library ieee;
use ieee.std_logic_1164.all;

architecture d of bfmh is
  signal yddhz : bit;
  signal zfva : std_logic_vector(0 downto 4);
  signal axxzmobquw : std_logic_vector(3 downto 2);
begin
  s : entity work.kkw
    port map (nqpjg => axxzmobquw, uaqq => zfva, zyw => yddhz);
  
  -- Single-driven assignments
  v <= 2#00100.11010#;
  yddhz <= yddhz;
  
  -- Multi-driven assignments
  axxzmobquw <= "W1";
  axxzmobquw <= axxzmobquw;
  axxzmobquw <= "U1";
  axxzmobquw <= "LZ";
end d;



-- Seed after: 11657735617952661751,6140041381800297705
