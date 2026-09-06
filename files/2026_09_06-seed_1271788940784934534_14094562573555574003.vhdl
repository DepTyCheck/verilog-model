-- Seed: 1271788940784934534,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity uesqymnko is
  port (tqtonzsadm : in time; vrhxbhpf : buffer std_logic; zaea : buffer integer_vector(4 to 1));
end uesqymnko;

architecture nthmwi of uesqymnko is
  
begin
  -- Single-driven assignments
  zaea <= (others => 0);
end nthmwi;

entity boktzxbb is
  port (jri : inout real);
end boktzxbb;

library ieee;
use ieee.std_logic_1164.all;

architecture d of boktzxbb is
  signal qala : integer_vector(4 to 1);
  signal wno : std_logic;
  signal ui : time;
  signal wxjgvi : integer_vector(4 to 1);
  signal oytx : std_logic;
  signal bh : time;
begin
  h : entity work.uesqymnko
    port map (tqtonzsadm => bh, vrhxbhpf => oytx, zaea => wxjgvi);
  zrrnii : entity work.uesqymnko
    port map (tqtonzsadm => ui, vrhxbhpf => wno, zaea => qala);
  
  -- Multi-driven assignments
  oytx <= 'Z';
  oytx <= 'H';
  wno <= '0';
end d;



-- Seed after: 16877206842807077952,14094562573555574003
