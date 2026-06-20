-- Seed: 14002766885268926913,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity rns is
  port (dvad : out integer; alaew : buffer std_logic; w : out time_vector(4 downto 3));
end rns;

architecture ijyxoccavp of rns is
  
begin
  -- Single-driven assignments
  dvad <= 16#7#;
  
  -- Multi-driven assignments
  alaew <= 'L';
  alaew <= 'L';
  alaew <= 'X';
end ijyxoccavp;

entity otsav is
  port (jdneto : inout time);
end otsav;

library ieee;
use ieee.std_logic_1164.all;

architecture sbw of otsav is
  signal dmopt : time_vector(4 downto 3);
  signal cowpewwyx : integer;
  signal tomv : time_vector(4 downto 3);
  signal sh : integer;
  signal tqexgrajm : time_vector(4 downto 3);
  signal wyvzqmdux : std_logic;
  signal yrir : integer;
begin
  nclb : entity work.rns
    port map (dvad => yrir, alaew => wyvzqmdux, w => tqexgrajm);
  g : entity work.rns
    port map (dvad => sh, alaew => wyvzqmdux, w => tomv);
  ikxoiqihw : entity work.rns
    port map (dvad => cowpewwyx, alaew => wyvzqmdux, w => dmopt);
  
  -- Single-driven assignments
  jdneto <= 1 hr;
  
  -- Multi-driven assignments
  wyvzqmdux <= '1';
  wyvzqmdux <= '-';
  wyvzqmdux <= '-';
end sbw;



-- Seed after: 16512356403158357498,17924494779688682807
