-- Seed: 16831384803836070812,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity wfairvoszk is
  port (kaoycl : out std_logic_vector(2 downto 2); qpxv : in std_logic; ayfgl : in bit_vector(4 to 0); zvtokjdwyl : inout integer);
end wfairvoszk;

architecture y of wfairvoszk is
  
begin
  -- Single-driven assignments
  zvtokjdwyl <= 8#0452#;
  
  -- Multi-driven assignments
  kaoycl <= (others => 'H');
end y;

library ieee;
use ieee.std_logic_1164.all;

entity lnsaunwqt is
  port (odlxmpi : in std_logic);
end lnsaunwqt;

library ieee;
use ieee.std_logic_1164.all;

architecture tdhxrukww of lnsaunwqt is
  signal nxrafk : integer;
  signal whsqp : std_logic;
  signal vsq : integer;
  signal qzudmayis : bit_vector(4 to 0);
  signal gzijnr : std_logic_vector(2 downto 2);
begin
  jexdrzzd : entity work.wfairvoszk
    port map (kaoycl => gzijnr, qpxv => odlxmpi, ayfgl => qzudmayis, zvtokjdwyl => vsq);
  lohxf : entity work.wfairvoszk
    port map (kaoycl => gzijnr, qpxv => whsqp, ayfgl => qzudmayis, zvtokjdwyl => nxrafk);
  
  -- Single-driven assignments
  qzudmayis <= (others => '0');
  
  -- Multi-driven assignments
  gzijnr <= "1";
  gzijnr <= gzijnr;
  gzijnr <= gzijnr;
  gzijnr <= gzijnr;
end tdhxrukww;



-- Seed after: 12311481043513634389,1112937151005418631
