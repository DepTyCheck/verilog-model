-- Seed: 1205638688094359697,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity vcbnhqa is
  port (ejnxlnurmg : buffer std_logic; uysppqel : linkage std_logic; rhbn : linkage bit);
end vcbnhqa;

architecture swotfsgmza of vcbnhqa is
  
begin
  -- Multi-driven assignments
  ejnxlnurmg <= '-';
  ejnxlnurmg <= 'X';
  ejnxlnurmg <= 'L';
  ejnxlnurmg <= 'Z';
end swotfsgmza;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (iavqcrp : buffer real; rcbz : buffer std_logic);
end y;

architecture vcwrvwb of y is
  
begin
  -- Multi-driven assignments
  rcbz <= '-';
end vcwrvwb;

entity vwoi is
  port (oi : in integer_vector(4 downto 4); c : out time);
end vwoi;

library ieee;
use ieee.std_logic_1164.all;

architecture jucmwcj of vwoi is
  signal amzccriau : bit;
  signal cj : std_logic;
  signal kuo : real;
  signal pkoskmb : real;
  signal bmdypwo : bit;
  signal nkwapfrc : std_logic;
  signal spvb : std_logic;
begin
  csni : entity work.vcbnhqa
    port map (ejnxlnurmg => spvb, uysppqel => nkwapfrc, rhbn => bmdypwo);
  kmiblpkfau : entity work.y
    port map (iavqcrp => pkoskmb, rcbz => nkwapfrc);
  xdy : entity work.y
    port map (iavqcrp => kuo, rcbz => nkwapfrc);
  zgapxyy : entity work.vcbnhqa
    port map (ejnxlnurmg => cj, uysppqel => spvb, rhbn => amzccriau);
  
  -- Single-driven assignments
  c <= 4_1_3_2_3 ns;
  
  -- Multi-driven assignments
  cj <= 'H';
  spvb <= '0';
end jucmwcj;



-- Seed after: 11765284311751245014,6882842853887419669
