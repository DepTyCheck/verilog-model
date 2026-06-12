-- Seed: 7231509834404717368,9125939553767483053



entity xv is
  port (ecg : out character; ktew : inout bit_vector(2 downto 4));
end xv;



architecture noyep of xv is
  
begin
  
end noyep;

library ieee;
use ieee.std_logic_1164.all;

entity rbk is
  port (sohdslyq : out std_logic_vector(4 to 1); xortpjpi : in std_logic_vector(3 to 4));
end rbk;



architecture zrc of rbk is
  signal gppnxelmfi : bit_vector(2 downto 4);
  signal sau : character;
  signal jatemtifq : bit_vector(2 downto 4);
  signal mmywatqc : character;
  signal kkptbbqj : bit_vector(2 downto 4);
  signal sldph : character;
  signal bh : bit_vector(2 downto 4);
  signal ajbw : character;
begin
  wbci : entity work.xv
    port map (ecg => ajbw, ktew => bh);
  rbtihj : entity work.xv
    port map (ecg => sldph, ktew => kkptbbqj);
  xdduw : entity work.xv
    port map (ecg => mmywatqc, ktew => jatemtifq);
  pkkgqxvz : entity work.xv
    port map (ecg => sau, ktew => gppnxelmfi);
end zrc;



-- Seed after: 7493316575913550805,9125939553767483053
