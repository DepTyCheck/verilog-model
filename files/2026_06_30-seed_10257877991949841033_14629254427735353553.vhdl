-- Seed: 10257877991949841033,14629254427735353553

entity pxdsinbcw is
  port (mbaddv : out time);
end pxdsinbcw;

architecture cu of pxdsinbcw is
  
begin
  -- Single-driven assignments
  mbaddv <= 1_3.3_3_4_3_2 ns;
end cu;

library ieee;
use ieee.std_logic_1164.all;

entity ibbxons is
  port (nrx : inout std_logic; gbotaalqv : buffer character);
end ibbxons;

architecture thkundg of ibbxons is
  signal axlidksak : time;
  signal ao : time;
  signal e : time;
begin
  idhe : entity work.pxdsinbcw
    port map (mbaddv => e);
  yqpctsrwwt : entity work.pxdsinbcw
    port map (mbaddv => ao);
  tgxbpoas : entity work.pxdsinbcw
    port map (mbaddv => axlidksak);
  
  -- Multi-driven assignments
  nrx <= '-';
  nrx <= 'X';
  nrx <= 'X';
end thkundg;



-- Seed after: 750485502686075292,14629254427735353553
