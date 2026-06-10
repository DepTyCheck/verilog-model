-- Seed: 11235480336451500240,5415160250146859793



entity bjnkb is
  port (rcque : buffer real; wbbikcyxv : out integer_vector(4 downto 3));
end bjnkb;



architecture elsjmzw of bjnkb is
  
begin
  
end elsjmzw;

library ieee;
use ieee.std_logic_1164.all;

entity wni is
  port (zztsepal : out integer; kow : inout std_logic);
end wni;



architecture qqkblbfm of wni is
  signal imaqbn : integer_vector(4 downto 3);
  signal zyxfibars : real;
  signal fmni : integer_vector(4 downto 3);
  signal jabop : real;
begin
  e : entity work.bjnkb
    port map (rcque => jabop, wbbikcyxv => fmni);
  rigym : entity work.bjnkb
    port map (rcque => zyxfibars, wbbikcyxv => imaqbn);
end qqkblbfm;



entity zrgimcw is
  port (fb : linkage time; kwdhxjhai : in integer; zmvm : out integer);
end zrgimcw;

library ieee;
use ieee.std_logic_1164.all;

architecture gdnv of zrgimcw is
  signal nhctemn : integer_vector(4 downto 3);
  signal cyjaqx : real;
  signal qedknz : integer_vector(4 downto 3);
  signal nbztj : real;
  signal sxaudfvcig : integer_vector(4 downto 3);
  signal czi : real;
  signal bxzfepzyc : std_logic;
begin
  lje : entity work.wni
    port map (zztsepal => zmvm, kow => bxzfepzyc);
  xy : entity work.bjnkb
    port map (rcque => czi, wbbikcyxv => sxaudfvcig);
  kneka : entity work.bjnkb
    port map (rcque => nbztj, wbbikcyxv => qedknz);
  no : entity work.bjnkb
    port map (rcque => cyjaqx, wbbikcyxv => nhctemn);
end gdnv;



-- Seed after: 16688633596941965227,5415160250146859793
