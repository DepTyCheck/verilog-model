-- Seed: 4129693493784710725,7311216359267151659

entity catocimmj is
  port (rpwtmvlsc : out boolean_vector(0 to 1); teau : in integer; ghezmchb : linkage integer);
end catocimmj;

architecture aabn of catocimmj is
  
begin
  -- Single-driven assignments
  rpwtmvlsc <= (FALSE, TRUE);
end aabn;

entity plgtho is
  port (zkks : in bit);
end plgtho;

architecture krvghmvuix of plgtho is
  signal mzgxbknc : integer;
  signal pditpdotnn : integer;
  signal qfxuo : boolean_vector(0 to 1);
  signal hazws : integer;
  signal dskvi : boolean_vector(0 to 1);
  signal apfjqvbi : integer;
  signal brxzjja : integer;
  signal bq : boolean_vector(0 to 1);
begin
  ylqgis : entity work.catocimmj
    port map (rpwtmvlsc => bq, teau => brxzjja, ghezmchb => apfjqvbi);
  f : entity work.catocimmj
    port map (rpwtmvlsc => dskvi, teau => brxzjja, ghezmchb => hazws);
  ggznk : entity work.catocimmj
    port map (rpwtmvlsc => qfxuo, teau => pditpdotnn, ghezmchb => mzgxbknc);
  
  -- Single-driven assignments
  pditpdotnn <= 4;
end krvghmvuix;

library ieee;
use ieee.std_logic_1164.all;

entity wottvwgl is
  port (arenfzpah : buffer std_logic_vector(2 to 4); albmyxhzv : buffer std_logic_vector(1 downto 1));
end wottvwgl;

architecture ddn of wottvwgl is
  signal xhbsdjq : integer;
  signal euzy : integer;
  signal kwj : boolean_vector(0 to 1);
begin
  ekckdyqc : entity work.catocimmj
    port map (rpwtmvlsc => kwj, teau => euzy, ghezmchb => xhbsdjq);
  
  -- Multi-driven assignments
  albmyxhzv <= albmyxhzv;
  albmyxhzv <= "H";
end ddn;



-- Seed after: 6354187131520563146,7311216359267151659
