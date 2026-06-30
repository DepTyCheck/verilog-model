-- Seed: 466047707842474498,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity vcdylatzd is
  port (njvpzypg : buffer integer; l : out std_logic; x : inout bit_vector(2 downto 1));
end vcdylatzd;

architecture y of vcdylatzd is
  
begin
  -- Multi-driven assignments
  l <= '0';
  l <= 'W';
  l <= 'X';
end y;

entity rtzqv is
  port (scdsbrdwa : out time);
end rtzqv;

architecture nyvpjbvf of rtzqv is
  
begin
  -- Single-driven assignments
  scdsbrdwa <= 2#0011.1# ps;
end nyvpjbvf;

library ieee;
use ieee.std_logic_1164.all;

entity yvqnzv is
  port (qqbnmofwux : inout std_logic; o : in integer);
end yvqnzv;

library ieee;
use ieee.std_logic_1164.all;

architecture optgabhtxv of yvqnzv is
  signal yob : bit_vector(2 downto 1);
  signal h : std_logic;
  signal yhci : integer;
  signal nysi : bit_vector(2 downto 1);
  signal so : integer;
begin
  rqb : entity work.vcdylatzd
    port map (njvpzypg => so, l => qqbnmofwux, x => nysi);
  doxwy : entity work.vcdylatzd
    port map (njvpzypg => yhci, l => h, x => yob);
  
  -- Multi-driven assignments
  qqbnmofwux <= 'L';
  qqbnmofwux <= '1';
  h <= 'L';
end optgabhtxv;



-- Seed after: 14975161383730365297,14629254427735353553
