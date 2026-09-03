-- Seed: 12844174460080124910,11127274767545411571

entity ijan is
  port (ybspijvi : inout time; crkywbn : buffer time_vector(3 to 3); dnd : linkage string(4 downto 3));
end ijan;

architecture rf of ijan is
  
begin
  -- Single-driven assignments
  crkywbn <= crkywbn;
  ybspijvi <= 2#1_1_1_1# ns;
end rf;

library ieee;
use ieee.std_logic_1164.all;

entity kcnzueq is
  port (izyhbla : in std_logic; dwot : out real; sg : linkage time; ywyicsu : out severity_level);
end kcnzueq;

architecture mtvpdymbf of kcnzueq is
  signal nrhinghxg : string(4 downto 3);
  signal k : time_vector(3 to 3);
  signal wtoca : time;
  signal medwglqkz : string(4 downto 3);
  signal umza : time_vector(3 to 3);
  signal ezm : time;
  signal cnvu : string(4 downto 3);
  signal ut : time_vector(3 to 3);
  signal o : time;
  signal qaddnuqvcj : string(4 downto 3);
  signal w : time_vector(3 to 3);
  signal jae : time;
begin
  vsepe : entity work.ijan
    port map (ybspijvi => jae, crkywbn => w, dnd => qaddnuqvcj);
  mskmmxrk : entity work.ijan
    port map (ybspijvi => o, crkywbn => ut, dnd => cnvu);
  wb : entity work.ijan
    port map (ybspijvi => ezm, crkywbn => umza, dnd => medwglqkz);
  fugkbh : entity work.ijan
    port map (ybspijvi => wtoca, crkywbn => k, dnd => nrhinghxg);
  
  -- Single-driven assignments
  ywyicsu <= FAILURE;
  dwot <= dwot;
end mtvpdymbf;



-- Seed after: 14088206469705481392,11127274767545411571
