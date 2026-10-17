-- Seed: 8128035767611172459,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity uuwlbk is
  port (gruitdxfxd : in std_logic_vector(1 downto 1); xgjeeipwg : inout integer);
end uuwlbk;

architecture bmzwvtdun of uuwlbk is
  
begin
  -- Single-driven assignments
  xgjeeipwg <= 16#6_8_7#;
end bmzwvtdun;

entity rcumn is
  port (hbmyayqkw : out integer_vector(3 downto 2));
end rcumn;

library ieee;
use ieee.std_logic_1164.all;

architecture qaxybo of rcumn is
  signal lcijwlld : integer;
  signal aqgzwaij : integer;
  signal ueejwo : std_logic_vector(1 downto 1);
begin
  rmq : entity work.uuwlbk
    port map (gruitdxfxd => ueejwo, xgjeeipwg => aqgzwaij);
  lbpvkj : entity work.uuwlbk
    port map (gruitdxfxd => ueejwo, xgjeeipwg => lcijwlld);
  
  -- Single-driven assignments
  hbmyayqkw <= hbmyayqkw;
  
  -- Multi-driven assignments
  ueejwo <= "H";
end qaxybo;



-- Seed after: 869023747731135030,5444032936641997477
