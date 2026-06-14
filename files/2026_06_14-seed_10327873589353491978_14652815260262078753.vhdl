-- Seed: 10327873589353491978,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity bnhmdqd is
  port (faxwlewjpm : inout std_logic_vector(1 downto 4); aueahwusv : inout std_logic; otzymn : out time; clw : buffer integer);
end bnhmdqd;

architecture zkkxanjao of bnhmdqd is
  
begin
  -- Multi-driven assignments
  faxwlewjpm <= "";
  aueahwusv <= '-';
end zkkxanjao;

entity flwh is
  port (jrptw : inout time; dhewbzqg : out bit_vector(0 to 4); acg : in character);
end flwh;

library ieee;
use ieee.std_logic_1164.all;

architecture ganiq of flwh is
  signal ntkolazzc : integer;
  signal zsj : std_logic;
  signal oikgfktf : std_logic_vector(1 downto 4);
begin
  zjfofwqgnk : entity work.bnhmdqd
    port map (faxwlewjpm => oikgfktf, aueahwusv => zsj, otzymn => jrptw, clw => ntkolazzc);
  
  -- Single-driven assignments
  dhewbzqg <= ('0', '0', '0', '0', '1');
end ganiq;



-- Seed after: 12128882399603393695,14652815260262078753
