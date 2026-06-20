-- Seed: 16309837009443918146,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (f : in bit_vector(0 to 3); ons : inout std_logic_vector(3 downto 2));
end k;

architecture ulcblg of k is
  
begin
  
end ulcblg;

entity vrbqxxo is
  port (shfmm : linkage time);
end vrbqxxo;

library ieee;
use ieee.std_logic_1164.all;

architecture ajbds of vrbqxxo is
  signal ygsxcbgz : std_logic_vector(3 downto 2);
  signal e : std_logic_vector(3 downto 2);
  signal ekz : bit_vector(0 to 3);
  signal v : std_logic_vector(3 downto 2);
  signal rnoryhqhqs : bit_vector(0 to 3);
begin
  nmpwlmc : entity work.k
    port map (f => rnoryhqhqs, ons => v);
  pmuzsean : entity work.k
    port map (f => rnoryhqhqs, ons => v);
  pbtq : entity work.k
    port map (f => ekz, ons => e);
  s : entity work.k
    port map (f => rnoryhqhqs, ons => ygsxcbgz);
  
  -- Single-driven assignments
  ekz <= ('0', '0', '0', '0');
  rnoryhqhqs <= ('1', '1', '1', '0');
  
  -- Multi-driven assignments
  v <= "XL";
end ajbds;



-- Seed after: 1050103698939229784,17924494779688682807
