-- Seed: 15440477745390554647,10871023049702252113

entity f is
  port (hyweddbeb : inout integer_vector(3 downto 4); akyybsc : inout boolean_vector(0 to 3); svecul : buffer real);
end f;

architecture jzxyhunkol of f is
  
begin
  
end jzxyhunkol;

library ieee;
use ieee.std_logic_1164.all;

entity iifzg is
  port (aeetx : out std_logic_vector(0 downto 4); murjrvbcto : buffer std_logic_vector(4 to 4); lab : out integer);
end iifzg;

architecture ecytypfd of iifzg is
  signal wu : real;
  signal srpllk : boolean_vector(0 to 3);
  signal tnk : integer_vector(3 downto 4);
  signal k : real;
  signal soovbc : boolean_vector(0 to 3);
  signal mpn : integer_vector(3 downto 4);
begin
  s : entity work.f
    port map (hyweddbeb => mpn, akyybsc => soovbc, svecul => k);
  yieagfh : entity work.f
    port map (hyweddbeb => tnk, akyybsc => srpllk, svecul => wu);
  
  -- Single-driven assignments
  lab <= lab;
  
  -- Multi-driven assignments
  aeetx <= "";
end ecytypfd;



-- Seed after: 236713453507872314,10871023049702252113
