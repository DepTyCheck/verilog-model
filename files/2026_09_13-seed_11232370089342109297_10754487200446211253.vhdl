-- Seed: 11232370089342109297,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity ce is
  port (dzroe : in std_logic; vxtlukw : inout std_logic; mwkjokhw : buffer std_logic_vector(3 to 1); ernisqrih : inout real);
end ce;

architecture botgi of ce is
  
begin
  -- Single-driven assignments
  ernisqrih <= ernisqrih;
  
  -- Multi-driven assignments
  mwkjokhw <= (others => '0');
end botgi;

entity ali is
  port (aqqqrrtzi : out bit);
end ali;

library ieee;
use ieee.std_logic_1164.all;

architecture ocaec of ali is
  signal e : real;
  signal nqxl : std_logic;
  signal ny : real;
  signal wnqq : std_logic_vector(3 to 1);
  signal fwnf : std_logic;
begin
  tgx : entity work.ce
    port map (dzroe => fwnf, vxtlukw => fwnf, mwkjokhw => wnqq, ernisqrih => ny);
  o : entity work.ce
    port map (dzroe => nqxl, vxtlukw => fwnf, mwkjokhw => wnqq, ernisqrih => e);
  
  -- Single-driven assignments
  aqqqrrtzi <= '1';
end ocaec;



-- Seed after: 11629483788892392485,10754487200446211253
