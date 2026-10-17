-- Seed: 3337818920114409246,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity wpopzybt is
  port (tjrmi : inout time_vector(2 to 2); hspapyy : in std_logic_vector(2 to 4); aehhxkicg : inout std_logic; rdz : linkage integer_vector(3 to 1));
end wpopzybt;

architecture ukzxjmv of wpopzybt is
  
begin
  -- Single-driven assignments
  tjrmi <= (others => 1 hr);
  
  -- Multi-driven assignments
  aehhxkicg <= aehhxkicg;
  aehhxkicg <= aehhxkicg;
  aehhxkicg <= 'Z';
  aehhxkicg <= 'U';
end ukzxjmv;

entity etu is
  port (zdzsf : buffer boolean);
end etu;

library ieee;
use ieee.std_logic_1164.all;

architecture a of etu is
  signal st : integer_vector(3 to 1);
  signal zpz : std_logic;
  signal fwdwbj : time_vector(2 to 2);
  signal ccr : integer_vector(3 to 1);
  signal rtiqtj : time_vector(2 to 2);
  signal yfdjmuokk : integer_vector(3 to 1);
  signal nozsnto : std_logic;
  signal q : std_logic_vector(2 to 4);
  signal w : time_vector(2 to 2);
begin
  bvvmsiab : entity work.wpopzybt
    port map (tjrmi => w, hspapyy => q, aehhxkicg => nozsnto, rdz => yfdjmuokk);
  ptlxy : entity work.wpopzybt
    port map (tjrmi => rtiqtj, hspapyy => q, aehhxkicg => nozsnto, rdz => ccr);
  oh : entity work.wpopzybt
    port map (tjrmi => fwdwbj, hspapyy => q, aehhxkicg => zpz, rdz => st);
  
  -- Single-driven assignments
  zdzsf <= TRUE;
  
  -- Multi-driven assignments
  q <= ('0', '-', 'Z');
  q <= q;
  q <= "H0W";
end a;



-- Seed after: 15638417288055068692,5444032936641997477
