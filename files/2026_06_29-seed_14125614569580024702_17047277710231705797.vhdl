-- Seed: 14125614569580024702,17047277710231705797

entity dgn is
  port (nggefibox : out time; r : linkage time; wcsn : inout boolean; cbcf : linkage integer);
end dgn;

architecture jv of dgn is
  
begin
  -- Single-driven assignments
  nggefibox <= 8#7_4.0_3# ms;
  wcsn <= FALSE;
end jv;

library ieee;
use ieee.std_logic_1164.all;

entity grt is
  port (lhec : out boolean_vector(3 downto 4); jwwljqfs : buffer real; zynw : out severity_level; urkzuxszaz : in std_logic);
end grt;

architecture trmp of grt is
  signal mjzkjtxtb : integer;
  signal xeahzzm : boolean;
  signal kwgwd : time;
  signal riarpefx : time;
  signal vjovilw : integer;
  signal h : boolean;
  signal pqcpynq : time;
  signal chkalxbko : time;
begin
  xtjex : entity work.dgn
    port map (nggefibox => chkalxbko, r => pqcpynq, wcsn => h, cbcf => vjovilw);
  czlgvbsu : entity work.dgn
    port map (nggefibox => riarpefx, r => kwgwd, wcsn => xeahzzm, cbcf => mjzkjtxtb);
end trmp;



-- Seed after: 13847164084655017395,17047277710231705797
