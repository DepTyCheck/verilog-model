-- Seed: 12089280402000958818,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity xrpuv is
  port (kx : inout bit; f : linkage std_logic_vector(0 downto 3); fosz : in time_vector(0 downto 1); a : buffer integer);
end xrpuv;

architecture snp of xrpuv is
  
begin
  -- Single-driven assignments
  a <= a;
  kx <= '1';
end snp;

library ieee;
use ieee.std_logic_1164.all;

entity cqktll is
  port (dqyupkvdod : linkage real; g : linkage std_logic; exdaf : inout std_logic; hmrded : inout time);
end cqktll;

library ieee;
use ieee.std_logic_1164.all;

architecture ohj of cqktll is
  signal lhsejzl : integer;
  signal wghknkkcit : time_vector(0 downto 1);
  signal nkkcrwymub : std_logic_vector(0 downto 3);
  signal ftm : bit;
  signal stcvlcgoq : integer;
  signal crchkeecn : bit;
  signal oetcl : integer;
  signal chnstqlvw : time_vector(0 downto 1);
  signal ybf : std_logic_vector(0 downto 3);
  signal gbb : bit;
begin
  ko : entity work.xrpuv
    port map (kx => gbb, f => ybf, fosz => chnstqlvw, a => oetcl);
  pxdijlgrf : entity work.xrpuv
    port map (kx => crchkeecn, f => ybf, fosz => chnstqlvw, a => stcvlcgoq);
  wq : entity work.xrpuv
    port map (kx => ftm, f => nkkcrwymub, fosz => wghknkkcit, a => lhsejzl);
end ohj;



-- Seed after: 9753115989791018330,5805648483995786113
