-- Seed: 14943714964532276925,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity gebjjy is
  port (ribww : inout integer; vk : linkage std_logic_vector(4 to 0); ukactapt : in integer);
end gebjjy;

architecture tjgkx of gebjjy is
  
begin
  -- Single-driven assignments
  ribww <= 2_2_2_1_0;
end tjgkx;

entity voa is
  port (pcjeadzu : out boolean; girwyjgkdw : out integer);
end voa;

library ieee;
use ieee.std_logic_1164.all;

architecture scyq of voa is
  signal jsiulhygv : std_logic_vector(4 to 0);
  signal yknpjt : integer;
  signal cvhjtwfmb : integer;
  signal azfbflbkse : std_logic_vector(4 to 0);
  signal phisqc : std_logic_vector(4 to 0);
  signal ivnyjurof : integer;
begin
  xqsycom : entity work.gebjjy
    port map (ribww => ivnyjurof, vk => phisqc, ukactapt => girwyjgkdw);
  xidjzqx : entity work.gebjjy
    port map (ribww => girwyjgkdw, vk => azfbflbkse, ukactapt => girwyjgkdw);
  hix : entity work.gebjjy
    port map (ribww => cvhjtwfmb, vk => azfbflbkse, ukactapt => yknpjt);
  a : entity work.gebjjy
    port map (ribww => yknpjt, vk => jsiulhygv, ukactapt => girwyjgkdw);
  
  -- Multi-driven assignments
  phisqc <= (others => '0');
  phisqc <= "";
  jsiulhygv <= "";
end scyq;



-- Seed after: 13090873358667183913,5472058987609252853
