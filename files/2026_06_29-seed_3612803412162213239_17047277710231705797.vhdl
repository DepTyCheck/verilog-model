-- Seed: 3612803412162213239,17047277710231705797

entity r is
  port (xl : out bit_vector(3 downto 0));
end r;

architecture qorcwk of r is
  
begin
  -- Single-driven assignments
  xl <= ('1', '0', '0', '0');
end qorcwk;

entity b is
  port (svmsakbgd : in time);
end b;

architecture l of b is
  signal d : bit_vector(3 downto 0);
  signal twn : bit_vector(3 downto 0);
begin
  ko : entity work.r
    port map (xl => twn);
  mny : entity work.r
    port map (xl => d);
end l;

library ieee;
use ieee.std_logic_1164.all;

entity jb is
  port (ug : buffer std_logic; cwvszvd : buffer integer_vector(1 to 4); ss : linkage time; i : out integer);
end jb;

architecture vsp of jb is
  signal dmbc : bit_vector(3 downto 0);
  signal dgzzdj : bit_vector(3 downto 0);
begin
  oszufk : entity work.r
    port map (xl => dgzzdj);
  dejgapbaem : entity work.r
    port map (xl => dmbc);
  
  -- Single-driven assignments
  i <= 00;
  cwvszvd <= (8#2#, 3_0_2_4, 0041, 2#1_0_1_0#);
  
  -- Multi-driven assignments
  ug <= '0';
end vsp;



-- Seed after: 5790734152118155522,17047277710231705797
