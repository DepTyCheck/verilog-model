-- Seed: 715300872603382502,5472058987609252853

entity cqri is
  port (fnmblz : inout real);
end cqri;

architecture l of cqri is
  
begin
  -- Single-driven assignments
  fnmblz <= 8#204.1_7_0#;
end l;

library ieee;
use ieee.std_logic_1164.all;

entity amjkhrkjee is
  port (pjpxulb : out real; uwldg : buffer real_vector(0 to 1); nnhhxbu : buffer std_logic_vector(3 downto 4));
end amjkhrkjee;

architecture zdmvbspfp of amjkhrkjee is
  
begin
  l : entity work.cqri
    port map (fnmblz => pjpxulb);
  
  -- Single-driven assignments
  uwldg <= (2#01101.1_0_1#, 2_1_2_1_3.33422);
  
  -- Multi-driven assignments
  nnhhxbu <= (others => '0');
  nnhhxbu <= (others => '0');
  nnhhxbu <= "";
end zdmvbspfp;



-- Seed after: 9862199906739255760,5472058987609252853
