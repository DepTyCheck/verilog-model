-- Seed: 3866415518718350633,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity rwdz is
  port (y : buffer std_logic_vector(2 downto 3));
end rwdz;

architecture vzrq of rwdz is
  
begin
  -- Multi-driven assignments
  y <= (others => '0');
  y <= y;
end vzrq;

entity gyotbos is
  port (ttzuc : in integer; ahyooypun : in boolean; r : out time; ppevlj : buffer character);
end gyotbos;

library ieee;
use ieee.std_logic_1164.all;

architecture uu of gyotbos is
  signal vkoak : std_logic_vector(2 downto 3);
begin
  yew : entity work.rwdz
    port map (y => vkoak);
  
  -- Multi-driven assignments
  vkoak <= "";
  vkoak <= vkoak;
  vkoak <= "";
end uu;

entity tscrrrekkk is
  port (jnqzzssj : buffer real_vector(1 downto 2); btsjcgz : linkage real; kojizf : inout integer; rlgfoutpl : out character);
end tscrrrekkk;

library ieee;
use ieee.std_logic_1164.all;

architecture vqi of tscrrrekkk is
  signal hqhs : std_logic_vector(2 downto 3);
  signal vf : time;
  signal ew : boolean;
begin
  osfjcil : entity work.gyotbos
    port map (ttzuc => kojizf, ahyooypun => ew, r => vf, ppevlj => rlgfoutpl);
  qzxj : entity work.rwdz
    port map (y => hqhs);
  vfn : entity work.rwdz
    port map (y => hqhs);
  
  -- Single-driven assignments
  kojizf <= 8#7_2_3_5_5#;
  jnqzzssj <= jnqzzssj;
  
  -- Multi-driven assignments
  hqhs <= hqhs;
end vqi;



-- Seed after: 15595585865357957954,4122021602305298647
