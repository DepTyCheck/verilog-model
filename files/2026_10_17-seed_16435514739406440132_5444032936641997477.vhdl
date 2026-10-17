-- Seed: 16435514739406440132,5444032936641997477

entity u is
  port (dfxy : linkage real_vector(4 downto 1); cinvkr : buffer string(2 downto 1));
end u;

architecture ursh of u is
  
begin
  -- Single-driven assignments
  cinvkr <= ('w', 'i');
end ursh;

library ieee;
use ieee.std_logic_1164.all;

entity wl is
  port (ndnt : buffer std_logic_vector(3 downto 3));
end wl;

architecture xstbr of wl is
  signal igwjumhhzy : string(2 downto 1);
  signal gjjfudwmtz : real_vector(4 downto 1);
  signal kdj : string(2 downto 1);
  signal nfknlxp : real_vector(4 downto 1);
  signal twqcflocdk : string(2 downto 1);
  signal trmsqjkzg : real_vector(4 downto 1);
begin
  skqtbxfhm : entity work.u
    port map (dfxy => trmsqjkzg, cinvkr => twqcflocdk);
  bdmxyjpo : entity work.u
    port map (dfxy => nfknlxp, cinvkr => kdj);
  oksypxjmq : entity work.u
    port map (dfxy => gjjfudwmtz, cinvkr => igwjumhhzy);
end xstbr;



-- Seed after: 16415283574312200404,5444032936641997477
