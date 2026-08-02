-- Seed: 5664889717454608805,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity oq is
  port (xlyxzqw : out std_logic_vector(2 downto 1));
end oq;

architecture m of oq is
  
begin
  
end m;

entity ffaect is
  port (f : buffer real; kvyioww : in real);
end ffaect;

library ieee;
use ieee.std_logic_1164.all;

architecture jannvuebu of ffaect is
  signal bi : std_logic_vector(2 downto 1);
begin
  mcim : entity work.oq
    port map (xlyxzqw => bi);
  
  -- Single-driven assignments
  f <= 4.02010;
  
  -- Multi-driven assignments
  bi <= bi;
  bi <= "UX";
end jannvuebu;

entity g is
  port (ipqbovwxs : inout time);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture gqttyjm of g is
  signal vlmncuyda : std_logic_vector(2 downto 1);
  signal p : std_logic_vector(2 downto 1);
begin
  jwpxe : entity work.oq
    port map (xlyxzqw => p);
  hizqhnj : entity work.oq
    port map (xlyxzqw => vlmncuyda);
  wzjpgqzfxv : entity work.oq
    port map (xlyxzqw => vlmncuyda);
  dvyk : entity work.oq
    port map (xlyxzqw => p);
  
  -- Single-driven assignments
  ipqbovwxs <= ipqbovwxs;
end gqttyjm;



-- Seed after: 18020137362986164699,13592003931158285879
