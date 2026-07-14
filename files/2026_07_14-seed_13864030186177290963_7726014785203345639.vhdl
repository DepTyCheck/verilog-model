-- Seed: 13864030186177290963,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity uw is
  port (ndlqz : linkage std_logic_vector(0 downto 1));
end uw;

architecture xlgt of uw is
  
begin
  
end xlgt;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity sxwxfy is
  port (puixbrx : inout physical_value_mirror; dmi : in std_logic);
end sxwxfy;

library ieee;
use ieee.std_logic_1164.all;

architecture xddgdi of sxwxfy is
  signal ijwcflwgc : std_logic_vector(0 downto 1);
  signal lijzjoess : std_logic_vector(0 downto 1);
begin
  hmwqgivt : entity work.uw
    port map (ndlqz => lijzjoess);
  zvtq : entity work.uw
    port map (ndlqz => ijwcflwgc);
  
  -- Multi-driven assignments
  lijzjoess <= (others => '0');
  ijwcflwgc <= (others => '0');
  lijzjoess <= lijzjoess;
  lijzjoess <= "";
end xddgdi;



-- Seed after: 2897307056410650612,7726014785203345639
