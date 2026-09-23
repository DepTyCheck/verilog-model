-- Seed: 15142410541587255990,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity baicwr is
  port (sctraryjx : linkage std_logic_vector(3 downto 0));
end baicwr;

architecture elhupvrpd of baicwr is
  
begin
  
end elhupvrpd;

entity cccan is
  port (smxn : out real);
end cccan;

library ieee;
use ieee.std_logic_1164.all;

architecture eunzkplrk of cccan is
  signal tqwgh : std_logic_vector(3 downto 0);
begin
  gqrc : entity work.baicwr
    port map (sctraryjx => tqwgh);
  eirs : entity work.baicwr
    port map (sctraryjx => tqwgh);
  
  -- Single-driven assignments
  smxn <= 16#0_5_0_5.3_7#;
  
  -- Multi-driven assignments
  tqwgh <= tqwgh;
end eunzkplrk;



-- Seed after: 10263822846882296446,8067602802092121131
