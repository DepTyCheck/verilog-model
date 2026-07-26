-- Seed: 1347019768945033547,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity czz is
  port (nfvupvhizh : out real; mdev : in std_logic; odmwdqlqht : in real);
end czz;

architecture euqk of czz is
  
begin
  -- Single-driven assignments
  nfvupvhizh <= odmwdqlqht;
end euqk;

library ieee;
use ieee.std_logic_1164.all;

entity uqtcmhwtyv is
  port (yupcvcqijn : inout std_logic; yjdzhplvoh : in std_logic);
end uqtcmhwtyv;

architecture csgzeyvvw of uqtcmhwtyv is
  signal vhxglmxeux : real;
  signal ajgq : real;
begin
  eghvmk : entity work.czz
    port map (nfvupvhizh => ajgq, mdev => yjdzhplvoh, odmwdqlqht => ajgq);
  uaq : entity work.czz
    port map (nfvupvhizh => vhxglmxeux, mdev => yjdzhplvoh, odmwdqlqht => ajgq);
  
  -- Multi-driven assignments
  yupcvcqijn <= yjdzhplvoh;
end csgzeyvvw;



-- Seed after: 3290971915520325728,7808623373429384027
