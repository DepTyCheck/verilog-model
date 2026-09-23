-- Seed: 6790143015299842211,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (nkifb : linkage std_logic_vector(4 to 2));
end b;

architecture lc of b is
  
begin
  
end lc;

library ieee;
use ieee.std_logic_1164.all;

entity unggbx is
  port (mx : buffer std_logic_vector(3 to 4); p : out time; zmjrf : inout time; sricpoy : linkage real);
end unggbx;

library ieee;
use ieee.std_logic_1164.all;

architecture mkkdbogh of unggbx is
  signal ipg : std_logic_vector(4 to 2);
begin
  s : entity work.b
    port map (nkifb => ipg);
  
  -- Single-driven assignments
  zmjrf <= p;
  p <= zmjrf;
  
  -- Multi-driven assignments
  ipg <= (others => '0');
  mx <= ('W', '1');
end mkkdbogh;



-- Seed after: 320100101304745539,8067602802092121131
