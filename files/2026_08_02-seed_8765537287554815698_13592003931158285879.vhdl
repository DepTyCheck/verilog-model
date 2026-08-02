-- Seed: 8765537287554815698,13592003931158285879

library ieee;
use ieee.std_logic_1164.all;

entity trcwm is
  port (r : out std_logic_vector(2 to 3); ufif : buffer std_logic);
end trcwm;

architecture yr of trcwm is
  
begin
  -- Multi-driven assignments
  r <= ('1', '1');
  r <= "-H";
  ufif <= ufif;
end yr;



-- Seed after: 3631734188246749349,13592003931158285879
