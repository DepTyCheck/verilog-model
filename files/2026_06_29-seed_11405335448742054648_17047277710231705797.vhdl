-- Seed: 11405335448742054648,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity sootmtakh is
  port (ktzsz : inout boolean; ohqnk : linkage std_logic_vector(3 to 3));
end sootmtakh;

architecture cj of sootmtakh is
  
begin
  -- Single-driven assignments
  ktzsz <= FALSE;
end cj;

entity oubncbqjxb is
  port (v : linkage string(1 downto 3));
end oubncbqjxb;

library ieee;
use ieee.std_logic_1164.all;

architecture lm of oubncbqjxb is
  signal qbfhfgpvb : std_logic_vector(3 to 3);
  signal vswevinkat : boolean;
begin
  mxvniav : entity work.sootmtakh
    port map (ktzsz => vswevinkat, ohqnk => qbfhfgpvb);
  
  -- Multi-driven assignments
  qbfhfgpvb <= "Z";
  qbfhfgpvb <= "Z";
  qbfhfgpvb <= (others => '-');
  qbfhfgpvb <= "H";
end lm;



-- Seed after: 4797235819591873785,17047277710231705797
