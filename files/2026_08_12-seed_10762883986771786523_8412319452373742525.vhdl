-- Seed: 10762883986771786523,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (vedf : buffer time; vqboaodr : buffer std_logic);
end i;

architecture v of i is
  
begin
  -- Single-driven assignments
  vedf <= vedf;
  
  -- Multi-driven assignments
  vqboaodr <= vqboaodr;
end v;

entity khqbmccu is
  port (toargn : buffer real; dhj : buffer boolean_vector(1 downto 2); ddfcqau : linkage severity_level; azb : out boolean);
end khqbmccu;

library ieee;
use ieee.std_logic_1164.all;

architecture swtc of khqbmccu is
  signal sgjlvmun : time;
  signal vmgwzuthbu : std_logic;
  signal sxmthxwabm : time;
begin
  xt : entity work.i
    port map (vedf => sxmthxwabm, vqboaodr => vmgwzuthbu);
  rqfgktxn : entity work.i
    port map (vedf => sgjlvmun, vqboaodr => vmgwzuthbu);
  
  -- Single-driven assignments
  azb <= FALSE;
  
  -- Multi-driven assignments
  vmgwzuthbu <= vmgwzuthbu;
  vmgwzuthbu <= '1';
end swtc;



-- Seed after: 4956407606624556732,8412319452373742525
