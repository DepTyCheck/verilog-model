-- Seed: 8596845274282698167,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity vhafqebmyc is
  port (ynyuom : inout std_logic_vector(3 to 3); ljokjsb : linkage time; kx : buffer boolean);
end vhafqebmyc;

architecture x of vhafqebmyc is
  
begin
  -- Single-driven assignments
  kx <= FALSE;
  
  -- Multi-driven assignments
  ynyuom <= (others => '-');
  ynyuom <= "1";
  ynyuom <= (others => 'H');
  ynyuom <= (others => '-');
end x;

library ieee;
use ieee.std_logic_1164.all;

entity uqk is
  port (pljtszbo : out integer; syqtnk : out std_logic_vector(0 downto 3); rbac : inout real; b : buffer bit);
end uqk;

library ieee;
use ieee.std_logic_1164.all;

architecture kgggc of uqk is
  signal foygs : boolean;
  signal w : time;
  signal gs : boolean;
  signal bumejd : time;
  signal jxclryup : std_logic_vector(3 to 3);
  signal rycejqn : boolean;
  signal spohcekp : time;
  signal muzoctqj : std_logic_vector(3 to 3);
  signal xzrkrftrr : boolean;
  signal nss : time;
  signal cqmyb : std_logic_vector(3 to 3);
begin
  icobshc : entity work.vhafqebmyc
    port map (ynyuom => cqmyb, ljokjsb => nss, kx => xzrkrftrr);
  dzbu : entity work.vhafqebmyc
    port map (ynyuom => muzoctqj, ljokjsb => spohcekp, kx => rycejqn);
  jnyfu : entity work.vhafqebmyc
    port map (ynyuom => jxclryup, ljokjsb => bumejd, kx => gs);
  yone : entity work.vhafqebmyc
    port map (ynyuom => jxclryup, ljokjsb => w, kx => foygs);
  
  -- Multi-driven assignments
  syqtnk <= "";
  muzoctqj <= "W";
  jxclryup <= (others => 'Z');
end kgggc;



-- Seed after: 2399889383108420382,17047277710231705797
