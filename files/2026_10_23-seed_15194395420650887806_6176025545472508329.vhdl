-- Seed: 15194395420650887806,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity vmcukhav is
  port (wxhlxmu : in boolean_vector(0 downto 2); bjb : out std_logic_vector(3 downto 0); almlvno : buffer time);
end vmcukhav;

architecture nm of vmcukhav is
  
begin
  -- Single-driven assignments
  almlvno <= 2#010.1_0_1_1# ms;
  
  -- Multi-driven assignments
  bjb <= "XUZL";
end nm;

library ieee;
use ieee.std_logic_1164.all;

entity zr is
  port (g : buffer time; xxfauvprd : inout std_logic; igjqmfva : inout integer);
end zr;

library ieee;
use ieee.std_logic_1164.all;

architecture jdoyh of zr is
  signal osvpvjnw : time;
  signal bppf : time;
  signal wpc : std_logic_vector(3 downto 0);
  signal krkezteqso : boolean_vector(0 downto 2);
begin
  fyxtzrerh : entity work.vmcukhav
    port map (wxhlxmu => krkezteqso, bjb => wpc, almlvno => bppf);
  pfudtdrk : entity work.vmcukhav
    port map (wxhlxmu => krkezteqso, bjb => wpc, almlvno => osvpvjnw);
  coqpbhf : entity work.vmcukhav
    port map (wxhlxmu => krkezteqso, bjb => wpc, almlvno => g);
  
  -- Single-driven assignments
  krkezteqso <= (others => TRUE);
  igjqmfva <= 8#11#;
  
  -- Multi-driven assignments
  wpc <= wpc;
end jdoyh;

entity tnl is
  port (iuttltsre : in time; bsyaxz : out time; ald : inout real; ttlkt : inout real);
end tnl;

architecture gnkvimz of tnl is
  
begin
  -- Single-driven assignments
  ttlkt <= ttlkt;
  bsyaxz <= 0 sec;
  ald <= ttlkt;
end gnkvimz;

entity k is
  port (jsw : inout time; jrctaiszm : buffer string(4 downto 2));
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture ymwnkilzz of k is
  signal q : std_logic_vector(3 downto 0);
  signal vcckwxuvw : boolean_vector(0 downto 2);
begin
  sxdpbkaupp : entity work.vmcukhav
    port map (wxhlxmu => vcckwxuvw, bjb => q, almlvno => jsw);
  
  -- Single-driven assignments
  jrctaiszm <= jrctaiszm;
  vcckwxuvw <= vcckwxuvw;
  
  -- Multi-driven assignments
  q <= "1-1L";
  q <= q;
end ymwnkilzz;



-- Seed after: 11930655176586574367,6176025545472508329
