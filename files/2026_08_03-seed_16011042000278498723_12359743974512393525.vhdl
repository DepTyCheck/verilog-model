-- Seed: 16011042000278498723,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity sk is
  port (s : in std_logic_vector(0 to 1));
end sk;

architecture xn of sk is
  
begin
  
end xn;

library ieee;
use ieee.std_logic_1164.all;

entity laupkt is
  port (pcvwdl : in std_logic);
end laupkt;

library ieee;
use ieee.std_logic_1164.all;

architecture paxyqjwk of laupkt is
  signal hntsyzn : std_logic_vector(0 to 1);
  signal rlnerspt : std_logic_vector(0 to 1);
begin
  bvx : entity work.sk
    port map (s => rlnerspt);
  o : entity work.sk
    port map (s => hntsyzn);
  
  -- Multi-driven assignments
  rlnerspt <= rlnerspt;
  hntsyzn <= ('H', '1');
  rlnerspt <= ('W', '0');
  rlnerspt <= ('U', 'Z');
end paxyqjwk;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (rqd : linkage boolean; njnwgpnuh : in real_vector(0 downto 1); h : linkage std_logic_vector(4 downto 1));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture jthq of n is
  signal oyghwiwb : std_logic;
begin
  kbocm : entity work.laupkt
    port map (pcvwdl => oyghwiwb);
  
  -- Multi-driven assignments
  oyghwiwb <= oyghwiwb;
  oyghwiwb <= 'X';
  oyghwiwb <= oyghwiwb;
  oyghwiwb <= '-';
end jthq;



-- Seed after: 2354199328473743033,12359743974512393525
