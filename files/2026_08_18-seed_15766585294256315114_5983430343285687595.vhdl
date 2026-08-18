-- Seed: 15766585294256315114,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (dqqbm : in std_logic_vector(4 to 1));
end w;

architecture sbsqeoinl of w is
  
begin
  
end sbsqeoinl;

entity n is
  port (wvmbbxiy : buffer time; mgm : in real);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture f of n is
  signal ifxpegr : std_logic_vector(4 to 1);
  signal grrrbqmqah : std_logic_vector(4 to 1);
begin
  fnbxwxwfeg : entity work.w
    port map (dqqbm => grrrbqmqah);
  vuvingdeo : entity work.w
    port map (dqqbm => ifxpegr);
  jtxu : entity work.w
    port map (dqqbm => grrrbqmqah);
  
  -- Single-driven assignments
  wvmbbxiy <= 3 sec;
  
  -- Multi-driven assignments
  grrrbqmqah <= ifxpegr;
end f;



-- Seed after: 3121892082715783098,5983430343285687595
