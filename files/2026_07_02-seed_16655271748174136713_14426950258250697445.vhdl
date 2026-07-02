-- Seed: 16655271748174136713,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity bhzhtymaq is
  port (tyimaeckh : out std_logic);
end bhzhtymaq;

architecture yyqbicha of bhzhtymaq is
  
begin
  
end yyqbicha;

library ieee;
use ieee.std_logic_1164.all;

entity id is
  port (m : buffer std_logic_vector(1 to 2));
end id;

library ieee;
use ieee.std_logic_1164.all;

architecture hqdxeh of id is
  signal xene : std_logic;
begin
  ptvg : entity work.bhzhtymaq
    port map (tyimaeckh => xene);
  
  -- Multi-driven assignments
  m <= ('U', 'U');
  xene <= xene;
  m <= m;
  xene <= 'X';
end hqdxeh;

use std.reflection.all;

entity pkmusrqfdi is
  port (fixmpe : inout boolean; dbrtvptw : inout array_value_mirror; aidvqog : inout file_value_mirror; plre : inout integer_subtype_mirror);
end pkmusrqfdi;

architecture mw of pkmusrqfdi is
  
begin
  -- Single-driven assignments
  fixmpe <= TRUE;
end mw;

entity sjdmx is
  port (bfobjrioy : inout real);
end sjdmx;

library ieee;
use ieee.std_logic_1164.all;

architecture cbvdkraz of sjdmx is
  signal rrrdws : std_logic;
begin
  llkxvgvuv : entity work.bhzhtymaq
    port map (tyimaeckh => rrrdws);
  
  -- Single-driven assignments
  bfobjrioy <= bfobjrioy;
  
  -- Multi-driven assignments
  rrrdws <= '0';
  rrrdws <= rrrdws;
end cbvdkraz;



-- Seed after: 2769275182437391340,14426950258250697445
