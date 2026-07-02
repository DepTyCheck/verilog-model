-- Seed: 6291544675396513732,14426950258250697445

use std.reflection.all;

entity jtgz is
  port (ttgrva : inout floating_value_mirror; gbiqrg : linkage boolean; eww : inout string(2 downto 5); jkrcyya : out integer_vector(4 to 0));
end jtgz;

architecture oymtvg of jtgz is
  
begin
  -- Single-driven assignments
  jkrcyya <= (others => 0);
end oymtvg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity n is
  port (oqssxjakx : inout std_logic_vector(3 downto 3); dmdd : inout physical_value_mirror);
end n;

use std.reflection.all;

architecture kdshlgb of n is
  signal zfrcbclnp : integer_vector(4 to 0);
  signal s : string(2 downto 5);
  signal apbtawknjx : boolean;
  shared variable tfzuvd : floating_value_mirror;
  signal cl : integer_vector(4 to 0);
  signal me : string(2 downto 5);
  signal mskmwrfkk : boolean;
  shared variable mova : floating_value_mirror;
begin
  crjgnqdhp : entity work.jtgz
    port map (ttgrva => mova, gbiqrg => mskmwrfkk, eww => me, jkrcyya => cl);
  jol : entity work.jtgz
    port map (ttgrva => tfzuvd, gbiqrg => apbtawknjx, eww => s, jkrcyya => zfrcbclnp);
  
  -- Multi-driven assignments
  oqssxjakx <= "0";
  oqssxjakx <= oqssxjakx;
  oqssxjakx <= oqssxjakx;
  oqssxjakx <= "1";
end kdshlgb;



-- Seed after: 17315614372821384261,14426950258250697445
