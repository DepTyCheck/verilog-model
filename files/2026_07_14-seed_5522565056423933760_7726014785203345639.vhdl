-- Seed: 5522565056423933760,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity rzso is
  port (trx : inout floating_value_mirror; ojfed : buffer std_logic; k : inout integer_subtype_mirror);
end rzso;

architecture hblis of rzso is
  
begin
  -- Multi-driven assignments
  ojfed <= '0';
  ojfed <= 'H';
  ojfed <= '-';
end hblis;

library ieee;
use ieee.std_logic_1164.all;

entity ha is
  port (ezvhbieois : linkage std_logic_vector(4 to 0));
end ha;

architecture bmwikeqm of ha is
  
begin
  
end bmwikeqm;

use std.reflection.all;

entity kwneeje is
  port (ixbgufqcq : inout floating_value_mirror);
end kwneeje;

library ieee;
use ieee.std_logic_1164.all;

architecture mhoadn of kwneeje is
  signal r : std_logic_vector(4 to 0);
begin
  nglqjvdgv : entity work.ha
    port map (ezvhbieois => r);
  
  -- Multi-driven assignments
  r <= r;
  r <= "";
end mhoadn;

use std.reflection.all;

entity so is
  port (pvrggsmu : inout enumeration_subtype_mirror);
end so;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture dfxv of so is
  shared variable hi : integer_subtype_mirror;
  signal kxcbmwh : std_logic;
  shared variable dpsrp : floating_value_mirror;
begin
  mxul : entity work.rzso
    port map (trx => dpsrp, ojfed => kxcbmwh, k => hi);
  
  -- Multi-driven assignments
  kxcbmwh <= kxcbmwh;
end dfxv;



-- Seed after: 14156124729661870488,7726014785203345639
