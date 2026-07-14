-- Seed: 10197748528653436810,7726014785203345639

use std.reflection.all;

entity fk is
  port (xb : inout access_subtype_mirror);
end fk;

architecture htxbrtel of fk is
  
begin
  
end htxbrtel;

use std.reflection.all;

entity dsigfzvl is
  port (zqamdsn : inout protected_subtype_mirror);
end dsigfzvl;

use std.reflection.all;

architecture tvswscm of dsigfzvl is
  shared variable magkszlnyz : access_subtype_mirror;
begin
  unmnd : entity work.fk
    port map (xb => magkszlnyz);
end tvswscm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity xqpk is
  port (l : inout record_value_mirror; lgexly : in real; jclfczlj : in std_logic_vector(3 to 2); mpbhmpo : buffer time);
end xqpk;

use std.reflection.all;

architecture skurqqi of xqpk is
  shared variable tolqymbeo : protected_subtype_mirror;
  shared variable zrqatrjxke : protected_subtype_mirror;
  shared variable uh : protected_subtype_mirror;
  shared variable g : protected_subtype_mirror;
begin
  hofqvrzk : entity work.dsigfzvl
    port map (zqamdsn => g);
  kwo : entity work.dsigfzvl
    port map (zqamdsn => uh);
  wkpcnrwoum : entity work.dsigfzvl
    port map (zqamdsn => zrqatrjxke);
  nu : entity work.dsigfzvl
    port map (zqamdsn => tolqymbeo);
  
  -- Single-driven assignments
  mpbhmpo <= 23102.2_0_1 ms;
end skurqqi;



-- Seed after: 3510077626661344431,7726014785203345639
