-- Seed: 15720183993360547684,14426950258250697445

entity x is
  port (sy : out integer);
end x;

architecture p of x is
  
begin
  
end p;

use std.reflection.all;

entity urnprgil is
  port (ricgrfd : inout real; bdahsuz : inout record_subtype_mirror; frbzjsdrbe : out integer);
end urnprgil;

architecture nsv of urnprgil is
  
begin
  -- Single-driven assignments
  frbzjsdrbe <= 8#5132#;
  ricgrfd <= 20134.3410;
end nsv;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity r is
  port (zmjnwrh : inout enumeration_subtype_mirror; nfdh : in severity_level; axuzqpw : out std_logic);
end r;

use std.reflection.all;

architecture ikbhc of r is
  signal ns : integer;
  signal emyryhf : integer;
  shared variable i : record_subtype_mirror;
  signal uhapofipgt : real;
begin
  immxtwoat : entity work.urnprgil
    port map (ricgrfd => uhapofipgt, bdahsuz => i, frbzjsdrbe => emyryhf);
  tif : entity work.x
    port map (sy => ns);
  
  -- Multi-driven assignments
  axuzqpw <= '0';
  axuzqpw <= axuzqpw;
  axuzqpw <= 'H';
end ikbhc;



-- Seed after: 13844629414749276641,14426950258250697445
