-- Seed: 10429036921634845773,6290177331721581829

use std.reflection.all;

entity jpn is
  port (mtaqsbdsym : inout protected_value_mirror; nsnymb : inout floating_value_mirror);
end jpn;

architecture oqp of jpn is
  
begin
  
end oqp;

entity qgvtqz is
  port (nkaanix : in severity_level);
end qgvtqz;

use std.reflection.all;

architecture ailupli of qgvtqz is
  shared variable fercn : floating_value_mirror;
  shared variable kkitnj : protected_value_mirror;
  shared variable xzzlrasooc : floating_value_mirror;
  shared variable laefytim : protected_value_mirror;
begin
  su : entity work.jpn
    port map (mtaqsbdsym => laefytim, nsnymb => xzzlrasooc);
  lhniik : entity work.jpn
    port map (mtaqsbdsym => kkitnj, nsnymb => fercn);
end ailupli;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity zw is
  port (kngrrn : inout access_subtype_mirror; h : buffer std_logic; nqdiq : inout integer_value_mirror);
end zw;

architecture x of zw is
  signal rhdotmao : severity_level;
begin
  ys : entity work.qgvtqz
    port map (nkaanix => rhdotmao);
  
  -- Single-driven assignments
  rhdotmao <= ERROR;
  
  -- Multi-driven assignments
  h <= h;
  h <= 'U';
  h <= h;
  h <= h;
end x;



-- Seed after: 12679321228213788410,6290177331721581829
