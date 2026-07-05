-- Seed: 1432936841706239821,3181554006726329157

use std.reflection.all;

entity ps is
  port (mbnu : in real; vjrkxpvf : inout floating_subtype_mirror);
end ps;

architecture xq of ps is
  
begin
  
end xq;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity mwdyhuh is
  port (mpvlguusl : in real; dqoum : in std_logic; jn : inout file_subtype_mirror);
end mwdyhuh;

use std.reflection.all;

architecture pe of mwdyhuh is
  shared variable zcua : floating_subtype_mirror;
begin
  ji : entity work.ps
    port map (mbnu => mpvlguusl, vjrkxpvf => zcua);
end pe;

use std.reflection.all;

entity oyehad is
  port (zorl : inout protected_value_mirror);
end oyehad;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xesogjzktp of oyehad is
  shared variable lqv : floating_subtype_mirror;
  signal wau : real;
  shared variable ljsayuql : file_subtype_mirror;
  signal a : std_logic;
  shared variable yx : floating_subtype_mirror;
  signal n : real;
begin
  hmyhowpd : entity work.ps
    port map (mbnu => n, vjrkxpvf => yx);
  gocfbo : entity work.mwdyhuh
    port map (mpvlguusl => n, dqoum => a, jn => ljsayuql);
  dglbskr : entity work.ps
    port map (mbnu => wau, vjrkxpvf => lqv);
  
  -- Multi-driven assignments
  a <= 'X';
end xesogjzktp;

use std.reflection.all;

entity wyrhmxqba is
  port (v : inout integer_subtype_mirror; vqyeiklng : inout value_mirror);
end wyrhmxqba;

use std.reflection.all;

architecture lpanezbtqw of wyrhmxqba is
  shared variable whujohr : floating_subtype_mirror;
  signal uscbly : real;
begin
  q : entity work.ps
    port map (mbnu => uscbly, vjrkxpvf => whujohr);
  
  -- Single-driven assignments
  uscbly <= 4_1_0.0_0_3_3_1;
end lpanezbtqw;



-- Seed after: 3799613082124733126,3181554006726329157
