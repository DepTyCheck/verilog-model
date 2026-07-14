-- Seed: 11437909376054790580,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity xcd is
  port ( aomuyznh : inout std_logic_vector(3 to 4)
  ; wf : buffer real
  ; sewbekstyg : linkage std_logic_vector(1 to 3)
  ; corez : inout integer_subtype_mirror
  );
end xcd;

architecture mpqinieoh of xcd is
  
begin
  -- Single-driven assignments
  wf <= 4_0_1_3.4_3_2;
  
  -- Multi-driven assignments
  aomuyznh <= "X1";
  aomuyznh <= aomuyznh;
  aomuyznh <= aomuyznh;
end mpqinieoh;

use std.reflection.all;

entity zbjahzic is
  port (htzzedhpg : inout record_subtype_mirror; ojl : in real);
end zbjahzic;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture lxwo of zbjahzic is
  shared variable nyxbul : integer_subtype_mirror;
  signal z : std_logic_vector(1 to 3);
  signal n : real;
  signal myqewvd : std_logic_vector(3 to 4);
begin
  mwl : entity work.xcd
    port map (aomuyznh => myqewvd, wf => n, sewbekstyg => z, corez => nyxbul);
  
  -- Multi-driven assignments
  z <= z;
  myqewvd <= ('W', '0');
  myqewvd <= ('U', 'Z');
end lxwo;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vwl is
  port (qm : out std_logic; coslam : inout protected_value_mirror);
end vwl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture erzwhyqcxk of vwl is
  shared variable ajtztkgjyd : integer_subtype_mirror;
  signal onzfi : real;
  shared variable dpy : integer_subtype_mirror;
  shared variable peiz : integer_subtype_mirror;
  signal mfex : std_logic_vector(1 to 3);
  signal ipmq : real;
  signal hurtcd : std_logic_vector(3 to 4);
  signal aqoyfl : real;
  shared variable jl : record_subtype_mirror;
begin
  tcdiavdkpr : entity work.zbjahzic
    port map (htzzedhpg => jl, ojl => aqoyfl);
  acngkq : entity work.xcd
    port map (aomuyznh => hurtcd, wf => ipmq, sewbekstyg => mfex, corez => peiz);
  uesjiwfxze : entity work.xcd
    port map (aomuyznh => hurtcd, wf => aqoyfl, sewbekstyg => mfex, corez => dpy);
  eqkausmv : entity work.xcd
    port map (aomuyznh => hurtcd, wf => onzfi, sewbekstyg => mfex, corez => ajtztkgjyd);
  
  -- Multi-driven assignments
  qm <= 'W';
  qm <= qm;
  qm <= '-';
  hurtcd <= "WZ";
end erzwhyqcxk;



-- Seed after: 11958056576978722833,7726014785203345639
