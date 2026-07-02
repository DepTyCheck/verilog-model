-- Seed: 17032449918017530569,14426950258250697445

use std.reflection.all;

entity yjx is
  port (hbdpjv : buffer real; stg : inout integer_subtype_mirror);
end yjx;

architecture imv of yjx is
  
begin
  -- Single-driven assignments
  hbdpjv <= 2#0.0_1_1_0_0#;
end imv;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity z is
  port (gaiyo : inout subtype_mirror; izh : out std_logic; ctjlevt : buffer time);
end z;

architecture auihcjzesc of z is
  
begin
  -- Single-driven assignments
  ctjlevt <= ctjlevt;
  
  -- Multi-driven assignments
  izh <= izh;
  izh <= 'L';
  izh <= '0';
end auihcjzesc;

use std.reflection.all;

entity qekmrdls is
  port (r : inout floating_value_mirror);
end qekmrdls;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture irkpbsd of qekmrdls is
  signal s : time;
  signal vfw : std_logic;
  shared variable eluccbvb : subtype_mirror;
  shared variable xlhmhpdt : integer_subtype_mirror;
  signal jxrtg : real;
  shared variable zaixosjdjs : integer_subtype_mirror;
  signal lda : real;
begin
  qulvvf : entity work.yjx
    port map (hbdpjv => lda, stg => zaixosjdjs);
  jskzp : entity work.yjx
    port map (hbdpjv => jxrtg, stg => xlhmhpdt);
  abo : entity work.z
    port map (gaiyo => eluccbvb, izh => vfw, ctjlevt => s);
  
  -- Multi-driven assignments
  vfw <= 'X';
  vfw <= vfw;
  vfw <= 'X';
end irkpbsd;



-- Seed after: 11935297828861445999,14426950258250697445
