-- Seed: 15632779880483147856,14426950258250697445

use std.reflection.all;

entity c is
  port (fmat : inout physical_value_mirror; sl : inout record_value_mirror);
end c;

architecture gbtxzpzecf of c is
  
begin
  
end gbtxzpzecf;

entity m is
  port (bf : buffer integer; vvo : out time; mi : in integer);
end m;

architecture op of m is
  
begin
  -- Single-driven assignments
  vvo <= vvo;
  bf <= mi;
end op;

use std.reflection.all;

entity kldg is
  port (ctfuayg : inout subtype_mirror; gqmzgxp : in real; mbd : linkage integer; iqe : inout physical_value_mirror);
end kldg;

use std.reflection.all;

architecture pmy of kldg is
  signal uymsg : integer;
  signal zypwd : time;
  signal q : integer;
  shared variable abcnaxf : record_value_mirror;
  shared variable gmv : physical_value_mirror;
  shared variable zjkdmlgrh : record_value_mirror;
  shared variable ftrx : physical_value_mirror;
  shared variable fn : record_value_mirror;
  shared variable ln : physical_value_mirror;
begin
  qcvalzpaw : entity work.c
    port map (fmat => ln, sl => fn);
  riucth : entity work.c
    port map (fmat => ftrx, sl => zjkdmlgrh);
  rb : entity work.c
    port map (fmat => gmv, sl => abcnaxf);
  dayfqj : entity work.m
    port map (bf => q, vvo => zypwd, mi => uymsg);
  
  -- Single-driven assignments
  uymsg <= q;
end pmy;



-- Seed after: 11024057648548599273,14426950258250697445
