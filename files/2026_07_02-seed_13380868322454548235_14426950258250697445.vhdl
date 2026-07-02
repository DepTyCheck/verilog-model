-- Seed: 13380868322454548235,14426950258250697445

use std.reflection.all;

entity e is
  port (qywenlb : inout physical_value_mirror);
end e;

architecture eezzd of e is
  
begin
  
end eezzd;

use std.reflection.all;

entity ponv is
  port (q : buffer severity_level; oej : inout integer_subtype_mirror; wrdxthjgy : inout floating_value_mirror; yla : linkage severity_level);
end ponv;

architecture gcsmtiyfmg of ponv is
  
begin
  -- Single-driven assignments
  q <= q;
end gcsmtiyfmg;

use std.reflection.all;

entity dtni is
  port (ruhtd : inout value_mirror; gnfanp : inout integer_subtype_mirror; x : inout access_value_mirror);
end dtni;

use std.reflection.all;

architecture ixvmg of dtni is
  shared variable kmqvo : physical_value_mirror;
  signal sbiefywhm : severity_level;
  shared variable dgjm : floating_value_mirror;
  shared variable oma : integer_subtype_mirror;
  signal xvb : severity_level;
  shared variable ybrec : physical_value_mirror;
begin
  cmwuf : entity work.e
    port map (qywenlb => ybrec);
  ysumkylekt : entity work.ponv
    port map (q => xvb, oej => oma, wrdxthjgy => dgjm, yla => sbiefywhm);
  kbjas : entity work.e
    port map (qywenlb => kmqvo);
end ixvmg;



-- Seed after: 6935881113344257230,14426950258250697445
