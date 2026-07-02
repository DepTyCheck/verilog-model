-- Seed: 12503959289494574312,14426950258250697445

use std.reflection.all;

entity tfamvo is
  port (atlm : out real; vbww : inout access_value_mirror; pmohegrwev : buffer time);
end tfamvo;

architecture fxly of tfamvo is
  
begin
  -- Single-driven assignments
  pmohegrwev <= pmohegrwev;
  atlm <= atlm;
end fxly;

entity mcfjun is
  port (tgnlyhkil : inout time; cek : in severity_level);
end mcfjun;

use std.reflection.all;

architecture his of mcfjun is
  signal rsxitjyo : time;
  shared variable nfjr : access_value_mirror;
  signal ccnbwyas : real;
  signal hmgotaymzk : time;
  shared variable ctvttcxerz : access_value_mirror;
  signal aqoekmwofl : real;
begin
  ryqjkazc : entity work.tfamvo
    port map (atlm => aqoekmwofl, vbww => ctvttcxerz, pmohegrwev => hmgotaymzk);
  xyovtjkyci : entity work.tfamvo
    port map (atlm => ccnbwyas, vbww => nfjr, pmohegrwev => rsxitjyo);
  
  -- Single-driven assignments
  tgnlyhkil <= 4 min;
end his;



-- Seed after: 4481190268475081336,14426950258250697445
