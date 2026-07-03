-- Seed: 334443984032620954,2158184632809654795

entity ztmpsngdij is
  port (tsjrjpwof : buffer time);
end ztmpsngdij;

architecture f of ztmpsngdij is
  
begin
  -- Single-driven assignments
  tsjrjpwof <= 0 us;
end f;

entity zdqiqikjf is
  port (ubx : buffer real);
end zdqiqikjf;

architecture enwxbzovp of zdqiqikjf is
  signal yaiy : time;
  signal mtwxyd : time;
begin
  bmpimpknf : entity work.ztmpsngdij
    port map (tsjrjpwof => mtwxyd);
  ehroxzqoi : entity work.ztmpsngdij
    port map (tsjrjpwof => yaiy);
  
  -- Single-driven assignments
  ubx <= 16#C_D.B#;
end enwxbzovp;

use std.reflection.all;

entity ezvzieb is
  port (vpemlw : inout physical_subtype_mirror; vm : buffer real);
end ezvzieb;

architecture sigfum of ezvzieb is
  signal qdvbhnzkl : time;
  signal qbti : real;
begin
  jrwko : entity work.zdqiqikjf
    port map (ubx => qbti);
  viklxa : entity work.ztmpsngdij
    port map (tsjrjpwof => qdvbhnzkl);
  
  -- Single-driven assignments
  vm <= vm;
end sigfum;



-- Seed after: 2767981528356214612,2158184632809654795
