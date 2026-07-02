-- Seed: 8869341101154138475,14426950258250697445

use std.reflection.all;

entity fhirvrul is
  port (lyepn : inout file_value_mirror);
end fhirvrul;

architecture m of fhirvrul is
  
begin
  
end m;

use std.reflection.all;

entity lhjj is
  port (ukuvim : inout real; sn : inout access_value_mirror);
end lhjj;

use std.reflection.all;

architecture rzd of lhjj is
  shared variable dreymkhpq : file_value_mirror;
  shared variable ixtasc : file_value_mirror;
begin
  pfrgzp : entity work.fhirvrul
    port map (lyepn => ixtasc);
  mkmvyscqcf : entity work.fhirvrul
    port map (lyepn => dreymkhpq);
  
  -- Single-driven assignments
  ukuvim <= 0_3_0.10331;
end rzd;

use std.reflection.all;

entity kloj is
  port (idriu : out bit; hdyvitezf : inout integer_value_mirror; auwwg : inout time);
end kloj;

use std.reflection.all;

architecture nhgwdzykhu of kloj is
  shared variable xryafwd : file_value_mirror;
begin
  y : entity work.fhirvrul
    port map (lyepn => xryafwd);
  
  -- Single-driven assignments
  auwwg <= 3 min;
end nhgwdzykhu;



-- Seed after: 7778472776022914576,14426950258250697445
