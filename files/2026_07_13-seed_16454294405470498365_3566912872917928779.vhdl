-- Seed: 16454294405470498365,3566912872917928779

use std.reflection.all;

entity bkutucrd is
  port (iblphvoc : inout integer_value_mirror; uaqily : inout file_value_mirror; qdwn : inout record_value_mirror; fyqfrryf : inout value_mirror);
end bkutucrd;

architecture qvfteynbyl of bkutucrd is
  
begin
  
end qvfteynbyl;

use std.reflection.all;

entity yle is
  port (af : inout integer; hvlmex : inout access_subtype_mirror);
end yle;

use std.reflection.all;

architecture pfmgx of yle is
  shared variable iwt : value_mirror;
  shared variable cwgm : record_value_mirror;
  shared variable gqassu : file_value_mirror;
  shared variable v : integer_value_mirror;
  shared variable xlweym : value_mirror;
  shared variable kfcincj : record_value_mirror;
  shared variable jngbrf : file_value_mirror;
  shared variable psaghkc : integer_value_mirror;
begin
  chzqj : entity work.bkutucrd
    port map (iblphvoc => psaghkc, uaqily => jngbrf, qdwn => kfcincj, fyqfrryf => xlweym);
  osgefuupmp : entity work.bkutucrd
    port map (iblphvoc => v, uaqily => gqassu, qdwn => cwgm, fyqfrryf => iwt);
  
  -- Single-driven assignments
  af <= 241;
end pfmgx;

use std.reflection.all;

entity ftrxnzntat is
  port (bajpbwzn : inout record_subtype_mirror; zqmsxe : inout floating_value_mirror);
end ftrxnzntat;

use std.reflection.all;

architecture ggdsgyab of ftrxnzntat is
  shared variable svr : value_mirror;
  shared variable s : record_value_mirror;
  shared variable cliwtyseik : file_value_mirror;
  shared variable ckffzujgim : integer_value_mirror;
begin
  osb : entity work.bkutucrd
    port map (iblphvoc => ckffzujgim, uaqily => cliwtyseik, qdwn => s, fyqfrryf => svr);
end ggdsgyab;

use std.reflection.all;

entity osezbndlz is
  port (npxrz : inout floating_value_mirror);
end osezbndlz;

use std.reflection.all;

architecture h of osezbndlz is
  shared variable biazaxpymq : access_subtype_mirror;
  signal qccbmhu : integer;
  shared variable t : record_subtype_mirror;
begin
  zus : entity work.ftrxnzntat
    port map (bajpbwzn => t, zqmsxe => npxrz);
  zakozlyz : entity work.yle
    port map (af => qccbmhu, hvlmex => biazaxpymq);
end h;



-- Seed after: 11845253101606272024,3566912872917928779
