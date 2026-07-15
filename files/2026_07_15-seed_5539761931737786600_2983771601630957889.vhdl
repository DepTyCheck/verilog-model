-- Seed: 5539761931737786600,2983771601630957889

entity oxij is
  port (nyropmslcd : in time);
end oxij;

architecture rlrgf of oxij is
  
begin
  
end rlrgf;

use std.reflection.all;

entity dr is
  port ( variable mvo : inout physical_value_mirror_pt
  ; variable ssjz : inout record_subtype_mirror_pt
  ; variable wmflsoezc : inout array_value_mirror_pt
  ; variable p : inout file_subtype_mirror_pt
  );
end dr;

architecture znmbm of dr is
  signal hqe : time;
  signal s : time;
begin
  x : entity work.oxij
    port map (nyropmslcd => s);
  hlp : entity work.oxij
    port map (nyropmslcd => hqe);
  ctfo : entity work.oxij
    port map (nyropmslcd => s);
  
  -- Single-driven assignments
  s <= 8#10# fs;
end znmbm;

use std.reflection.all;

entity u is
  port (variable t : inout enumeration_subtype_mirror_pt; umtixczv : inout integer; variable cnjwrqecu : inout subtype_mirror_pt);
end u;

use std.reflection.all;

architecture dpvnkbcy of u is
  shared variable kztbhunt : file_subtype_mirror_pt;
  shared variable vrnuzn : array_value_mirror_pt;
  shared variable lfdrjuuzh : record_subtype_mirror_pt;
  shared variable cqfnabqnll : physical_value_mirror_pt;
  signal ounmsq : time;
  shared variable nbk : file_subtype_mirror_pt;
  shared variable jrbnq : array_value_mirror_pt;
  shared variable xrdmdlcqf : record_subtype_mirror_pt;
  shared variable yo : physical_value_mirror_pt;
begin
  l : entity work.dr
    port map (mvo => yo, ssjz => xrdmdlcqf, wmflsoezc => jrbnq, p => nbk);
  dfsgk : entity work.oxij
    port map (nyropmslcd => ounmsq);
  ihfv : entity work.dr
    port map (mvo => cqfnabqnll, ssjz => lfdrjuuzh, wmflsoezc => vrnuzn, p => kztbhunt);
  
  -- Single-driven assignments
  umtixczv <= umtixczv;
  ounmsq <= ounmsq;
end dpvnkbcy;

use std.reflection.all;

entity dfypthw is
  port (variable ertfmnhjj : inout record_subtype_mirror_pt; variable lnqaibn : inout enumeration_value_mirror_pt);
end dfypthw;

use std.reflection.all;

architecture p of dfypthw is
  shared variable ipika : file_subtype_mirror_pt;
  shared variable ozknxnavl : array_value_mirror_pt;
  shared variable melitqaoe : record_subtype_mirror_pt;
  shared variable wtvsmcbczg : physical_value_mirror_pt;
  shared variable vujxyyhrcy : subtype_mirror_pt;
  signal slnikykevc : integer;
  shared variable uysuhpbjok : enumeration_subtype_mirror_pt;
  shared variable k : file_subtype_mirror_pt;
  shared variable jcr : array_value_mirror_pt;
  shared variable qcjuwqj : record_subtype_mirror_pt;
  shared variable lhx : physical_value_mirror_pt;
begin
  xppfdreke : entity work.dr
    port map (mvo => lhx, ssjz => qcjuwqj, wmflsoezc => jcr, p => k);
  mqdumaz : entity work.u
    port map (t => uysuhpbjok, umtixczv => slnikykevc, cnjwrqecu => vujxyyhrcy);
  bewilt : entity work.dr
    port map (mvo => wtvsmcbczg, ssjz => melitqaoe, wmflsoezc => ozknxnavl, p => ipika);
end p;



-- Seed after: 11375617570729780624,2983771601630957889
