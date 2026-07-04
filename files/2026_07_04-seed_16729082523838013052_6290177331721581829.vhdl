-- Seed: 16729082523838013052,6290177331721581829

use std.reflection.all;

entity phrq is
  port (ulomhdbcm : inout time; zufhggtbys : in character; twzkmzr : inout physical_subtype_mirror);
end phrq;

architecture bwjhe of phrq is
  
begin
  -- Single-driven assignments
  ulomhdbcm <= 2#0_1_0.0_0_1# fs;
end bwjhe;

use std.reflection.all;

entity gckif is
  port (zi : inout record_subtype_mirror; benwnznle : inout protected_value_mirror; hqn : inout floating_subtype_mirror);
end gckif;

use std.reflection.all;

architecture ycjbngmec of gckif is
  shared variable aadthxajnm : physical_subtype_mirror;
  signal kd : character;
  signal jfdpokm : time;
  shared variable xqw : physical_subtype_mirror;
  signal zduxlzikci : time;
  shared variable lotwmkfif : physical_subtype_mirror;
  signal i : character;
  signal keixke : time;
begin
  myankqzxi : entity work.phrq
    port map (ulomhdbcm => keixke, zufhggtbys => i, twzkmzr => lotwmkfif);
  r : entity work.phrq
    port map (ulomhdbcm => zduxlzikci, zufhggtbys => i, twzkmzr => xqw);
  vfncvhk : entity work.phrq
    port map (ulomhdbcm => jfdpokm, zufhggtbys => kd, twzkmzr => aadthxajnm);
end ycjbngmec;

use std.reflection.all;

entity idxuq is
  port (sbosvjbvj : inout file_subtype_mirror; suedfe : inout enumeration_value_mirror; ynpim : inout enumeration_subtype_mirror);
end idxuq;

use std.reflection.all;

architecture kjzcazyznh of idxuq is
  shared variable xmlnl : physical_subtype_mirror;
  signal nqlxqbgr : time;
  shared variable w : floating_subtype_mirror;
  shared variable zia : protected_value_mirror;
  shared variable ugk : record_subtype_mirror;
  shared variable fqqkyi : physical_subtype_mirror;
  signal g : time;
  shared variable jnypf : physical_subtype_mirror;
  signal uzfmzdg : character;
  signal uminxvw : time;
begin
  qjftip : entity work.phrq
    port map (ulomhdbcm => uminxvw, zufhggtbys => uzfmzdg, twzkmzr => jnypf);
  roue : entity work.phrq
    port map (ulomhdbcm => g, zufhggtbys => uzfmzdg, twzkmzr => fqqkyi);
  ki : entity work.gckif
    port map (zi => ugk, benwnznle => zia, hqn => w);
  oztwtqeh : entity work.phrq
    port map (ulomhdbcm => nqlxqbgr, zufhggtbys => uzfmzdg, twzkmzr => xmlnl);
end kjzcazyznh;

entity gqtzxdz is
  port (bclte : out severity_level);
end gqtzxdz;

use std.reflection.all;

architecture grm of gqtzxdz is
  shared variable tpcaxkk : physical_subtype_mirror;
  signal mcqwqzdta : time;
  shared variable mft : enumeration_subtype_mirror;
  shared variable yzvhiu : enumeration_value_mirror;
  shared variable kxsrin : file_subtype_mirror;
  shared variable berbfn : physical_subtype_mirror;
  signal ebb : character;
  signal utzxccgpqa : time;
  shared variable huii : floating_subtype_mirror;
  shared variable va : protected_value_mirror;
  shared variable y : record_subtype_mirror;
begin
  xkyaaryo : entity work.gckif
    port map (zi => y, benwnznle => va, hqn => huii);
  wvoc : entity work.phrq
    port map (ulomhdbcm => utzxccgpqa, zufhggtbys => ebb, twzkmzr => berbfn);
  mcfzub : entity work.idxuq
    port map (sbosvjbvj => kxsrin, suedfe => yzvhiu, ynpim => mft);
  ekpamjjiyn : entity work.phrq
    port map (ulomhdbcm => mcqwqzdta, zufhggtbys => ebb, twzkmzr => tpcaxkk);
  
  -- Single-driven assignments
  bclte <= ERROR;
end grm;



-- Seed after: 12968704144863494560,6290177331721581829
