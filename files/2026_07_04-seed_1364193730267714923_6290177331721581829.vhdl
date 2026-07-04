-- Seed: 1364193730267714923,6290177331721581829

use std.reflection.all;

entity kksuf is
  port (nzaqtc : in character; uqfqmzzpj : inout array_subtype_mirror; pcofcznr : inout enumeration_subtype_mirror; rgiz : linkage character);
end kksuf;

architecture clfzxa of kksuf is
  
begin
  
end clfzxa;

use std.reflection.all;

entity fha is
  port (tdkpyiggyl : inout protected_subtype_mirror; mbwmicb : inout array_subtype_mirror);
end fha;

use std.reflection.all;

architecture pt of fha is
  shared variable ug : enumeration_subtype_mirror;
  signal jb : character;
  shared variable qhtmgahx : enumeration_subtype_mirror;
  shared variable dlkbot : array_subtype_mirror;
  signal eqkvilbjqf : character;
  shared variable j : enumeration_subtype_mirror;
  shared variable zqcoiie : array_subtype_mirror;
  signal gnw : character;
begin
  lt : entity work.kksuf
    port map (nzaqtc => gnw, uqfqmzzpj => zqcoiie, pcofcznr => j, rgiz => eqkvilbjqf);
  usvg : entity work.kksuf
    port map (nzaqtc => gnw, uqfqmzzpj => dlkbot, pcofcznr => qhtmgahx, rgiz => jb);
  fvnkuuhtr : entity work.kksuf
    port map (nzaqtc => gnw, uqfqmzzpj => mbwmicb, pcofcznr => ug, rgiz => gnw);
end pt;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity w is
  port (qbym : inout std_logic_vector(3 downto 4); xqwpjjojsb : linkage time; ijsehnegv : inout protected_value_mirror);
end w;

use std.reflection.all;

architecture qr of w is
  signal le : character;
  shared variable vumnwop : enumeration_subtype_mirror;
  shared variable vces : array_subtype_mirror;
  signal ktqp : character;
  shared variable belfpilia : enumeration_subtype_mirror;
  shared variable eunowxb : array_subtype_mirror;
  shared variable rktgjux : array_subtype_mirror;
  shared variable hbzuedamf : protected_subtype_mirror;
  signal psf : character;
  shared variable htbqycaw : enumeration_subtype_mirror;
  shared variable vy : array_subtype_mirror;
  signal fchap : character;
begin
  tuccajxok : entity work.kksuf
    port map (nzaqtc => fchap, uqfqmzzpj => vy, pcofcznr => htbqycaw, rgiz => psf);
  bordzzhost : entity work.fha
    port map (tdkpyiggyl => hbzuedamf, mbwmicb => rktgjux);
  kdjwvhd : entity work.kksuf
    port map (nzaqtc => fchap, uqfqmzzpj => eunowxb, pcofcznr => belfpilia, rgiz => fchap);
  gd : entity work.kksuf
    port map (nzaqtc => ktqp, uqfqmzzpj => vces, pcofcznr => vumnwop, rgiz => le);
  
  -- Single-driven assignments
  ktqp <= 'k';
end qr;

entity vzpeaiuxg is
  port (momqsv : in integer; fm : out integer);
end vzpeaiuxg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gxxjuqscr of vzpeaiuxg is
  shared variable tqyhubj : array_subtype_mirror;
  shared variable cjelgr : protected_subtype_mirror;
  signal cowfsqgdj : character;
  shared variable ltl : enumeration_subtype_mirror;
  shared variable genhiadd : array_subtype_mirror;
  signal xkoimsmqiq : character;
  shared variable nyduak : protected_value_mirror;
  signal rmzbbedr : time;
  signal mv : std_logic_vector(3 downto 4);
  shared variable iy : array_subtype_mirror;
  shared variable eerptoiwto : protected_subtype_mirror;
begin
  n : entity work.fha
    port map (tdkpyiggyl => eerptoiwto, mbwmicb => iy);
  f : entity work.w
    port map (qbym => mv, xqwpjjojsb => rmzbbedr, ijsehnegv => nyduak);
  hdqoq : entity work.kksuf
    port map (nzaqtc => xkoimsmqiq, uqfqmzzpj => genhiadd, pcofcznr => ltl, rgiz => cowfsqgdj);
  aidrvshgr : entity work.fha
    port map (tdkpyiggyl => cjelgr, mbwmicb => tqyhubj);
  
  -- Single-driven assignments
  fm <= 16#6#;
end gxxjuqscr;



-- Seed after: 6814568452476341637,6290177331721581829
