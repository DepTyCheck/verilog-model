-- Seed: 16800985603157926552,14426950258250697445

use std.reflection.all;

entity krhojdylm is
  port (lkmjycjyvb : inout physical_value_mirror; n : in boolean_vector(4 downto 1); g : in integer; qy : inout access_value_mirror);
end krhojdylm;

architecture grcgy of krhojdylm is
  
begin
  
end grcgy;

use std.reflection.all;

entity weve is
  port (pjncybtqf : inout subtype_mirror; spms : inout enumeration_subtype_mirror);
end weve;

use std.reflection.all;

architecture v of weve is
  shared variable qsbpka : access_value_mirror;
  signal lnwfkxy : integer;
  signal qq : boolean_vector(4 downto 1);
  shared variable ky : physical_value_mirror;
  shared variable aikawvbbb : access_value_mirror;
  signal pchgxkcvh : boolean_vector(4 downto 1);
  shared variable yepj : physical_value_mirror;
  shared variable kbsneofj : access_value_mirror;
  signal avfcbx : integer;
  signal tbydm : boolean_vector(4 downto 1);
  shared variable uestzylzs : physical_value_mirror;
begin
  kfa : entity work.krhojdylm
    port map (lkmjycjyvb => uestzylzs, n => tbydm, g => avfcbx, qy => kbsneofj);
  dxona : entity work.krhojdylm
    port map (lkmjycjyvb => yepj, n => pchgxkcvh, g => avfcbx, qy => aikawvbbb);
  gan : entity work.krhojdylm
    port map (lkmjycjyvb => ky, n => qq, g => lnwfkxy, qy => qsbpka);
  
  -- Single-driven assignments
  tbydm <= tbydm;
  avfcbx <= avfcbx;
end v;

use std.reflection.all;

entity kjiei is
  port (iadtkdd : inout integer_value_mirror);
end kjiei;

use std.reflection.all;

architecture essmnvzwhk of kjiei is
  shared variable bjoacccn : access_value_mirror;
  signal wkrgo : boolean_vector(4 downto 1);
  shared variable zjem : physical_value_mirror;
  shared variable j : access_value_mirror;
  signal teb : integer;
  signal i : boolean_vector(4 downto 1);
  shared variable tybwnnyu : physical_value_mirror;
  shared variable pntez : access_value_mirror;
  signal w : integer;
  signal zc : boolean_vector(4 downto 1);
  shared variable tpysu : physical_value_mirror;
begin
  qqoyg : entity work.krhojdylm
    port map (lkmjycjyvb => tpysu, n => zc, g => w, qy => pntez);
  ipfezj : entity work.krhojdylm
    port map (lkmjycjyvb => tybwnnyu, n => i, g => teb, qy => j);
  eyipd : entity work.krhojdylm
    port map (lkmjycjyvb => zjem, n => wkrgo, g => w, qy => bjoacccn);
  
  -- Single-driven assignments
  zc <= (FALSE, FALSE, FALSE, FALSE);
end essmnvzwhk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity byjwhbjbqt is
  port (lmjnag : inout enumeration_subtype_mirror; wbyohue : inout std_logic_vector(3 downto 2); kfwzrrbzy : inout bit);
end byjwhbjbqt;

use std.reflection.all;

architecture pty of byjwhbjbqt is
  shared variable rwgq : access_value_mirror;
  signal fgiglggzhx : integer;
  shared variable koclajsx : physical_value_mirror;
  shared variable xrewwzap : integer_value_mirror;
  shared variable ubvtbi : access_value_mirror;
  signal bfmdtbe : integer;
  signal wwpslm : boolean_vector(4 downto 1);
  shared variable wt : physical_value_mirror;
begin
  u : entity work.krhojdylm
    port map (lkmjycjyvb => wt, n => wwpslm, g => bfmdtbe, qy => ubvtbi);
  mxkatw : entity work.kjiei
    port map (iadtkdd => xrewwzap);
  j : entity work.krhojdylm
    port map (lkmjycjyvb => koclajsx, n => wwpslm, g => fgiglggzhx, qy => rwgq);
  
  -- Single-driven assignments
  kfwzrrbzy <= kfwzrrbzy;
end pty;



-- Seed after: 4634764298175867918,14426950258250697445
