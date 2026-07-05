-- Seed: 804257603299594448,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity peob is
  port (nadfr : in bit; pzqgux : inout record_subtype_mirror; fkedhd : inout access_value_mirror; yddwbxuz : buffer std_logic_vector(2 downto 3));
end peob;

architecture uutcyonen of peob is
  
begin
  -- Multi-driven assignments
  yddwbxuz <= yddwbxuz;
  yddwbxuz <= "";
  yddwbxuz <= yddwbxuz;
end uutcyonen;

use std.reflection.all;

entity qdfsbhahi is
  port (wsvnp : inout enumeration_subtype_mirror; x : inout physical_value_mirror; nmckno : inout array_subtype_mirror);
end qdfsbhahi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture xvz of qdfsbhahi is
  shared variable hkxvkriiwn : access_value_mirror;
  shared variable j : record_subtype_mirror;
  signal hyedfwf : std_logic_vector(2 downto 3);
  shared variable yqxeupk : access_value_mirror;
  shared variable zqobfou : record_subtype_mirror;
  signal rouhdvbvzr : bit;
begin
  npkrtwvufp : entity work.peob
    port map (nadfr => rouhdvbvzr, pzqgux => zqobfou, fkedhd => yqxeupk, yddwbxuz => hyedfwf);
  ywddmhinq : entity work.peob
    port map (nadfr => rouhdvbvzr, pzqgux => j, fkedhd => hkxvkriiwn, yddwbxuz => hyedfwf);
  
  -- Multi-driven assignments
  hyedfwf <= hyedfwf;
end xvz;

use std.reflection.all;

entity aocsry is
  port (o : inout protected_subtype_mirror; ufrn : inout integer_subtype_mirror; teduvqgsq : inout enumeration_value_mirror);
end aocsry;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zxsq of aocsry is
  signal powvhcx : std_logic_vector(2 downto 3);
  shared variable rcwvqeoc : access_value_mirror;
  shared variable lkssbet : record_subtype_mirror;
  signal nxcm : std_logic_vector(2 downto 3);
  shared variable pxmfpksqfv : access_value_mirror;
  shared variable own : record_subtype_mirror;
  signal nrjqqph : bit;
begin
  lrhoterwto : entity work.peob
    port map (nadfr => nrjqqph, pzqgux => own, fkedhd => pxmfpksqfv, yddwbxuz => nxcm);
  qcl : entity work.peob
    port map (nadfr => nrjqqph, pzqgux => lkssbet, fkedhd => rcwvqeoc, yddwbxuz => powvhcx);
  
  -- Single-driven assignments
  nrjqqph <= nrjqqph;
  
  -- Multi-driven assignments
  nxcm <= powvhcx;
end zxsq;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hl is
  port (kziilyrd : inout physical_subtype_mirror; n : linkage std_logic_vector(0 to 2));
end hl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zx of hl is
  signal gueto : std_logic_vector(2 downto 3);
  shared variable dzu : access_value_mirror;
  shared variable pzwx : record_subtype_mirror;
  signal nsrm : std_logic_vector(2 downto 3);
  shared variable zwvnvykypq : access_value_mirror;
  shared variable upmszowdlz : record_subtype_mirror;
  signal bubcbebhtr : bit;
  signal xuhuqxq : std_logic_vector(2 downto 3);
  shared variable jjxeftic : access_value_mirror;
  shared variable sk : record_subtype_mirror;
  signal dx : bit;
begin
  wpam : entity work.peob
    port map (nadfr => dx, pzqgux => sk, fkedhd => jjxeftic, yddwbxuz => xuhuqxq);
  ouef : entity work.peob
    port map (nadfr => bubcbebhtr, pzqgux => upmszowdlz, fkedhd => zwvnvykypq, yddwbxuz => nsrm);
  ixmxl : entity work.peob
    port map (nadfr => dx, pzqgux => pzwx, fkedhd => dzu, yddwbxuz => gueto);
  
  -- Single-driven assignments
  dx <= dx;
  bubcbebhtr <= '1';
end zx;



-- Seed after: 4801011591791235054,3181554006726329157
