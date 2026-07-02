-- Seed: 11252760030253991232,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nv is
  port (iu : inout array_subtype_mirror; qfmigoe : in std_logic; wyjixwk : inout bit; qm : linkage string(5 downto 2));
end nv;

architecture uegjhw of nv is
  
begin
  
end uegjhw;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity h is
  port (fkcw : inout value_mirror; hhls : inout enumeration_subtype_mirror; fctdnhmora : inout enumeration_subtype_mirror; pptuocikj : inout std_logic);
end h;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture yjdvvc of h is
  signal ovucn : string(5 downto 2);
  signal dndgahi : bit;
  shared variable exkaq : array_subtype_mirror;
  signal exm : string(5 downto 2);
  signal jlkpl : bit;
  signal ipobt : std_logic;
  shared variable iopxg : array_subtype_mirror;
  signal ik : string(5 downto 2);
  signal itasdg : bit;
  signal hx : std_logic;
  shared variable zfaoemmmv : array_subtype_mirror;
begin
  lie : entity work.nv
    port map (iu => zfaoemmmv, qfmigoe => hx, wyjixwk => itasdg, qm => ik);
  wmelib : entity work.nv
    port map (iu => iopxg, qfmigoe => ipobt, wyjixwk => jlkpl, qm => exm);
  sfn : entity work.nv
    port map (iu => exkaq, qfmigoe => pptuocikj, wyjixwk => dndgahi, qm => ovucn);
  
  -- Multi-driven assignments
  pptuocikj <= 'U';
end yjdvvc;



-- Seed after: 15493815415141627220,14426950258250697445
