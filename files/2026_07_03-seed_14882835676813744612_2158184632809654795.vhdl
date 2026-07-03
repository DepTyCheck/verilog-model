-- Seed: 14882835676813744612,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity gv is
  port (ntatzt : buffer severity_level; pnn : out std_logic; k : inout array_value_mirror; prref : inout enumeration_subtype_mirror);
end gv;

architecture mgm of gv is
  
begin
  -- Single-driven assignments
  ntatzt <= ERROR;
end mgm;

entity flz is
  port (khwha : out time);
end flz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture rdyr of flz is
  shared variable pjfd : enumeration_subtype_mirror;
  shared variable tbqkun : array_value_mirror;
  signal bpupqbavt : severity_level;
  shared variable pd : enumeration_subtype_mirror;
  shared variable qa : array_value_mirror;
  signal pftb : std_logic;
  signal s : severity_level;
  shared variable pha : enumeration_subtype_mirror;
  shared variable hrklfopcp : array_value_mirror;
  signal adcugnxc : std_logic;
  signal gn : severity_level;
  shared variable jiwl : enumeration_subtype_mirror;
  shared variable wogeuks : array_value_mirror;
  signal uxxujfcm : std_logic;
  signal kfnygb : severity_level;
begin
  pcxtiipy : entity work.gv
    port map (ntatzt => kfnygb, pnn => uxxujfcm, k => wogeuks, prref => jiwl);
  jzvi : entity work.gv
    port map (ntatzt => gn, pnn => adcugnxc, k => hrklfopcp, prref => pha);
  msznym : entity work.gv
    port map (ntatzt => s, pnn => pftb, k => qa, prref => pd);
  uvlbm : entity work.gv
    port map (ntatzt => bpupqbavt, pnn => uxxujfcm, k => tbqkun, prref => pjfd);
  
  -- Single-driven assignments
  khwha <= 1 sec;
  
  -- Multi-driven assignments
  uxxujfcm <= '0';
  uxxujfcm <= uxxujfcm;
  uxxujfcm <= '-';
  uxxujfcm <= '-';
end rdyr;



-- Seed after: 16385427038554060344,2158184632809654795
