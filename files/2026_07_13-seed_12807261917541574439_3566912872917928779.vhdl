-- Seed: 12807261917541574439,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity kaobdenymu is
  port (fcyuydfdu : inout integer_subtype_mirror; hxfviyo : inout std_logic; x : buffer std_logic_vector(4 to 3));
end kaobdenymu;

architecture tcuyqd of kaobdenymu is
  
begin
  
end tcuyqd;

use std.reflection.all;

entity nvo is
  port (ee : inout protected_value_mirror; ibkjtunc : inout value_mirror; vi : in time; kzzdy : inout access_value_mirror);
end nvo;

architecture trjxc of nvo is
  
begin
  
end trjxc;

use std.reflection.all;

entity odmxplxxji is
  port (ylgks : in real; hsqdz : inout file_subtype_mirror);
end odmxplxxji;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture nga of odmxplxxji is
  signal abzo : std_logic;
  shared variable yygbgmsifh : integer_subtype_mirror;
  signal duegovfc : std_logic_vector(4 to 3);
  signal cld : std_logic;
  shared variable urf : integer_subtype_mirror;
begin
  fa : entity work.kaobdenymu
    port map (fcyuydfdu => urf, hxfviyo => cld, x => duegovfc);
  iwmopfgz : entity work.kaobdenymu
    port map (fcyuydfdu => yygbgmsifh, hxfviyo => abzo, x => duegovfc);
  
  -- Multi-driven assignments
  cld <= '0';
end nga;



-- Seed after: 11690695741527684194,3566912872917928779
