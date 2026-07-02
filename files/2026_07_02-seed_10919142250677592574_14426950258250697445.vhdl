-- Seed: 10919142250677592574,14426950258250697445

use std.reflection.all;

entity uvzvo is
  port (sqdq : inout physical_value_mirror);
end uvzvo;

architecture ugtrgpbjlu of uvzvo is
  
begin
  
end ugtrgpbjlu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity lgrac is
  port (vv : inout access_subtype_mirror; wz : linkage std_logic_vector(4 downto 2); efos : in std_logic);
end lgrac;

use std.reflection.all;

architecture qcd of lgrac is
  shared variable fshnjei : physical_value_mirror;
  shared variable elzofdvnq : physical_value_mirror;
begin
  pzx : entity work.uvzvo
    port map (sqdq => elzofdvnq);
  hohfse : entity work.uvzvo
    port map (sqdq => fshnjei);
end qcd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nigo is
  port (ydginpnna : out std_logic_vector(1 to 4); tdgmnzkzut : inout protected_value_mirror; btxhbzckrw : inout protected_subtype_mirror);
end nigo;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture aoaznwxfm of nigo is
  signal rr : std_logic;
  signal jsuhxxil : std_logic_vector(4 downto 2);
  shared variable szd : access_subtype_mirror;
  signal i : std_logic;
  signal fgdokuv : std_logic_vector(4 downto 2);
  shared variable rtzece : access_subtype_mirror;
  shared variable iqfipklt : physical_value_mirror;
begin
  pnqurbp : entity work.uvzvo
    port map (sqdq => iqfipklt);
  j : entity work.lgrac
    port map (vv => rtzece, wz => fgdokuv, efos => i);
  rqiudnee : entity work.lgrac
    port map (vv => szd, wz => jsuhxxil, efos => rr);
  
  -- Multi-driven assignments
  ydginpnna <= "H0WU";
  i <= 'X';
end aoaznwxfm;

use std.reflection.all;

entity jttxdflwsb is
  port (muorxp : inout floating_value_mirror; eeimgzka : inout bit; cdzrghmb : out integer_vector(4 downto 3); mzdcn : inout integer_subtype_mirror);
end jttxdflwsb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture oxoffmnwjj of jttxdflwsb is
  signal jzaitosc : std_logic;
  signal ps : std_logic_vector(4 downto 2);
  shared variable yomsrqy : access_subtype_mirror;
  shared variable feqhartyw : physical_value_mirror;
  shared variable fi : physical_value_mirror;
  shared variable npt : physical_value_mirror;
begin
  mffeodkfd : entity work.uvzvo
    port map (sqdq => npt);
  fxaxz : entity work.uvzvo
    port map (sqdq => fi);
  qgminfe : entity work.uvzvo
    port map (sqdq => feqhartyw);
  jeoc : entity work.lgrac
    port map (vv => yomsrqy, wz => ps, efos => jzaitosc);
  
  -- Single-driven assignments
  cdzrghmb <= cdzrghmb;
  eeimgzka <= eeimgzka;
  
  -- Multi-driven assignments
  ps <= ps;
  ps <= ('0', '-', 'X');
  ps <= "ZUH";
end oxoffmnwjj;



-- Seed after: 2700063757499534794,14426950258250697445
