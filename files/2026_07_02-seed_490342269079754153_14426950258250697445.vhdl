-- Seed: 490342269079754153,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity usovqda is
  port (quvwuipt : buffer std_logic; jmziwbn : inout access_subtype_mirror);
end usovqda;

architecture mvmuflzxxd of usovqda is
  
begin
  
end mvmuflzxxd;

use std.reflection.all;

entity jlwhpy is
  port (cou : inout integer_subtype_mirror);
end jlwhpy;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture bcphunezmc of jlwhpy is
  shared variable cftz : access_subtype_mirror;
  signal fupf : std_logic;
begin
  vgposjh : entity work.usovqda
    port map (quvwuipt => fupf, jmziwbn => cftz);
end bcphunezmc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wfvj is
  port (tdcmcbvfqp : linkage std_logic_vector(0 to 2); hdldjppcrt : inout integer_value_mirror; g : in severity_level; tcdagtjmi : inout character);
end wfvj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture eubya of wfvj is
  shared variable olgmtjjco : access_subtype_mirror;
  shared variable aezkjgurdf : access_subtype_mirror;
  signal eiqjvbzydj : std_logic;
  shared variable jqknbkooxi : access_subtype_mirror;
  signal ayabredjoh : std_logic;
begin
  vnkdb : entity work.usovqda
    port map (quvwuipt => ayabredjoh, jmziwbn => jqknbkooxi);
  oxfj : entity work.usovqda
    port map (quvwuipt => eiqjvbzydj, jmziwbn => aezkjgurdf);
  d : entity work.usovqda
    port map (quvwuipt => ayabredjoh, jmziwbn => olgmtjjco);
  
  -- Single-driven assignments
  tcdagtjmi <= 'x';
  
  -- Multi-driven assignments
  ayabredjoh <= 'Z';
end eubya;

use std.reflection.all;

entity kzh is
  port (gtzthoye : inout enumeration_subtype_mirror; ccfrerwskj : buffer real);
end kzh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture opcqbo of kzh is
  signal gjhvcjuwde : character;
  signal ibrc : severity_level;
  shared variable mwlxazgjqa : integer_value_mirror;
  signal lmcwzzd : std_logic_vector(0 to 2);
  shared variable srzkbjbn : integer_subtype_mirror;
  shared variable pdnbwjzk : access_subtype_mirror;
  signal ckawmchd : std_logic;
begin
  ldmdiym : entity work.usovqda
    port map (quvwuipt => ckawmchd, jmziwbn => pdnbwjzk);
  lkmxj : entity work.jlwhpy
    port map (cou => srzkbjbn);
  xquaa : entity work.wfvj
    port map (tdcmcbvfqp => lmcwzzd, hdldjppcrt => mwlxazgjqa, g => ibrc, tcdagtjmi => gjhvcjuwde);
  
  -- Single-driven assignments
  ccfrerwskj <= ccfrerwskj;
  ibrc <= WARNING;
end opcqbo;



-- Seed after: 6057313417538495393,14426950258250697445
