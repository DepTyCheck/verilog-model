-- Seed: 5638513095234151987,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nuwanuo is
  port (laon : inout severity_level; wsqmqzpdb : inout protected_value_mirror; tzb : out std_logic_vector(2 to 0));
end nuwanuo;

architecture xzi of nuwanuo is
  
begin
  -- Single-driven assignments
  laon <= FAILURE;
  
  -- Multi-driven assignments
  tzb <= (others => '0');
end xzi;

use std.reflection.all;

entity vimu is
  port (xds : inout file_subtype_mirror; f : inout floating_value_mirror; p : inout protected_subtype_mirror);
end vimu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture dd of vimu is
  signal rpleqvupbe : std_logic_vector(2 to 0);
  shared variable anqhpcjix : protected_value_mirror;
  signal wam : severity_level;
  shared variable rrgfkdirqw : protected_value_mirror;
  signal yqpevzjih : severity_level;
  signal qizwchhdgp : std_logic_vector(2 to 0);
  shared variable ub : protected_value_mirror;
  signal wstvktdd : severity_level;
begin
  qipm : entity work.nuwanuo
    port map (laon => wstvktdd, wsqmqzpdb => ub, tzb => qizwchhdgp);
  pir : entity work.nuwanuo
    port map (laon => yqpevzjih, wsqmqzpdb => rrgfkdirqw, tzb => qizwchhdgp);
  ht : entity work.nuwanuo
    port map (laon => wam, wsqmqzpdb => anqhpcjix, tzb => rpleqvupbe);
  
  -- Multi-driven assignments
  rpleqvupbe <= qizwchhdgp;
  qizwchhdgp <= "";
end dd;

library ieee;
use ieee.std_logic_1164.all;

entity ruc is
  port (orxwx : in string(1 to 4); knneowy : out character; gqvt : linkage std_logic_vector(4 downto 3));
end ruc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture u of ruc is
  signal hmym : std_logic_vector(2 to 0);
  shared variable sbkai : protected_value_mirror;
  signal fpubro : severity_level;
  shared variable ffmzkdauw : protected_value_mirror;
  signal sr : severity_level;
  signal xmtwhuyv : std_logic_vector(2 to 0);
  shared variable fhynsm : protected_value_mirror;
  signal umrf : severity_level;
begin
  aqnlsmp : entity work.nuwanuo
    port map (laon => umrf, wsqmqzpdb => fhynsm, tzb => xmtwhuyv);
  mdyat : entity work.nuwanuo
    port map (laon => sr, wsqmqzpdb => ffmzkdauw, tzb => xmtwhuyv);
  zyrk : entity work.nuwanuo
    port map (laon => fpubro, wsqmqzpdb => sbkai, tzb => hmym);
  
  -- Single-driven assignments
  knneowy <= knneowy;
  
  -- Multi-driven assignments
  xmtwhuyv <= (others => '0');
  xmtwhuyv <= "";
  xmtwhuyv <= "";
end u;



-- Seed after: 7456167274491015092,14426950258250697445
