-- Seed: 1471143785988630223,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity jspqjjs is
  port (wvnq : inout std_logic; jomycew : in bit; ozftwt : in std_logic);
end jspqjjs;

architecture zxfhw of jspqjjs is
  
begin
  -- Multi-driven assignments
  wvnq <= 'L';
  wvnq <= 'H';
end zxfhw;

entity ioz is
  port (tfcdyit : in time; lkzbrz : linkage time; xenqvgrgo : in character);
end ioz;

library ieee;
use ieee.std_logic_1164.all;

architecture ihlnai of ioz is
  signal zkamyxrrw : std_logic;
  signal iru : bit;
  signal iykr : std_logic;
  signal dtltmkb : std_logic;
  signal myziy : bit;
  signal ikzg : std_logic;
begin
  ecihtxcwtq : entity work.jspqjjs
    port map (wvnq => ikzg, jomycew => myziy, ozftwt => dtltmkb);
  enedqpycnw : entity work.jspqjjs
    port map (wvnq => iykr, jomycew => iru, ozftwt => zkamyxrrw);
  fcclmggjc : entity work.jspqjjs
    port map (wvnq => zkamyxrrw, jomycew => myziy, ozftwt => ikzg);
  
  -- Single-driven assignments
  myziy <= '1';
  iru <= '1';
end ihlnai;

library ieee;
use ieee.std_logic_1164.all;

entity kue is
  port (i : buffer std_logic_vector(3 to 4); miaoevaa : linkage boolean_vector(4 to 3); lhacll : buffer real);
end kue;

library ieee;
use ieee.std_logic_1164.all;

architecture qhst of kue is
  signal m : std_logic;
  signal omnimrqc : bit;
  signal mciqkn : bit;
  signal iwhcjvixmw : std_logic;
  signal qh : std_logic;
  signal hvpeyldn : bit;
  signal aevxq : std_logic;
begin
  ds : entity work.jspqjjs
    port map (wvnq => aevxq, jomycew => hvpeyldn, ozftwt => qh);
  p : entity work.jspqjjs
    port map (wvnq => iwhcjvixmw, jomycew => mciqkn, ozftwt => aevxq);
  lvknpgzd : entity work.jspqjjs
    port map (wvnq => aevxq, jomycew => omnimrqc, ozftwt => m);
  
  -- Single-driven assignments
  lhacll <= 8#4.75#;
  hvpeyldn <= '0';
  omnimrqc <= '1';
  
  -- Multi-driven assignments
  aevxq <= '1';
end qhst;



-- Seed after: 8549736332920666038,15300320181035395489
