-- Seed: 294918051936787589,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity rotk is
  port (daltgap : linkage integer; dqzfwgwak : in std_logic_vector(3 to 2); a : linkage std_logic_vector(3 to 3));
end rotk;

architecture mduwsqvenu of rotk is
  
begin
  
end mduwsqvenu;

library ieee;
use ieee.std_logic_1164.all;

entity tqqy is
  port (og : in std_logic_vector(0 to 4); kzts : in time);
end tqqy;

library ieee;
use ieee.std_logic_1164.all;

architecture zydwedid of tqqy is
  signal edwyjh : integer;
  signal gefjo : std_logic_vector(3 to 2);
  signal oqnhwyzcc : integer;
  signal hqq : std_logic_vector(3 to 3);
  signal zujztf : std_logic_vector(3 to 2);
  signal ldfvcm : integer;
  signal vljifb : std_logic_vector(3 to 3);
  signal vhgd : std_logic_vector(3 to 2);
  signal hjpgevu : integer;
begin
  vfibwqfoi : entity work.rotk
    port map (daltgap => hjpgevu, dqzfwgwak => vhgd, a => vljifb);
  ltrfxjyfam : entity work.rotk
    port map (daltgap => ldfvcm, dqzfwgwak => zujztf, a => hqq);
  wqlbhgmxd : entity work.rotk
    port map (daltgap => oqnhwyzcc, dqzfwgwak => gefjo, a => vljifb);
  b : entity work.rotk
    port map (daltgap => edwyjh, dqzfwgwak => gefjo, a => vljifb);
  
  -- Multi-driven assignments
  hqq <= "Z";
end zydwedid;

entity ttoxdl is
  port (zll : in real; abhl : linkage real);
end ttoxdl;

library ieee;
use ieee.std_logic_1164.all;

architecture qlrxc of ttoxdl is
  signal bhcmhx : time;
  signal vy : std_logic_vector(0 to 4);
  signal tro : std_logic_vector(3 to 3);
  signal fqvpio : integer;
  signal ixlntrot : std_logic_vector(3 to 3);
  signal hjo : integer;
  signal lizhqeu : std_logic_vector(3 to 3);
  signal sd : std_logic_vector(3 to 2);
  signal ukir : integer;
begin
  rrsiphyj : entity work.rotk
    port map (daltgap => ukir, dqzfwgwak => sd, a => lizhqeu);
  za : entity work.rotk
    port map (daltgap => hjo, dqzfwgwak => sd, a => ixlntrot);
  azxe : entity work.rotk
    port map (daltgap => fqvpio, dqzfwgwak => sd, a => tro);
  ku : entity work.tqqy
    port map (og => vy, kzts => bhcmhx);
  
  -- Single-driven assignments
  bhcmhx <= 2#1.1010# ns;
  
  -- Multi-driven assignments
  sd <= sd;
end qlrxc;

library ieee;
use ieee.std_logic_1164.all;

entity akrozss is
  port (kpipz : inout std_logic_vector(3 to 1); wsye : buffer real; lon : in time);
end akrozss;

library ieee;
use ieee.std_logic_1164.all;

architecture xir of akrozss is
  signal kepvsylqd : time;
  signal wdule : std_logic_vector(0 to 4);
  signal sndjrcq : real;
  signal m : std_logic_vector(3 to 3);
  signal aufkliqpxv : std_logic_vector(3 to 2);
  signal therinys : integer;
begin
  ugqdng : entity work.rotk
    port map (daltgap => therinys, dqzfwgwak => aufkliqpxv, a => m);
  bfolfyxlj : entity work.ttoxdl
    port map (zll => sndjrcq, abhl => wsye);
  iz : entity work.tqqy
    port map (og => wdule, kzts => kepvsylqd);
  
  -- Single-driven assignments
  sndjrcq <= 8#5.2_1_4#;
  kepvsylqd <= 0434.4 us;
  
  -- Multi-driven assignments
  wdule <= ('H', '1', '1', 'Z', 'U');
end xir;



-- Seed after: 12941072706396058543,16461708287571398341
