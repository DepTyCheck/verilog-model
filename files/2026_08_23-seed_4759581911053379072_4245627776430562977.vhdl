-- Seed: 4759581911053379072,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity fwttecixd is
  port (l : buffer std_logic_vector(2 to 4); lasim : linkage integer; wmd : buffer time; ua : out real);
end fwttecixd;

architecture rjiqntvj of fwttecixd is
  
begin
  -- Single-driven assignments
  ua <= 2#101.0101#;
  wmd <= 0 sec;
  
  -- Multi-driven assignments
  l <= ('X', 'U', '0');
end rjiqntvj;

library ieee;
use ieee.std_logic_1164.all;

entity fkhau is
  port (g : inout std_logic_vector(1 downto 1); wgsqgpshl : inout integer);
end fkhau;

library ieee;
use ieee.std_logic_1164.all;

architecture tdcjsnl of fkhau is
  signal k : real;
  signal wsbks : time;
  signal xzzmlqyon : integer;
  signal d : real;
  signal nlasynnqwu : time;
  signal nh : integer;
  signal bklgoqtc : std_logic_vector(2 to 4);
  signal am : real;
  signal qavyst : time;
  signal nbzenmbrh : integer;
  signal z : real;
  signal dqlwibz : time;
  signal azppluit : integer;
  signal rzn : std_logic_vector(2 to 4);
begin
  uxdxgiiq : entity work.fwttecixd
    port map (l => rzn, lasim => azppluit, wmd => dqlwibz, ua => z);
  kxfv : entity work.fwttecixd
    port map (l => rzn, lasim => nbzenmbrh, wmd => qavyst, ua => am);
  n : entity work.fwttecixd
    port map (l => bklgoqtc, lasim => nh, wmd => nlasynnqwu, ua => d);
  as : entity work.fwttecixd
    port map (l => rzn, lasim => xzzmlqyon, wmd => wsbks, ua => k);
  
  -- Single-driven assignments
  wgsqgpshl <= 1_0;
  
  -- Multi-driven assignments
  g <= (others => 'H');
  rzn <= "H0Z";
end tdcjsnl;

library ieee;
use ieee.std_logic_1164.all;

entity jxq is
  port (lvgfdwz : out string(3 downto 5); im : in time; kpxddak : inout std_logic);
end jxq;

library ieee;
use ieee.std_logic_1164.all;

architecture mj of jxq is
  signal nxuwqmgkkk : real;
  signal lcwgg : time;
  signal egahall : integer;
  signal ug : std_logic_vector(2 to 4);
  signal qv : integer;
  signal khvqafzzc : std_logic_vector(1 downto 1);
  signal vqdmdtdjk : real;
  signal trqnrsef : time;
  signal znf : integer;
  signal ppfuez : real;
  signal jimhbufu : time;
  signal r : integer;
  signal gfbbwoty : std_logic_vector(2 to 4);
begin
  zcset : entity work.fwttecixd
    port map (l => gfbbwoty, lasim => r, wmd => jimhbufu, ua => ppfuez);
  mglsigrs : entity work.fwttecixd
    port map (l => gfbbwoty, lasim => znf, wmd => trqnrsef, ua => vqdmdtdjk);
  ctxgutq : entity work.fkhau
    port map (g => khvqafzzc, wgsqgpshl => qv);
  ipmlm : entity work.fwttecixd
    port map (l => ug, lasim => egahall, wmd => lcwgg, ua => nxuwqmgkkk);
  
  -- Single-driven assignments
  lvgfdwz <= "";
  
  -- Multi-driven assignments
  kpxddak <= kpxddak;
  ug <= gfbbwoty;
  khvqafzzc <= "0";
  ug <= ('Z', 'H', 'H');
end mj;



-- Seed after: 1126910417506131977,4245627776430562977
