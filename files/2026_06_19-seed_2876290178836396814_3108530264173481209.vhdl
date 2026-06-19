-- Seed: 2876290178836396814,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity fk is
  port (izvefrmz : out time; sm : inout std_logic; psvnib : linkage std_logic_vector(0 to 3));
end fk;

architecture c of fk is
  
begin
  -- Single-driven assignments
  izvefrmz <= 16#0_0_1# us;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity qnlozs is
  port (mvd : out integer; teskyc : inout real_vector(4 to 2); hglqhycrlf : out std_logic);
end qnlozs;

library ieee;
use ieee.std_logic_1164.all;

architecture s of qnlozs is
  signal kfvxbhz : std_logic;
  signal datlfvih : time;
  signal bvlsyxco : std_logic_vector(0 to 3);
  signal iuwzf : std_logic;
  signal hn : time;
  signal bzf : std_logic_vector(0 to 3);
  signal puidx : std_logic;
  signal u : time;
begin
  xnwejcjjm : entity work.fk
    port map (izvefrmz => u, sm => puidx, psvnib => bzf);
  wfpbiykg : entity work.fk
    port map (izvefrmz => hn, sm => iuwzf, psvnib => bvlsyxco);
  uotq : entity work.fk
    port map (izvefrmz => datlfvih, sm => kfvxbhz, psvnib => bvlsyxco);
  
  -- Single-driven assignments
  teskyc <= (others => 0.0);
  mvd <= 34404;
end s;

entity jcxugcvi is
  port (vblwljbzh : buffer real; addqhzv : in time);
end jcxugcvi;

library ieee;
use ieee.std_logic_1164.all;

architecture hzvwj of jcxugcvi is
  signal ygjqcgghn : std_logic_vector(0 to 3);
  signal mrot : std_logic;
  signal gp : time;
  signal ld : std_logic_vector(0 to 3);
  signal leowufywg : std_logic;
  signal sl : time;
begin
  gqwb : entity work.fk
    port map (izvefrmz => sl, sm => leowufywg, psvnib => ld);
  thfhiyyb : entity work.fk
    port map (izvefrmz => gp, sm => mrot, psvnib => ygjqcgghn);
  
  -- Single-driven assignments
  vblwljbzh <= 21443.0_4_1_2;
  
  -- Multi-driven assignments
  mrot <= 'L';
  leowufywg <= 'X';
  mrot <= '0';
end hzvwj;

entity vimufcjr is
  port (be : linkage time; tvvgec : in integer; jyogcjmeu : in boolean; fnbb : buffer real);
end vimufcjr;

library ieee;
use ieee.std_logic_1164.all;

architecture oxtmlebq of vimufcjr is
  signal eaplvwz : std_logic;
  signal gvibnpuk : real_vector(4 to 2);
  signal dy : integer;
  signal ewidpqnbz : std_logic_vector(0 to 3);
  signal itvf : time;
  signal xstzzypnc : real_vector(4 to 2);
  signal zptlokvuu : integer;
  signal xzvaqrsa : std_logic_vector(0 to 3);
  signal ervwqxsbdb : std_logic;
  signal plttlwrp : time;
begin
  eldr : entity work.fk
    port map (izvefrmz => plttlwrp, sm => ervwqxsbdb, psvnib => xzvaqrsa);
  gnbsbqy : entity work.qnlozs
    port map (mvd => zptlokvuu, teskyc => xstzzypnc, hglqhycrlf => ervwqxsbdb);
  gfoc : entity work.fk
    port map (izvefrmz => itvf, sm => ervwqxsbdb, psvnib => ewidpqnbz);
  yqozxdoml : entity work.qnlozs
    port map (mvd => dy, teskyc => gvibnpuk, hglqhycrlf => eaplvwz);
  
  -- Single-driven assignments
  fnbb <= 8#2774.1_2_2_7#;
  
  -- Multi-driven assignments
  ewidpqnbz <= ('Z', 'Z', 'Z', '1');
  ewidpqnbz <= "--10";
  eaplvwz <= '0';
  eaplvwz <= '1';
end oxtmlebq;



-- Seed after: 6830028913315670649,3108530264173481209
