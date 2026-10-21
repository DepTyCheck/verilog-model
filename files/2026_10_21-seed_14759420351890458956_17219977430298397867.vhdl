-- Seed: 14759420351890458956,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity jvbrkj is
  port (msupory : inout time; gzlaf : linkage std_logic; ag : linkage std_logic);
end jvbrkj;

architecture jxo of jvbrkj is
  
begin
  -- Single-driven assignments
  msupory <= msupory;
end jxo;

library ieee;
use ieee.std_logic_1164.all;

entity wlccrdc is
  port (xnjavkvpwd : in std_logic);
end wlccrdc;

library ieee;
use ieee.std_logic_1164.all;

architecture txomp of wlccrdc is
  signal ulono : time;
  signal xhwrc : std_logic;
  signal kyg : time;
begin
  wdyhera : entity work.jvbrkj
    port map (msupory => kyg, gzlaf => xnjavkvpwd, ag => xhwrc);
  wmf : entity work.jvbrkj
    port map (msupory => ulono, gzlaf => xnjavkvpwd, ag => xnjavkvpwd);
  
  -- Multi-driven assignments
  xhwrc <= '1';
end txomp;

library ieee;
use ieee.std_logic_1164.all;

entity ro is
  port (prqxr : in std_logic_vector(1 downto 3); nu : buffer integer; lrgxxqprfl : in std_logic; rimipmw : inout std_logic);
end ro;

library ieee;
use ieee.std_logic_1164.all;

architecture csryxg of ro is
  signal svoxbkeh : std_logic;
  signal uhngigh : time;
begin
  seigdztz : entity work.jvbrkj
    port map (msupory => uhngigh, gzlaf => rimipmw, ag => svoxbkeh);
  
  -- Single-driven assignments
  nu <= 8#5#;
  
  -- Multi-driven assignments
  rimipmw <= 'U';
  svoxbkeh <= 'X';
end csryxg;

entity z is
  port (wj : out bit);
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture ecdmwd of z is
  signal cfvmglv : std_logic;
  signal m : std_logic;
  signal oewqy : time;
  signal pmb : integer;
  signal ywetzl : std_logic;
  signal hsbckrjj : integer;
  signal oi : std_logic_vector(1 downto 3);
begin
  kxfmncxlla : entity work.ro
    port map (prqxr => oi, nu => hsbckrjj, lrgxxqprfl => ywetzl, rimipmw => ywetzl);
  fzxeqgft : entity work.ro
    port map (prqxr => oi, nu => pmb, lrgxxqprfl => ywetzl, rimipmw => ywetzl);
  tgdvmdjg : entity work.jvbrkj
    port map (msupory => oewqy, gzlaf => m, ag => cfvmglv);
  
  -- Multi-driven assignments
  m <= ywetzl;
  oi <= "";
  oi <= (others => '0');
end ecdmwd;



-- Seed after: 12851465989560602938,17219977430298397867
