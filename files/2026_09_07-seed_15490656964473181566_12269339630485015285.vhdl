-- Seed: 15490656964473181566,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity lyomtb is
  port (mtbletfpip : in real; a : buffer std_logic);
end lyomtb;

architecture qnv of lyomtb is
  
begin
  
end qnv;

entity kpnamajxl is
  port (inquixdl : in time; iyqkbrzgr : buffer time);
end kpnamajxl;

library ieee;
use ieee.std_logic_1164.all;

architecture zoixm of kpnamajxl is
  signal qvg : real;
  signal plgivoojqf : std_logic;
  signal ruiuwmb : std_logic;
  signal ealvckk : real;
begin
  wdzijkiq : entity work.lyomtb
    port map (mtbletfpip => ealvckk, a => ruiuwmb);
  mzr : entity work.lyomtb
    port map (mtbletfpip => ealvckk, a => plgivoojqf);
  l : entity work.lyomtb
    port map (mtbletfpip => qvg, a => ruiuwmb);
  
  -- Single-driven assignments
  iyqkbrzgr <= 40002.2_4 fs;
  qvg <= ealvckk;
  ealvckk <= ealvckk;
  
  -- Multi-driven assignments
  ruiuwmb <= 'U';
end zoixm;

library ieee;
use ieee.std_logic_1164.all;

entity rzwqy is
  port (no : out std_logic; a : out real; lr : out std_logic_vector(4 downto 4));
end rzwqy;

library ieee;
use ieee.std_logic_1164.all;

architecture emgtrpn of rzwqy is
  signal rooazte : time;
  signal hgg : std_logic;
  signal hdjvtbim : real;
begin
  sivwicwu : entity work.lyomtb
    port map (mtbletfpip => a, a => no);
  cd : entity work.lyomtb
    port map (mtbletfpip => hdjvtbim, a => hgg);
  goregxofv : entity work.kpnamajxl
    port map (inquixdl => rooazte, iyqkbrzgr => rooazte);
  
  -- Single-driven assignments
  hdjvtbim <= a;
  
  -- Multi-driven assignments
  lr <= "-";
  lr <= lr;
end emgtrpn;



-- Seed after: 9243566832658721081,12269339630485015285
