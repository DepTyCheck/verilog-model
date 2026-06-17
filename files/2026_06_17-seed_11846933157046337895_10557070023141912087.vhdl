-- Seed: 11846933157046337895,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity ppymjhh is
  port (niiacmzker : linkage std_logic; gilc : linkage real; wbvdelnh : linkage severity_level; vfbk : inout time);
end ppymjhh;

architecture l of ppymjhh is
  
begin
  -- Single-driven assignments
  vfbk <= 2#0_1_0_0.01# fs;
end l;

library ieee;
use ieee.std_logic_1164.all;

entity tvgbwanmcq is
  port (zzkbchxntk : out real; cxiajv : inout std_logic; wrzwllc : in std_logic);
end tvgbwanmcq;

architecture fv of tvgbwanmcq is
  
begin
  -- Single-driven assignments
  zzkbchxntk <= 2#1.10#;
end fv;

library ieee;
use ieee.std_logic_1164.all;

entity hxbhhyzss is
  port (sxerudvy : inout std_logic_vector(3 downto 3); mcvozfke : in severity_level; psfduw : in std_logic; mm : linkage character);
end hxbhhyzss;

library ieee;
use ieee.std_logic_1164.all;

architecture bbqbs of hxbhhyzss is
  signal ppqdfr : time;
  signal oj : severity_level;
  signal feyxhbr : real;
  signal hiwmsqx : std_logic;
  signal hssi : real;
  signal lah : std_logic;
  signal cexjrgast : std_logic;
  signal b : real;
begin
  mztrharzpa : entity work.tvgbwanmcq
    port map (zzkbchxntk => b, cxiajv => cexjrgast, wrzwllc => lah);
  hjceavcvz : entity work.tvgbwanmcq
    port map (zzkbchxntk => hssi, cxiajv => lah, wrzwllc => lah);
  wvuducpza : entity work.ppymjhh
    port map (niiacmzker => hiwmsqx, gilc => feyxhbr, wbvdelnh => oj, vfbk => ppqdfr);
  
  -- Multi-driven assignments
  hiwmsqx <= '-';
  lah <= 'L';
end bbqbs;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (np : buffer integer; xtwgpmusf : inout std_logic_vector(3 downto 2); du : in integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture yze of h is
  signal otvipx : time;
  signal bfhqtho : real;
  signal hybhv : std_logic;
  signal nxxwhs : time;
  signal qy : severity_level;
  signal djc : real;
  signal lxzi : character;
  signal ony : severity_level;
  signal vobcuqco : std_logic_vector(3 downto 3);
  signal jzzk : std_logic;
  signal fglf : std_logic;
  signal cemio : real;
begin
  vqmvonrwvh : entity work.tvgbwanmcq
    port map (zzkbchxntk => cemio, cxiajv => fglf, wrzwllc => jzzk);
  ldrd : entity work.hxbhhyzss
    port map (sxerudvy => vobcuqco, mcvozfke => ony, psfduw => jzzk, mm => lxzi);
  zaqp : entity work.ppymjhh
    port map (niiacmzker => fglf, gilc => djc, wbvdelnh => qy, vfbk => nxxwhs);
  ily : entity work.ppymjhh
    port map (niiacmzker => hybhv, gilc => bfhqtho, wbvdelnh => ony, vfbk => otvipx);
  
  -- Single-driven assignments
  np <= 1;
  
  -- Multi-driven assignments
  xtwgpmusf <= "00";
end yze;



-- Seed after: 17906579562782929357,10557070023141912087
