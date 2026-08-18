-- Seed: 3121892082715783098,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity ho is
  port (vuizhcer : in real_vector(4 to 4); mqglt : inout std_logic; fsrfhadt : out integer; ppcraiganj : inout time);
end ho;

architecture evkafwgua of ho is
  
begin
  -- Single-driven assignments
  ppcraiganj <= ppcraiganj;
  fsrfhadt <= fsrfhadt;
end evkafwgua;

library ieee;
use ieee.std_logic_1164.all;

entity ywejh is
  port (vwl : out std_logic; lovwern : inout bit_vector(2 to 2); egveiy : linkage std_logic; jjzzxvt : buffer time);
end ywejh;

library ieee;
use ieee.std_logic_1164.all;

architecture xh of ywejh is
  signal ralxbqn : integer;
  signal mipi : real_vector(4 to 4);
  signal knp : time;
  signal wf : integer;
  signal bqiyi : std_logic;
  signal ltzacyky : real_vector(4 to 4);
  signal oz : time;
  signal nwmjet : integer;
  signal vflynpwt : time;
  signal td : integer;
  signal jhv : std_logic;
  signal pceun : real_vector(4 to 4);
begin
  l : entity work.ho
    port map (vuizhcer => pceun, mqglt => jhv, fsrfhadt => td, ppcraiganj => vflynpwt);
  lsxo : entity work.ho
    port map (vuizhcer => pceun, mqglt => jhv, fsrfhadt => nwmjet, ppcraiganj => oz);
  tcw : entity work.ho
    port map (vuizhcer => ltzacyky, mqglt => bqiyi, fsrfhadt => wf, ppcraiganj => knp);
  vcbul : entity work.ho
    port map (vuizhcer => mipi, mqglt => vwl, fsrfhadt => ralxbqn, ppcraiganj => jjzzxvt);
  
  -- Single-driven assignments
  lovwern <= lovwern;
  
  -- Multi-driven assignments
  vwl <= 'L';
end xh;

library ieee;
use ieee.std_logic_1164.all;

entity pyqlfmlbml is
  port (dtsbu : inout std_logic);
end pyqlfmlbml;

architecture ay of pyqlfmlbml is
  signal rzg : time;
  signal wgm : bit_vector(2 to 2);
begin
  vitqd : entity work.ywejh
    port map (vwl => dtsbu, lovwern => wgm, egveiy => dtsbu, jjzzxvt => rzg);
  
  -- Multi-driven assignments
  dtsbu <= '0';
  dtsbu <= 'L';
  dtsbu <= 'H';
end ay;



-- Seed after: 7616251913581045883,5983430343285687595
