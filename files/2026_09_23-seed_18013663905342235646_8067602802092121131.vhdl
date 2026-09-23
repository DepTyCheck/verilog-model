-- Seed: 18013663905342235646,8067602802092121131

entity dhwcob is
  port (n : out real);
end dhwcob;

architecture hj of dhwcob is
  
begin
  -- Single-driven assignments
  n <= 2#0_0_0.1001#;
end hj;

library ieee;
use ieee.std_logic_1164.all;

entity renkg is
  port (m : linkage time; kdllwyg : linkage std_logic);
end renkg;

architecture qt of renkg is
  signal wiav : real;
  signal vwp : real;
  signal dfe : real;
begin
  qzqzwjhddu : entity work.dhwcob
    port map (n => dfe);
  xbssqizswy : entity work.dhwcob
    port map (n => vwp);
  pwflmvctug : entity work.dhwcob
    port map (n => wiav);
end qt;

library ieee;
use ieee.std_logic_1164.all;

entity dotbqtlozs is
  port (eymxcx : out bit_vector(3 to 4); nkmwti : buffer std_logic; mzlgonkbzr : in std_logic; tokdexbng : in time);
end dotbqtlozs;

architecture mbgcx of dotbqtlozs is
  signal ajmv : real;
  signal mytp : real;
begin
  pwtvdo : entity work.dhwcob
    port map (n => mytp);
  sczzvd : entity work.dhwcob
    port map (n => ajmv);
  
  -- Single-driven assignments
  eymxcx <= ('0', '1');
  
  -- Multi-driven assignments
  nkmwti <= '1';
  nkmwti <= '0';
  nkmwti <= 'X';
  nkmwti <= mzlgonkbzr;
end mbgcx;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (cwp : inout std_logic_vector(3 downto 0); kimdj : inout real);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture zxzn of g is
  signal ubsmszat : std_logic;
  signal mtmptoirch : time;
  signal dhhspdq : std_logic;
  signal ubdsqcvo : time;
begin
  jsvur : entity work.dhwcob
    port map (n => kimdj);
  km : entity work.renkg
    port map (m => ubdsqcvo, kdllwyg => dhhspdq);
  coysi : entity work.renkg
    port map (m => mtmptoirch, kdllwyg => ubsmszat);
end zxzn;



-- Seed after: 3949526353487448463,8067602802092121131
