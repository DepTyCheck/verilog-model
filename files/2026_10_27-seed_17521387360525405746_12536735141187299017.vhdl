-- Seed: 17521387360525405746,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity xbyttr is
  port (kvykvcv : out std_logic; vebf : in std_logic_vector(2 downto 1); g : in std_logic);
end xbyttr;

architecture t of xbyttr is
  
begin
  -- Multi-driven assignments
  kvykvcv <= g;
  kvykvcv <= g;
  kvykvcv <= 'H';
  kvykvcv <= kvykvcv;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity wcgm is
  port (wgwjhjil : out integer; bhvm : inout std_logic_vector(3 to 2));
end wcgm;

library ieee;
use ieee.std_logic_1164.all;

architecture dwxhjzdos of wcgm is
  signal niiflzhox : std_logic;
  signal s : std_logic_vector(2 downto 1);
  signal wxi : std_logic;
begin
  hysxjmljxb : entity work.xbyttr
    port map (kvykvcv => wxi, vebf => s, g => niiflzhox);
  
  -- Single-driven assignments
  wgwjhjil <= 16#3_2_1#;
  
  -- Multi-driven assignments
  bhvm <= bhvm;
  bhvm <= "";
end dwxhjzdos;

entity jkfdipfhfi is
  port (jd : inout time);
end jkfdipfhfi;

library ieee;
use ieee.std_logic_1164.all;

architecture vdqdqbw of jkfdipfhfi is
  signal ietwgilu : std_logic;
  signal ycxstrj : std_logic;
  signal jofwsl : std_logic_vector(2 downto 1);
  signal routqxgmzh : std_logic;
begin
  yixerg : entity work.xbyttr
    port map (kvykvcv => routqxgmzh, vebf => jofwsl, g => ycxstrj);
  qgvrhn : entity work.xbyttr
    port map (kvykvcv => ietwgilu, vebf => jofwsl, g => routqxgmzh);
  
  -- Single-driven assignments
  jd <= jd;
end vdqdqbw;

library ieee;
use ieee.std_logic_1164.all;

entity czyerftp is
  port (xllunmmouq : buffer time; cdoxi : inout std_logic_vector(2 downto 4); wagubjbr : buffer integer);
end czyerftp;

library ieee;
use ieee.std_logic_1164.all;

architecture aaudnnv of czyerftp is
  signal ksavr : std_logic_vector(2 downto 1);
  signal ankqdhleau : std_logic;
begin
  hcuxsmsew : entity work.xbyttr
    port map (kvykvcv => ankqdhleau, vebf => ksavr, g => ankqdhleau);
  
  -- Single-driven assignments
  wagubjbr <= wagubjbr;
end aaudnnv;



-- Seed after: 14111666213557583673,12536735141187299017
