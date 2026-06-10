-- Seed: 15251158444980561236,5415160250146859793

library ieee;
use ieee.std_logic_1164.all;

entity vsqpua is
  port (tbmjg : in real; rjhijhh : out time; yh : in real; mp : buffer std_logic_vector(3 downto 4));
end vsqpua;



architecture ksnemxjtu of vsqpua is
  
begin
  
end ksnemxjtu;

library ieee;
use ieee.std_logic_1164.all;

entity ibr is
  port (fn : inout std_logic; cvxincga : out time; ipu : out severity_level; tjwms : out boolean);
end ibr;

library ieee;
use ieee.std_logic_1164.all;

architecture rpun of ibr is
  signal ibp : std_logic_vector(3 downto 4);
  signal amteugoiu : real;
  signal pex : real;
  signal j : std_logic_vector(3 downto 4);
  signal ojwtcudhl : time;
  signal uqt : real;
begin
  zvc : entity work.vsqpua
    port map (tbmjg => uqt, rjhijhh => ojwtcudhl, yh => uqt, mp => j);
  fsuznpg : entity work.vsqpua
    port map (tbmjg => pex, rjhijhh => cvxincga, yh => amteugoiu, mp => ibp);
end rpun;

library ieee;
use ieee.std_logic_1164.all;

entity lzqku is
  port (uhp : in std_logic_vector(1 to 3); fdtfprvg : linkage real);
end lzqku;

library ieee;
use ieee.std_logic_1164.all;

architecture bvs of lzqku is
  signal zjxppyfpfw : boolean;
  signal ytmjasx : severity_level;
  signal w : time;
  signal uvmamolhzk : std_logic;
  signal dbhxsrypn : real;
  signal sbpiypjjbz : time;
  signal kqdw : real;
  signal qreexpudhj : std_logic_vector(3 downto 4);
  signal qjdmairsz : time;
  signal ava : real;
begin
  ehlpvugbcr : entity work.vsqpua
    port map (tbmjg => ava, rjhijhh => qjdmairsz, yh => ava, mp => qreexpudhj);
  rtqnxotgir : entity work.vsqpua
    port map (tbmjg => kqdw, rjhijhh => sbpiypjjbz, yh => dbhxsrypn, mp => qreexpudhj);
  d : entity work.ibr
    port map (fn => uvmamolhzk, cvxincga => w, ipu => ytmjasx, tjwms => zjxppyfpfw);
end bvs;



entity sr is
  port (dknjguqu : inout time);
end sr;

library ieee;
use ieee.std_logic_1164.all;

architecture leotqpxu of sr is
  signal atp : std_logic_vector(1 to 3);
  signal l : boolean;
  signal t : severity_level;
  signal lbz : std_logic;
  signal us : std_logic_vector(3 downto 4);
  signal gxdz : real;
  signal vtjzs : time;
  signal nar : real;
begin
  vfipd : entity work.vsqpua
    port map (tbmjg => nar, rjhijhh => vtjzs, yh => gxdz, mp => us);
  dwjrsrknl : entity work.ibr
    port map (fn => lbz, cvxincga => dknjguqu, ipu => t, tjwms => l);
  dskdohmj : entity work.lzqku
    port map (uhp => atp, fdtfprvg => gxdz);
  aeeks : entity work.lzqku
    port map (uhp => atp, fdtfprvg => gxdz);
end leotqpxu;



-- Seed after: 1971271622979823967,5415160250146859793
