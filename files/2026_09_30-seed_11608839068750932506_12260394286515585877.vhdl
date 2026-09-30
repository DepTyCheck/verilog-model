-- Seed: 11608839068750932506,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity gdql is
  port (kgffchu : buffer std_logic_vector(0 downto 0); od : in std_logic; en : linkage boolean_vector(2 downto 3); j : in time);
end gdql;

architecture f of gdql is
  
begin
  -- Multi-driven assignments
  kgffchu <= kgffchu;
end f;

library ieee;
use ieee.std_logic_1164.all;

entity rcjv is
  port (oeutj : buffer integer_vector(2 downto 3); lrqpmgtnip : out integer; o : inout real; l : out std_logic_vector(0 downto 1));
end rcjv;

library ieee;
use ieee.std_logic_1164.all;

architecture eigxjuceec of rcjv is
  signal jlsncbtirb : time;
  signal fbaie : boolean_vector(2 downto 3);
  signal emngv : std_logic;
  signal pxc : time;
  signal yiksku : boolean_vector(2 downto 3);
  signal aoroja : std_logic;
  signal devjhzixb : std_logic_vector(0 downto 0);
begin
  jtognz : entity work.gdql
    port map (kgffchu => devjhzixb, od => aoroja, en => yiksku, j => pxc);
  uknacjidwm : entity work.gdql
    port map (kgffchu => devjhzixb, od => emngv, en => fbaie, j => jlsncbtirb);
end eigxjuceec;

library ieee;
use ieee.std_logic_1164.all;

entity epeyhevaah is
  port (k : buffer std_logic_vector(1 to 0); cxbmh : in integer_vector(2 downto 3); nznr : in time);
end epeyhevaah;

library ieee;
use ieee.std_logic_1164.all;

architecture xxtonacbad of epeyhevaah is
  signal ois : std_logic_vector(0 downto 1);
  signal dxssh : real;
  signal huoswjtyp : integer;
  signal uby : integer_vector(2 downto 3);
  signal d : boolean_vector(2 downto 3);
  signal orfwmro : time;
  signal ottnzuwl : boolean_vector(2 downto 3);
  signal uauqpdjbw : std_logic;
  signal zqotg : std_logic_vector(0 downto 0);
begin
  mqxziu : entity work.gdql
    port map (kgffchu => zqotg, od => uauqpdjbw, en => ottnzuwl, j => orfwmro);
  uund : entity work.gdql
    port map (kgffchu => zqotg, od => uauqpdjbw, en => d, j => nznr);
  ay : entity work.rcjv
    port map (oeutj => uby, lrqpmgtnip => huoswjtyp, o => dxssh, l => ois);
  
  -- Single-driven assignments
  orfwmro <= nznr;
  
  -- Multi-driven assignments
  k <= k;
end xxtonacbad;



-- Seed after: 7790508977704810816,12260394286515585877
