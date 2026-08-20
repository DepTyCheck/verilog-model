-- Seed: 15055999135913234674,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity eca is
  port (w : out std_logic; hyyz : buffer time; kehff : out severity_level; jmlsy : inout std_logic_vector(1 to 3));
end eca;

architecture n of eca is
  
begin
  -- Single-driven assignments
  kehff <= kehff;
  hyyz <= hyyz;
  
  -- Multi-driven assignments
  w <= w;
  w <= 'U';
  jmlsy <= "111";
end n;

entity xaau is
  port (xhqversbyb : linkage integer; hnmwrwkrhr : linkage integer_vector(2 downto 0); ztvbrosb : in time; vegonzvy : in time);
end xaau;

library ieee;
use ieee.std_logic_1164.all;

architecture evqwvcjy of xaau is
  signal jxses : severity_level;
  signal bgrhtbbq : time;
  signal pwuaq : std_logic_vector(1 to 3);
  signal slnncqqh : severity_level;
  signal pec : time;
  signal syvhcqewfg : std_logic;
begin
  ngaz : entity work.eca
    port map (w => syvhcqewfg, hyyz => pec, kehff => slnncqqh, jmlsy => pwuaq);
  llgibxqqfk : entity work.eca
    port map (w => syvhcqewfg, hyyz => bgrhtbbq, kehff => jxses, jmlsy => pwuaq);
  
  -- Multi-driven assignments
  pwuaq <= ('W', '-', 'U');
  syvhcqewfg <= syvhcqewfg;
  syvhcqewfg <= syvhcqewfg;
  syvhcqewfg <= syvhcqewfg;
end evqwvcjy;

entity n is
  port (gbmtsdev : buffer string(1 to 1); k : out time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture rvzqacmu of n is
  signal hbguplvh : std_logic_vector(1 to 3);
  signal reo : severity_level;
  signal matipq : std_logic;
  signal lqahsvat : integer_vector(2 downto 0);
  signal jlqmaryur : integer;
  signal ihjoxvw : std_logic_vector(1 to 3);
  signal grshw : severity_level;
  signal zwhxftbvee : time;
  signal pauddbtyc : std_logic;
begin
  bqjrihko : entity work.eca
    port map (w => pauddbtyc, hyyz => zwhxftbvee, kehff => grshw, jmlsy => ihjoxvw);
  plj : entity work.xaau
    port map (xhqversbyb => jlqmaryur, hnmwrwkrhr => lqahsvat, ztvbrosb => zwhxftbvee, vegonzvy => k);
  ansj : entity work.eca
    port map (w => matipq, hyyz => k, kehff => reo, jmlsy => hbguplvh);
  
  -- Single-driven assignments
  gbmtsdev <= gbmtsdev;
  
  -- Multi-driven assignments
  matipq <= 'H';
end rvzqacmu;



-- Seed after: 12581185732769781798,499459191852795575
