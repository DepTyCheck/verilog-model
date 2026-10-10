-- Seed: 9998793682483968909,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (t : inout std_logic; xgwzahrgi : out real; b : buffer std_logic);
end e;

architecture ka of e is
  
begin
  -- Single-driven assignments
  xgwzahrgi <= xgwzahrgi;
  
  -- Multi-driven assignments
  t <= b;
end ka;

library ieee;
use ieee.std_logic_1164.all;

entity dmhsskcw is
  port (jjywohkg : linkage string(5 downto 5); lclbqo : out std_logic; laqsgso : linkage std_logic_vector(1 to 3));
end dmhsskcw;

library ieee;
use ieee.std_logic_1164.all;

architecture qexztywm of dmhsskcw is
  signal j : std_logic;
  signal jsb : real;
  signal hwpi : std_logic;
  signal kyirhrvhb : real;
  signal jexsunju : std_logic;
  signal pwzdyv : real;
  signal fa : std_logic;
  signal inoxvfglym : std_logic;
  signal hlfivzsr : real;
  signal ihhm : std_logic;
begin
  xbdvaarg : entity work.e
    port map (t => ihhm, xgwzahrgi => hlfivzsr, b => inoxvfglym);
  ztfphc : entity work.e
    port map (t => fa, xgwzahrgi => pwzdyv, b => jexsunju);
  l : entity work.e
    port map (t => fa, xgwzahrgi => kyirhrvhb, b => hwpi);
  hhczhyq : entity work.e
    port map (t => jexsunju, xgwzahrgi => jsb, b => j);
  
  -- Multi-driven assignments
  lclbqo <= 'L';
  inoxvfglym <= lclbqo;
  lclbqo <= 'W';
  jexsunju <= fa;
end qexztywm;

entity n is
  port (hzygso : linkage bit);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture mp of n is
  signal gr : std_logic_vector(1 to 3);
  signal zb : std_logic;
  signal ygx : string(5 downto 5);
begin
  tmlpnngl : entity work.dmhsskcw
    port map (jjywohkg => ygx, lclbqo => zb, laqsgso => gr);
  
  -- Multi-driven assignments
  zb <= zb;
  gr <= gr;
end mp;

entity ltzbt is
  port (anjyhm : inout bit; cqm : in bit; qmfpparqg : buffer time);
end ltzbt;

library ieee;
use ieee.std_logic_1164.all;

architecture kwhqm of ltzbt is
  signal qabaorzywx : std_logic_vector(1 to 3);
  signal o : std_logic;
  signal torzz : string(5 downto 5);
begin
  hthgoydz : entity work.dmhsskcw
    port map (jjywohkg => torzz, lclbqo => o, laqsgso => qabaorzywx);
  
  -- Single-driven assignments
  qmfpparqg <= qmfpparqg;
  anjyhm <= anjyhm;
  
  -- Multi-driven assignments
  o <= o;
  o <= 'L';
end kwhqm;



-- Seed after: 13170725412786099142,511364357853360275
