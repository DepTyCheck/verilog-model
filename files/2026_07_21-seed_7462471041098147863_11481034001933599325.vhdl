-- Seed: 7462471041098147863,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity iwrhcolmb is
  port (wakxaujpw : linkage integer; hdajohfz : in std_logic_vector(0 to 1); bjzl : out bit);
end iwrhcolmb;

architecture ytxwrsd of iwrhcolmb is
  
begin
  -- Single-driven assignments
  bjzl <= '1';
end ytxwrsd;

entity zhmw is
  port (dphzhxhq : buffer integer);
end zhmw;

library ieee;
use ieee.std_logic_1164.all;

architecture usbievc of zhmw is
  signal cefjwsvu : bit;
  signal ng : std_logic_vector(0 to 1);
  signal um : integer;
  signal gkqwqdtt : bit;
  signal p : integer;
  signal zahsbtdxu : bit;
  signal gmuton : std_logic_vector(0 to 1);
  signal kax : bit;
  signal ajxjwuh : std_logic_vector(0 to 1);
  signal miysffd : integer;
begin
  zslwsafis : entity work.iwrhcolmb
    port map (wakxaujpw => miysffd, hdajohfz => ajxjwuh, bjzl => kax);
  bitwyk : entity work.iwrhcolmb
    port map (wakxaujpw => dphzhxhq, hdajohfz => gmuton, bjzl => zahsbtdxu);
  ibngdki : entity work.iwrhcolmb
    port map (wakxaujpw => p, hdajohfz => gmuton, bjzl => gkqwqdtt);
  srcisy : entity work.iwrhcolmb
    port map (wakxaujpw => um, hdajohfz => ng, bjzl => cefjwsvu);
  
  -- Multi-driven assignments
  ajxjwuh <= "Z-";
  ajxjwuh <= ('X', 'X');
  ajxjwuh <= gmuton;
  ajxjwuh <= ('Z', 'X');
end usbievc;

library ieee;
use ieee.std_logic_1164.all;

entity wsidkvtwbf is
  port (aj : out real; eeh : linkage integer; qldibf : out integer; obznarenw : buffer std_logic);
end wsidkvtwbf;

library ieee;
use ieee.std_logic_1164.all;

architecture yvdcmt of wsidkvtwbf is
  signal kza : bit;
  signal hjlja : bit;
  signal yhzqwmhm : std_logic_vector(0 to 1);
  signal ulccu : integer;
begin
  ao : entity work.iwrhcolmb
    port map (wakxaujpw => ulccu, hdajohfz => yhzqwmhm, bjzl => hjlja);
  mn : entity work.iwrhcolmb
    port map (wakxaujpw => qldibf, hdajohfz => yhzqwmhm, bjzl => kza);
  
  -- Multi-driven assignments
  yhzqwmhm <= "HZ";
end yvdcmt;

library ieee;
use ieee.std_logic_1164.all;

entity tsrsiaa is
  port (pho : in std_logic; e : inout boolean; amcqxgzfkj : linkage real_vector(3 to 2); nifggquuh : out bit);
end tsrsiaa;

library ieee;
use ieee.std_logic_1164.all;

architecture tgulxjs of tsrsiaa is
  signal gf : std_logic;
  signal hmc : integer;
  signal y : integer;
  signal hpwdn : real;
begin
  t : entity work.wsidkvtwbf
    port map (aj => hpwdn, eeh => y, qldibf => hmc, obznarenw => gf);
  
  -- Single-driven assignments
  nifggquuh <= '1';
  e <= TRUE;
  
  -- Multi-driven assignments
  gf <= 'H';
  gf <= gf;
  gf <= pho;
end tgulxjs;



-- Seed after: 18427396487607593623,11481034001933599325
