-- Seed: 3450767483013538208,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (vytsdbk : out bit; lukgw : buffer boolean_vector(1 to 1); sfp : linkage integer; d : inout std_logic);
end c;



architecture kimmqgmvnn of c is
  
begin
  
end kimmqgmvnn;



entity dvdpcxh is
  port (z : out boolean_vector(4 to 0); pgummw : out real; bbeyygmzky : out bit_vector(1 downto 3); av : out severity_level);
end dvdpcxh;

library ieee;
use ieee.std_logic_1164.all;

architecture wtbafphffs of dvdpcxh is
  signal urvvs : std_logic;
  signal thlvbofmg : boolean_vector(1 to 1);
  signal muzbhzeph : bit;
  signal vkaqult : std_logic;
  signal lmyii : integer;
  signal vvrtewnzo : boolean_vector(1 to 1);
  signal eumymld : bit;
begin
  donfz : entity work.c
    port map (vytsdbk => eumymld, lukgw => vvrtewnzo, sfp => lmyii, d => vkaqult);
  fjvvbmu : entity work.c
    port map (vytsdbk => muzbhzeph, lukgw => thlvbofmg, sfp => lmyii, d => urvvs);
end wtbafphffs;



entity awba is
  port (pieyeng : linkage integer; mxkkzn : linkage time);
end awba;



architecture wsfwirlg of awba is
  signal yzsvn : severity_level;
  signal lg : bit_vector(1 downto 3);
  signal qllddx : real;
  signal hrlgdpsapw : boolean_vector(4 to 0);
begin
  fsdfhy : entity work.dvdpcxh
    port map (z => hrlgdpsapw, pgummw => qllddx, bbeyygmzky => lg, av => yzsvn);
end wsfwirlg;

library ieee;
use ieee.std_logic_1164.all;

entity he is
  port (tkiifogr : inout time; caloxjea : out std_logic_vector(2 to 1));
end he;

library ieee;
use ieee.std_logic_1164.all;

architecture iycctx of he is
  signal ewbn : integer;
  signal s : boolean_vector(1 to 1);
  signal a : bit;
  signal dp : std_logic;
  signal smzbndqop : integer;
  signal qtyaxzdku : boolean_vector(1 to 1);
  signal ov : bit;
begin
  flxtzpb : entity work.c
    port map (vytsdbk => ov, lukgw => qtyaxzdku, sfp => smzbndqop, d => dp);
  syopzvfn : entity work.c
    port map (vytsdbk => a, lukgw => s, sfp => smzbndqop, d => dp);
  jl : entity work.awba
    port map (pieyeng => ewbn, mxkkzn => tkiifogr);
end iycctx;



-- Seed after: 8375154270058519865,18238119570016518405
