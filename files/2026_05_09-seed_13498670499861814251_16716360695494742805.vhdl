-- Seed: 13498670499861814251,16716360695494742805

library ieee;
use ieee.std_logic_1164.all;

entity flodvbvane is
  port (mshnuungzk : linkage std_logic; zohinpluej : buffer integer; ce : linkage integer; oou : buffer std_logic);
end flodvbvane;



architecture gukvhp of flodvbvane is
  
begin
  
end gukvhp;

library ieee;
use ieee.std_logic_1164.all;

entity mn is
  port (aotqhh : in integer; gmgptwpn : out boolean; xxuw : out std_logic; j : buffer real);
end mn;

library ieee;
use ieee.std_logic_1164.all;

architecture nyqxyznrnp of mn is
  signal iya : integer;
  signal b : std_logic;
  signal rvdtkcse : std_logic;
  signal hp : integer;
  signal luv : std_logic;
  signal ofgmu : integer;
  signal vafi : integer;
begin
  t : entity work.flodvbvane
    port map (mshnuungzk => xxuw, zohinpluej => vafi, ce => ofgmu, oou => luv);
  yista : entity work.flodvbvane
    port map (mshnuungzk => xxuw, zohinpluej => hp, ce => aotqhh, oou => rvdtkcse);
  e : entity work.flodvbvane
    port map (mshnuungzk => b, zohinpluej => iya, ce => vafi, oou => xxuw);
end nyqxyznrnp;

library ieee;
use ieee.std_logic_1164.all;

entity hxpcsffnkw is
  port (qgokhmus : linkage std_logic; nz : inout integer; zlmdzssmu : buffer std_logic);
end hxpcsffnkw;



architecture wrwsklkrzr of hxpcsffnkw is
  
begin
  
end wrwsklkrzr;

library ieee;
use ieee.std_logic_1164.all;

entity ssedzbnngn is
  port (rpj : inout time; vbzjbuadmx : out real; pp : in std_logic);
end ssedzbnngn;

library ieee;
use ieee.std_logic_1164.all;

architecture pc of ssedzbnngn is
  signal lz : std_logic;
  signal sd : integer;
  signal fzejp : integer;
  signal ragqzajt : std_logic;
begin
  kspsc : entity work.hxpcsffnkw
    port map (qgokhmus => ragqzajt, nz => fzejp, zlmdzssmu => ragqzajt);
  i : entity work.flodvbvane
    port map (mshnuungzk => pp, zohinpluej => sd, ce => fzejp, oou => lz);
end pc;



-- Seed after: 15134729288731122530,16716360695494742805
