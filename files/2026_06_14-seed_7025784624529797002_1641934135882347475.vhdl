-- Seed: 7025784624529797002,1641934135882347475

library ieee;
use ieee.std_logic_1164.all;

entity wydw is
  port (ggbtcbotfd : inout std_logic; frsd : inout boolean);
end wydw;



architecture ijviphhn of wydw is
  
begin
  
end ijviphhn;



entity jgbvrafaun is
  port (mjgiaa : inout bit; gviq : out severity_level);
end jgbvrafaun;

library ieee;
use ieee.std_logic_1164.all;

architecture lwyqzqubt of jgbvrafaun is
  signal fwhonf : boolean;
  signal xexex : boolean;
  signal lweiq : boolean;
  signal fmijlc : boolean;
  signal jtkmocnle : std_logic;
begin
  h : entity work.wydw
    port map (ggbtcbotfd => jtkmocnle, frsd => fmijlc);
  ronkn : entity work.wydw
    port map (ggbtcbotfd => jtkmocnle, frsd => lweiq);
  yplhkw : entity work.wydw
    port map (ggbtcbotfd => jtkmocnle, frsd => xexex);
  bg : entity work.wydw
    port map (ggbtcbotfd => jtkmocnle, frsd => fwhonf);
end lwyqzqubt;

library ieee;
use ieee.std_logic_1164.all;

entity yygzlylkd is
  port (tluvgysh : inout severity_level; vqliebbmo : in boolean_vector(4 downto 3); pc : out std_logic_vector(0 to 4); aimlcy : linkage integer);
end yygzlylkd;

library ieee;
use ieee.std_logic_1164.all;

architecture wo of yygzlylkd is
  signal y : boolean;
  signal crbs : std_logic;
  signal umxc : severity_level;
  signal cmzdqhc : bit;
  signal abqondoo : bit;
  signal bmqv : severity_level;
  signal u : bit;
begin
  h : entity work.jgbvrafaun
    port map (mjgiaa => u, gviq => bmqv);
  vkoxdyo : entity work.jgbvrafaun
    port map (mjgiaa => abqondoo, gviq => tluvgysh);
  bmmxv : entity work.jgbvrafaun
    port map (mjgiaa => cmzdqhc, gviq => umxc);
  dwghbz : entity work.wydw
    port map (ggbtcbotfd => crbs, frsd => y);
end wo;



entity f is
  port (xr : out integer_vector(4 downto 3); ecwefy : linkage time; toqofmi : linkage bit; avcuykzwrn : inout integer_vector(0 downto 2));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture icnwqkwtec of f is
  signal jzxbapf : boolean;
  signal yikfj : std_logic;
  signal eaqaydq : integer;
  signal vprleahzt : std_logic_vector(0 to 4);
  signal rtgj : boolean_vector(4 downto 3);
  signal l : severity_level;
  signal llaaucwy : severity_level;
  signal abh : bit;
begin
  c : entity work.jgbvrafaun
    port map (mjgiaa => abh, gviq => llaaucwy);
  nqbpcz : entity work.yygzlylkd
    port map (tluvgysh => l, vqliebbmo => rtgj, pc => vprleahzt, aimlcy => eaqaydq);
  x : entity work.wydw
    port map (ggbtcbotfd => yikfj, frsd => jzxbapf);
end icnwqkwtec;



-- Seed after: 12490002415741492255,1641934135882347475
