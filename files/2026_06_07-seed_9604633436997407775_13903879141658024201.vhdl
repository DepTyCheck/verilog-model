-- Seed: 9604633436997407775,13903879141658024201



entity wwypoec is
  port (fn : in integer; i : out bit);
end wwypoec;



architecture kg of wwypoec is
  
begin
  
end kg;

library ieee;
use ieee.std_logic_1164.all;

entity fzsd is
  port (gjw : in std_logic; l : in std_logic_vector(4 downto 3); ol : buffer real; ggd : inout time);
end fzsd;



architecture olcnhj of fzsd is
  
begin
  
end olcnhj;



entity ytsprxpbe is
  port (opfe : buffer boolean_vector(1 downto 4));
end ytsprxpbe;

library ieee;
use ieee.std_logic_1164.all;

architecture qkj of ytsprxpbe is
  signal y : time;
  signal lcukq : real;
  signal xgac : std_logic_vector(4 downto 3);
  signal mm : std_logic;
begin
  u : entity work.fzsd
    port map (gjw => mm, l => xgac, ol => lcukq, ggd => y);
end qkj;



entity gcgsljyr is
  port (iwwhmoxjj : out boolean_vector(3 downto 3));
end gcgsljyr;



architecture thq of gcgsljyr is
  signal xzyysxv : bit;
  signal yc : bit;
  signal uhrmqm : bit;
  signal jajprfw : integer;
  signal dtwk : boolean_vector(1 downto 4);
begin
  ufsjbdtdhv : entity work.ytsprxpbe
    port map (opfe => dtwk);
  g : entity work.wwypoec
    port map (fn => jajprfw, i => uhrmqm);
  y : entity work.wwypoec
    port map (fn => jajprfw, i => yc);
  tpcwtqfmp : entity work.wwypoec
    port map (fn => jajprfw, i => xzyysxv);
end thq;



-- Seed after: 11991979559733876485,13903879141658024201
