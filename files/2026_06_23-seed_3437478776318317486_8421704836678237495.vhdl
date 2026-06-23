-- Seed: 3437478776318317486,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity opi is
  port (wsntbnp : in std_logic_vector(2 to 4); inypfjtech : inout bit);
end opi;

architecture wvrmnvnzp of opi is
  
begin
  -- Single-driven assignments
  inypfjtech <= '0';
end wvrmnvnzp;

entity kwnavuldb is
  port (u : buffer integer_vector(1 downto 4));
end kwnavuldb;

architecture eqaayelb of kwnavuldb is
  
begin
  
end eqaayelb;

library ieee;
use ieee.std_logic_1164.all;

entity hu is
  port (wpyf : in bit_vector(3 to 4); mdn : buffer std_logic_vector(1 to 3); wvucm : out time_vector(1 to 4));
end hu;

library ieee;
use ieee.std_logic_1164.all;

architecture qidhk of hu is
  signal aoap : bit;
  signal ph : std_logic_vector(2 to 4);
  signal c : integer_vector(1 downto 4);
  signal hxrh : bit;
  signal gkd : bit;
begin
  xgp : entity work.opi
    port map (wsntbnp => mdn, inypfjtech => gkd);
  um : entity work.opi
    port map (wsntbnp => mdn, inypfjtech => hxrh);
  viveqrpiw : entity work.kwnavuldb
    port map (u => c);
  v : entity work.opi
    port map (wsntbnp => ph, inypfjtech => aoap);
  
  -- Single-driven assignments
  wvucm <= (2#1_0# ps, 0.1_0 ms, 16#F7C50# ms, 3 sec);
  
  -- Multi-driven assignments
  mdn <= "-00";
  mdn <= ('1', 'H', 'X');
  mdn <= "LX-";
  mdn <= "H0X";
end qidhk;

entity xdgldjzp is
  port (tfuwfsg : inout real_vector(2 to 1));
end xdgldjzp;

library ieee;
use ieee.std_logic_1164.all;

architecture sirf of xdgldjzp is
  signal ukprbxqf : integer_vector(1 downto 4);
  signal qxtsyxivt : bit;
  signal oernzfurd : std_logic_vector(2 to 4);
begin
  p : entity work.opi
    port map (wsntbnp => oernzfurd, inypfjtech => qxtsyxivt);
  n : entity work.kwnavuldb
    port map (u => ukprbxqf);
  
  -- Single-driven assignments
  tfuwfsg <= (others => 0.0);
  
  -- Multi-driven assignments
  oernzfurd <= "HLX";
  oernzfurd <= "1ZX";
  oernzfurd <= "1-U";
  oernzfurd <= "HZ0";
end sirf;



-- Seed after: 17467814733365447495,8421704836678237495
